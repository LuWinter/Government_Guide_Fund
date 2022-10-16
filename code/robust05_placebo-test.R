
##############################################################
# Programmer: Lu Winter
# Date Created: 2022-10-09
# Task: Input preprocessed data and perform analysis
##############################################################


# 0. Initial Setup -------------------------------------------------------------

## 加载R包
library(dplyr)
library(RStata)
library(purrr)
library(DBI)
library(lubridate)
library(stringr)
library(fixest)
library(furrr)
library(ggplot2)
library(echarts4r)

## 预定义输入输出路径
data_path <- "data"
output_path <- "output"
db_path <- "data/GGF_project_store.sqlite"

## 建立数据库连接
con_sqlite <- dbConnect(RSQLite::SQLite(), db_path)
dbListTables(con_sqlite)

# Stata设置
options("RStata.StataPath" = "/Applications/Stata/StataSE.app/Contents/MacOS/stata-se")
options("RStata.StataVersion" = 17)


# 1. Perform Test ---------------------------------------------------------

# merged_for_reg <- readRDS(file.path(output_path, "merged-for-reg_2022-10-07.rds"))
merged_for_reg_reduced <- readRDS(file.path(output_path, "merged-for-reg-reduced_2022-10-07.rds"))

placebo_data <- merged_for_reg_reduced %>% 
  filter(Year >= 2016) %>% 
  mutate(
    EPS_P = EPS / YearOpen,
    Industry = ifelse(str_sub(IndustryCode, 1, 1) != "C", 
                      str_sub(IndustryCode, 1, 1), 
                      str_sub(IndustryCode, 1, 2))
  )

stata(
  src = "code/robust01_simple-reg.do",
  data.in = filter(merged_for_reg_reduced, Year >= 2016)
)

feols(
  fml = EPS_P ~ DR*Ret*GGF + DR*Ret*Size + DR*Ret*Lev + DR*Ret*MHRatio + DR*Ret*Age + DR*Ret*GDP_p | 
    Year + Industry + Province + Year^Industry + Year^Province, 
  data = placebo_data, 
  vcov = "iid"
) %>% coeftable()

plan(multisession, workers = 6)
tictoc::tic()
future_map(
  .x = 1:5000, 
  .f = \(x) {
    placebo_data$FakeGGF <- rbinom(n = 15642, size = 1, prob = 620/15642)
    res <- coeftable(feols(
      EPS_P ~ DR*Ret*FakeGGF + DR*Ret*Size + DR*Ret*Lev + DR*Ret*MHRatio + DR*Ret*Age + DR*Ret*GDP_p | 
        Year + Industry + Province + Year^Industry + Year^Province,  
      data = placebo_data, 
      vcov = "iid"
    ))
    res["DR:Ret:FakeGGF", c("Estimate", "t value", "Pr(>|t|)")]
  },
  .options = furrr_options(seed = TRUE)
) -> placebo_result_full
tictoc::toc()

placebo_result_df <- reduce(
  .x = placebo_result_full, 
  .f = bind_rows
)


# 2. Plot Output ----------------------------------------------------------

### t值 
ggplot(data = placebo_result_df) +
  geom_density(aes(x = `t value`)) +
  geom_vline(xintercept = 2.08) +
  theme_light()

### 系数核密度
ggplot(data = placebo_result_df) +
  geom_density(aes(x = Estimate)) +
  geom_vline(xintercept = 0, linetype = 4) +
  geom_vline(xintercept = 0.07385598, linetype = 1) +
  theme_light() +
  labs(x = "Estimate Coeffcient", y = "Density")

### 
ggplot(data = placebo_result_df) +
  geom_point(aes(x = Estimate, y = `Pr(>|t|)`), shape = 20) +
  geom_hline(yintercept = 0.05, linetype = 4) +
  theme_light() +
  labs(x = "Estimate Coeffcient", y = "P Value")

merged_for_reg_reduced %>% 
  mutate(Year = Year + 1) %>% 
  group_by(Year) %>% 
  summarise(
    n = sum(GGF), 
    r = n / n(),
    min = 0.02
  ) %>% 
  mutate(Year = factor(Year)) %>% 
  e_charts(x = Year) %>% 
  e_bar(serie = n, legend = FALSE) %>% 
  e_line(serie = r, legend = FALSE, y_index = 1) %>% 
  e_line(serie = min, legend = FALSE, y_index = 1)


