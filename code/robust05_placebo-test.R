
##############################################################
# Programmer: Lu Winter
# Date Created: 2022-10-09
# Task: Perform placebo test
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
  ) %>% 
  stata(
    src = "winsor2 Size Lev GDP_p MHRatio Age, cuts(1 99) by(Year) replace", 
    data.in = .,
    data.out = TRUE
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
    placebo_data$FakeGGF <- rbinom(n = nrow(placebo_data), size = 1, prob = 620/nrow(placebo_data))
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
names(placebo_result_df) <- c("Estimate", "t_value", "p_value")
placebo_result_df <- placebo_result_df %>% 
  mutate(
    norm_t = rnorm(5000, mean(t_value), sd(t_value)),
    norm_est = rnorm(5000, mean(Estimate), sd(Estimate))
  )

rio::export(placebo_result_df, "output/placebo-test_2022-11-04.dta")


# 2. Plot Output ----------------------------------------------------------

### t值 
ggplot(data = placebo_result_df) +
  geom_density(aes(x = t_value)) +
  geom_density(aes(x = norm_t)) +
  geom_vline(xintercept = 2.08) +
  theme_light()

### 系数核密度
ggplot(data = placebo_result_df) +
  geom_density(aes(x = Estimate), linetype = 1) +
  geom_density(aes(x = norm_est), linetype = 4) +
  geom_vline(xintercept = 0, linetype = 4) +
  geom_vline(xintercept = 0.07385598, linetype = 1) +
  theme_light() +  
  labs(x = "Estimate Coeffcient", y = "Density")

estimate_long <- tibble(
  n = 1:5000,
  `Placebo Test` = placebo_result_df$Estimate,
  `Normal Distribution` = placebo_result_df$norm_est
) %>% 
  tidyr::pivot_longer(
    cols = c("Placebo Test", "Normal Distribution"),
    names_to = "LineType"
  )
ggplot(data = estimate_long, group = LineType) +
  geom_density(aes(x = value, linetype = LineType), adjust = 2.5) +
  geom_vline(xintercept = 0, linetype = 9) +
  geom_vline(xintercept = 0.07385598, linetype = 4, show.legend = T) +
  theme_light() +
  labs(x = "Estimate Coeffcient", y = "Density")

dbDisconnect(con_sqlite)
