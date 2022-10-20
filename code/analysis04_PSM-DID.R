
##############################################################
# Programmer: Lu Winter
# Date Created: 2022-09-29
# Task: Perform PSM-DID design
##############################################################


# 0. Initial Setup --------------------------------------------------------

## 加载R包
library(readr)
library(dplyr)
library(purrr)
library(DBI)
library(lubridate)
library(RStata)
library(stringr)

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


# 1. 收集处理组 ----------------------------------------------------------------

identifier <- dbReadTable(con_sqlite, "identifier")
merged_SH_GGF <- readRDS(file.path(output_path, "merged_Big10SH_GGF.rds"))
accounting_conservatism <- rio::import(file.path(output_path, "accounting_conservatism.dta"))

## 处理行业
identifier <- identifier %>% 
  mutate(
    Industry = ifelse(str_sub(IndustryCode, 1, 1) != "C",
                      str_sub(IndustryCode, 1, 1),
                      str_sub(IndustryCode, 1, 2))
  )

## 所有被持股的样本
merged_SH_GGF_dupl <- merged_SH_GGF |> 
  filter(GGF == 1) |> 
  mutate(Year = year(Reptdt)) |> 
  select(Stkcd, Year, GGF, GGFName = 股东全称3,
         GGFLevel = 基金级别, GGFProvince = 注册省份,
         HoldNum = S0302a, HoldRatio = S0304a, HoldRank = S0306a)

## 如果同一年有多家基金持股，只选持股比例最大的
merged_SH_GGF_nodupl <- merged_SH_GGF_dupl |> 
  group_by(Stkcd, Year) |> 
  filter(HoldRatio == max(HoldRatio)) |> 
  ungroup() |> 
  distinct(Stkcd, Year, .keep_all = TRUE)

## 筛选标准 1. 第一年被持股 2. 前五大股东或持股超过5%
treat_little <- merged_SH_GGF_nodupl |> 
  group_by(Stkcd) |> 
  filter(Year == min(Year), Year >= 2011) |> 
  filter(HoldRatio >= 5 | HoldRank <= 5) |> 
  ungroup()

## 先将little样本扩展到全时间序列
treat_full <- treat_little[, c("Stkcd", "Year")] |> 
  mutate(
    b3 = Year - 3,
    b2 = Year - 2,
    b1 = Year - 1,
    a0 = Year,
    a1 = Year + 1,
    a2 = Year + 2
  ) |> 
  rename(Year_a0 = Year) |> 
  tidyr::pivot_longer(
    cols = b3:a2, 
    names_to = "Time", 
    values_to = "Year"
  ) |> 
  mutate(
    Before = ifelse(Year < Year_a0, 1, 0),
    Post = ifelse(Year >= Year_a0, 1, 0)
  )

accounting_conservatism_little <- accounting_conservatism |>  
  select(Stkcd, Year, EPS, Ret, DR, YearOpen) |> 
  na.omit()

treat_full <- treat_full |> 
  left_join(identifier, by = c("Stkcd", "Year")) |> 
  filter(!is.na(IndustryCode)) |> 
  left_join(
    merged_SH_GGF_nodupl[, c("Stkcd", "Year", "HoldRatio")],
    by = c("Stkcd", "Year")
  ) |> 
  left_join(
    accounting_conservatism_little, 
    by = c("Stkcd", "Year")
  ) |> 
  group_by(Stkcd) |> 
  filter(sum((!is.na(HoldRatio)) * (!is.na(Ret)) * Post) >= 1) |> 
  filter(sum((!is.na(Ret)) * Before) >= 1) |> 
  ungroup() |> 
  filter(!is.na(Ret))


# 2. 匹配控制组 ----------------------------------------------------------------

treat_for_match <- treat_full |> 
  filter(Time == "b1")

### 用于匹配的变量（上一期）
### 1.市值  2.SOE    3.增长率  4.ROA
### 5.LEV   6.研发   7.托宾Q   8.机构持股


## 2.1 合并PSM控制变量

### 数据来源：CSMAR公司研究 - 财务指标分析 - 相对价值指标
### 数据属性：2000-2021年度 全部股票代码 4个变量 50375条观测
### 输出变量：市值MarketValue、托宾Q值TobinQ
corp_value <- read_csv(file.path(data_path, "2022-10-03_corp-value.csv"))
corp_value <- corp_value %>% 
  mutate(Year = year(Accper)) %>% 
  mutate(LOGMV = log(F100801A)) %>% 
  select(Stkcd, Year, LOGMV, TobinQ = F100903A)

### 数据来源：CSMAR公司研究 - 财务报表 - 资产负债表 & 利润表 (筛选出年报)
### 数据属性：2001-2021年度 全部股票代码 13个变量 104531条观测
### 输出变量：ROA、LEV、Growth
financial_sheet_more <- read_csv(file.path(data_path, "Financial_Sheet/2022-10-03_financial-sheets.csv"))
colnames(financial_sheet_more) <- c(
  "Stkcd", "Accper", "Type", "FixedAsset", "IntangibleAsset",
  "TotalAsset", "TotalLiability", "ShareEquity", "TotalRevenue",
  "Revenue", "SalesCost", "NetProfit", "RDCost"
)
financial_sheet_more <- financial_sheet_more %>% 
  filter(Type == "A") %>% 
  mutate(Year = year(Accper)) %>% 
  select(Stkcd, Year, TotalRevenue, Revenue, 
         NetProfit, TotalLiability, TotalAsset)

financial_sheet_more <- financial_sheet_more %>% 
  mutate(
    Revenue = ifelse(is.na(Revenue), TotalRevenue, Revenue)
  ) %>% 
  mutate(
    ROA = NetProfit / TotalAsset,
    LEV = TotalLiability / TotalAsset,
    Growth = Revenue / Revenue
  ) %>% 
  select(
    Stkcd, Year, ROA, LEV, Growth
  )

control_variables <- readRDS(
  file.path(output_path, "control-variables_2022-10-07.rds")
)

# corp_value$Year <- corp_value$Year + 1
# financial_sheet_more$Year <- financial_sheet_more$Year + 1
# control_variables$Year <- control_variables$Year + 1

### 匹配控制组的特征变量
sample_full <- identifier %>% 
  left_join(
    y = corp_value, 
    by = c("Stkcd", "Year")
  ) %>% 
  left_join(
    y = financial_sheet_more, 
    by = c("Stkcd", "Year")
  ) %>% 
  left_join(
    y = control_variables %>% select( 
      Stkcd, Year, RDRatio, INS, SOE), 
    by = c("Stkcd", "Year")
  )
sample_full <- sample_full %>% 
  left_join(
    y = treat_little %>% select(
      Stkcd, Year, GGF),
    by = c("Stkcd", "Year")
  ) %>% 
  mutate(GGF = ifelse(is.na(GGF), 0, GGF)) %>% 
  group_by(Stkcd) %>% 
  mutate(delete_corp = sum(GGF)) %>% 
  filter(delete_corp == 0 | (delete_corp != 0 & GGF == 1)) %>%
  ungroup() %>% 
  select(-delete_corp)
count(sample_full, GGF)

sample_nona <- na.omit(sample_full)
count(sample_nona, GGF)


### 下面计算PSM得分，然后对每个GGF == 1样本，
### 匹配一个同年度、同行业、得分最邻近的GGF==0样本
### Q: 为什么不分年度计算？
### A：大部分年份的GGF == 1样本过少
stata_command_str <- "
gen ROA2 = ROA ^ 2
gen TobinQ2 = TobinQ ^ 2
gen RDRatio2 = RDRatio ^ 2

#delimit ;
pscore GGF LOGMV TobinQ TobinQ2 Growth ROA ROA2
    LEV RDRatio RDRatio2 INS SOE, 
    logit comsup blockid(block) pscore(prop_score) ;
#delimit cr

drop ROA2 TobinQ2 RDRatio2
"

sample_nona <- stata(
  src = stata_command_str, 
  data.in = sample_nona,
  data.out = TRUE
)
colnames(sample_nona)
count(sample_nona, GGF, is.na(prop_score))


treat_sample <- sample_nona %>% 
  filter(GGF == 1)
matched_treat_corp <- treat_sample$Stkcd
matched_treat_full <- 
  treat_full[treat_full$Stkcd %in% matched_treat_corp, ]

control_sample <- sample_nona %>% 
  filter(GGF == 0)

stkcd_vec <- c()
year_vec <- c()
for (i in 1:nrow(treat_sample)) {
  year <- treat_sample[i, "Year"]
  indu <- treat_sample[i, "Industry"]
  score <- treat_sample[i, "prop_score"]
  
  print(paste0("第", i, "个查找: Year = ", year, " Indu = ", indu))
  temp_sample <- control_sample %>% 
    filter(Year == year, Industry == indu)
  index <- which.min(temp_sample$prop_score - score)
  
  target_corp <- temp_sample[index, 1]
  stkcd_vec <- append(stkcd_vec, target_corp)
  year_vec <- append(year_vec, temp_sample[index, 2])
  
  control_sample <- control_sample[!(control_sample$Stkcd %in% target_corp), ]
  control_sample <- control_sample[sample(1:nrow(control_sample), nrow(control_sample), replace = FALSE), ]
  
  rm(year, indu, score, temp_sample, index, target_corp)
}

matched_control_corp <- 
  tibble(Stkcd = stkcd_vec, Year = year_vec)
### 注意，匹配到的都是event year前一年的

matched_control_full <- matched_control_corp %>% 
  mutate(
    b3 = Year - 2,
    b2 = Year - 1,
    b1 = Year,
    a0 = Year + 1,
    a1 = Year + 2,
    a2 = Year + 3
  ) |> 
  mutate(Year_a0 = Year + 1) %>% 
  select(-Year) %>% 
  tidyr::pivot_longer(
    cols = b3:a2, 
    names_to = "Time", 
    values_to = "Year"
  ) %>% 
  mutate(
    Before = ifelse(Year < Year_a0, 1, 0),
    Post = ifelse(Year >= Year_a0, 1, 0)
  )
matched_control_full <- matched_control_full %>% 
  left_join(identifier, by = c("Stkcd", "Year")) |> 
  filter(!is.na(IndustryCode)) |> 
  mutate(HoldRatio = 0) |> 
  left_join(
    accounting_conservatism_little, 
    by = c("Stkcd", "Year")
  ) |> 
  group_by(Stkcd) |> 
  filter(sum((!is.na(Ret)) * Post) >= 1) |> 
  filter(sum((!is.na(Ret)) * Before) >= 1) |> 
  ungroup() |> 
  filter(!is.na(Ret))



# 3. 执行PSM—DID检验 ----------------------------------------------------------

nrow(matched_treat_full)
nrow(matched_control_full)
matched_full <- bind_rows(
  mutate(matched_control_full, Treat = 0),
  mutate(matched_treat_full, Treat = 1)
) %>% 
  mutate(
    Post_Treat = Post * Treat
  )

# matched_full <- 
#   rio::import(file.path(output_path, "PSM-DID_2022-10-06.dta"))

corp_province <- con_sqlite %>% 
  dbReadTable(name = "identifier_info") %>% 
  select(Stkcd, Year, Province)
matched_full <- matched_full %>% 
  left_join(corp_province, by = c("Stkcd", "Year"))
count(matched_full, Post, Treat)

# stata(
#   src = "code/robust03_PSM.do",
#   data.in = matched_full
# )

rio::export(matched_full, file = file.path(output_path, "PSM-DID_2022-10-17.dta"))

dbDisconnect(con_sqlite)



