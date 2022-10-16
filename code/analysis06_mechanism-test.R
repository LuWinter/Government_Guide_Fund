
##############################################################
# Programmer: Lu Winter
# Date Created: 2022-10-06
# Task: Explore how GGF can influence accounting conservatism
##############################################################


# 0. Initial Setup --------------------------------------------------------

## 加载R包
library(readr)
library(dplyr)
library(DBI)
library(RStata)

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


# 1. GGF保本压力 --------------------------------------------------------------

### 数据来源：CSMAR公司研究 - 财务报表 - 资产负债表 & 利润表 (筛选出年报)
### 数据属性：2001-2021年度 全部股票代码 13个变量 104531条观测
### 输出变量：ROA
financial_sheet_more <- read_csv(file.path(data_path, "Financial_Sheet/2022-10-03_financial-sheets.csv"))
colnames(financial_sheet_more) <- c(
  "Stkcd", "Accper", "Type", "FixedAsset", "IntangibleAsset",
  "TotalAsset", "TotalLiability", "ShareEquity", "TotalRevenue",
  "Revenue", "SalesCost", "NetProfit", "RDCost"
)
financial_sheet_more <- financial_sheet_more %>% 
  filter(Type == "A") %>% 
  mutate(Year = lubridate::year(Accper)) %>% 
  select(Stkcd, Year, NetProfit, TotalAsset) %>% 
  mutate(ROA = NetProfit / TotalAsset, .keep = "unused")

merged_for_reg_reduced <- 
  readRDS(file.path(output_path, "merged-for-reg-reduced_2022-10-03.rds"))

financial_sheet_more$Year <- financial_sheet_more$Year + 1
merged_for_reg_reduced <- merged_for_reg_reduced %>% 
  left_join(
    y = financial_sheet_more, 
    by = c("Stkcd", "Year")
  ) %>% 
  mutate(IsLoss = ifelse(ROA < 0, 1, 0))

count(merged_for_reg_reduced, Year, GGF, IsLoss)

stata(
  src = "code/robust01_simple-reg.do",
  data.in = filter(
    merged_for_reg_reduced, Year >= 2016, IsLoss == 1
  )
)
### 结果发现，上一年度已经亏损的企业，如果被引导基金支持，
### 其会计稳健性不升反降，这是为什么呢？


# 2. GGF治理效应 --------------------------------------------------------------

cg_score <- dbReadTable(
  conn = con_sqlite, 
  name = "control-variables_2022-10-03"
) %>% 
  select(Stkcd, Year, CG)

merged_for_reg_reduced <- 
  readRDS(file.path(output_path, "merged-for-reg-reduced_2022-10-03.rds"))

merged_for_reg_reduced <- merged_for_reg_reduced %>% 
  select(-CG) %>% 
  left_join(
    y = cg_score, 
    by = c("Stkcd", "Year")
  )


stata(
  src = "code/robust01_simple-reg.do",
  data.in = filter(merged_for_reg_reduced, Year >= 2012)
)




