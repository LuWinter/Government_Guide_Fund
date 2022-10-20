
##############################################################
# Programmer: Lu Winter
# Date Created: 2022-10-20
# Task: Check other conservatism models
##############################################################


# 0. Initial Setup -------------------------------------------------------------

library(dplyr)
library(RStata)
library(purrr)
library(DBI)
library(lubridate)
library(fixest)
library(stringr)
source("code/func_tools.R")

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


# 1. Prepare Data ---------------------------------------------------------

identifier <- dbReadTable(
  conn = con_sqlite,
  name = "identifier"
)
merged_for_reg <- readRDS(
  file = file.path(output_path, "merged-for-reg_2022-10-07.rds")
)
merged_for_reg_reduced <- readRDS(
  file = file.path(output_path, "merged-for-reg-reduced_2022-10-07.rds")
)

con_fin <- dbConnect(
  drv = RSQLite::SQLite(), 
  "../Common/Financial-Sheets/financial_sheets.sqlite"
)
dbListTables(con_fin)

balance_sheet_db <- tbl(con_fin, "balance_sheet")
income_statement_db <- tbl(con_fin, "income_statement")
cash_flow_db <- tbl(con_fin, "cash_flow_statement")

robust_data <- balance_sheet_db %>% 
  left_join(
    y = income_statement_db,
    by = c("Stkcd", "Year")
  ) %>% 
  left_join(
    y = cash_flow_db,
    by = c("Stkcd", "Year")
  ) %>% 
  select(
    Stkcd, 
    Year,
    NetProfit,
    TotalAsset,
    NetOperatingCF
  ) %>% 
  collect()

robust_data <- identifier %>% 
  left_join(
    y = robust_data,
    by = c("Stkcd", "Year")
  ) %>% 
  lag_n_year(
    n = 1, 
    key = "Stkcd", 
    value = c("NetProfit", "TotalAsset"), 
    by = "Year"
  ) %>% 
  mutate(
    NI = (NetProfit - NetProfit_lag1) / TotalAsset_lag1,
    Accrual = (NetProfit - NetOperatingCF) / TotalAsset_lag1,
    CFO = NetOperatingCF / TotalAsset_lag1,
    DNI = ifelse(NI < 0, 1, 0),
    DCFO = ifelse(CFO < 0, 1, 0)
  ) %>% 
  select(
    Stkcd,
    Year,
    NI,
    Accrual,
    CFO,
    DNI,
    DCFO
  )


# 2. Perform Test ---------------------------------------------------------

merged_for_reg_reduced %>% 
  left_join(
    y = robust_data,
    by = c("Stkcd", "Year")
  ) %>% 
  lag_n_year(
    n = 1, 
    key = "Stkcd", 
    value = c("NI", "DNI", "C_Score", "G_Score"), 
    by = "Year"
  ) %>% 
  filter(Year >= 2016) %>% 
  stata(
    src = "code/robust04_C-score.do",
    data.in = .
  )

dbDisconnect(con_sqlite)
dbDisconnect(con_fin)

