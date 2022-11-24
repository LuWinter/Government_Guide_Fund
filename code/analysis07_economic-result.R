
##############################################################
# Programmer: Lu Winter
# Date Created: 2022-11-22
# Task: Explore economic result of GGF holding
##############################################################


# 0. Initial Setup --------------------------------------------------------

## 加载R包
library(dplyr)
library(DBI)
library(RStata)
library(fixest)

## 预定义输入输出路径
data_path <- "data"
output_path <- "output"
db_store <- "../Data-for-Accounting-Research/accounting_data.sqlite"

## 建立数据库连接
con_findata <- dbConnect(RSQLite::SQLite(), db_store)
dbListTables(con_findata)

# Stata设置
options("RStata.StataPath" = "/Applications/Stata/StataSE.app/Contents/MacOS/stata-se")
options("RStata.StataVersion" = 17)


# 1. Load Data ------------------------------------------------------------

little_data <- rio::import(file.path(output_path, "PSM-DID_2022-10-17.dta"))

common_vari_db <- tbl(con_findata, "common_variables")
earning_quality_db <- tbl(con_findata, "earning_quality")
tax_avoid_db <- tbl(con_findata, "tax_avoid")
risk_taking_db <- tbl(con_findata, "risk_taking")

common_vari <- common_vari_db %>% 
  select(
    Stkcd,
    Year,
    Size,
    Lev,
    Growth,
    ROA,
    SOE,
    INI,
    Big4,
    Age
  ) %>% 
  collect()

earning_quality <- earning_quality_db %>% 
  select(
    Stkcd,
    Year,
    ABSDA
  ) %>% 
  collect()

tax_avoid <- tax_avoid_db %>% 
  select(
    Stkcd,
    Year,
    TA,
    BT_gap,
    TaxAvoid,
    TaxBurden,
    TaxBurdenAdj
  ) %>% 
  collect()

risk_taking <- risk_taking_db %>% 
  select(
    Stkcd,
    Year,
    RiskTaking
  ) %>% 
  collect()


merged_little_data <- left_join(
  x = little_data,
  y = earning_quality,
  by = c("Stkcd", "Year")
) %>% 
  left_join(
    x = .,
    y = common_vari,
    by = c("Stkcd", "Year")
  ) %>% 
  left_join(
    x = .,
    y = tax_avoid,
    by = c("Stkcd", "Year")
  ) %>% 
  left_join(
    x = .,
    y = risk_taking,
    by = c("Stkcd", "Year")
  ) %>% 
  mutate(
    HoldRatio = ifelse(is.na(HoldRatio), 0, HoldRatio)
  )

feols(
  fml = l(ABSDA, 1) ~ HoldRatio + Size + Lev + Big4 + Growth + ROA |
    Year + Province + Industry,
  data = filter(merged_little_data),
  panel.id = c("Stkcd", "Year"),
  vcov = "iid"
)

stata(
  src = "code/analysis07_economic-result.do",
  data.in = merged_little_data
)

dbDisconnect(con_findata)


