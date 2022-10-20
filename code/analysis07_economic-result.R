
##############################################################
# Programmer: Lu Winter
# Date Created: 2022-10-15
# Task: Explore economic result of GGF holding
##############################################################


# 0. Initial Setup --------------------------------------------------------

## 加载R包
library(readr)
library(dplyr)
library(DBI)
library(readxl)
library(RStata)
library(stringr)
library(purrr)
library(furrr)
library(tidyr)
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


# 1. Stock Price Crash Risk -----------------------------------------------

identifier <- dbReadTable(
  conn = con_sqlite,
  name = "identifier"
)
identifier_info <- dbReadTable(
  conn = con_sqlite,
  name = "identifier_info"
)

## 读取并处理市场周回报数据
week_market_return <- 
  read_csv(file.path(data_path, "stock_return/2022-10-16_week-market-return.csv"))
week_market_return <- week_market_return %>% 
  mutate(
    Year = as.numeric(str_sub(Trdwnt, 1, 4)),
    Week = as.numeric(str_sub(Trdwnt, 6, 7))
  ) %>% 
  filter(Markettype == 53, Year >= 2000) %>% 
  select(Year, Week, MarketReturn = Cwretwdos) %>% 
  mutate(nweek = row_number())
  
## 读取并处理个股周回报数据
week_return <- 
  read_csv(file.path(data_path, "stock_return/2022-10-16_week-return_part01.csv"))
week_return_plus <- 
  read_csv(file.path(data_path, "stock_return/2022-10-16_week-return_part02.csv")) %>% 
  mutate(Stkcd = as.character(Stkcd))
week_return_plus2 <- 
  read_csv(file.path(data_path, "stock_return/2022-10-16_week-return_part03.csv")) %>% 
  mutate(Stkcd = as.character(Stkcd))

week_return <- bind_rows(week_return, week_return_plus, week_return_plus2)
rm(week_return_plus, week_return_plus2)

identifier$sign <- 1
week_return <- week_return %>% 
  mutate(
    Year = as.numeric(str_sub(Trdwnt, 1, 4)),
    Week = as.numeric(str_sub(Trdwnt, 6, 7))
  ) %>% 
  left_join(identifier, by = c("Stkcd", "Year")) %>% 
  filter(sign == 1) %>% 
  select(Stkcd, Year, Week, Return = Wretwd)
identifier$sign <- NULL

## 合并前后五周的市场收益率
week_return <- week_return %>% 
  left_join(
    y = week_market_return, 
    by = c("Year", "Week")
  ) %>% 
  mutate(mnweek = nweek - 1) %>% 
  left_join(
    y = week_market_return[, 3:4], 
    by = c("mnweek" = "nweek"), 
    suffix = c("", "_b1")
  ) %>% 
  mutate(mnweek = nweek - 2) %>% 
  left_join(
    y = week_market_return[, 3:4], 
    by = c("mnweek" = "nweek"), 
    suffix = c("", "_b2")
  ) %>% 
  mutate(mnweek = nweek + 1) %>% 
  left_join(
    y = week_market_return, 
    by = c("mnweek" = "nweek"), 
    suffix = c("", "_a1")
  ) %>% 
  mutate(mnweek = nweek + 2) %>% 
  left_join(
    y = week_market_return, 
    by = c("mnweek" = "nweek"), 
    suffix = c("", "_a2")
  ) %>% 
  select(Stkcd, Year, Week, nweek, contains("Return"))

week_return_little <- week_return %>% 
  filter(Year >= 2010) %>% 
  arrange(Stkcd, Year, Week) %>% 
  group_by(Stkcd, Year) %>% 
  filter(n() >= 5) %>% 
  ungroup() %>% 
  drop_na()

stkcd_year_list <- distinct(week_return_little, Stkcd, Year)

plan(multisession, workers = 6)
residuals_list <- future_map2(
  .x = stkcd_year_list$Stkcd,
  .y = stkcd_year_list$Year,
  .f = \(x, y) {
    data <- filter(week_return_little, Stkcd == x, Year == y)
    residuals(lm(Return ~ MarketReturn + MarketReturn_a1 + 
       MarketReturn_a2 + MarketReturn_b1 + MarketReturn_b2, 
       data = data))
  })
residuals_vec <- unlist(residuals_list)
week_return_little$w <- log(residuals_vec + 1)

week_return_result <- week_return_little %>% 
  mutate(w = ifelse(is.na(w), -4, w)) %>% 
  group_by(Stkcd, Year) %>% 
  mutate(
    n = n(),
    Up = ifelse(w >= mean(w), 1, 0),
    Down = ifelse(w < mean(w), 1, 0)
  ) %>% 
  summarise(
    SIGMA = sd(w),
    RET = mean(w),
    NCSKEW = -1 * (n * (n-1) ^ 1.5 * sum(w ^ 3)) / ((n-1) * (n-2) * (sum(w ^ 2) ^ 1.5)),
    DUVOL = log(((sum(Up) - 1) * sum((Down * w) ^ 2)) / ((sum(Down) - 1) * sum((Up * w) ^ 2)))
  ) %>% 
  mutate(
    NCSKEW = ifelse(is.infinite(NCSKEW), NA, NCSKEW),
    DUVOL = ifelse(is.infinite(DUVOL), NA, DUVOL)
  ) %>% 
  ungroup() %>% 
  distinct(Stkcd, Year, .keep_all = TRUE) %>% 
  drop_na()

rm(week_return, week_market_return, residuals_list, residuals_vec, stkcd_year_list)


# 2. Earning Quality ------------------------------------------------------

### 数据来源：CSMAR公司研究 - 财务报表 - 资产负债表 & 利润表 (筛选出年报)
### 数据属性：2001-2021年度 全部股票代码 13个变量 104531条观测
financial_sheet_more <- 
  read_csv(file.path(data_path, "Financial_Sheet/2022-10-03_financial-sheets.csv"))
colnames(financial_sheet_more) <- c(
  "Stkcd", "Accper", "Type", "FixedAsset", "IntangibleAsset",
  "TotalAsset", "TotalLiability", "ShareEquity", "TotalRevenue",
  "Revenue", "SalesCost", "NetProfit", "RDCost"
)
financial_sheet_more <- financial_sheet_more %>% 
  filter(Type == "A") %>% 
  mutate(Year = lubridate::year(Accper)) %>% 
  select(
    Stkcd, Year, TotalAsset, FixedAsset, 
    TotalRevenue, Revenue, NetProfit
  )

financial_sheet_more <- financial_sheet_more %>% 
  mutate(
    FixedAsset = ifelse(is.na(FixedAsset), 0, FixedAsset),
    TotalRevenue = ifelse(is.na(TotalRevenue), 0, TotalRevenue),
    Revenue = ifelse(is.na(Revenue), TotalRevenue, Revenue)
  )

receivable_data <- 
  read_csv(file.path(data_path, "Financial_Sheet/2022-10-16_receivable-data.csv"))
receivable_data <- receivable_data %>% 
  filter(Typrep == "A") %>% 
  mutate(
    Year = lubridate::year(Accper),
    A001111000 = ifelse(is.na(A001111000), 0, A001111000)
  ) %>% 
  select(Stkcd, Year, Receivable = A001111000)

operating_cash_flow <- 
  read_csv(file.path(data_path, "Financial_Sheet/2022-10-16_operating-cash-flow.csv"))
operating_cash_flow <- operating_cash_flow %>% 
  filter(Typrep == "A") %>% 
  mutate(
    Year = lubridate::year(Accper),
    C001000000 = ifelse(is.na(C001000000), 0, C001000000)
  ) %>% 
  select(Stkcd, Year, CFO = C001000000)

source("code/func_tools.R")
merged_for_da <- identifier %>% 
  left_join(financial_sheet_more, by = c("Stkcd", "Year")) %>% 
  left_join(receivable_data, by = c("Stkcd", "Year")) %>% 
  left_join(operating_cash_flow, by = c("Stkcd", "Year")) %>% 
  lag_n_year(
    n = 1, key = "Stkcd", by = "Year",
    value = c("Receivable", "Revenue", "TotalAsset")
  ) %>% 
  mutate(
    dSale = (Revenue - Receivable_lag1) / TotalAsset_lag1,
    dAR = (Receivable - Receivable_lag1) / TotalAsset_lag1,
    PPE = FixedAsset / TotalAsset_lag1,
    TA = (NetProfit - CFO) / TotalAsset_lag1,
    ROA = NetProfit / TotalAsset,
    FCF = CFO / Revenue,
    IndustryCode = ifelse(str_sub(IndustryCode, 1, 1) == "C",
                          str_sub(IndustryCode, 1, 2),
                          str_sub(IndustryCode, 1, 1))
  ) %>% 
  select(
    Stkcd, Year, IndustryCode, dSale, dAR, PPE, TA, ROA, FCF
  ) %>% 
  group_by(IndustryCode, Year) %>% 
  filter(n() >= 16) %>% 
  ungroup() %>% 
  drop_na()
rm(financial_sheet_more, receivable_data, operating_cash_flow)
  
industry_year_list <- merged_for_da %>% 
  distinct(IndustryCode, Year)
coef_result <- map2(
    .x = industry_year_list$IndustryCode, 
    .y = industry_year_list$Year, 
    .f = \(x, y) {
      data <- filter(merged_for_da, IndustryCode == x, Year == y) 
      coef <- coef(lm(TA ~ dSale + PPE, data = data))
      c(IndustryCode = x, Year = y, coef)
    }
  ) %>% 
  reduce(bind_rows) %>% 
  mutate(
    Year = as.numeric(Year),
    a0 = as.numeric(`(Intercept)`),
    a1 = as.numeric(dSale),
    a2 = as.numeric(PPE)
  ) %>% 
  select(IndustryCode, Year, a0, a1, a2)

merged_for_da <- merged_for_da %>% 
  left_join(coef_result, by = c("IndustryCode", "Year")) %>% 
  mutate(
    NDA = a0 + a1 * (dSale - dAR) + a2 * PPE,
    DA = TA - NDA,
    ABSDA = abs(DA)
  ) %>% 
  select(Stkcd, Year, ABSDA, FCF, ROA)
rm(industry_year_list, coef_result)


# 3. Tax Avoidance --------------------------------------------------------

nominal_tax <- read_xlsx(file.path(data_path, "2022-10-16_income-tax-ratio.xlsx")) %>% 
  select(Stkcd, Year, TaxRatio) %>% 
  mutate(
    Stkcd = str_pad(Stkcd, width = 6, side = "left", pad = "0"),
    TaxRatio = TaxRatio / 100
  )

real_tax <- read_csv(file.path(data_path, "2022-10-17_calcu-tax.csv"))
colnames(real_tax) <- c("Stkcd", "Date", "Typrep", "DTAsset", "TotalAsset", "DTLiability", "TotalProfit", "IncomeTax")
real_tax <- real_tax %>% 
  mutate(
    Year = lubridate::year(Date),
    DTAsset = ifelse(is.na(DTAsset), 0, DTAsset),
    DTLiability = ifelse(is.na(DTLiability), 0, DTLiability),
    IncomeTax = ifelse(is.na(IncomeTax), 0, IncomeTax)
  ) %>% 
  filter(Typrep == "A") %>% 
  # filter(TotalProfit > 0, IncomeTax > 0) %>%
  mutate(
    DTExpense = DTLiability - DTAsset,
    RealTaxRatio = (IncomeTax - DTExpense) / TotalProfit
  ) %>% 
  select(
    Stkcd, Year, DTExpense, IncomeTax, RealTaxRatio,
    TotalProfit, TotalAsset
  )
tax_avoid <- real_tax %>% 
  # filter(RealTaxRatio > 0, RealTaxRatio < 1) %>%
  left_join(nominal_tax, by = c("Stkcd", "Year")) %>% 
  mutate(ShouldTax = IncomeTax / TaxRatio) %>% 
  mutate(
    TA = TaxRatio - RealTaxRatio,
    BTD = (TotalProfit - ShouldTax) / TotalAsset,
  ) %>% 
  select(Stkcd, Year, TA, BTD)


# 4. Data Merge and Perform Test ------------------------------------------

# merged_Big10SH_GGF_nodupl <- readRDS(file.path(output_path, "merged_Big10SH_GGF_nodupl.rds"))
# merged_Big10SH_GGF_nodupl$Year <- merged_Big10SH_GGF_nodupl$Year - 1
# 
# accounting_conservatism <- rio::import(file = file.path(output_path, "accounting_conservatism.dta"))
# control_variables <- readRDS(file = file.path(output_path, "control-variables_2022-10-07.rds"))
# 
# merged_for_reg <- identifier %>% 
#   left_join(accounting_conservatism, by = c("Stkcd", "Year")) %>% 
#   left_join(merged_Big10SH_GGF_nodupl, by = c("Stkcd", "Year")) %>% 
#   mutate(
#     GGF = ifelse(is.na(GGF), 0, GGF),
#     HoldRatio = ifelse(is.na(HoldRatio), 0, HoldRatio)
#   ) %>% 
#   filter(Year >= 2012) %>% 
#   left_join(control_variables, by = c("Stkcd", "Year", "IndustryCode"))
# 
# merged_for_reg <- merged_for_reg %>% 
#   left_join(
#     y = week_return_result, 
#     by = c("Stkcd", "Year")
#   ) %>% 
#   left_join(
#     y = merged_for_da, 
#     by = c("Stkcd", "Year")
#   )
# 
# merged_for_reg_reduced <- merged_for_reg %>% 
#   group_by(Stkcd) %>% 
#   filter(n() > 2) %>% 
#   ungroup()

# saveRDS(week_return_result, "output/SPCR_2022-10-17.rds")
# saveRDS(merged_for_da, "output/Earning-Quality_2022-10-17.rds")
# saveRDS(tax_avoid, "output/Tax-Avoid_2022-10-17.rds")
week_return_result <- read_rds("output/SPCR_2022-10-17.rds")
merged_for_da <- read_rds("output/Earning-Quality_2022-10-17.rds")
tax_avoid <- read_rds("output/Tax-Avoid_2022-10-17.rds")

PSM_sample <- rio::import("output/PSM-DID_2022-10-17.dta")
PSM_sample <- PSM_sample %>%
  # mutate(Year = Year + 1) %>%
  left_join(
    y = week_return_result,
    by = c("Stkcd", "Year")
  ) %>%
  # mutate(Year = Year - 1) %>%
  left_join(
    y = merged_for_da,
    by = c("Stkcd", "Year")
  ) %>% 
  left_join(
    y = tax_avoid,
    by = c("Stkcd", "Year")
  ) %>% 
  # left_join(
  #   y = identifier_info[c("Stkcd", "Year", "Province")],
  #   by = c("Stkcd", "Year")
  # ) %>% 
  select(- Industry)

stata(
  src = "code/analysis07_economic-result.do", 
  data.in = PSM_sample
)

PSM_sample %>% 
  stata(
    src = "winsor2 ABSDA BTD, cuts(10 99) replace",
    data.in = .,
    data.out = TRUE
  ) %>% 
  mutate(
    BTD = ifelse(is.infinite(BTD), NA, BTD)
  ) %>% 
  drop_na(ABSDA, BTD) %>% 
  group_by(Treat, Time) %>% 
  summarise(
    ABSDA = mean(ABSDA),
    BTD = mean(BTD)
  ) %>% 
  mutate(
    Time_n = ifelse(Time == "a0", 0,
             ifelse(Time == "a1", 1,
             ifelse(Time == "a2", 2,
             ifelse(Time == "b1", -1,
             ifelse(Time == "b2", -2, -3)))))
  ) %>% 
  arrange(Treat, Time_n) %>% 
  filter(Treat == 0) %>% 
  e_charts(x = Time_n) %>% 
  e_line(serie = BTD)

dbDisconnect(con_sqlite)


