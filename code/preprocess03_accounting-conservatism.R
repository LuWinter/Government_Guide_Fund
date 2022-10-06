
##############################################################
# Programmer: Lu Winter
# Date Created: 2022-08-28
# Task: According to Basu(1997) and Khan(2009), 
#       calculate accounting conservatism variable
##############################################################


# 0. Initial Setup -------------------------------------------------------------

## 加载R包
library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(lubridate)
library(purrr)
library(DBI)

## 预定义输入输出路径
data_path <- "data"
output_path <- "output"
db_path <- "data/GGF_project_store.sqlite"

## 建立数据库连接
con_sqlite <- dbConnect(RSQLite::SQLite(), db_path)
dbListTables(con_sqlite)


# 1. 处理上市公司基本面数据 ----------------------------------------------------------

# 1.1 处理上市公司基本信息数据
identifier<- dbReadTable(con_sqlite, "identifier")

# 1.2 处理上市公司年股票回报率数据
### 数据来源：CSMAR股票市场 - 股票市场交易 - 年个股回报率文件
### 数据属性：2000-2021年度 全部股票代码 17个变量 53302条观测
year_return <- read_xlsx(file.path(data_path, "stock_return/2022-08-09_year-return.xlsx"))
year_return <- year_return %>% 
  mutate(Year = as.numeric(交易年份)) %>% 
  filter(Year >= 2010) %>% 
  select(Stkcd = 证券代码, Year, YearClose = 年收盘价, 
         YearOpen = 年开盘价, MarketValue = 年个股总市值, 
         YRet = 考虑现金红利再投资的年个股回报率) 
head(year_return)

# # 1.3 合并上一年度收盘价
# year_return_back <- year_return[, c("Stkcd", "Year", "YearClose")]
# year_return <- year_return %>% 
#   filter(Year >= 2010) %>% 
#   left_join(year_return_back, 
#             by = c("Stkcd", "LastYear" = "Year")) %>% 
#   rename(LastYearClose = YearClose.y,
#          YearClose = YearClose.x)
# rm(year_return_back)
### 38602个观测值中有3128个未匹配到上年度收盘价
### 这是因为新上市造成的，因此下面用年度开盘价作为替代 (取消)

# 1.4 计算个股年回报率和超额回报率
# year_return <-  year_return %>%
#   mutate(LastYearClose = ifelse(is.na(LastYearClose), YearOpen, LastYearClose),
#          Ret = ifelse(is.na(YearReturn), YearClose / LastYearClose - 1, YearReturn))


# 2. 累积月份数据计算股票年收益 --------------------------------------------------------
### 数据来源：CSMAR股票市场 - 股票市场交易 - 月个股回报率文件
### 数据属性：2010.1-2022.9年度 全部股票代码 6个变量 477863条观测
month_return <- 
  read_csv(file.path(data_path, "stock_return/2022-10-02_month-return.csv"))
colnames(month_return)
month_return <- month_return %>% 
  mutate(Year = as.numeric(str_extract(Trdmnt, "^[0-9]{4}")),
         Month = as.numeric(str_extract(Trdmnt, "[0-9]{2}$"))) %>% 
  mutate(NewYear = ifelse(Month < 5, Year - 1, Year)) %>% 
  filter(NewYear >= 2010, NewYear <= 2021) %>% 
  group_by(Stkcd, NewYear) %>% 
  summarise(Ret = prod(Mretwd + 1) - 1) %>% 
  ungroup()

### 数据来源：CSMAR股票市场 - 综合市场交易数据 — 综合月市场回报率文件
### 数据属性：20110.1-2022.9年度 全部市场代码 3个变量 2907条观测
month_market_return <- 
  read_csv(file.path(data_path, "stock_return/2022-10-02_month-market-return.csv"))
head(month_market_return)
month_market_return <- month_market_return %>% 
  filter(Markettype == 53) %>% 
  mutate(Year = as.numeric(str_extract(Trdmnt, "^[0-9]{4}")),
         Month = as.numeric(str_extract(Trdmnt, "[0-9]{2}$"))) %>% 
  mutate(NewYear = ifelse(Month < 5, Year - 1, Year)) %>% 
  group_by(NewYear) %>% 
  filter(n() == 12) %>% 
  summarise(MarketRet = prod(Cmretwdos + 1) - 1) %>% 
  ungroup()

month_return <- month_return %>% 
  left_join(month_market_return, by = c("NewYear")) %>% 
  mutate(adjRet = Ret - MarketRet)
head(month_return)


# 3. 合并所有数据 ---------------------------------------------------------------

### 数据来源：CSMAR公司研究 - 财务指标分析 - 每股指标
### 数据属性：2010-2021年度 全部股票代码 5个变量 92652条观测
earning_per_share <- read_csv(file.path(data_path, "2022-10-02_earning-per-share.csv"))
earning_per_share <- earning_per_share %>% 
  filter(Typrep == "A") %>% 
  mutate(Year = year(Accper)) %>% 
  select(Stkcd, Year, EPS = F090101B)

### 数据来源：CSMAR公司研究 - 财务报表 - 资产负债表 & 利润表 (未筛选)
### 数据属性：2000-2021年度 全部股票代码 9个变量 498247条观测
financial_sheet <- read_xlsx(file.path(data_path, "Financial_Sheet/2022-08-09_financial-sheets.xlsx"))
financial_sheet <- financial_sheet %>% 
  filter(month(会计期间) == 12, 报表类型 == "A", year(会计期间) >= 2010) %>% 
  mutate(Year = year(会计期间)) %>% 
  select(Stkcd = 证券代码, Year, 负债合计, 资产总计)

merged_for_khan <- identifier %>% 
  left_join(financial_sheet, by = c("Stkcd", "Year")) %>% 
  left_join(year_return, by = c("Stkcd", "Year")) %>% 
  left_join(month_return, by = c("Stkcd", "Year" = "NewYear")) %>% 
  left_join(earning_per_share, by = c("Stkcd", "Year")) %>% 
  mutate(DR = as.numeric(Ret < 0),
         adjDR = as.numeric(adjRet < 0),
         Size = log(资产总计),
         Lev = 负债合计 / 资产总计,
         MB = MarketValue / 资产总计 * 1000) %>% 
  select(Stkcd, Year, EPS, Ret, adjRet, DR, adjDR,
         Size, Lev, MB, YearOpen, YearClose)

pmap_lgl(merged_for_khan, .f = \(...) sum(is.na(list(...))) == 0) %>% table()
### 缺失数据样本2536个，无缺失样本33087个，一共35623个样本
merged_for_khan <- merged_for_khan[pmap_lgl(merged_for_khan, .f = \(...) sum(is.na(list(...))) == 0), ]


# 4. 计算会计稳健性 --------------------------------------------------------------

# 2.1 计算公式
formula_basu <- formula(EPS / YearOpen ~ DR + Ret + DR:Ret)
formula_khan <- formula(EPS / YearOpen ~ DR + 
                          Ret + Size:Ret + MB:Ret + Lev:Ret + 
                          DR:Ret + DR:Size:Ret + DR:MB:Ret + DR:Lev:Ret)
formula_khan_adj <- formula(EPS / YearOpen ~ adjDR + 
                              adjRet + Size:adjRet + MB:adjRet + Lev:adjRet + 
                              adjDR:adjRet + adjDR:Size:adjRet + adjDR:MB:adjRet + adjDR:Lev:adjRet)
### Reference: 李争光, 赵西卜, 曹丰, & 刘向强. (2015). 机构投资者异质性与
### 会计稳健性——来自中国上市公司的经验证据. 南开管理评论, 18(03), 111–121.
coef_res <- map(c(2010:2021), \(x) coef(
  lm(formula = formula_khan_adj, data = subset(merged_for_khan, Year == x))
))
coef_res2 <- tibble(
  Year = 2010:2021,
  a1 = map_dbl(coef_res, 3),
  a2 = map_dbl(coef_res, 4),
  a3 = map_dbl(coef_res, 5),
  a4 = map_dbl(coef_res, 6),
  b1 = map_dbl(coef_res, 7),
  b2 = map_dbl(coef_res, 8),
  b3 = map_dbl(coef_res, 9),
  b4 = map_dbl(coef_res, 10)
)
head(coef_res2)
accounting_conservatism <- merged_for_khan %>% 
  left_join(coef_res2, by = c("Year")) %>% 
  mutate(G_Score = a1 + a2 * Size + a3 * MB + a4 * Lev,
         C_Score = b1 + b2 * Size + b3 * MB + b4 * Lev) %>% 
  select(-matches("[ab][0-9]"))
rm(coef_res, coef_res2)

# 2.2 保存计算结果
rio::export(
  x = accounting_conservatism,
  file = file.path(output_path, "accounting_conservatism.dta")
)
dbWriteTable(
  conn = con_sqlite,
  name = "accounting_conservatism_adj",
  value = accounting_conservatism,
  overwrite = TRUE
)
dbDisconnect(con_sqlite)


