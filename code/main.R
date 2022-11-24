
##############################################################
# Programmer: Lu Winter
# Date Created: 2022-08-30
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


# 1. 合并预处理数据 --------------------------------------------------------------

merged_Big10SH_GGF_nodupl <- readRDS(file.path(output_path, "merged_Big10SH_GGF_nodupl.rds"))
accounting_conservatism <- rio::import(file = file.path(output_path, "accounting_conservatism.dta"))
control_variables <- readRDS(file = file.path(output_path, "control-variables_2022-10-03.rds"))
identifier <- dbReadTable(con_sqlite, "identifier")

# accounting_conservatism <- accounting_conservatism %>%
#   select(-Ret, -DR) %>%
#   rename(Ret = adjRet, DR = adjDR)

control_variables %>% 
  apply(MARGIN = 2, FUN = \(x) sum(is.na(x)))
control_variables <- control_variables %>% 
  filter(if_all(!matches("StrategyScore|RDRatio"), ~ !is.na(.x)))
### 除了StrategyScore外的缺失样本删去

accounting_conservatism <- accounting_conservatism %>% 
  mutate(Year = Year - 1)
### 所有的解释变量都要滞后一期
### 例如：16年的解释变量匹配17年的会计稳健性

merged_for_reg <- identifier %>% 
  left_join(accounting_conservatism, by = c("Stkcd", "Year")) %>% 
  select(
    Stkcd, Year, G_Score, C_Score, EPS, 
    YearOpen, YearClose, Ret, DR, Size, MB, Lev
  ) %>% 
  left_join(merged_Big10SH_GGF_nodupl, by = c("Stkcd", "Year")) %>% 
  mutate(GGF = ifelse(is.na(GGF), 0, 1)) %>% 
  filter(Year >= 2012) %>% 
  left_join(control_variables, by = c("Stkcd", "Year")) %>% 
  mutate(Age = Year - lubridate::year(EstablishDate))

### 检查变量缺失情况
merged_for_reg %>%   
  apply(MARGIN = 2, FUN = \(x) sum(is.na(x)))
merged_for_reg <- merged_for_reg %>% 
  filter(!is.na(CG), !is.na(YearOpen)) 
merged_for_reg %>%   
  apply(MARGIN = 2, FUN = \(x) sum(is.na(x)))

## 变量总计：32
## 标识（2）：Stkcd Year
## 会计稳健性（7）：G_Score C_Score EPS YearOpen YearClose DR Ret
## 引导基金（6）：GovFund GGFLevel GGFProvince HoldNum HoldRatio HoldRank
## 控制变量（14）：Size MB Lev RegionFin SOE Big4 INS SuperINS MHRatio 
## ListingYear GDP_p StrategyScore CG RDRatio 
## 固定效应（3）：IndustryCode Province City
merged_for_reg <- merged_for_reg[, c("Stkcd", "Year", 
     "G_Score", "C_Score", "EPS", "YearOpen", "YearClose", "DR", "Ret",
     "GGF", "GGFLevel", "GGFProvince", "HoldNum", "HoldRatio", "HoldRank",
     "Size", "MB", "Lev", "RegionFin", "SOE", "Big4", "INS", "SuperINS", "MHRatio", 
     "ListingYear", "GDP_p", "StrategyScore", "CG", "Age", "RDRatio",  
     "IndustryCode", "Province", "City")]

merged_for_reg_reduced <- merged_for_reg %>% 
  group_by(Stkcd) %>% 
  filter(n() > 2) %>% 
  ungroup()

saveRDS(
  object = merged_for_reg, 
  file = file.path(output_path, paste0("merged-for-reg_", today(), ".rds"))
)
saveRDS(
  object = merged_for_reg_reduced, 
  file = file.path(output_path, paste0("merged-for-reg-reduced_", today(), ".rds"))
)
# merged_for_reg <- readRDS(file.path(output_path, "merged-for-reg_2022-10-07.rds"))
merged_for_reg_reduced <- readRDS(file.path(output_path, "merged-for-reg-reduced_2022-10-07.rds"))


# 2. 增补的数据 ----------------------------------------------------------------

### 董监高派遣
GGF_related <- readRDS(
  file = "../Data-for-Accounting-Research/data/gov-guide-fund/GGF_related.rds"
)
GGF_related_processed <- GGF_related %>% 
  mutate(
    Related = Director + JianShi
  ) %>% 
  group_by(Stkcd, Year) %>% 
  summarise(Related = sum(Related)) %>% 
  ungroup()
merged_for_reg_reduced <- merged_for_reg_reduced %>% 
  left_join(
    x = .,
    y = GGF_related_processed,
    by = c("Stkcd", "Year")
  )
### 风险规避
return_volatility <- readRDS(
  file = "../Data-for-Accounting-Research/data/2022-11-21_return-volatility.rds"
)
return_volatility <- return_volatility %>% 
  select(
    Stkcd,
    Year,
    contains("sd")
  )
merged_for_reg_reduced <- merged_for_reg_reduced %>% 
  left_join(
    x = .,
    y = return_volatility,
    by = c("Stkcd", "Year") 
  )
### 公司战略得分（重新制作）
corp_strategy <- readRDS(
  file = "../Data-for-Accounting-Research/temp/strategy_score.rds"
)
merged_for_reg_reduced <- merged_for_reg_reduced %>% 
  select(-StrategyScore) %>% 
  left_join(
    x = .,
    y = corp_strategy[c("Stkcd", "Year", "StrategyScore")],
    by = c("Stkcd", "Year")
  )


# 3. 模型检验 -----------------------------------------------------------------

stata(
  src = "code/analysis01_descriptive-table.do",
  data.in = filter(merged_for_reg_reduced, Year >= 2016)
)

stata(
  src = "code/analysis02_basic-test.do",
  data.in = filter(merged_for_reg_reduced, Year >= 2016)
)

stata(
  src = "code/analysis03_heterogeneity-test.do",
  data.in = filter(merged_for_reg_reduced, Year >= 2016)
)

stata(
  src = "code/analysis05_AM-new-rule.do",
  data.in = filter(merged_for_reg_reduced, Year >= 2012)
)


dbDisconnect(con_sqlite)


# temp_sample <- merged_for_reg_reduced %>%
#   filter(Year >= 2016) %>%
#   stata(
#     src = '
#       gen EPS_P = EPS / YearOpen
#       gen IndustryCode2 = cond(substr(IndustryCode, 1, 1) != "C", substr(IndustryCode, 1, 1), substr(IndustryCode, 1, 2))
#       egen Industry = group(IndustryCode2)
# 
#       egen Province2 = group(Province)
#       rename Province Province_str
#       rename Province2 Province
#       gen same_province = strmatch(GGFProvince, Province_str)
# 
#       winsor2 Size Lev MHRatio RDRatio GDP_p INS Age SuperINS, cuts(1 99) by(Year) trim replace
#     ',
#     data.in = .,
#     data.out = TRUE
#   ) %>% 
#   filter(GGF == 1)
# 
# temp_sample %>%
#   fixest::feols(
#     fml = EPS_P ~
#       DR*Ret*sdROA3 + DR*Ret*Size + DR*Ret*Lev + 
#       DR*Ret*MHRatio + DR*Ret*Age + DR*Ret*GDP_p|
#       Stkcd + Year,
#     vcov = "iid",
#     panel.id = c("Stkcd", "Year"),
#     data = .
#     ) %>%
#   summary()


