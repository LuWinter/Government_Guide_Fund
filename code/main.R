
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
accounting_conservatism <- readRDS(file = file.path(output_path, "accounting_conservatism.rds"))
control_variables <- readRDS(file = file.path(output_path, "control_variables.rds"))

merged_Big10SH_GGF_nodupl <- merged_Big10SH_GGF_nodupl %>%
  mutate(Year = Year + 1)

# accounting_conservatism <- accounting_conservatism %>%
#   select(-Ret, -DR) %>%
#   rename(Ret = adjRet, DR = adjDR)

control_variables <- control_variables %>% 
  filter(if_all(!matches("StrategyScore|ESGRating|RDRatio"), ~ !is.na(.x)))
### 除了StrategyScore外，有19244个无缺样本

merged_for_reg <- accounting_conservatism %>% 
  select(Stkcd, Year, G_Score, C_Score, EPS, YearOpen, YearClose, 
         Ret, DR, Size, MB, Lev) %>% 
  left_join(merged_Big10SH_GGF_nodupl, by = c("Stkcd", "Year")) %>% 
  mutate(GGF = ifelse(is.na(GovFund), 0, 1)) %>% 
  filter(Year >= 2016) %>% 
  left_join(control_variables, by = c("Stkcd", "Year")) %>% 
  mutate(Age = Year - lubridate::year(EstablishDate))
merged_for_reg %>%   
  apply(MARGIN = 2, FUN = \(x) sum(is.na(x)))
merged_for_reg <- merged_for_reg %>% 
  filter(!is.na(CG)) 
merged_for_reg %>%   
  apply(MARGIN = 2, FUN = \(x) sum(is.na(x)))

## 变量总计：35
## 标识（2）：Stkcd Year
## 会计稳健性（7）：G_Score C_Score EPS YearOpen YearClose DR Ret
## 引导基金（7）：GovFund GGFLevel GGFProvince IsFirstHold HoldNum HoldRatio HoldRank
## 控制变量（16）：Size MB Lev RegionFin SOE Big4 INS SuperINS MHRatio 
## ListingYear GDP_p Subsidies StrategyScore CG RDRatio ESGRating
## 固定效应（3）：IndustryCode Province City
merged_for_reg <- merged_for_reg[, c("Stkcd", "Year", 
     "G_Score", "C_Score", "EPS", "YearOpen", "YearClose", "DR", "Ret",
     "GGF", "GGFLevel", "GGFProvince", "IsFirstHold", "HoldNum", "HoldRatio", "HoldRank",
     "Size", "MB", "Lev", "RegionFin", "SOE", "Big4", "INS", "SuperINS", "MHRatio", 
     "ListingYear", "GDP_p", "Subsidies", "StrategyScore", "CG", "Age", "RDRatio", "ESGRating", 
     "IndustryCode", "Province", "City")]

merged_for_reg_reduced <- merged_for_reg %>% 
  group_by(Stkcd) %>% 
  filter(n() > 2) %>% 
  ungroup()

# saveRDS(merged_for_reg, file = paste0("merged-for-reg_", today(), ".rds"))
# saveRDS(merged_for_reg_reduced, file = paste0("merged-for-reg-reduced_", today(), ".rds"))


# 2. 模型检验 -----------------------------------------------------------------

stata(src = "code/analysis02_basic-test.do",
      data.in = filter(merged_for_reg, Year >= 2017))

stata(src = "code/analysis03_mechanism-test.do",
      data.in = filter(merged_for_reg, Year >= 2017))

dbDisconnect(con_sqlite)

