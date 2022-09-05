
##############################################################
# Programmer: Lu Winter
# Date Created: 2022-08-28
# Task: Calculate control variables
##############################################################


# 0. Initial Setup -------------------------------------------------------------

## 加载R包
library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(purrr)
library(lubridate)
library(DBI)

## 预定义输入输出路径
data_path <- "Government_Guide_Fund/data"
output_path <- "Government_Guide_Fund/output"
db_path <- "Government_Guide_Fund/data/GGF_project_store.sqlite"

## 建立数据库连接
con_sqlite <- dbConnect(RSQLite::SQLite(), db_path)
dbListTables(con_sqlite)

## Load basic information of listed corporations
identifier_info <- dbReadTable(conn = con_sqlite, name = "identifier_info")
head(identifier_info)


# 1. 股权性质 -----------------------------------------------------------------
### 输出变量：大股东持股比例OneHolder/TenHolder 是否国有SOE

### 数据来源：CSMAR公司研究 - 股权性质 - 中国上市公司股权性质文件
### 数据属性：2003-2021年度 全部股票代码 9个变量 47928条观测
equity_nature <- read_xlsx(file.path(data_path, "Equity_Nature/2022-09-02_equity-nature.xlsx"))
equity_nature <- equity_nature %>% 
  mutate(Year = year(截止日期),
         SOE =  ifelse(is.na(股权性质), 0, 
                ifelse(股权性质 == "国企", 1, 0))) %>% 
  select(Stkcd = 证券代码, Year, SOE,
         OneHolder = `第一大股东持股比率(%)`,
         TenHolder = `前十大股东持股比例(%)`)
equity_nature %>% sample_n(20)
equity_nature %>% map_dbl(\(x) sum(is.na(x)))

### 数据来源：CNRDS基础库 - 股权结构 - 实际控制人
### 数据属性：1992-2021年度 全部股票代码 8个变量 53185条观测
equity_nature_cnrds <- read_xlsx(file.path(data_path, "Equity_Nature/2022-09-01_actual-controller_CNRDS.xlsx"))
equity_nature_cnrds <- equity_nature_cnrds %>%
  mutate(Year = as.numeric(Year),
         OneHolder = as.numeric(LrgHldRt)) %>%
  filter(Year >= 2016) %>%
  mutate(SOE = ifelse(is.na(ACNtr), 0,
               ifelse(str_detect(ACNtr, "地方|国"), 1, 0))) %>%
  select(Stkcd = SCode, Year, SOE, OneHolder)

### 数据来源：CNRDS基础库 - 股东持股 - 大股东持股
### 数据属性：2003-2021年度 全部股票代码 6个变量 479413条观测
ten_holder_cnrds <- read_xlsx(file.path(data_path, "Equity_Nature/2022-09-01_major-shareholders_CNRDS.xlsx"))
ten_holder_cnrds <- ten_holder_cnrds %>%
  mutate(Year = year(Date),
         ShrRt = as.numeric(ShrRt)) %>%
  filter(Year >= 2016) %>%
  select(Stkcd = SCode, Year, ShrRt) %>%
  group_by(Stkcd, Year) %>%
  summarise(TenHolder = sum(ShrRt)) %>%
  ungroup()
equity_nature_cnrds <- equity_nature_cnrds %>%
  full_join(ten_holder_cnrds, by = c("Stkcd", "Year"))


# 2. 审计特征 -----------------------------------------------------------------
### 数据来源：CSMAR公司研究 - 财务报告审计意见 - 审计意见表文件
### 数据属性：2010-2021年度 全部股票代码 5个变量 38337条观测
### 输出变量：是否四大审计师Big4
auditor_info <- read_xlsx(file.path(data_path, "2022-08-14_auditor-info.xlsx"))
auditor_info <- auditor_info %>% 
  filter(month(会计截止日期) == 12) %>% 
  mutate(Big4 = as.numeric(str_detect(境内审计事务所, "(安永|德勤|毕马威|普华永道)")),
         Year = year(会计截止日期)) %>% 
  select(Stkcd = 证券代码, Year, Big4)
auditor_info %>% sample_n(15)


# 3. 机构投资 -----------------------------------------------------------------
### 数据来源：CSMAR公司研究 - 机构投资者 - 机构持股明细表
### 数据属性：2016-2021年度 全部股票代码 6个变量 1957576条观测
### 输出变量：机构投资者持股比例INS 监督型机构投资者持股比例SuperINS
insti_investor <- read_xlsx(file.path(data_path, "2022-08-15_insti-investor-part01.xlsx"))
temp <- read_xlsx(file.path(data_path, "2022-08-15_insti-investor-part02.xlsx"))
insti_investor <- bind_rows(insti_investor, temp)
rm(temp)
insti_investor <- insti_investor %>% 
  mutate(Year = year(EndDate),
         super_insti = ifelse(Systematics == "基金持股", 1,
                       ifelse(Systematics == "社保基金持股", 1, 
                       ifelse(Systematics == "QFII持股", 1, 0)))) %>% 
  group_by(Symbol, Year) %>% 
  summarise(INS = sum(HoldProportion),
            SuperINS = sum(super_insti * HoldProportion)) %>% 
  ungroup() %>% 
  rename(Stkcd = Symbol)
insti_investor %>% sample_n(20)


# 4. 高管持股 -----------------------------------------------------------------
### 数据来源：CSMAR公司研究 - 治理结构 - 高管人数、持股及薪酬情况表
### 数据属性：2010-2021年度 全部股票代码 6个变量 76243条观测
### 输出变量：独立董事比例IndedirRatio 管理层持股ManagerHold
management_info <- read_xlsx(file.path(data_path, "2022-08-15_manager-info.xlsx"))
management_info <- management_info %>% 
  filter(StatisticalCaliber == 1) %>% 
  mutate(IndeDirRatio = IndependentDirectorNumber / DirectorNumber,
         Year = year(Enddate)) %>% 
  select(Stkcd = Symbol, Year, IndeDirRatio, ManagerHold = ManageHoldshares)
management_info %>% sample_n(20)


# 5. 治理结构 -----------------------------------------------------------------
### 数据来源：CSMAR公司研究 - 治理结构 - 治理综合信息文件 & 股本结构文件 & 三会基本信息文件
### 数据属性：2010-2021年度 全部股票代码 8个变量 38122条观测
### 输出变量：股东大会会议数GeneralMeeting 两职合一TwoJobs 
governance_structure <- read_xlsx(file.path(data_path, "2022-08-30_corp-governance.xlsx"))
head(governance_structure)
colnames(governance_structure) <- c("Stkcd", "EndDate", "EmployerNum",
  "TwoJobs", "TotalShares", "DirecBoardMeeting", "SuperBoardMeeting", "GeneralMeeting")
governance_structure <- governance_structure %>% 
  mutate(Year = year(EndDate), 
         TwoJobs = ifelse(TwoJobs == 2, 0, TwoJobs)) %>% 
  select(-EndDate)
governance_structure %>% sample_n(20)


# 6. 地区经济 -----------------------------------------------------------------
# 6.1 城市人均GDP
### 数据来源：EPS统计数据平台
### 数据属性：2016-2021年度 全部地级市 3个变量 1485条观测
### 输出变量：城市人均国民生产总值GDP_p
city_gdp <- read_xls(file.path(data_path, "2022-08-23_city-gdp_EPS.xls"))
colnames(city_gdp)[1] <- "City"
colnames(city_gdp)[2] <- "Year"
colnames(city_gdp)[3] <- "GDP_p"
city_gdp <- city_gdp[, c(2, 1, 3)]
city_gdp <- city_gdp %>% 
  mutate(Year = as.numeric(Year),
         GDP_p = as.numeric(GDP_p)) %>% 
  mutate(City = str_remove(City, "（.+）")) %>% 
  mutate(City = ifelse(str_detect(City, "市$") == FALSE, 
                       paste0(City, "市"), City))

# 6.2 省区人均GDP
### 数据来源：EPS统计数据平台
### 数据属性：2016-2021年度 全部省区 3个变量 186条观测
### 输出变量：省区人均国民生产总值GDP_p2
prov_gdp <- read_xls(file.path(data_path, "2022-08-23_prov-gdp_EPS.xls"))
colnames(prov_gdp)[1] <- "Province"
colnames(prov_gdp)[2] <- "Year"
colnames(prov_gdp)[3] <- "GDP_p2"
prov_gdp <- prov_gdp[, c(2, 1, 3)] %>% 
  mutate(Year = as.numeric(Year),
         GDP_p2 = as.numeric(GDP_p2))

# 6.3 地区金融化
### 数据来源：CSMAR公司研究 - 上市公司基本信息 - 上市公司基本信息年度表
### 数据属性：2000-2021年度 全部股票代码 37个变量 52629条观测
### 输出变量：地区金融化水平RegionFin
region_fin <- read_xlsx(file.path(data_path, "2022-08-04_corporate-info.xlsx"))
fin_centre <- "上海|北京|深圳|广州|杭州|南京|天津|成都|重庆|宁波"
region_fin <- region_fin %>% 
  mutate(RegionFin = as.numeric(str_detect(RegisterAddress, fin_centre)),
         Year = year(EndDate)) %>% 
  select(Stkcd = Symbol, Year, RegionFin) %>% 
  filter(Year >= 2016)
rm(fin_centre)  
region_fin %>% sample_n(20)


# 7. 政府补助 -----------------------------------------------------------------
### 数据来源：CSMAR公司研究 - 财务报表附注 - 政府补助
### 数据属性：2015-2021年度 4881个股票代码 9个变量
### 输出变量：政府补助Subsidies
gov_subsidies_wind <- read_csv(file.path(data_path, "2022-08-23_gov-subsidies_Wind.csv"))
gov_subsidies_wind$证券简称 <- NULL
colnames(gov_subsidies_wind)[1] <- "Stkcd"
head(gov_subsidies_wind)
gov_subsidies_wind <- gov_subsidies_wind %>% 
  tidyr::gather(key = "Year", value = "Subsidies", -Stkcd) %>% 
  mutate(Year = as.numeric(str_remove(Year, "年")),
         Stkcd = str_remove(Stkcd, "(\\.SZ|\\.SH|\\.BJ)"))
gov_subsidies_wind %>% sample_n(20)  ### 缺失值不补0


# 8. 公司战略 -----------------------------------------------------------------
### 输出变量：公司战略得分StrategyScore

# 8.1 处理上市公司财务指标
### 数据来源：CSMAR公司研究 - 财务报表 - 资产负债表 & 利润表 (筛选出年报)
### 数据属性：2010-2021年度 全部股票代码 9个变量 78702条观测
financial_sheet_more <- read_xlsx(file.path(data_path, "Financial_Sheet/2022-08-30_financial-sheets.xlsx"))
colnames(financial_sheet_more) <- c("Stkcd", "Accper", "Type",
                                    "FixedAsset", "IntangibleAsset", "TotalAsset", 
                                    "TotalRevenue", "Revenue", "SalesCost")
financial_sheet_more <- financial_sheet_more %>% 
  filter(Type == "A") %>% 
  mutate(Year = year(Accper)) %>% 
  select(-Accper, -Type)
apply(financial_sheet_more, MARGIN = 2, FUN = \(x) sum(is.na(x)))
### 数据缺失情况如下，FixedAsset: 3 IntangibleAsset: 186
### TotalAsset: 0 TotalRevenue: 6 Revenue: 533 SalesCost: 926 
### 考虑到固定资产、无形资产为资产负债表直接科目，营业总收入、
### 销售费用为利润表直接科目，此四项理论上无缺失值，要填补为0，
### 营业收入为利润表间接科目，可能存在缺失值，由营业总收入补充
financial_sheet_more <- financial_sheet_more %>% 
  mutate(FixedAsset = ifelse(is.na(FixedAsset), 0, FixedAsset),
         IntangibleAsset = ifelse(is.na(IntangibleAsset), 0, IntangibleAsset),
         SalesCost = ifelse(is.na(SalesCost), 0, SalesCost),
         TotalRevenue = ifelse(is.na(TotalRevenue), 0, TotalRevenue),
         Revenue = ifelse(is.na(Revenue), TotalRevenue, Revenue))

# 8.2 合并上市公司员工人数、总股数、行业
### 数据来源：CSMAR公司研究 - 上市公司基本信息 - 上市公司基本信息年度表
### 数据属性：2000-2021年度 全部股票代码 37个变量 52629条观测
listed_corp_info <- read_xlsx(file.path(data_path, "2022-08-04_corporate-info.xlsx"))
listed_corp_info <- listed_corp_info %>% 
  mutate(Year = year(EndDate)) %>% 
  select(Stkcd = Symbol, ShortName, Year, IndustryCode)

corp_strategy <- left_join(financial_sheet_more, 
                           governance_structure[c("Stkcd", "Year", "EmployerNum")], 
                           by = c("Stkcd", "Year"))  %>% 
  left_join(listed_corp_info, by = c("Stkcd", "Year")) %>% 
  mutate(IndustryCode = ifelse(str_sub(IndustryCode, 1, 1) == "C",
                               str_sub(IndustryCode, 1, 2),
                               str_sub(IndustryCode, 1, 1))) %>% 
  mutate(ToRemove = ifelse(is.na(IndustryCode), 1, 
                    ifelse(str_detect(ShortName, "ST"), 1,
                    ifelse(str_detect(ShortName, "B"), 1,
                    ifelse(IndustryCode == "J", 1,
                    ifelse(str_sub(Stkcd, 1, 1) %in% c("0", "3", "6"), 0, 1))))))

corp_strategy %>% sample_n(20)
corp_strategy %>% 
  filter(ToRemove == 0) %>% 
  apply(MARGIN = 2, FUN = \(x) sum(is.na(x)))

corp_strategy <- corp_strategy %>% 
  filter(ToRemove == 0, !is.na(EmployerNum))   ### 有效观测35623个

# 8.3 计算基础指标
corp_strategy <- corp_strategy %>%   
  mutate(RD = IntangibleAsset / TotalAsset,
         EMPS = EmployerNum / Revenue,
         SEXP = SalesCost / Revenue,
         PPE = FixedAsset / TotalAsset) %>% 
  select(Stkcd, Year, IndustryCode, Revenue, EmployerNum, RD, EMPS, SEXP, PPE)

# 8.4 年度总得分函数
source("Government_Guide_Fund/code/func_tools.R")
calcu_cg_score <- function(origin_data, target_year) {
  ### 准备数据
  data <- origin_data %>% 
    filter(Year <= target_year, target_year - 4 <= Year) %>%
    group_by(Stkcd) %>% filter(n() == 5)
  data <- lag_n_year(data, n = 1, key = "Stkcd", value = "Revenue", by = "Year")
  ### 计算五年值
  data_strategy <- data %>% 
    mutate(REV = ifelse(Year == target_year - 4, 0, (Revenue - Revenue_lag1) / Revenue_lag1)) %>% 
    group_by(Stkcd) %>% 
    mutate(RD5 = mean(RD), EMPS5 = mean(EMPS), REV5 = mean(REV), 
           SEXP5 = mean(SEXP), PPE5 = mean(PPE),
           sdEMP5 = sd(EmployerNum) / mean(EmployerNum)) %>% 
    ungroup() %>% 
    select(Stkcd, IndustryCode, RD5, EMPS5, REV5, SEXP5, sdEMP5, PPE5) %>% 
    distinct(Stkcd, .keep_all = TRUE)
  ### 计算得分
  data_strategy <- data_strategy %>% 
    mutate(PPE5_r = - PPE5) %>% 
    group_by(IndustryCode) %>% 
    mutate(RD5_Score = make_score(RD5),
           EMPS5_Score = make_score(EMPS5),
           REV5_Score = make_score(REV5),
           SEXP5_Score = make_score(SEXP5),
           sdEMP5_Score = make_score(sdEMP5),
           PPE5_Score = make_score(PPE5_r)) %>%
    ungroup() %>% 
    mutate(StrategyScore = RD5_Score + EMPS5_Score + REV5_Score + 
           SEXP5_Score + sdEMP5_Score + PPE5_Score)
  bind_cols(data_strategy[, c("Stkcd", "StrategyScore")], Year = target_year)
}

# 8.5 计算各年度总得分
strategy_score_list <- map(2016:2021, \(x) calcu_cg_score(corp_strategy, x))
strategy_score <- reduce(strategy_score_list, bind_rows)
rm(strategy_score_list, financial_sheet_more, corp_strategy)


# 9. 公司治理 -----------------------------------------------------------------
### 主成分分析计算公司治理综合得分
### 1.股权集中度 HolderConcen  负面  2.独立董事比例 IndeDirRatio     正面  
### 3.两职合一 TwoJobs         负面  4.股东大会次数 GeneralMeeting   正面  
### 5.管理者持股 MHRatio       正面  6.股权制衡度   HolderBalance    正面
### 7.产权性质 SOE             负面  8.监督型机构持股比例 SuperINS   正面

# 9.1 合并并处理计算指标
### 负面指标都要进行正向转换，转换后所有指标都是越高越好 
governance_data <- identifier_info[, c(1, 3)] %>% 
  left_join(management_info, by = c("Stkcd", "Year")) %>% 
  left_join(equity_nature, by = c("Stkcd", "Year")) %>% 
  left_join(insti_investor, by = c("Stkcd", "Year")) %>% 
  left_join(governance_structure, by = c("Stkcd", "Year")) %>% 
  mutate(MHRatio = ManagerHold / TotalShares, 
         HolderBalance = (TenHolder-OneHolder) / OneHolder) %>% 
  mutate(HolderConcen = 1 / OneHolder,
         TwoJobs = ifelse(is.na(TwoJobs), NA,
                   ifelse(TwoJobs == 1, 0, 1)),
         SOE = ifelse(is.na(SOE), NA,
               ifelse(SOE == 1, 0, 1))) %>%
  select(Stkcd, Year, IndeDirRatio, TwoJobs, GeneralMeeting, 
         MHRatio, HolderBalance, SOE, SuperINS)
summary(governance_data) ### 缺失值分布较为均匀

# 9.2 主成分分析
psych::KMO(governance_data[, -c(1:2)])       # KMO检验      应大于0.5
bartlett.test(governance_data[, -c(1:2)])    # Bartlett检验 应显著
governance_pca <- prcomp(~ ., data = governance_data[, -c(1:2)], 
                         na.action = na.exclude, scale. = TRUE)
governance_pca$x %>% as_tibble() 
governance_pca$rotation           # 检查载荷系数符号
governance_pca %>% summary()      # 确定主成分个数为5

# 9.3 计算并合并主成分得分
CG_Score <- governance_pca$x[, 1:5] %*% 
  as.matrix((governance_pca$sdev ^ 2)[1:5] / sum((governance_pca$sdev ^ 2)[1:5]))
governance_score <- bind_cols(
  governance_data[c("Stkcd", "Year")],
  CG = as.vector(CG_Score)
)
governance_score %>% sample_n(20) ### 缺失值1833个
rm(CG_Score, governance_data, governance_pca)


# 10. 研发投入 ----------------------------------------------------------------

rd_cost <- readxl::read_xlsx(file.path(data_path, "2022-09-05_RD-cost.xlsx"), guess_max = 15000)
rd_cost <- rd_cost %>% 
  select(Stkcd = 股票代码, Year = 会计年度, 
         RDCost = 研发支出, RDRatio = 研发支出占营业收入比例) %>% 
  mutate(Year = as.numeric(Year) + 1,
         RDCost = as.numeric(RDCost),
         RDRatio = as.numeric(stringr::str_remove(RDRatio, "%")))


# 11. ESG评级 ---------------------------------------------------------------

ESG_rating


# 11. 合并控制变量 --------------------------------------------------------------
## 4个标识:
## Stkcd ShortName Year ToRemove
## 19个控制变量: 
## ListingYear SOE OneHolder TenHolder Big4 INS SuperINS IndeDirRatio
## EmployerNum TwoJobs DirecBoardMeeting SuperBoardMeeting
## GeneralMeeting RegionFin Subsidies StrategyScore CG MHRatio GDP_p
## 3个固定效应:
## IndustryCode Province City

# count(listed_corp_info, Year)       # 2000-2021
# count(equity_nature, Year)          # 2003-2021
# count(auditor_info, Year)           # 2010-2021
# count(insti_investor, Year)         # 2016-2021
# count(management_info, Year)        # 2010-2021
# count(governance_structure, Year)   # 2010-2021
# count(city_gdp, Year)               # 2016-2020
# count(prov_gdp, Year)               # 2015-2020
# count(region_fin, Year)             # 2010-2021
# count(gov_subsidies_wind, Year)     # 2015-2021
# count(strategy_score, Year)         # 2016-2021
# count(governance_score, Year)       # 2016-2021     
list(
  listed_corp_info, equity_nature, auditor_info, insti_investor, 
  management_info, governance_structure, region_fin, 
  gov_subsidies_wind, strategy_score, governance_score
) %>% map(.f = \(x) count(x, Year))

control_variables <- list(
    identifier_info, equity_nature, auditor_info, insti_investor, 
    management_info, governance_structure, region_fin, 
    gov_subsidies_wind, strategy_score, governance_score
  ) %>% 
  map(.f = \(x) filter(x, Year >= 2016)) %>% 
  reduce(.f = \(x, y) left_join(x, y, by = c("Stkcd", "Year"))) %>% 
  mutate(MHRatio = ManagerHold / TotalShares) %>% 
  select(Stkcd, Year, IndustryCode, ListingYear, EstablishDate, 
         Province, City, SOE, Big4, INS, SuperINS, RegionFin, 
         Subsidies, StrategyScore, CG, MHRatio)

control_variables %>% 
  apply(MARGIN = 2, FUN = \(x) sum(is.na(x)))

city_gdp <- mutate(city_gdp, Year = Year + 1)
prov_gdp <- mutate(prov_gdp, Year = Year + 1)
control_variables <- control_variables %>% 
  left_join(city_gdp, by = c("City", "Year")) %>%
  mutate(prov_temp = str_remove(Province, "省|市")) %>% 
  left_join(prov_gdp, by = c("prov_temp" = "Province", "Year")) %>% 
  mutate(GDP_p = ifelse(is.na(GDP_p), GDP_p2, GDP_p)) %>% 
  select(-prov_temp, -GDP_p2)

write_rds(x = control_variables,
          file = file.path(output_path, "control_variables.rds"))

dbDisconnect(con_sqlite)

