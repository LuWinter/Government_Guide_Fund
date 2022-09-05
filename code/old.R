
##############################################################
# Programmer: Lu Winter
# Date Created: 2022-08-28
# Task: Calculate control variables
##############################################################


library(dplyr)
library(stringr)
library(RSQLite)
library(readr)
library(readxl)
library(purrr)
library(lubridate)
library(rio)

# 预定义输入输出路径
code_path <- "Government_Guide_Fund/R"
data_path <- "Government_Guide_Fund/data"
output_path <- "Government_Guide_Fund/output"
db_path <- "Government_Guide_Fund/data/GGF_project_store.sqlite"
# 建立数据库连接
con_sqlite <- dbConnect(RSQLite::SQLite(), db_path)
dbListTables(con_sqlite)
# Stata设置
options("RStata.StataPath" = "/Applications/Stata/StataSE.app/Contents/MacOS/stata-se")
options("RStata.StataVersion" = 17)

# 1. 引导基金处理 ---------------------------------------------------------------

# # 导入引导基金数据
# gov_fund_list <- read_csv(
#   file = file.path(data_path, "Gov_Guide_Fund/gov_guide_fund_little.csv"), 
#   na = "--", guess_max = 2000)
# gov_fund_list <- gov_fund_list %>%
#   mutate(`基金全称2` = str_remove_all(`基金全称`, "（.{4}）$"))
# gov_fund_investor <- read_csv(
#   file = file.path(data_path, "Gov_Guide_Fund/gov_guide_fund_full.csv"), 
#   na = "--", guess_max = 2000)
# gov_fund_list_add <- read_xlsx(
#   path = file.path(data_path, "Gov_Guide_Fund/gov_guide_fund_add.xlsx"),)
# 
# # 计算引导基金注册省份
# founded_province <- gov_fund_list$注册地区 %>% 
#   str_split(pattern = "\\|") %>% 
#   map(2)
# founded_province_vec <- c()
# for(i in 1:length(founded_province)) {
#   if (!is.null(founded_province[[i]])) {
#     founded_province_vec <- c(founded_province_vec, founded_province[[i]])
#   } else {
#     founded_province_vec <- c(founded_province_vec, NA)
#   }
# }
# gov_fund_list$注册省份 <- founded_province_vec
# rm(i, founded_province, founded_province_vec)
# 
# # 计算引导基金出资背景
# gov_fund_investor <- gov_fund_investor %>% 
#   mutate(`出资人是否国资2` = ifelse(`出资人数量` == 1, "是", `出资人是否国资`)) %>% 
#   mutate(`出资人是否国资2` = ifelse(`出资人是否国资2` == "是", 1, 0)) %>% 
#   group_by(`基金简称`) %>% 
#   mutate(`非国资出资人数量` = `出资人数量` - sum(`出资人是否国资2`))
# gov_fund_list <- bind_cols(
#   gov_fund_list, 
#   distinct(gov_fund_investor[, c(1, 9, 7)])[, 2:3]) %>% 
#   mutate(包含非国资出资 = ifelse(非国资出资人数量 > 0, 1, 0))

# 导入引导基金与上市公司合并数据
merged_Big10SH_GGF <- read_rds(file.path(output_path, "merged_Big10SH_GGF.rds"))
merged_Big10SH_GGF_dupl <- merged_Big10SH_GGF %>% 
  filter(GovFund == 1) %>% 
  mutate(Year = year(Reptdt)) %>% 
  select(Stkcd, Year, GovFund, GGFName = 股东全称3,
         GGFLevel = 基金级别, GGFProvince = 注册省份,
         HoldNum = S0302a, HoldRatio = S0304a, HoldRank = S0306a)
## 855行，由于存在多个引导基金同年度持股一家上市公司，因此按Stkcd和Year，存在重复观测
## 对于重复观测，保存持股比例高的,即排名靠前的
merged_Big10SH_GGF_nodupl <- merged_Big10SH_GGF_dupl %>% 
  group_by(Stkcd, Year) %>% 
  mutate(minHoldRank = min(HoldRank)) %>% 
  filter(HoldRank == minHoldRank) %>% 
  select(-minHoldRank) %>% 
  ungroup()
## 去重后得到807个观测  


# 计算引导基金是否第一年持股
merged_Big10SH_GGF_nodupl <- merged_Big10SH_GGF_nodupl %>% 
  group_by(Stkcd) %>% 
  mutate(FirstHoldYear = min(Year)) %>% 
  ungroup() %>% 
  mutate(IsFirstHold = ifelse(Year == FirstHoldYear, 1, 0)) %>% 
  select(-FirstHoldYear)
# 写入到数据库
# dbWriteTable(conn = con_sqlite, 
#              name = "GGF_list_qingke", 
#              value = gov_fund_list)
# dbWriteTable(conn = con_sqlite,
#              name = "GGF_investor_qingke",
#              value = gov_fund_investor)
# dbWriteTable(conn = con_sqlite,
#              name = "GGF_list_add",
#              value = gov_fund_list_add)
## 后续应当直接在数据库中增补引导基金数据
dbListTables(con_sqlite)
dbDisconnect(con_sqlite)


# 2. 计算会计稳健性 --------------------------------------------------------------

# 导入上市公司基本信息数据
listed_corp_info <- read_xlsx(file.path(data_path, "Listed_Corporate_Info/STK_LISTEDCOINFOANL.xlsx"))
listed_corp_info <- listed_corp_info %>% 
  mutate(Year = year(EndDate),
         ListingYear = year(LISTINGDATE)) %>% 
  select(Stkcd = Symbol, ShortName, Year, IndustryCode, ListingYear, 
         Province = PROVINCE, City = CITY, RegisterAddress)
## 这里要剔除公司样本再计算会计稳健性, 剔除范围是
## 金融类样本、ST、* ST 类样本、只发行 B 股样本、相关数据缺失的样本
## 1. 按年合并公司名称与行业代码
## 2. 删除名称中带ST和B的，删除金融业
listed_corp_info <- listed_corp_info %>% 
  mutate(ToRemove = ifelse(str_detect(ShortName, "ST"), 1,
                    ifelse(str_detect(ShortName, "B"), 1,
                    ifelse(str_sub(IndustryCode, 1, 1) == "J", 1, 0))))
count(listed_corp_info, ToRemove)

# 计算地区金融化程度
listed_corp_info <- listed_corp_info %>% 
  mutate(RegionFin = as.numeric(str_detect(RegisterAddress, "上海|北京|深圳|广州|杭州|南京|天津|成都|重庆|宁波"))) %>% 
  select(-RegisterAddress)

# 导入上市公司年股票回报率数据
year_return <- read_xlsx(file.path(data_path, "Year_Return/TRD_Year.xlsx"))
year_return <- year_return %>% 
  mutate(交易年份 = as.numeric(交易年份),
         LastYear = 交易年份 - 1) %>%
  rename(Stkcd = 证券代码, Year = 交易年份)
year_return <- year_return %>% 
  left_join(listed_corp_info[, c("Stkcd", "Year", "ListingYear", "ToRemove")],
            by = c("Stkcd", "Year")) %>% 
  filter(Year >= 2009)

# 合并上一年度收盘价
year_return_back <- year_return[, c(1, 2, 6)]
year_return <- year_return %>% 
  filter(Year >= 2010) %>% 
  left_join(year_return_back, 
            by = c("Stkcd", "LastYear" = "Year")) %>% 
  rename(上年度收盘价 = 年收盘价.y,
         年收盘价 = 年收盘价.x)
rm(year_return_back)
# count(year_return, is.na(上年度收盘价), 交易年份) %>% print(n = 50)

# 计算个股年回报率和超额回报率
year_return <-  year_return %>%
  select("Stkcd", "Year", "年开盘日期", "年开盘价",
         "年收盘日期", "年收盘价", "上年度收盘价",
         "年个股交易股数","年个股总市值", 
         "考虑现金红利再投资的年个股回报率",
         "年个股流通市值", "ToRemove") %>%
  mutate(上年度收盘价_增补 = ifelse(is.na(上年度收盘价), 年开盘价, 上年度收盘价)) %>% 
  mutate(RET = ifelse(is.na(考虑现金红利再投资的年个股回报率),
                            年收盘价 / 上年度收盘价_增补 - 1,
                            考虑现金红利再投资的年个股回报率))

# 导入上市公司财报数据
financial_sheet <- read_xlsx(file.path(data_path, "Financial_Sheet/FS_Comins(Merge Query).xlsx"))
financial_sheet <- financial_sheet %>% 
  filter(month(会计期间) == 12, 报表类型 == "A", year(会计期间) >= 2010) %>% 
  mutate(Year = year(会计期间)) %>% 
  select(-会计期间, -报表类型) %>% 
  rename(Stkcd = 证券代码)

# 合并个股财务报表数据与年回报率数据
financial_sheet <- financial_sheet %>% 
  left_join(year_return, by = c("Stkcd", "Year")) %>% 
  mutate(DR = as.numeric(RET < 0),
         Size = log(资产总计),
         Lev = 负债合计 / 资产总计,
         MB = 年个股总市值 / 资产总计) %>% 
  filter(Size > 0, ToRemove == 0) %>% 
  select(-ToRemove)
colnames(financial_sheet)
colnames(financial_sheet)[3] <- "EPS"
colnames(financial_sheet)[10] <- "YOP"
colnames(financial_sheet)[12] <- "YCP"
colnames(financial_sheet)[18] <- "LYCP"


# 计算会计稳健性指标
formula_basu <- formula(EPS / LYCP ~ DR + RET + DR:RET)
formula_khan <- formula(EPS / LYCP ~ DR + RET + 
                          Size:RET + MB:RET + Lev:RET + 
                          DR:RET + DR:Size:RET + DR:MB:RET + Lev:DR:RET)
coef_res <- map(c(2010:2021), \(x) coef(
  lm(formula = formula_khan, data = subset(financial_sheet, Year == x))
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
accounting_conservatism <- financial_sheet %>% 
  left_join(coef_res2, by = c("Year")) %>% 
  mutate(G_Score = a1 + a2 * Size + a3 * MB + a4 * Lev,
         C_Score = b1 + b2 * Size + b3 * MB + b4 * Lev)
rm(coef_res, coef_res2)
write_rds(x = accounting_conservatism,
          file = file.path(output_path, "accounting_conservatism.rds"))
dbWriteTable(conn = con_sqlite,
             name = "accounting_conservatism_fix",
             value = accounting_conservatism,
             overwrite = TRUE)


# 3. 控制变量处理 ---------------------------------------------------------------

# 3.1 股权性质——大股东持股比例、是否国有SOEd
equity_nature <- read_xlsx(file.path(data_path, "Equity_Nature/EN_EquityNatureAll.xlsx"))
equity_nature <- equity_nature %>% 
  mutate(Year = year(截止日期),
         SOE = ifelse(股权性质 == "国企", 1, 0)) %>% 
  select(Stkcd = 证券代码, Year, SOE,
         OneHolder = `第一大股东持股比率(%)`,
         TenHolder = `前十大股东持股比例(%)`)
head(equity_nature)

# 3.2 审计——是否四大审计师Big4
auditor_info <- read_xlsx(file.path(data_path, "Audit/FIN_Audit.xlsx"))
auditor_info <- auditor_info %>% 
  filter(month(会计截止日期) == 12) %>% 
  mutate(Big4 = as.numeric(str_detect(境内审计事务所, "(安永|德勤|毕马威|普华永道)")),
         Year = year(会计截止日期)) %>% 
  select(Stkcd = 证券代码, Year, Big4)

# 3.3 机构投资者——机构投资者持股比例INS、监督型机构投资者持股比例SuperINS
insti_investor <- read_xlsx(file.path(data_path, "Institu_Holder/INI_Holder_Detail.xlsx"))
temp <- read_xlsx(file.path(data_path, "Institu_Holder/INI_Holder_Detail1.xlsx"))
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

# 3.4 高管特征——独立董事比例IndedirRatio、管理层持股ManagerHold
management_info <- read_xlsx(file.path(data_path, "Management_Info/CG_ManagerShareSalary.xlsx"))
head(management_info)
management_info <- management_info %>% 
  filter(StatisticalCaliber == 1) %>% 
  mutate(IndeDirRatio = IndependentDirectorNumber / DirectorNumber,
         Year = year(Enddate)) %>% 
  select(Stkcd = Symbol, Year, IndeDirRatio, ManagerHold = ManageHoldshares)

# 3.5 治理结构——监事会会议数、股东大会会议数、两职合一
corporate_committee <- read_xlsx(file.path(data_path, "Corporate_committee/CG_Agm(Merge Query).xlsx"))
head(corporate_committee)
colnames(corporate_committee) <- c("Symbol", "EndDate", "SuperBoardMeeting", "GeneralMeeting", "TwoJobs")
corporate_committee <- corporate_committee %>% 
  mutate(Year = year(EndDate), 
         TwoJobs = ifelse(TwoJobs == 2, 0, TwoJobs)) %>% 
  select(Stkcd = Symbol, Year, SuperBoardMeeting, GeneralMeeting, TwoJobs)

# 3.6 地区经济发展水平——GDP
city_gdp <- read_xls(file.path(data_path, "City_GDP/EPS_GDP.xls"))
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
prov_gdp <- read_xls(file.path(data_path, "City_GDP/EPS_Prov_GDP.xls"))
colnames(prov_gdp)[1] <- "Province"
colnames(prov_gdp)[2] <- "Year"
colnames(prov_gdp)[3] <- "GDP_p2"
prov_gdp <- prov_gdp[, c(2, 1, 3)] %>% 
  mutate(Year = as.numeric(Year),
         GDP_p2 = as.numeric(GDP_p2))

# 3.7 政府补助——Subsidies
gov_subsidies <- read_xlsx(file.path(data_path, "Gov_Subsidies/FN_FN056.xlsx"))
colnames(gov_subsidies)
gov_subsidies <- gov_subsidies %>% 
  filter(Typrep == 1, Fn05601 == "合计") %>% 
  select(-Typrep, -Fn05601, -Fn05604) %>% 
  group_by(Stkcd, Accper) %>% 
  summarise(Fn05602 = sum(Fn05602, na.rm = TRUE), 
            Fn05603 = sum(Fn05603, na.rm = TRUE)) %>% 
  ungroup()
gov_subsidies_wind <- read_csv(file.path(data_path, "Gov_Subsidies/Wind.csv"))
gov_subsidies_wind$证券简称 <- NULL
colnames(gov_subsidies_wind)[1] <- "Stkcd"
head(gov_subsidies_wind)
gov_subsidies_wind <- gov_subsidies_wind %>% 
  tidyr::gather(key = "Year", value = "Subsidies", -Stkcd) %>% 
  mutate(Year = as.numeric(str_remove(Year, "年")),
         Stkcd = str_remove(Stkcd, "(\\.SZ|\\.SH|\\.BJ)"))
gov_subsidies_wind

# 3.8 公司战略——StrategyScore
financial_sheet_more <- read_xlsx(file.path(data_path, "Financial_Sheet/FS_0824.xlsx"))
colnames(financial_sheet_more)[1] <- "Stkcd"
colnames(financial_sheet_more)[2] <- "Accper"
financial_sheet_more <- financial_sheet_more %>% 
  filter(month(Accper) == 12, 报表类型 == "A") %>% 
  mutate(Year = year(Accper)) %>% 
  select(-Accper, -营业收入, -报表类型)
head(financial_sheet_more)
colnames(financial_sheet_more)[2] <- "FixedAsset"
colnames(financial_sheet_more)[3] <- "IntangibleAsset"
colnames(financial_sheet_more)[4] <- "TotalAsset"
colnames(financial_sheet_more)[5] <- "Revenue"
colnames(financial_sheet_more)[6] <- "SalesCost"
source("Government_Guide_Fund/R/func_tools.R")

employ_info <- read_xlsx(file.path(data_path, "Management_Info/CG_Ybasic.xlsx"))
head(employ_info)
employ_info <- employ_info %>% 
  mutate(Year = year(Reptdt)) %>% 
  select(Stkcd, Year, EmployerNum = Y0601b)
corp_strategy <- left_join(financial_sheet_more, employ_info, 
                           by = c("Stkcd", "Year"))
rm(financial_sheet_more, employ_info)

## 整理数据，计算指标值
corp_strategy <- corp_strategy %>% 
  filter(Year >= 2017) %>% 
  group_by(Stkcd) %>% 
  mutate(num = n()) %>% 
  filter(num == 5) %>% 
  ungroup()
corp_strategy <- corp_strategy %>%   
  mutate(RD = IntangibleAsset / TotalAsset,
         EMPS = EmployerNum / Revenue,
         SEXP = SalesCost / Revenue,
         PPE = FixedAsset / TotalAsset) %>% 
  select(Stkcd, Year, Revenue, EmployerNum, RD, EMPS, SEXP, PPE)
corp_strategy <- corp_strategy %>% 
  lag_n_year(n = 1, key = "Stkcd", value = "Revenue", by = "Year") %>% 
  mutate(REV = ifelse(Year == 2017, 0, (Revenue - Revenue_lag1) / Revenue_lag1)) %>% 
  group_by(Stkcd) %>% 
  mutate(RD5 = mean(RD), EMPS5 = mean(EMPS), REV5 = mean(REV), 
         SEXP5 = mean(SEXP), PPE5 = mean(PPE),
         sdEMP5 = sd(EmployerNum, na.rm = TRUE) / mean(EmployerNum)) %>% 
  select(Stkcd, Year, RD5, EMPS5, REV5, SEXP5, sdEMP5, PPE5) %>% 
  ungroup()

## 计算指标得分函数
make_score <- function(value_vec) {
  ### 计算分段值
  vec <- purrr::map_dbl(.x = seq(0, 1, 0.2)[-1], 
                        .f = \(x) quantile(value_vec, x, na.rm = TRUE))
  ### 向量化计算得分
  score_vec <- purrr::map_dbl(.x = value_vec,
                              .f = \(x) quantile_5(x, vec))
  score_vec
}

## 合并行业类型
corp_strategy <- corp_strategy %>% 
  left_join(listed_corp_info[, c("Stkcd", "Year", "IndustryCode")], 
            by = c("Stkcd", "Year")) %>% 
  mutate(IndustryCode = ifelse(str_sub(IndustryCode, 1, 1) == "C",
                               str_sub(IndustryCode, 1, 2),
                               str_sub(IndustryCode, 1, 1)))

## 计算指标得分
corp_strategy <- corp_strategy %>% 
  group_by(Year, IndustryCode) %>% 
  mutate(PPE5_r = - PPE5) %>% 
  mutate(RD5_Score = make_score(RD5),
         EMPS5_Score = make_score(EMPS5),
         REV5_Score = make_score(REV5),
         SEXP5_Score = make_score(SEXP5),
         sdEMP5_Score = make_score(sdEMP5),
         PPE5_Score = make_score(PPE5_r)) %>% 
  select(-PPE5_r) %>% 
  ungroup()

corp_strategy %>% count(RD5_Score)
corp_strategy %>% count(EMPS5_Score)
corp_strategy %>% count(REV5_Score)
corp_strategy %>% count(SEXP5_Score)
corp_strategy %>% count(sdEMP5_Score)
corp_strategy %>% count(PPE5_Score)

corp_strategy <- corp_strategy %>% 
  mutate(TotalScore = RD5_Score + EMPS5_Score + REV5_Score + 
           SEXP5_Score + sdEMP5_Score + PPE5_Score)
corp_strategy_result <- corp_strategy %>% 
  select(Stkcd, Year, StrategyScore = TotalScore) 


# 4. 模型检验 -----------------------------------------------------------------

# 4.1 合并会计稳健性、引导基金持股、控制变量
## 在计算会计稳健性时就已经去掉了特定样本
merged_for_reg <- accounting_conservatism %>% 
  select(Stkcd, Year, G_Score, C_Score,
         EPS, YOP, YCP, LYCP, DR, RET, Size, MB, Lev,
         MarketValue = 年个股总市值) %>% 
  left_join(merged_Big10SH_GGF_nodupl, by = c("Stkcd", "Year")) %>% # 引导基金
  left_join(listed_corp_info, by = c("Stkcd", "Year")) %>%          # 行业
  left_join(equity_nature, by = c("Stkcd", "Year")) %>%             # 股权
  left_join(auditor_info, by = c("Stkcd", "Year")) %>%              # 审计
  left_join(insti_investor, by = c("Stkcd", "Year")) %>%            # 机构
  left_join(management_info, by = c("Stkcd", "Year")) %>%           # 管理层
  left_join(corporate_committee, by = c("Stkcd", "Year")) %>%       # 委员会
  left_join(gov_subsidies_wind, by = c("Stkcd", "Year")) %>%        # 政府补贴
  left_join(corp_strategy_result, by = c("Stkcd", "Year"))
  
merged_for_reg <- merged_for_reg %>% 
  left_join(city_gdp, by = c("City", "Year")) %>%
  mutate(prov_temp = str_remove(Province, "省|市")) %>% 
  left_join(prov_gdp, by = c("prov_temp" = "Province", "Year")) %>% 
  mutate(
    # INS = ifelse(is.na(INS), 0, INS),
    # SuperINS = ifelse(is.na(SuperINS), 0, SuperINS),
    GovFund = if_else(is.na(GovFund), 0, 1),
    GDP_p = ifelse(is.na(GDP_p) == TRUE, GDP_p2, GDP_p)
  ) %>% 
  select(-prov_temp, -GDP_p2) %>% 
  rename(GGF = GovFund) %>% 
  filter(Year > 2015)

# 重新排序必需的变量
colnames(merged_for_reg)
merged_for_reg <- merged_for_reg[, c("Stkcd", "ShortName", "Year",
      "G_Score", "C_Score", "EPS", "YOP", "YCP", "LYCP", "DR", "RET",
      "GGF", "GGFLevel", "GGFProvince", "IsFirstHold", 
      "HoldNum", "HoldRatio", "HoldRank",
      "Size", "MB", "Lev", "RegionFin", "MarketValue", "SOE", 
      "OneHolder", "TenHolder", "Big4", "INS", "SuperINS", 
      "IndeDirRatio", "ManagerHold", "SuperBoardMeeting", "StrategyScore",
      "GeneralMeeting", "TwoJobs", "ListingYear", "GDP_p", "Subsidies",
      "IndustryCode", "Province", "City")]
## 变量总计：41 + (1个CG)
## 标识（3）：Stkcd ShortName Year
## 会计稳健性（8）：G_Score C_Score EPS YOP YCP LYCP DR RET
## 引导基金（7）：GovFund GGFLevel GGFProvince IsFirstHold HoldNum HoldRatio HoldRank
## 控制变量（20）：Size MB Lev RegionFin MarketValue SOE OneHolder 
## TenHolder Big4 INS SuperINS IndeDirRatio ManagerHold SuperBoardMeeting
## GeneralMeeting TwoJobs ListingYear GDP_p Subsidies StrategyScore
## 固定效应（3）：IndustryCode Province City

# 4.2 构建公司治理主成分
## 1.第一大股东持股比例 OneHolder 负面  2.独立董事比例 IndeDirRatio                     正面  
## 3.两职合一 TwoJobs             负面  4.股东大会次数 GeneralMeeting                   正面  
## 5.管理者持股比例 ManagerHold   正面  6.股权制衡度 (TenHolder-OneHolder)/OneHolder    正面
## 7.产权性质 SOE                 负面  8.监督型机构持股比例 SuperINS                   正面     
governance_data <- merged_for_reg[, c("OneHolder", "IndeDirRatio", "TwoJobs",
    "GeneralMeeting", "ManagerHold", "MarketValue", "YCP", "TenHolder", "SOE", "SuperINS")]
governance_data <- governance_data %>% 
  mutate(ManagerHoldRatio = ManagerHold * YCP / (MarketValue * 1000) * 100, 
         OtherTenHolder = (TenHolder - OneHolder)/OneHolder) %>% 
  select(-YCP, -ManagerHold, -MarketValue, -TenHolder) %>% 
  mutate(OneHolder = 1 / OneHolder,
         TwoJobs = ifelse(TwoJobs == 1, 0, 1))
governance_data_scaled <- scale(governance_data, center = TRUE, scale = TRUE) %>% as_tibble()
summary(governance_data)
psych::KMO(governance_data)       # KMO检验
bartlett.test(governance_data)    # Bartlett检验
governance_pca <- prcomp(~ ., data = governance_data, 
                         na.action = na.exclude, 
                         scale. = TRUE)
governance_pca$x %>% as_tibble() 
governance_pca$rotation           # 检查载荷系数符号
governance_pca %>% summary()      # 确定主成分个数为5
CG_Score <- governance_pca$x[, 1:5] %*% 
  as.matrix((governance_pca$sdev ^ 2)[1:5] / sum((governance_pca$sdev ^ 2)[1:5]))
merged_for_reg$CG = CG_Score[, 1] # CG_Score为21318行

# 4.3 存储合并结果
export(x = merged_for_reg, 
       file.path(output_path, "merged_for_reg_21318.dta"))
# rm(equity_nature, auditor_info, insti_investor,
#    management_info, corporate_committee)
# rm(financial_sheet, year_return, accounting_conservatism)

merged_for_reg_reduced <- merged_for_reg %>% 
  group_by(Stkcd) %>% 
  mutate(num = n()) %>% 
  filter(num > 2) %>% 
  select(-num) %>% 
  ungroup()

merged_for_reg_reduced %>% 
  group_by(Stkcd) %>% 
  summarise(YearNum = n(),
            HoldNum = sum(GGF)) %>% 
  ungroup() %>% 
  filter(HoldNum > 0) %>% 
  count(YearNum - HoldNum)
## 使用不删除年份的样本，总共286家匹配到的上市公司中，有95家被引导基金
## 始终持股，这意味着使用公司固定效应可能面临严重的共线性问题

dim(merged_for_reg_reduced)
export(x = merged_for_reg_reduced, 
       file.path(output_path, "merged_for_reg_reduced_19796.dta"))

# 4.4 检验测试
formula_basu_ggf <- formula(EPS / LYCP ~ 
        DR + RET + DR:RET
      + GovFund + GovFund:DR + GovFund:RET + GovFund:DR:RET
      # + Size + Size:DR + Size:RET + Size:DR:RET
      # + MB + MB:DR + MB:RET + MB:DR:RET
      # + Lev + Lev:DR + Lev:RET + Lev:DR:RET
      + factor(Year) + factor(IndustryCode)
)
formula_khan_ggf <- formula(C_Score ~ GovFund
      + SOE + Big4 + INS
      + factor(Year) + factor(IndustryCode)
)
merged_for_reg %>% 
  lm(formula_khan_ggf, data = .) %>% 
  summary()

# save.image()
save.image("Government_Guide_Fund/temp/2022-08-27_envir-image.RData")


