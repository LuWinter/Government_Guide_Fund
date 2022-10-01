
##############################################################
# Programmer: Lu Winter
# Date Created: 2022-09-29
# Task: Perform PSM-DID design
##############################################################


# 0. Initial Setup --------------------------------------------------------

## 加载R包
library(dplyr)
library(purrr)
library(DBI)
library(lubridate)
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


# 1. 收集处理组 ----------------------------------------------------------------

identifier <- dbReadTable(con_sqlite, "identifier")
merged_SH_GGF <- readRDS(file.path(output_path, "merged_Big10SH_GGF.rds"))
accounting_conservatism <- readRDS(file.path(output_path, "accounting_conservatism.rds"))

## 所有被持股的样本
merged_SH_GGF_dupl <- merged_SH_GGF |> 
  filter(GovFund == 1) |> 
  mutate(Year = year(Reptdt)) |> 
  select(Stkcd, Year, GovFund, GGFName = 股东全称3,
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
### 有效的处理组为86家企业，371条观测
### 时间序列上[38, 55, 81, 84, 63, 50]

stata(
  src = "code/analysis04_PSM-DID.do", 
  data.in = treat_full
)
### Q:为什么加入稳健聚类标准误后，显著性严重下降？



# 2. 匹配控制组 ----------------------------------------------------------------

treat_for_match <- treat_full |> 
  filter(Time == "b1")

### 用于匹配的变量（上一期）
### 1.市值  2.SOE    3.增长率  4.ROA
### 5.LEV   6.研发   7.HHI     8.机构持股







