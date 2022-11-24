
##############################################################
# Programmer: Lu Winter
# Date Created: 2022-09-16
# Task: Perform robust test
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


# 1. Select Sample --------------------------------------------------------

identifier <- dbReadTable(
  conn = con_sqlite,
  name = "identifier"
)
merged_for_reg <- 
  readRDS(file.path(output_path, "merged-for-reg_2022-10-07.rds"))
merged_for_reg_reduced <- 
  readRDS(file.path(output_path, "merged-for-reg-reduced_2022-10-07.rds"))


### 样本为 1. 从2017到被GGF持股为止的年度  2. 被GGF持股的年度
within_corps <- merged_for_reg_reduced |> 
  group_by(Stkcd) |> 
  mutate(FirstGGFYear = Year[which(GGF * Year != 0)[1]]) |> 
  filter(Year < FirstGGFYear | GGF == 1)

stata(
  src = "code/robust01_simple-reg.do",
  data.in = filter(within_corps, Year >= 2016)
)
### 发现：只加入一般固定效应，结论很显著；
### 加入高维固定效应后，结论非常不显著；
### 用公司层面的方差聚类来代替高维固定效应，结论不显著
### 问题：为什么结论受到高维固定效应（主要是年度#行业）影响很大？
### 猜想：引导基金在某些年度，重点支持了某些行业

within_corps |> 
  ungroup() |> 
  mutate(
    Industry = ifelse(
      stringr::str_sub(IndustryCode, 1, 1) != "C", 
      stringr::str_sub(IndustryCode, 1, 1),
      stringr::str_sub(IndustryCode, 1, 2))
  ) |> 
  mutate(
    Industry = as.factor(Industry),
    Year = as.factor(Year)
  ) |> 
  group_by(Industry) |> 
  filter(n() > 30, GGF == 1) |> 
  ungroup() |> 
  count(Industry, Year) |> 
  arrange(Industry) |> 
  print(n = 100)
### 根据年度-行业统计结果可知，GGF在18·19年间持股数量猛增，
### 尤其是C2、C3、I行业

### 用持股比例代替是否持股
div_10 <- function(x, vec) {
  for (i in 1:10) {
    if (x < quantile(vec, 0.1 * i))
    {return(i)}
  }
  return(NA)
}

test_holdratio <- merged_for_reg_reduced |> 
  filter(!is.na(HoldRatio))
test_holdratio$HoldRatio <- map_dbl(
  .x = test_holdratio$HoldRatio,
  .f = \(x) div_10(x, test_holdratio$HoldRatio)
)
test_holdratio <- bind_rows(
  test_holdratio, 
  filter(merged_for_reg_reduced, is.na(HoldRatio))
) |> 
  mutate(
    HoldRatio = ifelse(is.na(HoldRatio), 0, HoldRatio)
  )
stata(
  src = "code/robust02_hold-ratio.do",
  data.in = filter(test_holdratio, Year >= 2016)
)
### 结果发现，不是非常显著，不同的样本显著性有所区别，
### 这或许可以通过调整样本来解决

stata(
  src = "code/robust03_PSM.do",
  data.in = filter(merged_for_reg_reduced, Year >= 2017)
)


test_C_score <- merged_for_reg_reduced
test_C_score$C_Score <- map_dbl(
  .x = test_C_score$C_Score,
  .f = \(x) div_10(x, test_C_score$C_Score)
)
stata(
  src = "code/robust04_C-score.do",
  data.in = filter(test_C_score, Year >= 2016)
)

dbDisconnect(con_sqlite)

