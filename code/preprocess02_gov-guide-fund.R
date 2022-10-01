
##############################################################
# Programmer: Lu Winter
# Date Created: 2022-08-28
# Task: 1. Clean and process government guide fund data
#       2. Generate variable GovFund
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


# 1. 处理上市公司十大股东 -----------------------------------------------------------

# 1.1 分别读入两段股东数据
### 数据来源：CSMAR公司研究 - 股东 - 十大股东文件 (筛选年度报告)
### 数据属性：2003-2021年度 全部股票代码 18个变量 488571条观测
Big10_Shareholders <- read_csv(
  file = file.path(data_path, "2022-07-22_big10-shareholders-part01.csv"), 
)
Big10_Shareholders_plus <- read_csv(
  file = file.path(data_path, "2022-07-22_big10-shareholders-part02.csv")
)

# 1.2 检查变量类型后合并
str(Big10_Shareholders)
str(Big10_Shareholders_plus)
Big10_Shareholders_plus$Stkcd <- as.character(Big10_Shareholders_plus$Stkcd)
Big10_Shareholders <- bind_rows(Big10_Shareholders, Big10_Shareholders_plus)
Big10_Shareholders <- Big10_Shareholders %>% 
  filter(month(Reptdt) == 12)
rm(Big10_Shareholders_plus)


# 2. 初步处理引导基金 -------------------------------------------------------------

# 2.1 分别读入两段引导基金数据
### 数据来源：清科基金数据库 - 政府引导基金 (截止2022-06-30)
### 数据属性：全部年度 全部变量 1910条观测
GGF_list_qingke <- read_xls(
  path = file.path(data_path, "Gov_Guide_Fund/Gov_Guide_Fund_page1.xls")
)
GGF_list_qingke_plus <- read_xls(
  path = file.path(data_path, "Gov_Guide_Fund/Gov_Guide_Fund_page2.xls")
)

# 2.2 重组引导基金数据
fill_empty_data <- function(data) {
  for (row_index in 1:nrow(data)) {
    if (is.na(data[row_index, 1]) & !is.na(data[row_index, 17])) {
      temp <- data[row_index, 17:19]
      data[row_index, ] = data[row_index - 1, ]
      data[row_index, 17:19] <- temp
    }
  }
  return(data)
}
GGF_list_qingke <- bind_rows(
  fill_empty_data(GGF_list_qingke),
  fill_empty_data(GGF_list_qingke_plus)
)
str(GGF_list_qingke)
rm(GGF_list_qingke_plus)

# 2.3 计算引导基金出资背景1
GGF_investor_qingke <- GGF_list_qingke[, c(1:2, 16, 17:19)] %>% 
  group_by(`基金简称`) %>% 
  mutate(`出资人数量` = n()) %>% 
  ungroup()

# 2.4 只保留引导基金基本属性并去重
GGF_list_qingke <- GGF_list_qingke[, -(17:19)] %>% 
  distinct()

# 2.5 计算引导基金注册时间与省份
founded_year <- GGF_list_qingke$成立时间 %>% 
  str_sub(start = 1, end = 4)
founded_province <- GGF_list_qingke$注册地区 %>% 
  str_split(pattern = "\\|") %>% 
  map(2)
founded_province_vec <- c()
for(i in 1:length(founded_province)) {
  if (!is.null(founded_province[[i]])) {
    founded_province_vec <- c(founded_province_vec, founded_province[[i]])
  } else {
    founded_province_vec <- c(founded_province_vec, NA)
  }
}
founded_province <- founded_province_vec
GGF_list_qingke <- bind_cols(
  GGF_list_qingke, 
  `成立年份` = founded_year,
  `注册省份` = founded_province
)
rm(founded_province_vec, i, founded_province, founded_year)

# 2.6 计算引导基金出资背景2
GGF_investor_qingke <- GGF_investor_qingke %>% 
  mutate(`出资人是否国资2` = ifelse(`出资人数量` == 1, "是", `出资人是否国资`)) %>% 
  mutate(`出资人是否国资2` = ifelse(`出资人是否国资2` == "是", 1, 0)) %>% 
  group_by(`基金简称`) %>% 
  mutate(`非国资出资人数量` = `出资人数量` - sum(`出资人是否国资2`)) %>% 
  select(-`出资人是否国资2`)
GGF_list_qingke <- bind_cols(
  GGF_list_qingke, 
  distinct(GGF_investor_qingke[, c(1, 8, 7)])[, 2:3]
)


# 3. 测试匹配引导基金与十大股东 --------------------------------------------------------

# 3.1 去除引导基金全称最后的括号，进行匹配
GGF_list_qingke <- GGF_list_qingke %>%
  mutate(`基金全称2` = str_remove_all(`基金全称`, "（.{4}）$"))
Big10_Shareholders %>% 
  mutate(`股东全称2` = str_remove_all(S0301a, "\\(.{4}\\)$")) %>% 
  left_join(GGF_list_qingke[, c(1, 27, 7:8, 23:26)], 
            by = c("股东全称2" = "基金全称2")) %>%
  filter(!is.na(`成立年份`)) %>% 
  count(年度 = year(Reptdt), name = "匹配样本量")
### 2017～2021得到 68 + 77 + 94 + 129 + 165 = 533

# 3.2 进一步整理十大股东名称
### 1.拆分连字符  2.去除括号
clean_shareholder_name <- function(old_name) {
  if (str_detect(old_name, "[-－]") & !str_detect(old_name, "^[a-zA-Z]")) {
    new_name = tail(
      as.vector(
        str_split(old_name, "[-－]", simplify = TRUE)
      ), 
    1)
  } else {
    new_name = old_name
  }
  new_name = str_remove(new_name, "[（\\(].{1,4}[）\\)]$")
  return(new_name)
}
Big10_Shareholders$`股东全称3` <- map_chr(Big10_Shareholders$S0301a, clean_shareholder_name)
GGF_list_qingke$`基金全称2` <- map_chr(GGF_list_qingke$基金全称, clean_shareholder_name)

Big10_Shareholders %>% 
  left_join(GGF_list_qingke[, c(1, 27, 7:8, 23:26)], 
            by = c("股东全称3" = "基金全称2")) %>% 
  filter(!is.na(`成立年份`)) %>%
  count(年度 = year(Reptdt), name = "匹配样本量")
### 2017～2021得到 73 + 85 + 106 + 148 + 194 = 606

# 3.4 读入增补的引导基金数据
GGF_list_add <- read_xlsx(file.path(data_path, "Gov_Guide_Fund/gov_guide_fund_add.xlsx"))
colnames(GGF_list_add)
GGF_list_add <- GGF_list_add %>% 
  filter(str_detect(基金全称, "基金"))
### 由于在Excel上进行增补数据的更新，所以更新后同步到数据库
dbWriteTable(
  conn = con_sqlite,
  name = "GGF_list_add",
  value = GGF_list_add,
  overwrite = TRUE
)

# 3.5 合并全部引导基金数据
GGF_list_all <- bind_rows(
  select(GGF_list_qingke, 基金全称 = 基金全称2, 注册省份, 基金级别),
  select(GGF_list_add, 基金全称, 注册省份, 基金级别)
)
GGF_list_all$GGF <- 1
GGF_list_all <- distinct(GGF_list_all, 基金全称, .keep_all = TRUE)
dbWriteTable(
  conn = con_sqlite,
  name = "GGF_list_all",
  value = GGF_list_all,
  overwrite = TRUE
)

# 3.6 合并引导基金与十大股东数据
merged_SH_GGF <- Big10_Shareholders %>% 
  left_join(GGF_list_all, 
            by = c("股东全称3" = "基金全称"))
merged_SH_GGF %>% 
  filter(GGF == 1) %>% 
  count(年度 = year(Reptdt), name = "匹配样本量")
### 使用2022-08-11增补的数据: 2017～2021得到 99 + 111 + 130 + 177 + 222 = 739
### 使用2022-08-28增补的数据: 2017～2021得到 145 + 167 + 204 + 268 + 315 = 1099

# 3.7 对未合并的十大股东进行测试
### 引导基金关键词: 创业(投资)基金
### 产业(发展|投资|促进)基金   发展(..)基金  建设(..)基金
check_not_merged <- function(data = merged_SH_GGF, keyword) {
  data[str_detect(data$股东全称3, keyword) &
       month(data$Reptdt) == 12 &
       is.na(data$GGF) == TRUE &
       str_detect(data$股东全称3, "基金") &
       str_detect(data$股东全称3, "证券") == FALSE, ] %>% 
    select(Stkcd, Reptdt, 股东全称3) %>%
    count(股东全称3, sort = TRUE) %>% 
    print(n = 50)
}
check_not_merged(keyword = "文化")
### 正则表达式Tips
### (1) 零宽正向后行断言 (?<=pattern)基金 (匹配前面的内容满足pattern)
### (2) 零宽负向后行断言 (?<!pattern)基金 (匹配前面的内容不满足pattern)

# 3.8 抽取已合并的十大股东样本
merged_SH_GGF_dupl <- merged_SH_GGF %>% 
  filter(GGF == 1) %>% 
  mutate(Year = year(Reptdt)) %>% 
  select(Stkcd, Year, GGF, GGFName = 股东全称3,
         GGFLevel = 基金级别, GGFProvince = 注册省份,
         HoldNum = S0302a, HoldRatio = S0304a, HoldRank = S0306a)
### 1308个观测，由于存在多个引导基金同年度持股一家上市公司，因此存在重复观测
### 对于重复观测，按Stkcd和Year聚合，保存持股比例高的,即排名靠前的
merged_SH_GGF_nodupl <- merged_SH_GGF_dupl |> 
  group_by(Stkcd, Year) |> 
  filter(HoldRatio == max(HoldRatio)) |> 
  ungroup() |> 
  distinct(Stkcd, Year, .keep_all = TRUE)
merged_SH_GGF_nodupl |> dim()
### 去重后得到1196个观测，其中2017-2021年有995个

# 3.9 保存十大股东与引导基金合并数据
write_rds(
  x = merged_SH_GGF, 
  file = file.path(output_path, "merged_Big10SH_GGF.rds")
)
write_rds(
  x = merged_SH_GGF_nodupl, 
  file = file.path(output_path, "merged_Big10SH_GGF_nodupl.rds")
)
dbWriteTable(
  conn = con_sqlite,
  name = "merged_Big10SH_GGF_nodupl",
  value = merged_SH_GGF_nodupl,
  overwrite = TRUE
)
dbWriteTable(
  conn = con_sqlite,
  name = "merged_Big10SH_GGF_dupl",
  value = merged_SH_GGF_dupl,
  overwrite = TRUE
)

dbDisconnect(con_sqlite)
rm(list = ls())
