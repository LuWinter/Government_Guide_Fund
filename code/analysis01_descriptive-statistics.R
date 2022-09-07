
##############################################################
# Programmer: Lu Winter
# Date Created: 2022-08-28
# Task: Perform descriptive statistics
##############################################################


# 0. Initial Setup --------------------------------------------------------

## 加载R包
library(dplyr)
library(echarts4r)
library(stringr)
library(lubridate)
library(readxl)
library(readr)
library(purrr)
library(RSQLite)

## 预定义输入输出路径
data_path <- "data"
output_path <- "output"
db_path <- "data/GGF_project_store.sqlite"

## 建立数据库连接
con_sqlite <- dbConnect(RSQLite::SQLite(), db_path)
dbListTables(con_sqlite)


# 1. Load data ------------------------------------------------------------

## 导入上市公司股东与引导基金匹配结果
merged_Big10SH_GGF_nodupl <- dbReadTable(conn = con_sqlite, 
                                         name = "merged_Big10SH_GGF_nodupl")

## 导入政府引导基金
GGF_list_all <- dbReadTable(conn = con_sqlite, 
                            name = "GGF_list_all")

## 导入上市公司信息
identifier_info <- dbReadTable(conn = con_sqlite,
                               name = "identifier_info")

## 筛选匹配样本
merged_GGF_identifier <- merged_Big10SH_GGF_nodupl %>% 
  left_join(identifier_info, by = c("Stkcd", "Year")) %>% 
  filter(!is.na(ShortName))

## 导入最终回归样本
final_sample <- read_rds(file.path(output_path, "old/merged-for-reg-reduced_2022-09-05.rds"))
final_sample <- filter(final_sample, GGF == 1)


# 2. Basic statistics -----------------------------------------------------

## 匹配样本的年度分布
merged_Big10SH_GGF_nodupl%>% 
  count(Year) %>% 
  filter(Year >= 2010) %>% 
  mutate(Year = factor(Year)) %>% 
  e_charts(x = `Year`) %>% 
  e_bar(serie = n, name = "匹配样本量", legend = FALSE,
        itemStyle = htmlwidgets::JS("{normal: 
            {label : {show: true, fontSize: 14, position: 'outside'}}
        }")) %>% 
  e_title("引导基金持股的年份分布")

## 引导基金的持股时间
merged_Big10SH_GGF_nodupl %>% 
  count(GGFName, Stkcd, name = "持股时间") %>% 
  count(持股时间) %>% 
  mutate(持股时间 = paste0(持股时间, "年", " (", n, ")")) %>% 
  e_charts(x = `持股时间`) %>% 
  e_pie(serie = n, name = "年限", legend = FALSE) %>% 
  e_title("引导基金的持股时间")

## 引导基金首次持股距IPO时长
merged_Big10SH_GGF_nodupl %>% 
  group_by(Stkcd, GGFName) %>% 
  summarise(first_hold = min(Year)) %>% 
  ungroup() %>% 
  left_join(identifier_info, by = c("Stkcd", "first_hold" = "Year")) %>% 
  filter(!is.na(ShortName)) %>% 
  mutate(time_interval = as.integer(first_hold - ListingYear)) %>%  
  count(时间间隔 = time_interval) %>% 
  mutate(时长段 = ifelse(时间间隔 <= 1, "不超过1年", 
                  ifelse(时间间隔 <= 5, "2~5年",
                  ifelse(时间间隔 <= 10, "6～10年", 
                  "10年以上")))) %>% 
  group_by(时长段) %>%
  summarise(n = sum(n)) %>% 
  mutate(时长段 = paste0(时长段, " (", n, ")")) %>% 
  e_charts(x = `时长段`) %>% 
  e_pie(n, legend = FALSE) %>% 
  e_title("引导基金首次持股距IPO时长")


listed_corp_info <- read_xlsx(file.path(data_path, "2022-08-04_corporate-info.xlsx"))
listed_corp_info <- mutate(listed_corp_info, Year = year(EndDate))

# 匹配样本的行业分布
industry_distribution <- merged_Big10SH_GGF_nodupl %>% 
  count(Stkcd, Year) %>%
  left_join(y = listed_corp_info[, c("Symbol", "IndustryName", "Year")],
            by = c("Stkcd" = "Symbol", "Year")) %>% 
  count(IndustryName) %>% 
  filter(n >= 20) %>% 
  ungroup()
other_industry_obs <- nrow(merged_Big10SH_GGF_nodupl) - sum(industry_distribution$n)
industry_distribution %>% 
  arrange(-n) %>% 
  add_row(IndustryName = "其他行业", n = other_industry_obs) %>% 
  mutate(IndustryName = paste0(IndustryName, " (", n, ")")) %>% 
  e_charts(x = IndustryName) %>%
  e_pie(serie = n, name = "数量", legend = FALSE)


# 3. GGF Level ----------------------------------------------------------------

# 与基金持股时间
merged_Big10SH_GGF_nodupl %>% 
  group_by(Stkcd) %>% 
  mutate(持股时间 = n()) %>% 
  ungroup() %>% 
  distinct(Stkcd, GGFName, .keep_all = TRUE) %>% 
  filter(GGFLevel != "--") %>% 
  count(GGFLevel, 持股时间) %>% 
  mutate(时长段 = ifelse(持股时间 <= 1, "1年", 
                  ifelse(持股时间 <= 3, "2~3年",
                  ifelse(持股时间 <= 5, "4～5年", "5年以上")))) %>% 
  group_by(GGFLevel, 时长段)  %>% 
  summarise(n = sum(n)) %>% 
  mutate(ratio = paste0(round(n / sum(n) * 100, 1) , "%"))

# 与首次持股距IPO时长
ipo_time <- listed_corp_info %>% 
  mutate(ListingYear = year(LISTINGDATE)) %>% 
  select(Stkcd = Symbol, ListingYear) %>% 
  distinct(Stkcd, .keep_all = TRUE)

merged_Big10SH_GGF_nodupl %>% 
  group_by(Stkcd) %>% 
  mutate(first_hold = min(Year)) %>% 
  ungroup() %>% 
  distinct(Stkcd, GGFName, .keep_all = TRUE) %>% 
  left_join(ipo_time, by = c("Stkcd")) %>% 
  # filter(!is.na(ShortName)) %>%
  mutate(time_interval = as.integer(first_hold - ListingYear)) %>% 
  count(时间间隔 = time_interval, GGFLevel) %>% 
  mutate(时长段 = ifelse(时间间隔 <= 1, "不超过1年", 
                  ifelse(时间间隔 <= 5, "2~5年",
                  ifelse(时间间隔 <= 10, "6～10年", "10年以上")))) %>% 
  group_by(GGFLevel, 时长段) %>%
  summarise(n = sum(n)) %>% 
  filter(GGFLevel != "--") %>%
  group_by(GGFLevel) %>% 
  mutate(ratio = paste0(round(n / sum(n) * 100, 1) , "%"))

# 与持股高新技术企业
listed_corp_industry <- listed_corp_info %>%
  mutate(Year = year(EndDate)) %>% 
  select(Stkcd = Symbol, Year, IndustryCode)

merged_Big10SH_GGF_nodupl %>% 
  distinct(Stkcd, GGFName, .keep_all = TRUE) %>% 
  left_join(y = listed_corp_industry, by = c("Stkcd", "Year")) %>% 
  mutate(high_tech = if_else(IndustryCode == "C27" | IndustryCode == "C37" |
                             IndustryCode == "C39" | IndustryCode == "C40", 1,
                     if_else(str_sub(IndustryCode, 1, 1) == "I" | 
                             str_sub(IndustryCode, 1, 1) == "M", 1, 0))) %>% 
  filter(GGFLevel != "--") %>% 
  count(GGFLevel, high_tech) %>% 
  group_by(GGFLevel) %>% 
  mutate(ratio = paste0(round(n / sum(n) * 100, 1) , "%"))

listed_corp_info %>% 
  mutate(high_tech = if_else(IndustryCode == "C27" | IndustryCode == "C37" |
                             IndustryCode == "C39" | IndustryCode == "C40", 1,
                     if_else(str_sub(IndustryCode, 1, 1) == "I" | 
                             str_sub(IndustryCode, 1, 1) == "M", 1, 0))) %>% 
  filter(!is.na(high_tech)) %>% 
  count(Year = year(EndDate), high_tech) %>% 
  filter(Year >= 2016)


# 4. t-test --------------------------------------------------------------------

# 与持股高新技术企业
merged_GGF_corp <- merged_Big10SH_GGF_nodupl %>%
  group_by(Stkcd) %>%
  mutate(持股时间 = n(),
         first_hold = min(Year)) %>%
  ungroup() %>% 
  left_join(ipo_time, by = c("Stkcd")) %>%
  mutate(time_interval = as.integer(first_hold - ListingYear)) %>%
  mutate(时间间隔 = time_interval) %>% 
  select(Stkcd, Year, GGFName, GGFLevel, GGFProvince, 持股时间, 时间间隔) %>% 
  left_join(y = listed_corp_industry,
            by = c("Stkcd", "Year")) %>%
  mutate(high_tech = if_else(IndustryCode == "C27" | IndustryCode == "C37" |
                             IndustryCode == "C39" | IndustryCode == "C40", 1,
                     if_else(str_sub(IndustryCode, 1, 1) == "I" |
                             str_sub(IndustryCode, 1, 1) == "M", 1, 0))) %>% 
  # distinct(GGFName, Stkcd, .keep_all = TRUE) %>% 
  select(-Year, -IndustryCode) %>% 
  mutate(国家级基金 = ifelse(GGFLevel == "国家级", 1, 
                      ifelse(GGFLevel == "--", NA, 0)),
         发达地区基金 = ifelse(GGFProvince == "北京市" | 
                               GGFProvince == "上海市" |
                               GGFProvince == "广东省", 1, 0))

## 变量描述性统计与t检验
t.test(merged_GGF_corp$持股时间, merged_GGF_corp$国家级基金)
t.test(merged_GGF_corp$持股时间, merged_GGF_corp$发达地区基金)

t.test(merged_GGF_corp$时间间隔, merged_GGF_corp$国家级基金)
t.test(merged_GGF_corp$时间间隔, merged_GGF_corp$发达地区基金)

t.test(merged_GGF_corp$high_tech, merged_GGF_corp$国家级基金)
t.test(merged_GGF_corp$high_tech, merged_GGF_corp$发达地区基金)

c("国家级基金", "发达地区基金") %>% map(.f = function(x) {
  merged_GGF_corp %>% 
    filter(!is.na(eval(parse(text = x)))) %>% 
    group_by(Group = eval(parse(text = x))) %>% 
    summarise(      n         = n(),
                    mean_hold_time  = mean(持股时间),
                    sd_hold_time    = sd(持股时间),
                    mean_first_hold = mean(时间间隔),
                    sd_first_hold   = sd(时间间隔),
                    mean_high_tech  = mean(high_tech),
                    sd_high_tech    = sd(high_tech)) %>% 
    t()
  }
)


