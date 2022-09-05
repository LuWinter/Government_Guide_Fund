
library(dplyr)
library(echarts4r)
library(stringr)
library(lubridate)
library(readxl)
library(readr)
library(purrr)
library(RSQLite)


# 1. Read data ------------------------------------------------------------

data_path <- "Government_Guide_Fund/data/"

# 导入已合并的上市公司
merged_result <- read_rds("Government_Guide_Fund/output/merged_Big10SH_GGF.rds")
merged_result <- merged_result %>% 
  filter(flag == 1)
merged_result <- merged_result[, -c(20, 21)]

# 导入政府引导基金
gov_fund_little <- read_csv(file = paste0(data_path, "Gov_Guide_Fund/gov_guide_fund_little.csv"))
gov_fund_little <- gov_fund_little %>%
  mutate(`基金全称2` = str_remove_all(`基金全称`, "（.{4}）$"))
gov_fund_full <- read_csv(file = paste0(data_path, "Gov_Guide_Fund/gov_guide_fund_full.csv"))

# 导入上市公司信息
listed_corporate_info <- read_xlsx(path = paste0(data_path, "Listed_Corporate_Info/STK_LISTEDCOINFOANL.xlsx"))


# 2. Basic statistics -----------------------------------------------------

# 匹配样本的年度分布
merged_result %>% 
  count(匹配年份 = year(Reptdt)) %>% 
  mutate(匹配年份 = factor(匹配年份)) %>% 
  e_charts(x = `匹配年份`) %>% 
  e_bar(n, name = "匹配样本量")

# 引导基金的持股时间
merged_result %>% 
  count(股东全称3, Stkcd, name = "持股时间") %>% 
  count(持股时间) %>% 
  mutate(持股时间 = paste0(持股时间, "年")) %>% 
  e_charts(x = `持股时间`) %>% 
  e_pie(n, name = "年限", legend = FALSE) %>% 
  e_title("引导基金的持股时间")

# 引导基金首次持股距IPO时长
ipo_time <- listed_corporate_info %>% 
  select(Symbol, LISTINGDATE) %>% 
  mutate(Listed_year = year(LISTINGDATE)) %>% 
  distinct()
merged_result %>% 
  group_by(Stkcd, 股东全称3) %>% 
  summarise(first_hold = min(Reptdt)) %>% 
  ungroup() %>% 
  left_join(ipo_time, by = c("Stkcd" = "Symbol")) %>% 
  mutate(time_interval = as.integer(date(first_hold) - date(LISTINGDATE))) %>% 
  count(时间间隔 = ceiling(time_interval / 365)) %>% 
  mutate(时长段 = ifelse(时间间隔 <= 1, "不超过1年", 
                  ifelse(时间间隔 <= 5, "2~5年",
                  ifelse(时间间隔 <= 10, "6～10年", 
                  "10年以上")))) %>% 
  group_by(时长段) %>%
  summarise(n = sum(n)) %>% 
  e_charts(x = `时长段`) %>% 
  e_pie(n, legend = FALSE) %>% 
  e_title("引导基金首次持股距IPO时长")

# 匹配样本的行业分布
# merged_result %>% 
#   filter(flag == 1) %>% 
#   count(Stkcd, Year = year(Reptdt)) %>% 
#   left_join(y = Listed_Corporate_Info[, c(3, 4, 6, 7, 38)], 
#             by = c("Stkcd" = "Symbol", "Year")) %>% 
#   count(Industry = str_sub(IndustryCode, 1, 1)) %>% 
#   left_join(y = Industry_Code[, c(1, 3)], by = c("Industry" = "行业编码")) %>% 
#   filter(n >= 5) %>% 
#   e_charts(x = `行业中文名称`) %>% 
#   e_pie(n, name = "数量")


# 3. 引导基金行政级别 -------------------------------------------------------------

# 与基金持股时间
merged_result %>% 
  count(股东全称3, Stkcd, name = "持股时间") %>% 
  left_join(y = gov_fund_little[, c(23, 7:9, 16)], 
            by = c("股东全称3" = "基金全称2")) %>% 
  filter(!is.na(基金级别), 基金级别 != "--") %>% 
  count(基金级别, 持股时间) %>% 
  mutate(时长段 = ifelse(持股时间 <= 1, "1年", 
                  ifelse(持股时间 <= 3, "2~3年",
                  ifelse(持股时间 <= 5, "4～5年", "5年以上")))) %>% 
  group_by(基金级别, 时长段) %>% 
  summarise(n = sum(n)) %>% 
  mutate(ratio = paste0(round(n / sum(n) * 100, 1) , "%"))

# 与首次持股距IPO时长
merged_result %>% 
  group_by(Stkcd, 股东全称3) %>% 
  summarise(first_hold = min(Reptdt)) %>% 
  ungroup() %>% 
  left_join(ipo_time, by = c("Stkcd" = "Symbol")) %>% 
  mutate(time_interval = as.integer(date(first_hold) - date(LISTINGDATE))) %>% 
  mutate(时间间隔 = ceiling(time_interval / 365)) %>% 
  mutate(时长段 = ifelse(时间间隔 <= 1, "不超过1年", 
                  ifelse(时间间隔 <= 5, "2~5年",
                  ifelse(时间间隔 <= 10, "6～10年", "10年以上")))) %>% 
  left_join(y = gov_fund_little[, c(23, 7:9, 16)], 
            by = c("股东全称3" = "基金全称2")) %>% 
  filter(!is.na(基金级别), 基金级别 != "--") %>% 
  count(基金级别, 时长段) %>% 
  group_by(基金级别) %>% 
  mutate(ratio = paste0(round(n / sum(n) * 100, 1) , "%"))

# 与持股高新技术企业
listed_corporate_industry <- listed_corporate_info %>% 
  select(Symbol, EndDate, IndustryCode) %>% 
  mutate(Year = year(EndDate))
merged_result %>% 
  mutate(Year = year(Reptdt)) %>% 
  left_join(y = listed_corporate_industry, by = c("Stkcd" = "Symbol", "Year")) %>% 
  mutate(high_tech = if_else(IndustryCode == "C27" | IndustryCode == "C37" |
                             IndustryCode == "C39" | IndustryCode == "C40", 1,
                     if_else(str_sub(IndustryCode, 1, 1) == "I" | 
                             str_sub(IndustryCode, 1, 1) == "M", 1, 0))) %>% 
  left_join(y = gov_fund_little[, c(23, 7:9, 16)], 
            by = c("股东全称3" = "基金全称2")) %>% 
  filter(!is.na(基金级别), 基金级别 != "--") %>% 
  count(基金级别, high_tech) %>% 
  group_by(基金级别) %>% 
  mutate(ratio = paste0(round(n / sum(n) * 100, 1) , "%"))
listed_corporate_info %>% 
  mutate(high_tech = if_else(IndustryCode == "C27" | IndustryCode == "C37" |
                             IndustryCode == "C39" | IndustryCode == "C40", 1,
                     if_else(str_sub(IndustryCode, 1, 1) == "I" | 
                             str_sub(IndustryCode, 1, 1) == "M", 1, 0))) %>% 
  count(year(EndDate), high_tech)


# 4. 引导基金出资背景 -------------------------------------------------------------

# 计算出资背景
gov_fund_full <- gov_fund_full %>% 
  mutate(`出资人是否国资2` = ifelse(`出资人数量` == 1, "是", `出资人是否国资`)) %>% 
  mutate(`出资人是否国资2` = ifelse(`出资人是否国资2` == "是", 1, 0)) %>% 
  group_by(`基金简称`) %>% 
  mutate(`非国资出资人数量` = `出资人数量` - sum(`出资人是否国资2`))
gov_fund_little <- bind_cols(
  gov_fund_little, 
  distinct(gov_fund_full[, c(1, 9, 7)])[, 2:3]) %>% 
  mutate(包含非国资出资 = ifelse(非国资出资人数量 > 0, 1, 0))
merged_result %>% 
  mutate(Year = year(Reptdt)) %>% 
  left_join(y = listed_corporate_industry, by = c("Stkcd" = "Symbol", "Year")) %>% 
  mutate(high_tech = if_else(IndustryCode == "C27" | IndustryCode == "C37" |
                             IndustryCode == "C39" | IndustryCode == "C40", 1,
                     if_else(str_sub(IndustryCode, 1, 1) == "I" | 
                             str_sub(IndustryCode, 1, 1) == "M", 1, 0))) %>% 
  left_join(y = gov_fund_little[, c(23, 7:9, 26)], 
            by = c("股东全称3" = "基金全称2")) %>% 
  filter(!is.na(包含非国资出资)) %>% 
  count(包含非国资出资, high_tech) %>% 
  group_by(包含非国资出资) %>% 
  mutate(ratio = paste0(round(n / sum(n) * 100, 1) , "%"))

# 与基金持股时间
merged_result %>% 
  count(股东全称3, Stkcd, name = "持股时间") %>% 
  left_join(y = gov_fund_little[, c(23, 7:9, 26)], 
            by = c("股东全称3" = "基金全称2")) %>% 
  filter(!is.na(包含非国资出资)) %>% 
  count(包含非国资出资, 持股时间) %>% 
  mutate(持股时长 = ifelse(持股时间 <= 1, "1年", 
                    ifelse(持股时间 <= 3, "2~3年",
                    ifelse(持股时间 <= 5, "4～5年", "5年以上")))) %>% 
  group_by(包含非国资出资, 持股时长) %>% 
  summarise(n = sum(n)) %>% 
  mutate(ratio = paste0(round(n / sum(n) * 100, 1) , "%"))

# 与首次持股距IPO时间
merged_result %>% 
  group_by(Stkcd, 股东全称3) %>% 
  summarise(first_hold = min(Reptdt)) %>% 
  ungroup() %>% 
  left_join(ipo_time, by = c("Stkcd" = "Symbol")) %>% 
  mutate(time_interval = as.integer(date(first_hold) - date(LISTINGDATE))) %>% 
  mutate(时间间隔 = ceiling(time_interval / 365)) %>% 
  mutate(时长段 = ifelse(时间间隔 <= 1, "不超过1年", 
                  ifelse(时间间隔 <= 5, "2~5年",
                  ifelse(时间间隔 <= 10, "6～10年", "10年以上")))) %>% 
  left_join(y = gov_fund_little[, c(23, 7:9, 26)], 
            by = c("股东全称3" = "基金全称2")) %>% 
  filter(!is.na(包含非国资出资)) %>% 
  count(包含非国资出资, 时长段) %>% 
  group_by(包含非国资出资) %>% 
  mutate(ratio = paste0(round(n / sum(n) * 100, 1) , "%"))

# 与持股高新技术企业
merged_result_add <- merged_result %>% 
  group_by(股东全称3, Stkcd) %>% 
  mutate(持股时间 = n(), 
         first_hold = min(Reptdt)) %>% 
  ungroup() %>% 
  left_join(ipo_time, by = c("Stkcd" = "Symbol")) %>% 
  mutate(time_interval = as.integer(date(first_hold) - date(LISTINGDATE))) %>% 
  mutate(时间间隔 = ceiling(time_interval / 365)) %>% 
  select(股东全称3, Stkcd, Reptdt, 持股时间, 时间间隔) %>% 
  mutate(Year = year(Reptdt)) %>% 
  left_join(y = listed_corporate_industry[, c(1, 3, 4)], 
            by = c("Stkcd" = "Symbol", "Year")) %>% 
  mutate(high_tech = if_else(IndustryCode == "C27" | IndustryCode == "C37" |
                             IndustryCode == "C39" | IndustryCode == "C40", 1,
                     if_else(str_sub(IndustryCode, 1, 1) == "I" | 
                             str_sub(IndustryCode, 1, 1) == "M", 1, 0))) %>% 
  select(-Reptdt, -IndustryCode) %>% 
  distinct(股东全称3, Stkcd, .keep_all = TRUE) %>% 
  left_join(y = gov_fund_little[, c(23, 7:9, 26)], 
            by = c("股东全称3" = "基金全称2"))


# 5. t检验与描述性统计 ------------------------------------------------------------

# 计算基金注册省份
founded_province <- merged_result_add$注册地区 %>% 
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
merged_result_add$注册地区 <- founded_province_vec
rm(founded_province, founded_province_vec)

# 整理合并加工变量并备份
merged_result_add <- merged_result_add %>% 
  mutate(国家级基金 = ifelse(基金级别 == "国家级", 1, 
                             ifelse(基金级别 == "--", NA, 0)),
         产业基金 = ifelse(基金分类 == "产业基金", 1,
                           ifelse(基金分类 == "--", NA, 0)),
         发达地区基金 = ifelse(注册地区 == "北京市" | 注册地区 == "上海市" |
                               注册地区 == "广东省", 1, 0)) %>% 
  select(-基金级别, -基金分类, -注册地区)
merged_result_add <- merged_result_add[, c(2, 5, 1, 3:4, 6:10)]
colnames(merged_result_add)[3] <- "引导基金全称"
# write_rds(x = merged_result_add, file = "Government_Guide_Fund/output/merged_result_add_370.rds")

# 变量描述性统计与t检验
t.test(merged_result_add$持股时间, merged_result_add$国家级基金)
t.test(merged_result_add$持股时间, merged_result_add$包含非国资出资)
t.test(merged_result_add$持股时间, merged_result_add$发达地区基金)
t.test(merged_result_add$持股时间, merged_result_add$产业基金)

t.test(merged_result_add$时间间隔, merged_result_add$国家级基金)
t.test(merged_result_add$时间间隔, merged_result_add$包含非国资出资)
t.test(merged_result_add$时间间隔, merged_result_add$发达地区基金)
t.test(merged_result_add$时间间隔, merged_result_add$产业基金)

t.test(merged_result_add$high_tech, merged_result_add$国家级基金)
t.test(merged_result_add$high_tech, merged_result_add$包含非国资出资)
t.test(merged_result_add$high_tech, merged_result_add$发达地区基金)
t.test(merged_result_add$high_tech, merged_result_add$产业基金)

merged_result_add %>% 
  group_by(发达地区基金) %>% 
  summarise(      n         = n(),
            mean_hold_time  = mean(持股时间),
            sd_hold_time    = sd(持股时间),
            mean_first_hold = mean(时间间隔),
            sd_first_hold   = sd(时间间隔),
            mean_high_tech  = mean(high_tech),
            sd_high_tech    = sd(high_tech))





