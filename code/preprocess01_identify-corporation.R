
##############################################################
# Programmer: Lu Winter
# Date Created: 2022-09-05 
# Task: Identify every corporate-years of the sample
##############################################################


# 0. Initial Setup -------------------------------------------------------------

## load packages
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(DBI)

## define directory path
data_path <- "data"
db_path <- "data/GGF_project_store.sqlite"

## 建立数据库连接
con_sqlite <- dbConnect(RSQLite::SQLite(), db_path)
dbListTables(con_sqlite)


# 2. Identify Corporate-Years --------------------------------------------

## 上市公司基本信息
### 数据来源：CSMAR公司研究 - 上市公司基本信息 - 上市公司基本信息年度表
### 数据属性：2000-2021年度 全部股票代码 37个变量 52629条观测
listed_corp_info <- 
  read_xlsx(file.path(data_path, "2022-08-04_corporate-info.xlsx"))
### 这里要剔除特定公司, 剔除范围是
### 金融类样本、ST类样本、只发行B股样本
### 1. 按年合并公司名称与行业代码
### 2. 删除名称中带ST和B的，删除金融业
identifier <- listed_corp_info |> 
  mutate(Year = year(EndDate),
         ListingYear = year(LISTINGDATE)) |> 
  select(Stkcd = Symbol, ShortName, Year, IndustryCode, ListingYear, EstablishDate,
         Province = PROVINCE, City = CITY) |> 
  mutate(ToRemove = ifelse(is.na(IndustryCode), 1, 
                    ifelse(str_detect(ShortName, "ST"), 1,
                    ifelse(str_detect(ShortName, "B"), 1,
                    ifelse(str_sub(IndustryCode, 1, 1) == "J", 1,
                    ifelse(str_sub(Stkcd, 1, 1) %in% c("0", "3", "6"), 0, 1)))))) %>% 
  filter(Year >= 2010, ToRemove == 0) |> 
  select(-ToRemove)
count(identifier, Year)

## Write to SQLite database
dbWriteTable(
  conn = con_sqlite, 
  name = "identifier_info", 
  value = identifier,
  overwrite = TRUE
)
dbWriteTable(
  conn = con_sqlite, 
  name = "identifier", 
  value = identifier[, c(1, 3, 4)],
  overwrite = TRUE
)

dbListTables(conn = con_sqlite)
dbDisconnect(conn = con_sqlite)

rm(list = ls())

