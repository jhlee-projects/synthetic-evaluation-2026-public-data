##---------------------------------------------------##
## Title: Data preparation
## File: 01_prepare_data.R
## Update: 25.12.04
## Author: Juhee Lee
##---------------------------------------------------##

# setting ----------------------------------------------####
## load packages  
library(dplyr)

if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

# Data Handling ----------------------------------------####

## Raw data should be downloaded from MDIS and placed in the same directory as this script
dat_raw <- read.csv("data/raw/household_2024.csv", fileEncoding = "euc-kr")

vars <- c("가구주_성별코드","가구주_연령","가구주_학력코드","가구주_수학구분코드","가구주_취업여부","가구주_7차직업분류코드","가구주_종사상지위코드","배우자코드",
          "가구구분코드","가구원수","취업인원수","세대구분코드","노인가구여부","모자가구여부","맞벌이가구여부","일반가구여부",
          "도시읍면부구분코드","거처구분코드","자동차보유대수","입주형태코드","월세평가금액","전세보증금","월세사글세","주거용전용면적","주택소유유무",
          "전국전체가구_1인이상소득5분위코드","전국근로자가구_1인이상소득5분위코드","전국근로자외가구_1인이상소득5분위코드",
          "도시전체가구_1인이상소득5분위코드","도시근로자가구_1인이상소득5분위코드","도시근로자외가구_1인이상소득5분위코드", "소득구간코드",
          "가계지출_소비지출_식료품비주류음료구입비","가계지출_소비지출_주류담배구입비", "가계지출_소비지출_의류신발구입비",
          "가계지출_소비지출_주거수도광열비","가계지출_소비지출_가정용품가사서비스이용금액", "가계지출_소비지출_보건제품구입비",
          "가계지출_소비지출_교통비","가계지출_소비지출_통신비","가계지출_소비지출_오락문화비", "가계지출_소비지출_교육비",
          "가계지출_소비지출_음식숙박비","가계지출_소비지출_기타상품서비스이용금액","가계지출_소비지출금액","가계지출_비소비지출금액","가계지출금액")

missing_vars <- setdiff(vars, names(dat_raw))
if (length(missing_vars) > 0) {
  stop("The following variables are missing in the raw data: ",
       paste(missing_vars, collapse = ", "))
}

# Rule consistency checks (used to identify logical constraints) ----------####

dat_raw %>% with(table(배우자코드, 가구주_연령, useNA="ifany"))
dat_raw %>% with(table(배우자코드, 가구원수, useNA="ifany"))

dat_raw %>% with(table(가구주_학력코드, 가구주_수학구분코드, useNA="ifany"))

dat_raw %>% with(table(가구주_취업여부, 가구구분코드, useNA="ifany"))
dat_raw %>% filter(가구구분코드==2) %>% select(가구주_취업여부) %>% table(useNA="ifany")

dat_raw %>% with(table(가구주_취업여부, 가구주_7차직업분류코드, useNA="ifany"))
dat_raw %>% with(table(가구주_취업여부, 가구주_종사상지위코드, useNA="ifany"))
dat_raw %>% with(table(가구주_7차직업분류코드, 가구주_종사상지위코드, useNA="ifany"))

dat_raw %>% filter(노인가구여부==0) %>% with(table(세대구분코드, 가구주_연령, useNA="ifany"))
dat_raw %>% filter(세대구분코드%in%c(1,2), 가구주_연령 >= 64) %>% select(노인가구여부) %>% table

dat_raw %>% with(table(모자가구여부, 가구주_성별코드, useNA="ifany"))
dat_raw %>% with(table(모자가구여부, 배우자코드, useNA="ifany"))
dat_raw %>% with(table(모자가구여부, 세대구분코드, useNA="ifany"))
dat_raw %>% with(table(모자가구여부, 가구원수, useNA="ifany"))

dat_raw %>% with(table(맞벌이가구여부, 배우자코드, useNA="ifany"))

dat_raw %>% with(table(일반가구여부, 노인가구여부, useNA="ifany"))
dat_raw %>% with(table(일반가구여부, 모자가구여부, useNA="ifany"))
dat_raw %>% with(table(일반가구여부, 맞벌이가구여부, useNA="ifany"))

dat_raw %>% with(table(일반가구여부, 맞벌이가구여부, useNA="ifany"))

dat_raw %>% filter(입주형태코드 == 1) %>% select(월세평가금액, 전세보증금, 월세사글세) %>% summary
dat_raw %>% filter(입주형태코드 == 2) %>% select(월세평가금액, 전세보증금, 월세사글세) %>% summary
dat_raw %>% filter(입주형태코드 == 3) %>% select(월세평가금액, 전세보증금, 월세사글세) %>% summary
dat_raw %>% filter(입주형태코드 == 4) %>% select(월세평가금액, 전세보증금, 월세사글세) %>% summary
dat_raw %>% filter(입주형태코드 == 5) %>% select(월세평가금액, 전세보증금, 월세사글세) %>% summary
dat_raw %>% filter(입주형태코드 == 6) %>% select(월세평가금액, 전세보증금, 월세사글세) %>% summary
dat_raw %>% filter(입주형태코드 == 7) %>% select(월세평가금액, 전세보증금, 월세사글세) %>% summary

dat_raw %>% with(table(주택소유유무, 입주형태코드, useNA="ifany"))

dat_raw %>% with(table(전국전체가구_1인이상소득5분위코드, 전국근로자가구_1인이상소득5분위코드, useNA="ifany"))
dat_raw %>% with(table(전국전체가구_1인이상소득5분위코드, 전국근로자외가구_1인이상소득5분위코드, useNA="ifany"))

dat_raw %>% with(table(가구구분코드, 전국근로자가구_1인이상소득5분위코드, useNA="ifany"))
dat_raw %>% with(table(가구구분코드, 전국근로자외가구_1인이상소득5분위코드, useNA="ifany"))

dat_raw %>% with(table(도시읍면부구분코드, 도시전체가구_1인이상소득5분위코드, useNA="ifany"))
dat_raw %>% with(table(가구구분코드, 도시근로자가구_1인이상소득5분위코드, useNA="ifany"))
dat_raw %>% with(table(가구구분코드, 도시근로자외가구_1인이상소득5분위코드, useNA="ifany"))

dat_raw %>% with(table(도시전체가구_1인이상소득5분위코드, 도시근로자가구_1인이상소득5분위코드, useNA="ifany"))
dat_raw %>% with(table(도시전체가구_1인이상소득5분위코드, 도시근로자외가구_1인이상소득5분위코드, useNA="ifany"))

dat_raw %>% with(table(전국전체가구_1인이상소득5분위코드, 도시전체가구_1인이상소득5분위코드, useNA="ifany"))
dat_raw %>% with(table(전국근로자가구_1인이상소득5분위코드, 도시근로자가구_1인이상소득5분위코드, useNA="ifany"))

tmp1 <- dat_raw[,vars] %>% select(가계지출_소비지출_식료품비주류음료구입비:가계지출_소비지출_기타상품서비스이용금액) %>% rowSums
tmp2 <- dat_raw$가계지출_소비지출금액
cat("Proportion of records violating consumption expenditure identity:",
    mean(tmp1 != tmp2), "\n")

tmp1 <- dat_raw[,vars] %>% select(가계지출_소비지출금액:가계지출_비소비지출금액) %>% rowSums
tmp2 <- dat_raw$가계지출금액
cat("Proportion of records violating total expenditure identity:",
    mean(tmp1 != tmp2), "\n")

# Data Handling ----------------------------------------####
dat_orig <- within(dat_raw[, vars], {
  가구구분코드 = factor(가구구분코드, 1:2)
  
  노인가구여부 = factor(노인가구여부, 0:1)
  모자가구여부 = factor(모자가구여부, 0:1)
  맞벌이가구여부 = factor(맞벌이가구여부, 0:1)
  일반가구여부 = factor(일반가구여부, 0:1)
  
  세대구분코드 = factor(세대구분코드, 1:3)
  배우자코드 = factor(배우자코드, 1:3)
  가구주_성별코드 = factor(가구주_성별코드, 1:2)
  가구주_학력코드 = factor(가구주_학력코드, 1:8)
  가구주_수학구분코드 = factor(가구주_수학구분코드, 0:5)
  가구주_취업여부 = factor(가구주_취업여부, 1:2)
  가구주_7차직업분류코드 = factor(가구주_7차직업분류코드, c(1:9, "A", "Z"))
  가구주_종사상지위코드 = factor(가구주_종사상지위코드, c(1:5,7))
  
  거처구분코드 = factor(거처구분코드, 1:7)
  입주형태코드 = factor(입주형태코드, 1:7)
  주택소유유무 = factor(주택소유유무, 1:2)
  
  도시읍면부구분코드 = factor(도시읍면부구분코드, 0:1)
  
  전국전체가구_1인이상소득5분위코드     = factor(전국전체가구_1인이상소득5분위코드, 1:5, ordered = TRUE)
  전국근로자가구_1인이상소득5분위코드   = factor(전국근로자가구_1인이상소득5분위코드, 1:5, ordered = TRUE)
  전국근로자외가구_1인이상소득5분위코드 = factor(전국근로자외가구_1인이상소득5분위코드, 1:5, ordered = TRUE)
  
  도시전체가구_1인이상소득5분위코드     = factor(도시전체가구_1인이상소득5분위코드, 1:5, ordered = TRUE)
  도시근로자가구_1인이상소득5분위코드   = factor(도시근로자가구_1인이상소득5분위코드, 1:5, ordered = TRUE)
  도시근로자외가구_1인이상소득5분위코드 = factor(도시근로자외가구_1인이상소득5분위코드, 1:5, ordered = TRUE)
  
  소득구간코드 = factor(소득구간코드, 1:8, ordered = TRUE)

})

tmp <- dat_orig %>% select(가계지출_소비지출_식료품비주류음료구입비:가계지출_소비지출_기타상품서비스이용금액) %>% rowSums
dat_orig <- dat_orig %>% mutate(가계지출_소비지출금액 = tmp,
                                가계지출금액 = 가계지출_소비지출금액 + 가계지출_비소비지출금액)

save(dat_orig, file = "data/processed/dat_orig.rda")

