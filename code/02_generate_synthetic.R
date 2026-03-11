##---------------------------------------------------##
## Title: Synthesis
## Update: 26.01.05
## Author: Juhee Lee
##---------------------------------------------------##

# setting ----------------------------------------------####
## load packages  
library(synthpop)
library(dplyr)


## load data ####
load("data/processed/dat_orig.rda")

# Synthesis --------------------------------------------####
## vars ####
vars <- c("가구구분코드", "세대구분코드", "배우자코드", "가구주_성별코드", "가구주_연령", "가구주_학력코드", "가구주_수학구분코드",
          "가구주_취업여부", "가구주_7차직업분류코드", "가구주_종사상지위코드", "가구원수", "취업인원수", 
          "노인가구여부", "모자가구여부", "맞벌이가구여부", "일반가구여부", "거처구분코드", "자동차보유대수",
          "입주형태코드", "월세평가금액", "전세보증금", "월세사글세", "주택소유유무", "주거용전용면적","도시읍면부구분코드",
          "전국전체가구_1인이상소득5분위코드", "전국근로자가구_1인이상소득5분위코드", "전국근로자외가구_1인이상소득5분위코드",
          "도시전체가구_1인이상소득5분위코드", "도시근로자가구_1인이상소득5분위코드", "도시근로자외가구_1인이상소득5분위코드", "소득구간코드",
          "가계지출_소비지출_식료품비주류음료구입비", "가계지출_소비지출_주류담배구입비",
          "가계지출_소비지출_의류신발구입비", "가계지출_소비지출_주거수도광열비","가계지출_소비지출_가정용품가사서비스이용금액",
          "가계지출_소비지출_보건제품구입비","가계지출_소비지출_교통비","가계지출_소비지출_통신비","가계지출_소비지출_오락문화비",
          "가계지출_소비지출_교육비","가계지출_소비지출_음식숙박비","가계지출_소비지출_기타상품서비스이용금액",
          "가계지출_소비지출금액", "가계지출_비소비지출금액", "가계지출금액"
          )
## predict matrix ####
pred <- matrix(0, nrow = length(vars), ncol = length(vars),
               dimnames = list(vars, vars))
pred[lower.tri(pred, diag = FALSE)] <- 1


## method ####
meth <- rep("cart", length(vars)); names(meth) = vars

meth[1] <- "sample"

meth["가구원수"] <- "hhsize"
meth["취업인원수"] <- "empnum"

meth["일반가구여부"] <- "~factor(ifelse((노인가구여부 == '0') & (모자가구여부 == '0') & (맞벌이가구여부 == '0'), '1', '0'), 0:1)"

meth["전세보증금"] <- "zeroup"
meth["월세사글세"] <- "zeroup"

meth["전국근로자가구_1인이상소득5분위코드"]   <- "natwincq"
meth["전국근로자외가구_1인이상소득5분위코드"] <- "natnwincq"

meth["도시전체가구_1인이상소득5분위코드"]     <- "urbincq"
meth["도시근로자가구_1인이상소득5분위코드"]   <- "urbwincq"
meth["도시근로자외가구_1인이상소득5분위코드"] <- "urbnwincq"

meth[paste0("가계지출_소비지출_",
            c("식료품비주류음료구입비","주류담배구입비", "의류신발구입비",
              "주거수도광열비", "가정용품가사서비스이용금액",
              "보건제품구입비", "교통비", "통신비","오락문화비","교육비",
              "음식숙박비","기타상품서비스이용금액"))] <- "zeroup"

meth["가계지출_비소비지출금액"] <- "zeroup"


meth["가계지출_소비지출금액"] <-
  paste0("~I(가계지출_소비지출_",
         paste(c("식료품비주류음료구입비","주류담배구입비", "의류신발구입비", "주거수도광열비", "가정용품가사서비스이용금액",
                 "보건제품구입비", "교통비", "통신비","오락문화비","교육비","음식숙박비","기타상품서비스이용금액"),
               collapse = " + 가계지출_소비지출_"),
         ")")

meth["가계지출금액"] <- "~I(가계지출_소비지출금액 + 가계지출_비소비지출금액)"

### custom
syn.hhsize <- function(y, x, xp, ...){ ## 가구원수
  z <- synthpop::syn.cart(y, x, xp, ...)
  
  spouse <- as.character(xp[, "배우자코드"])
  
  z_num <- suppressWarnings(as.numeric(as.character(z$res)))
  if (all(is.na(z_num))) return(list(res = z_num))

  z_num <- ifelse(spouse == "1", pmax(z_num, 2), z_num)
  
  return(list(res = z_num))
}

syn.empnum <- function(y, x, xp, ...){ ## 취업인원수
  z <- synthpop::syn.cart(y, x, xp, ...) 
  
  hhsize <- xp[, "가구원수"]
  hhcode <- as.character(xp[, "가구구분코드"])
  
  z_num <- suppressWarnings(as.numeric(as.character(z$res)))
  if (all(is.na(z_num))) return(list(res = z_num))
  
  z_num <- ifelse(hhcode == '1', pmax(z_num, 1), z_num)
  z_num <- pmin(z_num, hhsize)
  
  return(list(res=z_num))
}

syn.natwincq <- function(y, x, xp, ...){ ## 전국근로자가구_1인이상소득5분위코드
  origIndex <- x$가구구분코드  == '1'
  syntIndex <- xp$가구구분코드 == '1'
  
  natincq <- suppressWarnings(as.numeric(as.character(xp[syntIndex, "전국전체가구_1인이상소득5분위코드"])))
  z <- synthpop::syn.cart(y[origIndex], x[origIndex,], xp[syntIndex,], ...)
  
  z_tmp <- suppressWarnings(as.numeric(as.character(z$res)))
  if (all(is.na(z_tmp))) return(list(res = z_tmp))
  
  z_tmp <- pmin(pmax(natincq - 1 , z_tmp), natincq)

  z_num <- rep(NA, nrow(xp))
  z_num[syntIndex] <- z_tmp
  
  return(list(res=z_num))
}

syn.natnwincq <- function(y, x, xp, ...){ ## 전국근로자외가구_1인이상소득5분위코드
  origIndex <- x$가구구분코드  == '2'
  syntIndex <- xp$가구구분코드 == '2'
  
  natincq <- suppressWarnings(as.numeric(as.character(xp[syntIndex, "전국전체가구_1인이상소득5분위코드"])))
  z <- synthpop::syn.cart(y[origIndex], x[origIndex,], xp[syntIndex,], ...)
  
  z_tmp <- suppressWarnings(as.numeric(as.character(z$res)))
  if (all(is.na(z_tmp))) return(list(res = z_tmp))
  
  z_tmp <- pmin(pmax(natincq , z_tmp), natincq + 1)
  
  z_num <- rep(NA, nrow(xp))
  z_num[syntIndex] <- z_tmp
  
  return(list(res=z_num))
}

syn.urbincq <- function(y, x, xp, ...){ ## 도시전체가구_1인이상소득5분위코드
  origIndex <- x$도시읍면부구분코드  == '1'
  syntIndex <- xp$도시읍면부구분코드 == '1'
  
  natincq <- suppressWarnings(as.numeric(as.character(xp[syntIndex, "전국전체가구_1인이상소득5분위코드"])))
  z <- synthpop::syn.cart(y[origIndex], x[origIndex,], xp[syntIndex,], ...)
  
  z_tmp <- suppressWarnings(as.numeric(as.character(z$res)))
  if (all(is.na(z_tmp))) return(list(res = z_tmp))
  
  z_tmp <- pmin(pmax(natincq - 1, z_tmp), natincq)
  
  z_num <- rep(NA, nrow(xp))
  z_num[syntIndex] <- z_tmp
  
  return(list(res=z_num))
}

syn.urbwincq <- function(y, x, xp, ...){ ## 도시근로자가구_1인이상소득5분위코드
  origIndex <- (x$도시읍면부구분코드  == '1')&(x$가구구분코드  == '1')
  syntIndex <- (xp$도시읍면부구분코드 == '1')&(xp$가구구분코드 == '1')
  
  urbincq <- suppressWarnings(as.numeric(as.character(xp[syntIndex, "도시전체가구_1인이상소득5분위코드"])))
  z <- synthpop::syn.cart(y[origIndex], x[origIndex,], xp[syntIndex,], ...)
  
  z_tmp <- suppressWarnings(as.numeric(as.character(z$res)))
  if (all(is.na(z_tmp))) return(list(res = z_tmp))
  
  z_tmp <- pmin(pmax(urbincq - 1, z_tmp), urbincq)
  
  z_num <- rep(NA, nrow(xp))
  z_num[syntIndex] <- z_tmp
  
  return(list(res=z_num))
}

syn.urbnwincq <- function(y, x, xp, ...){ ## 도시근로자가구_1인이상소득5분위코드
  origIndex <- (x$도시읍면부구분코드  == '1')&(x$가구구분코드  == '2')
  syntIndex <- (xp$도시읍면부구분코드 == '1')&(xp$가구구분코드 == '2')
  
  urbincq <- suppressWarnings(as.numeric(as.character(xp[syntIndex, "도시전체가구_1인이상소득5분위코드"])))
  z <- synthpop::syn.cart(y[origIndex], x[origIndex,], xp[syntIndex,], ...)
  
  z_tmp <- suppressWarnings(as.numeric(as.character(z$res)))
  if (all(is.na(z_tmp))) return(list(res = z_tmp))
  
  z_tmp <- pmin(pmax(urbincq, z_tmp), urbincq + 1)
  
  z_num <- rep(NA, nrow(xp))
  z_num[syntIndex] <- z_tmp
  
  return(list(res=z_num))
}

syn.zeroup <- function(y, x, xp, ...){ ## 0 or min
  z <- synthpop::syn.cart(y, x, xp, ...)
  z_tmp <- suppressWarnings(as.numeric(as.character(z$res)))
  
  z_num <- z_tmp
  z_num[z_num>0] <- synthpop::syn.smooth(ysyn = z_num[z_num>0], yobs = y[y>0], smoothing = "density")
  
  return(list(res = z_num))
}

# y <- dat_orig$가계지출_소비지출_식료품비주류음료구입비
# x <- dat_orig[,vars]
# xp <- dat_synt

## smoothing ####
smth <- list(월세평가금액 = "density"#, 전세보증금 = "density", 월세사글세 = "density"#,
             # 가계지출_소비지출_식료품비주류음료구입비 = "density", 가계지출_소비지출_주류담배구입비 = "density", 
             # 가계지출_소비지출_의류신발구입비 = "density", 가계지출_소비지출_주거수도광열비 ="density",
             # 가계지출_소비지출_가정용품가사서비스이용금액 = "density", 가계지출_소비지출_보건제품구입비 = "density",
             # 가계지출_소비지출_교통비 = "density", 가계지출_소비지출_통신비 = "density", 가계지출_소비지출_오락문화비 = "density",
             # 가계지출_소비지출_교육비 = "density", 가계지출_소비지출_음식숙박비 = "density",
             # 가계지출_소비지출_기타상품서비스이용금액 = "density", 가계지출_비소비지출금액 = "density"
             )

## rule ####
rule <- rval <- list()

rule[["가구주_수학구분코드"]] = "가구주_학력코드 == '1'"
rval[["가구주_수학구분코드"]] = "0"

rule[["가구주_취업여부"]] = "가구구분코드 == '1'"
rval[["가구주_취업여부"]] = "1"

rule[["가구주_종사상지위코드"]] = c("가구주_취업여부 == '2'", "가구주_7차직업분류코드 == 'A'")
rval[["가구주_종사상지위코드"]] = c(NA, '1')

rule[["노인가구여부"]] = c("가구주_연령 < 64", "세대구분코드 == '3'")
rval[["노인가구여부"]] = c("0", "0")

rule[["모자가구여부"]] = c("가구주_성별코드 == '1'", "세대구분코드 != '2'", "배우자코드 == '1'", "가구원수<2")
rval[["모자가구여부"]] = rep("0", 4)

rule[["맞벌이가구여부"]] = "배우자코드 != '1'"
rval[["맞벌이가구여부"]] = "0"

rule[["전세보증금"]] = c("입주형태코드 == '1'", "입주형태코드 == '2'", "입주형태코드 == '7'")
rval[["전세보증금"]] = rep(0, 3)

rule[["월세사글세"]] = c("입주형태코드 == '1'", "입주형태코드 == '2'", "입주형태코드 == '4'")
rval[["월세사글세"]] = rep(0, 3)

rule[["주택소유유무"]] = "입주형태코드 == '1'"
rval[["주택소유유무"]] = '1'

## synthesis ####
set.seed(20251201)
synt_obj <- syn(dat_orig[,vars], method = meth, smoothing = smth, rules = rule, rvalues = rval, minnumlevels = 5)

dat_synt <- synt_obj$syn

save(synt_obj, dat_synt, file = "data/processed/dat_synt.rda")