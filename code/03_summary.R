##---------------------------------------------------##
## Title: Summary Results
## Update: 26.01.28
## Author: Juhee Lee
##---------------------------------------------------##

# setting ----------------------------------------------####
## load packages  
library(synthpop)
library(dplyr)

## load data 
load("data/processed/dat_orig.rda")
load("data/processed/dat_synt.rda")

## load source code
source("code/functions.R")

# Risk measure --------------------------------------------####
## density-based measure ####
mat_dist <- dist_euclid_ft(dat_orig, dat_synt)
vec_dcr <- DCR_ft(mat_dist$dist_os)
vec_nndr <- nndr_ft(mat_dist$dist_os)
vec_dupi <- DUPI_ft(mat_dist$dist_os, mat_dist$dist_oo)

save(mat_dist, vec_dcr, vec_nndr, vec_dupi, file = "results/dist_based_results.rda")

png("results/figures/Fig_dist_based.png", 900, 450)
par(mfrow=c(1,2))
hist(vec_dcr, main = " ", xlab="DCR", axes=FALSE, breaks=50, ylim = c(0, 3050), cex.lab=1.5)
axis(1, seq(-5, 30, 5), cex.axis=1.3)
axis(2, seq(-500, 3500, 500), cex.axis=1.3)
hist(vec_nndr, freq=TRUE, main = " ", xlab="NNDR", axes=TRUE, breaks=50, ylim = c(0, 3050), cex.lab=1.5)
axis(1, seq(0, 1, 0.2))
axis(2, seq(-500, 3500, 500))
dev.off()

tab_dist <- matrix(NA, nr=3, nc=5)
tab_dist[1,] <- c(quantile(vec_dcr, probs=c(0,0.01,0.05, 0.5)), mean(vec_dcr))
tab_dist[2,] <- c(quantile(vec_nndr, probs=c(0,0.01,0.05, 0.5)), mean(vec_nndr))
tab_dist[3,5] <- vec_dupi$dupi

dimnames(tab_dist) <- list(c("DCR", "NNDR", "DUPI"), c("Min.", "1%", "5%", "Median", "Mean"))

write.csv(tab_dist, file = "results/table_dist_based.csv")

# Utility Measures ----------------------------------------####
## editing rules ####
### logical rule 1 ####
tab_edit1 <- table(dat_synt$가구원수, dat_synt$취업인원수)


### logical rule 2####
tmp1 <- dat_orig %>% mutate(보증금여부 = 전세보증금>0) %>% group_by(입주형태코드) %>%
  summarize(보증금있음=sum(보증금여부), 보증금없음 = sum(!보증금여부))

tmp2 <- dat_synt %>% mutate(보증금여부 = 전세보증금>0) %>% group_by(입주형태코드) %>%
  summarize(보증금있음=sum(보증금여부), 보증금없음 = sum(!보증금여부))

tab_edit2 <- left_join(tmp1, tmp2, by="입주형태코드", suffix = c(".orig", ".synt"))

### sum constraint(rule) ####
dat_tmp <- dat_synt %>% mutate(가계지출_tmp = 가계지출_소비지출금액 + 가계지출_비소비지출금액,
                               가계지출_k = 가계지출금액/1000,
                               가계지출_sumk = 가계지출_tmp/1000)

png(500, 500, filename = "results/figures/Fig_sum_constraint.png")
plot(가계지출_k~가계지출_sumk, data = dat_tmp, type = "n",
     xlab = "Sum of Component Expenditures (thousand KRW)", ylab = "Total Expenditure (thousand KRW)",
     cex.lab=1.5, cex.axis = 1.3)
abline(0,1, col="grey70", lwd=2, lty=2)
points(가계지출_k~가계지출_sumk, data = dat_tmp, pch=16)
dev.off()


## compare statistics ####
### Expenditure ####
tab_stats1 <- matrix(NA, nr=2, nc=3)
tab_stats1[1,] <- dat_orig %>% select(가계지출금액, 가계지출_소비지출금액, 가계지출_비소비지출금액) %>% colMeans(na.rm=TRUE)
tab_stats1[2,] <- dat_synt %>% select(가계지출금액, 가계지출_소비지출금액, 가계지출_비소비지출금액) %>% colMeans(na.rm=TRUE)
dimnames(tab_stats1) <- list(c("Original", "Synthetic"), c("Total_Expenditure", "Consumption_expenditure", "Non_consumption_expenditure"))
write.csv(tab_stats1, file = "results/table_expenditure_mean.csv", fileEncoding = "CP949")

### consumption expenditure plot ####
dat_tmp_orig <- dat_orig %>% select(가계지출_소비지출_식료품비주류음료구입비:가계지출금액) %>%
  colSums(na.rm=TRUE)

dat_tmp_synt <- dat_synt[,names(dat_orig)] %>% select(가계지출_소비지출_식료품비주류음료구입비:가계지출금액) %>%
  colSums(na.rm=TRUE)

mat_tmp1 <- rbind(dat_tmp_orig, dat_tmp_synt)
mat_tmp1 <- mat_tmp1[,1:12] / mat_tmp1[,13] * 100

cate_names <- c(
  "Food",
  "Alcohol\n& tobacco",
  "Clothing",
  "Housing",
  "Household\ngoods",
  "Health",
  "Transport",
  "Communication",
  "Recreation",
  "Education",
  "Restaurants\n& hotels",
  "Misc."
)


dat_tmp <- dat_synt %>% select(전국전체가구_1인이상소득5분위코드, 가계지출_소비지출금액) %>%
  mutate(Group = "Synthetic")

dat_tmp_all <- dat_orig %>% select(전국전체가구_1인이상소득5분위코드, 가계지출_소비지출금액) %>%
  mutate(Group= "Original") %>%
  bind_rows(dat_tmp)

synt_mean <- dat_synt %>%
  group_by(전국전체가구_1인이상소득5분위코드) %>%
  summarise(mean = mean(가계지출_소비지출금액, na.rm = TRUE))

orig_mean <- dat_orig %>%
  group_by(전국전체가구_1인이상소득5분위코드) %>%
  summarise(mean = mean(가계지출_소비지출금액, na.rm = TRUE))

mat_tmp2 <- rbind(orig_mean$mean, synt_mean$mean)


png("results/figures/Fig_consum_compare.png",
    width = 2200, height = 900, res = 150)
par(mfrow = c(1,2),
    mar = c(4, 5, 2, 1),
    mgp = c(3.3, 0.8, 0),
    las = 1,
    cex = 1.15)

barplot(mat_tmp1,
        beside = TRUE,
        col = c("grey30", "grey80"),
        names.arg = c("Food", "Alcohol\n& tobacco", "Clothing", "Housing",
                      "Household\ngoods", "Health", "Transport", "Communication",
                      "Recreation", "Education", "Restaurants\n& hotels", "Misc."),
        ylab = "Share of consumption expenditure (%)",
        ylim = c(0, 20),
        yaxt = "n",
        cex.names = 0.9,
        cex.axis = 1.05,
        cex.lab = 1.1)

axis(2, cex.axis = 1.05)
usr <- par("usr")
segments(usr[1], usr[3], usr[1], usr[4])

legend("topleft",
       legend = c("Original", "Synthetic"),
       fill = c("grey30", "grey80"),
       bty = "n",
       cex = 1.0)

barplot(mat_tmp2 / 1000,
        beside = TRUE,
        col = c("grey30", "grey80"),
        names.arg = c("1st", "2nd", "3rd", "4th", "5th"),
        ylab = "Average consumption expenditure (thousand KRW)",
        yaxt = "n",
        cex.names = 1.0,
        cex.axis = 1.05,
        cex.lab = 1.1)

axis(2, cex.axis = 1.05)
usr <- par("usr")
segments(usr[1], usr[3], usr[1], usr[4])

legend("topleft",
       legend = c("Original", "Synthetic"),
       fill = c("grey30", "grey80"),
       bty = "n",
       cex = 1.0)

dev.off()


### PCC plot ####
dat_synt <- dat_synt[,names(dat_orig)]
is_num <- sapply(dat_orig, function(s) !is.factor(s))

orig_pcc <- cor(dat_orig[,is_num])[lower.tri(cor(dat_orig[,is_num]))]
synt_pcc <- cor(dat_synt[,is_num])[lower.tri(cor(dat_synt[,is_num]))]

png("results/figures/Fig_PCC.png", width = 900, height = 900, res = 150)
par(cex = 1.2,        
    mgp = c(2.5,0.8,0),
    las = 1)

plot(orig_pcc, synt_pcc,
     xlab = "Original",
     ylab = "Synthetic",
     type = "n",
     cex.lab = 1.3,
     cex.axis = 1.1)

abline(0,1, col="grey70", lwd=2, lty=2)

points(orig_pcc, synt_pcc,
       pch = 16,
       cex = 0.8)

dev.off()

## propensity score ####
### logistic results ####
# spMSE_logistic <- s_pMSE_logistic_ft(orig = dat_orig, synt = dat_synt)
# spMSE_logistic$s_pMSE
# 
# ### cart results ####
# spMSE_cart <- s_pMSE_cart_ft(orig = dat_orig, synt = dat_synt, B=1000)
# 
# save(spMSE_cart, spMSE_logistic, file = "results/pMSE_results.rda")

load("results/pMSE_results.rda")

### density plot ####
index <- c(rep(0, nrow(dat_orig)), rep(1, nrow(dat_synt)))

ps_dens_lgst_orig <- density(spMSE_logistic$ps_vec[index==0])
ps_dens_lgst_synt <- density(spMSE_logistic$ps_vec[index==1])

ps_dens_cart_orig <- density(spMSE_cart$ps_vec[index==0], bw = 0.02)
ps_dens_cart_synt <- density(spMSE_cart$ps_vec[index==1], bw = 0.02)

ymax <- max(ps_dens_lgst_orig$y, ps_dens_lgst_synt$y,
            ps_dens_cart_orig$y, ps_dens_cart_synt$y)

png(filename = "results/figures/Fig_PS.png",
    width = 1600, height = 900, res = 150)
par(cex = 1.2,        
    mgp = c(2.5,0.8,0),
    las = 1, mfrow=c(1,2))

plot(ps_dens_lgst_orig,
     lwd = 3, lty = 1,
     main = "",
     xlab = "Propensity score",
     ylab = "Density",
     xlim = c(0,1),
     ylim = c(0,15))
lines(ps_dens_lgst_synt,
      lwd = 3, lty = 2)

legend("topleft",
       legend = c("Original", "Synthetic"),
       lty = c(1,2),
       lwd = 3,
       bty = "n")

plot(ps_dens_cart_orig,
     lwd = 3, lty = 1,
     main = "",
     xlab = "Propensity score",
     ylab = "Density",
     xlim = c(0,1),
     ylim = c(0,15))
lines(ps_dens_cart_synt,
      lwd = 3, lty = 2)

legend("topleft",
       legend = c("Original", "Synthetic"),
       lty = c(1,2),
       lwd = 3,
       bty = "n")

dev.off()

### pMSE ####
spMSE_logistic$s_pMSE
spMSE_cart$s_pMSE

