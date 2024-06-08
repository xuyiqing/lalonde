# LaLonde (1986) after Nearly Four Decades: Lessons Learned
# Guido Imbens & Yiqing Xu
# 2024

## Female Sample Constructed by Calonico and Smith (2017)

rm(list = ls())
source("code/functions_est.R")
source("code/functions_plot.R")

                                                                      
                                                                      
#  $$$$$$\   $$$$$$\   $$$$$$\   $$$$$$\   $$$$$$\   $$$$$$\   $$$$$$\  
# $$  __$$\ $$  __$$\ $$  __$$\ $$  __$$\  \____$$\ $$  __$$\ $$  __$$\ 
# $$ /  $$ |$$ |  \__|$$$$$$$$ |$$ /  $$ | $$$$$$$ |$$ |  \__|$$$$$$$$ |
# $$ |  $$ |$$ |      $$   ____|$$ |  $$ |$$  __$$ |$$ |      $$   ____|
# $$$$$$$  |$$ |      \$$$$$$$\ $$$$$$$  |\$$$$$$$ |$$ |      \$$$$$$$\ 
# $$  ____/ \__|       \_______|$$  ____/  \_______|\__|       \_______|
# $$ |                          $$ |                                    
# $$ |                          $$ |                                    
# \__|                          \__|                                    

library(haven)
library(labelled)


d <- haven::read_dta("data/cs/NSW_AFDC_CS.dta")
d <- as.data.frame(zap_label(d))
for (i in 1:ncol(d)) {
        if (is.numeric(d[, i])) {
            d[, i] <- as.vector(d[, i])
        }
}

# keep reconstructed Lalonde Sample & PSID-1
d <- subset(d, lalonde == 1 | psid1 == 1)
names(d)
d$u75 <- ifelse(d$re75 == 0, 1, 0)

# keep relevant variables
covar <- c("age", "educ", "nodegree", "married", "black", "hisp", "nchildren75", "re75", "u75")
Y <- "re79"

var <- c(Y, "treated", covar, "psid1")
d <- d[, var]

colnames(d)[2] <- "treat"
names(d)
d$treat[which(d$psid1 == 1)] <- 0 # nonexperimental controls

# experimental sample
lcs <- subset(d, psid1 == 0)
lcs$psid1 <- NULL
table(lcs$treat)
names(lcs)
sum(!complete.cases(lcs)) # check missingness

# nonexperimental sample
lcs_psid <- subset(d, treat == 1 | psid1 == 1)
lcs_psid$treat[which(lcs_psid$psid1 == 1)] <- 0
lcs_psid$psid1 <- NULL
table(lcs_psid$treat)
names(lcs_psid)
sum(!complete.cases(lcs_psid)) # check missingness

# extended sample
lcs_psid.plus <- d
lcs_psid.plus$exp <- 1 - lcs_psid.plus$psid1

#             $$\                $$\               
#             $$ |               $$ |              
#  $$$$$$$\ $$$$$$\    $$$$$$\ $$$$$$\    $$$$$$$\ 
# $$  _____|\_$$  _|   \____$$\\_$$  _|  $$  _____|
# \$$$$$$\    $$ |     $$$$$$$ | $$ |    \$$$$$$\  
#  \____$$\   $$ |$$\ $$  __$$ | $$ |$$\  \____$$\ 
# $$$$$$$  |  \$$$$  |\$$$$$$$ | \$$$$  |$$$$$$$  |
# \_______/    \____/  \_______|  \____/ \_______/ 

############################
## Summary Statistics
############################

X <- c("age", "educ", "nodegree", "married", "black", "hisp", "re75", "u75")

# mean
stat.lcs.tr <- apply(lcs[which(lcs$treat == 1), X], 2, mean)
stat.lcs.co <- apply(lcs[which(lcs$treat == 0), X], 2, mean)
stat.psid <- apply(lcs_psid[which(lcs_psid$treat == 0), X], 2, mean)
out <- cbind.data.frame(stat.lcs.tr, stat.lcs.co, stat.psid)
out[c("re75"), ] <- out[c("re75"), ]/1000

# sd
stat.lcs.tr <- apply(lcs[which(lcs$treat == 1), X], 2, sd)
stat.lcs.co <- apply(lcs[which(lcs$treat == 0), X], 2, sd)
stat.psid <- apply(lcs_psid[which(lcs_psid$treat == 0), X], 2, sd)
out2 <- cbind.data.frame(stat.lcs.tr, stat.lcs.co, stat.psid)
out2[c("re75"), ] <- out2[c("re75"), ]/1000

# columns are samples
n <- nrow(out)   
sav <- matrix("", n*2, ncol(out))
for (j in 1:n) {
    a <- out[j, ]
    b <- out2[j, ]
    sav[j*2-1, ] <- sprintf("%.2f", a)
    sav[j*2, ] <- paste0("(", sprintf("%.2f",b), ")")       
}
rownames(sav) <- rep(X, each = 2)
print(sav)

table(lcs$treat)
table(lcs_psid$treat)


write.csv(sav, file = "tables/stats_lcs.csv", row.names = FALSE)




#   $$\               $$\               
#   $$ |              \__|              
# $$$$$$\    $$$$$$\  $$\ $$$$$$\$$$$\  
# \_$$  _|  $$  __$$\ $$ |$$  _$$  _$$\ 
#   $$ |    $$ |  \__|$$ |$$ / $$ / $$ |
#   $$ |$$\ $$ |      $$ |$$ | $$ | $$ |
#   \$$$$  |$$ |      $$ |$$ | $$ | $$ |
#    \____/ \__|      \__|\__| \__| \__|
                                 

Y <- "re79"
treat <- "treat"

# redifine covariates: removing "nchildren75" to be used as placebo outcome
covar <- c("age", "educ", "nodegree", "married", "black", "hisp", "re75", "u75")

## trimming
p.forest <- probability_forest(X = lcs_psid.plus[, covar], 
    Y = as.factor(lcs_psid.plus$exp), seed = 1234, num.trees = 4000) # experimental indicator as treated
lcs_psid.plus$ps <- p.forest$predictions[,2]
range(lcs_psid.plus$ps)
#hist(lcs_psid.plus$ps)

# histogram of propensity scores
round(quantile(lcs_psid.plus$ps[which(lcs_psid.plus$exp == 0 & lcs_psid.plus$ps > 0.6)], probs = seq(0, 1, 0.1)),3)
threshold <- 0.9 # only 50 PSID control units have ps > 0.9
nrow(subset(lcs_psid.plus, exp == 0 & ps > threshold)) # 27 control units
nrow(subset(lcs_psid.plus, exp == 1 & ps > threshold))

# trim experimental data
lcs_trim_psid <- subset(lcs_psid.plus, exp == 1 & ps <= threshold) 
lcs_trim_psid$ps <- NULL
lcs_trim_psid$exp <- NULL
lcs_trim_psid$psid1 <- NULL
table(lcs_trim_psid$treat) # 773 units: 393 treated vs 380 controls; 1185-773 = 412 experimental units dropped
head(lcs_trim_psid)
summary(lm(re79 ~ treat, data = lcs_trim_psid))

# propensity score matching without replacement
set.seed(1234) # need to set seed b/c tie-breaking is random
data <- subset(lcs_psid.plus, (treat == 1 | exp == 0) & ps <= threshold) # only use psid1 controls
table(data$treat)
data$ps <- probability_forest(X = data[, covar], 
    Y = as.factor(data$treat), seed = 1234, num.trees = 4000)$predictions[,2]
mout <- Match(Y = data$re78, Tr = data$treat, X = data$ps, estimand = "ATT", M = 1,
      BiasAdjust = FALSE, replace=FALSE, ties = FALSE)
lcs_psid_trim <- data[c(mout$index.treated, mout$index.control), ]
table(lcs_psid_trim$treat) # 1064 units: 532 treated vs 532 controls

# estimate propensity score again
lcs_psid_trim$ps_new <- probability_forest(X = lcs_psid_trim[, covar], 
    Y = as.factor(lcs_psid_trim$treat), 
    seed = 1234, num.trees = 4000)$predictions[,2]
table(lcs_psid_trim$treat)

save(lcs, lcs_psid, lcs_trim_psid, lcs_psid_trim, file = "data/lcs.RData")


#                                         $$\                     
#                                         $$ |                    
#  $$$$$$\ $$\    $$\  $$$$$$\   $$$$$$\  $$ | $$$$$$\   $$$$$$\  
# $$  __$$\\$$\  $$  |$$  __$$\ $$  __$$\ $$ | \____$$\ $$  __$$\ 
# $$ /  $$ |\$$\$$  / $$$$$$$$ |$$ |  \__|$$ | $$$$$$$ |$$ /  $$ |
# $$ |  $$ | \$$$  /  $$   ____|$$ |      $$ |$$  __$$ |$$ |  $$ |
# \$$$$$$  |  \$  /   \$$$$$$$\ $$ |      $$ |\$$$$$$$ |$$$$$$$  |
#  \______/    \_/     \_______|\__|      \__| \_______|$$  ____/ 
#                                                       $$ |      
#                                                       $$ |      
#                                                       \__|      


# Log Odds (NSW, Calonico and Smith Sample)
pdf("graphs/lalonde/odds_lcs_exp.pdf", width = 6, height = 6)
lcs$ps <- probability_forest(X = lcs[, covar], 
    Y = as.factor(lcs$treat), seed = 1234, num.trees = 4000)$predictions[,2]
plot_hist(lcs, "ps", "treat", breaks = 40, odds = TRUE, xlim = c(-1.5, 1.5))
graphics.off()

# trimmed based on PSID controls
pdf("graphs/lalonde/odds_lcs_trim_psid.pdf", width = 6, height = 6)
lcs_trim_psid$ps <- probability_forest(X = lcs_trim_psid[, covar], 
    Y = as.factor(lcs_trim_psid$treat), seed = 1234, num.trees = 4000)$predictions[,2]
plot_hist(lcs_trim_psid, "ps", "treat", breaks = 40, odds = TRUE, density = TRUE, xlim = c(-3, 3))
graphics.off()

### LCS-PSID

# Log Odds (NSW Treat & PSID1 Controls)
pdf("graphs/lalonde/odds_lcs_psid.pdf", width = 6, height = 6)
lcs_psid$ps <- probability_forest(X = lcs_psid[, covar], 
    Y = as.factor(lcs_psid$treat), seed = 1234, num.trees = 4000)$predictions[,2]
plot_hist(lcs_psid, "ps", "treat", breaks = 40, odds = TRUE, xlim = c(-11, 7))
graphics.off()


# Reestimated Log Odds (NSW Treat & PSID1 Controls - 1:1 Matched on PS)
pdf("graphs/lalonde/odds_lcs_psid_trim.pdf", width = 6, height = 6)
lcs_psid_trim$ps <- probability_forest(X = lcs_psid_trim[, covar], 
    Y = as.factor(lcs_psid_trim$treat), seed = 1234, num.trees = 4000)$predictions[,2]
plot_hist(lcs_psid_trim, "ps_new", "treat", breaks = 40, odds = TRUE, xlim = c(-3, 3))
graphics.off()



#                       $$\     $$\                          $$\               
#                       $$ |    \__|                         $$ |              
#  $$$$$$\   $$$$$$$\ $$$$$$\   $$\ $$$$$$\$$$$\   $$$$$$\ $$$$$$\    $$$$$$\  
# $$  __$$\ $$  _____|\_$$  _|  $$ |$$  _$$  _$$\  \____$$\\_$$  _|  $$  __$$\ 
# $$$$$$$$ |\$$$$$$\    $$ |    $$ |$$ / $$ / $$ | $$$$$$$ | $$ |    $$$$$$$$ |
# $$   ____| \____$$\   $$ |$$\ $$ |$$ | $$ | $$ |$$  __$$ | $$ |$$\ $$   ____|
# \$$$$$$$\ $$$$$$$  |  \$$$$  |$$ |$$ | $$ | $$ |\$$$$$$$ | \$$$$  |\$$$$$$$\ 
#  \_______|\_______/    \____/ \__|\__| \__| \__| \_______|  \____/  \_______|
                                                                             
load("data/lcs.RData")                                                                    

set.seed(1234)
# experimental
out1 <- estimate_all(lcs, Y, "treat", covar)
out2 <- estimate_all(lcs_trim_psid, Y, "treat", covar)
# no experimental
out3 <- estimate_all(lcs_psid, Y, "treat", covar)
out4 <- estimate_all(lcs_psid_trim, Y, "treat", covar)

save(out1, out2, out3, out4, file = "output/est_lcs.RData")

######################
## produce table
######################

load("output/est_lcs.RData")
a <- list(out3, out4)
# columns are samples
n <- nrow(out1) + 1   # add experimental benchmark
sav <- matrix("", n, length(a)*3-1)
for (j in 1:length(a)) {
    out <- a[[j]]
    for (i in 1: (n-1)) {
        sav[i+1, j*3-2] <- sprintf("%.0f", out[i, 1])
        sav[i+1, j*3-1] <- paste0("(", sprintf("%.0f", out[i, 2]), ")")    
    }
}
sav[1, 1] <- sprintf("%.0f", out1[1, 1]) # full experimental
sav[1, 4] <- sprintf("%.0f", out2[1, 1]) # trimmed experimental (PSID)
sav[1, 2] <- paste0("(", sprintf("%.0f", out1[1, 2]), ")")    
sav[1, 5] <- paste0("(", sprintf("%.0f", out2[1, 2]), ")")    
print(sav)

write.csv(sav, file = "tables/lcs.csv", row.names = FALSE)

######################
## plot coefficients
######################


load("output/est_lcs.RData")

pdf("graphs/lalonde/coef_lcs_psid.pdf", width = 12, height = 3)
band <- out1[1, 3:4]
est <- out1[1, 1]
plot_coef(out3, band = band, line = est, ylim = c(-6000, 6000), main = "(A) LCS-PSID")
graphics.off()


pdf("graphs/lalonde/coef_lcs_psid_trim.pdf", width = 12, height = 3)
band <- out2[1, 3:4]
est <- out2[1, 1]
plot_coef(out4, band = band, line = est, ylim = c(-6000, 6000), main = "(B) Trimmed LCS-PSID")
graphics.off()


#                     $$\     $$\     
#                     $$ |    $$ |    
#  $$$$$$$\ $$$$$$\ $$$$$$\ $$$$$$\   
# $$  _____|\____$$\\_$$  _|\_$$  _|  
# $$ /      $$$$$$$ | $$ |    $$ |    
# $$ |     $$  __$$ | $$ |$$\ $$ |$$\ 
# \$$$$$$$\\$$$$$$$ | \$$$$  |\$$$$  |
#  \_______|\_______|  \____/  \____/ 
                                    
                                    

set.seed(1234)
catt.lcs <- catt(lcs, Y, treat, covar)
catt.lcs.psid <- catt(lcs_trim_psid, Y, treat, covar) # trimmed experimental data
catt.psid <- catt(lcs_psid, Y, treat, covar)
catt.psid.trim <- catt(lcs_psid_trim, Y, treat, covar)

pdf("graphs/lalonde/catt_lcs_psid.pdf", width = 5.5, height = 5.5)
par(mar = c(4, 4, 1, 1))
catt1 <- catt.lcs$catt
att1 <- catt.lcs$att[1]
catt2 <- catt.psid$catt
att2 <- catt.psid$att[1]
plot_catt(catt1, catt2, att1, att2, "CATT (Experimental)", "CATT (PSID-Full)",
    main = "", c(-8000, 8000))
graphics.off()

## Trimmed sample
pdf("graphs/lalonde/catt_lcs_psid_trim.pdf", width = 5.5, height = 5.5)
par(mar = c(4, 4, 1, 1))
catt1 <- catt.lcs.psid$catt
att1 <- catt.lcs.psid$att[1]
catt2 <- catt.psid.trim$catt
att2 <- catt.psid.trim$att[1]
plot_catt(catt1, catt2, att1, att2, "CATT (Experimental)", "CATT (PSID-Trimmed)",
    main = "", c(-8000, 8000))
graphics.off()


#            $$\               
#            $$ |              
#  $$$$$$\ $$$$$$\    $$$$$$\  
# $$  __$$\\_$$  _|  $$  __$$\ 
# $$ /  $$ | $$ |    $$$$$$$$ |
# $$ |  $$ | $$ |$$\ $$   ____|
# \$$$$$$$ | \$$$$  |\$$$$$$$\ 
#  \____$$ |  \____/  \_______|
#       $$ |                   
#       $$ |                   
#       \__|                   

load("output/qte_lcs.rds")

set.seed(1234)
## experimental
#qte.lcs <- est_qte(Y, treat, NULL, data = lcs)
qte.lcs.psid <- est_qte(Y, treat, NULL, data = lcs_trim_psid)
## non-experimental
#qte.lcs_psid <- est_qte(Y, treat, covar, data = lcs_psid) # adjusted
#qte.lcs_psid0 <- est_qte(Y, treat, NULL, data = lcs_psid) # unadjusted
qte.lcs_psid.trim <- est_qte(Y, treat, covar, data = lcs_psid_trim) # adjusted
qte.lcs_psid.trim0 <- est_qte(Y, treat, NULL, data = lcs_psid_trim) # unadjusted

save(qte.lcs, qte.lcs.psid, 
    qte.lcs_psid, qte.lcs_psid0, 
    qte.lcs_psid.trim, qte.lcs_psid.trim0, 
    file = "output/qte_lcs.rds")

############################
## plotting 
############################

load("output/qte_lcs.rds")

                           
pdf("graphs/lalonde/qte_lcs.pdf", width = 6.5, height = 5)
plot_qte(qte.lcs, main = "Experimental", ylim = c(-5000, 10000),
    col = c(4, "#ADD8E670"))
graphics.off()


# PSID
pdf("graphs/lalonde/qte_lcs_psid.pdf", width = 6.5, height = 5)
plot_qte(qte.lcs_psid, qte.lcs_psid0, qte.lcs, main = "LCS-PSID", ylim = c(-25000, 15000))
legend("bottomleft", legend = c("Experimental", "Unadjusted", "Adjusted"), 
    lty = 1, pch = c(16, 17, 16), col = c(4, 2, 1), bty = "n")
graphics.off()

# PSID trimmed
pdf("graphs/lalonde/qte_lcs_psid_trim.pdf", width = 6.5, height = 5)
plot_qte(qte.lcs_psid.trim, qte.lcs_psid.trim0, qte.lcs.psid, main = "LCS-PSID (Trimmed)", ylim = c(-25000, 15000))
legend("bottomleft", legend = c("Experimental", "Unadjusted", "Adjusted"), 
    lty = 1, pch = c(16, 17, 16), col = c(4, 2, 1), bty = "n")
graphics.off()


                                        
                                        
#  $$$$$$$\  $$$$$$\  $$$$$$$\   $$$$$$$\ 
# $$  _____|$$  __$$\ $$  __$$\ $$  _____|
# \$$$$$$\  $$$$$$$$ |$$ |  $$ |\$$$$$$\  
#  \____$$\ $$   ____|$$ |  $$ | \____$$\ 
# $$$$$$$  |\$$$$$$$\ $$ |  $$ |$$$$$$$  |
# \_______/  \_______|\__|  \__|\_______/ 
                                        
library("sensemakr")                                        


## trimming
p.forest <- probability_forest(X = lcs_psid[, covar], 
    Y = as.factor(lcs_psid$treat), seed = 1234, num.trees = 4000)
lcs_psid$ps <- p.forest$predictions[,2]
lcs_psid <- subset(lcs_psid, ps > 0.1 & ps < 0.9)

fml <- as.formula(paste(Y, "~", treat, "+", paste(covar, collapse = "+")))
mod.lcs.exp <- lm(fml, data = lcs)
mod.lcs.psid <- lm(fml, data = lcs_psid)
summary(mod.lcs.exp)$coefficients["treat", ]
summary(mod.lcs.psid)$coefficients["treat", ]

bm <- c("re75")
sens.lcs.exp <- sensemakr(model = mod.lcs.exp, treatment = treat, benchmark_covariates = bm, kd = 1, sensitivity.of = "t-value")
sens.lcs.psid <- sensemakr(model = mod.lcs.psid, treatment = treat, benchmark_covariates = bm, kd = 1:3, sensitivity.of = "t-value")

pdf("graphs/lalonde/sens_lcs.pdf", width = 7, height = 7)
plot(sens.lcs.exp)
graphics.off()


pdf("graphs/lalonde/sens_lcs_psid.pdf", width = 7, height = 7)
plot(sens.lcs.psid)
graphics.off()


#           $$\                               $$\                 
#           $$ |                              $$ |                
#  $$$$$$\  $$ | $$$$$$\   $$$$$$$\  $$$$$$\  $$$$$$$\   $$$$$$\  
# $$  __$$\ $$ | \____$$\ $$  _____|$$  __$$\ $$  __$$\ $$  __$$\ 
# $$ /  $$ |$$ | $$$$$$$ |$$ /      $$$$$$$$ |$$ |  $$ |$$ /  $$ |
# $$ |  $$ |$$ |$$  __$$ |$$ |      $$   ____|$$ |  $$ |$$ |  $$ |
# $$$$$$$  |$$ |\$$$$$$$ |\$$$$$$$\ \$$$$$$$\ $$$$$$$  |\$$$$$$  |
# $$  ____/ \__| \_______| \_______| \_______|\_______/  \______/ 
# $$ |                                                            
# $$ |                                                            
# \__|                                                            

load("data/lcs.RData")

######################
## estimates
######################


Y <- "nchildren75"
treat <- "treat"
covar <- c("age", "educ", "nodegree", "married", "black", "hisp", "re75", "u75")


set.seed(1234)
# experimental
out1 <- estimate_all(lcs, Y, "treat", covar)
out2 <- estimate_all(lcs_trim_psid, Y, "treat", covar)
# no experimental
out3 <- estimate_all(lcs_psid, Y, "treat", covar)
out4 <- estimate_all(lcs_psid_trim, Y, "treat", covar)

save(out1, out2, out3, out4, file = "output/est_lcs_pl.RData")


## produce table
load("output/est_lcs_pl.RData")
a <- list(out3, out4)
# columns are samples
n <- nrow(out1) + 1   # add experimental benchmark
sav <- matrix("", n, length(a)*3-1)
for (j in 1:length(a)) {
    out <- a[[j]]
    for (i in 1: (n-1)) {
        sav[i+1, j*3-2] <- sprintf("%.2f", out[i, 1])
        sav[i+1, j*3-1] <- paste0("(", sprintf("%.2f", out[i, 2]), ")")    
    }
}
sav[1, 1] <- sprintf("%.2f", out1[1, 1]) # full experimental
sav[1, 4] <- sprintf("%.2f", out2[1, 1]) # trimmed experimental (PSID)
sav[1, 2] <- paste0("(", sprintf("%.2f", out1[1, 2]), ")")    
sav[1, 5] <- paste0("(", sprintf("%.2f", out2[1, 2]), ")")    
print(sav)
write.csv(sav, file = "tables/lcs_pl.csv", row.names = FALSE)


## plot coefficients
load("output/est_lcs_pl.RData")

pdf("graphs/lalonde/coef_lcs_pl_psid.pdf", width = 12, height = 3)
band <- out1[1, 3:4]
est <- out1[1, 1]
plot_coef(out3, band = band, line = est, ylim = c(-1.5, 1), main = "(A) LCS-PSID")
graphics.off()


pdf("graphs/lalonde/coef_lcs_pl_psid_trim.pdf", width = 12, height = 3)
band <- out2[1, 3:4]
est <- out2[1, 1]
plot_coef(out4, band = band, line = est, ylim = c(-1.5, 1), main = "(B) Trimmed LCS-PSID")
graphics.off()

gc() # free up memory