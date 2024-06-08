# LaLonde (1986) after Nearly Four Decades: Lessons Learned
# Guido Imbens & Yiqing Xu
# 2024

## Sensitivity Analysis

rm(list = ls())
source("code/functions_est.R")
source("code/functions_plot.R")
                                                                    
library("sensemakr")
library("grf")

# $$\       $$$$$$$\  $$\      $$\ 
# $$ |      $$  __$$\ $$ | $\  $$ |
# $$ |      $$ |  $$ |$$ |$$$\ $$ |
# $$ |      $$ |  $$ |$$ $$ $$\$$ |
# $$ |      $$ |  $$ |$$$$  _$$$$ |
# $$ |      $$ |  $$ |$$$  / \$$$ |
# $$$$$$$$\ $$$$$$$  |$$  /   \$$ |
# \________|\_______/ \__/     \__|
                                 

load("data/lalonde.RData")
load("data/trimmed.RData")


data <- ldw
Y <- "re78"
treat <- "treat"
covar <- c("age", "education", "black", "hispanic", "married", "nodegree", "re74", "re75", "u74", "u75")

## trimming
p.forest <- probability_forest(X = ldw_cps[, covar], 
    Y = as.factor(ldw_cps$treat), seed = 1234, num.trees = 4000)
ldw_cps$ps <- p.forest$predictions[,2]
ldw_cps <- subset(ldw_cps, ps > 0.1 & ps < 0.9)

p.forest <- probability_forest(X = ldw_psid[, covar], 
    Y = as.factor(ldw_psid$treat), seed = 1234, num.trees = 4000)
ldw_psid$ps <- p.forest$predictions[,2]
ldw_psid <- subset(ldw_psid, ps > 0.1 & ps < 0.9)


fml <- as.formula(paste(Y, "~", treat, "+", paste(covar, collapse = "+")))
mod.ldw.exp <- lm(fml, data = ldw)
mod.ldw.cps <- lm(fml, data = ldw_cps)
mod.ldw.psid <- lm(fml, data = ldw_psid)
summary(mod.ldw.exp)$coefficients["treat", ]
summary(mod.ldw.cps)$coefficients["treat", ]
summary(mod.ldw.psid)$coefficients["treat", ]

bm <- c("re75")
sens.ldw.exp <- sensemakr(model = mod.ldw.exp, treatment = treat, benchmark_covariates = bm, kd = 1, sensitivity.of = "t-value")
sens.ldw.cps <- sensemakr(model = mod.ldw.cps, treatment = treat, benchmark_covariates = bm, kd = 1:3, sensitivity.of = "t-value")
sens.ldw.psid <- sensemakr(model = mod.ldw.psid, treatment = treat, benchmark_covariates = bm, kd = 1:3, sensitivity.of = "t-value")

pdf("graphs/lalonde/sens_ldw.pdf", width = 7, height = 7)
plot(sens.ldw.exp)
graphics.off()

pdf("graphs/lalonde/sens_ldw_cps.pdf", width = 7, height = 7)
plot(sens.ldw.cps)
graphics.off()

pdf("graphs/lalonde/sens_ldw_psid.pdf", width = 7, height = 7)
plot(sens.ldw.psid)
graphics.off()



# $$\                $$\                                $$\                 $$\      $$\           $$\           
# $$ |               $$ |                               $$ |                $$$\    $$$ |          $$ |          
# $$ |      $$$$$$\  $$ |      $$$$$$\  $$$$$$$\   $$$$$$$ | $$$$$$\        $$$$\  $$$$ | $$$$$$\  $$ | $$$$$$\  
# $$ |      \____$$\ $$ |     $$  __$$\ $$  __$$\ $$  __$$ |$$  __$$\       $$\$$\$$ $$ | \____$$\ $$ |$$  __$$\ 
# $$ |      $$$$$$$ |$$ |     $$ /  $$ |$$ |  $$ |$$ /  $$ |$$$$$$$$ |      $$ \$$$  $$ | $$$$$$$ |$$ |$$$$$$$$ |
# $$ |     $$  __$$ |$$ |     $$ |  $$ |$$ |  $$ |$$ |  $$ |$$   ____|      $$ |\$  /$$ |$$  __$$ |$$ |$$   ____|
# $$$$$$$$\\$$$$$$$ |$$$$$$$$\\$$$$$$  |$$ |  $$ |\$$$$$$$ |\$$$$$$$\       $$ | \_/ $$ |\$$$$$$$ |$$ |\$$$$$$$\ 
# \________|\_______|\________|\______/ \__|  \__| \_______| \_______|      \__|     \__| \_______|\__| \_______|


## datasets to be used: nsw, nsw_trim_cps, nsw_trim_psid
Y <- "re78"
treat <- "treat"
covar <- c("age", "education", "black", "hispanic", "married", "nodegree", "re75", "u75")

fml <- as.formula(paste(Y, "~", treat, "+", paste(covar, collapse = "+")))
mod.nsw.exp <- lm(fml, data = nsw)
mod.nsw.cps <- lm(fml, data = nsw_trim_cps)
mod.nsw.psid <- lm(fml, data = nsw_trim_psid)
summary(mod.nsw.exp)$coefficients["treat", ]
summary(mod.nsw.cps)$coefficients["treat", ]
summary(mod.nsw.psid)$coefficients["treat", ]

sens.nsw.exp <- sensemakr(model = mod.nsw.exp, treatment = treat, benchmark_covariates = bm, kd = 1, sensitivity.of = "t-value")
sens.nsw.cps <- sensemakr(model = mod.nsw.cps, treatment = treat, benchmark_covariates = bm, kd = 1:3, sensitivity.of = "t-value")
sens.nsw.psid <- sensemakr(model = mod.nsw.psid, treatment = treat, benchmark_covariates = bm, kd = 1:3, sensitivity.of = "t-value")

pdf("graphs/lalonde/sens_nsw.pdf", width = 7, height = 7)
plot(sens.nsw.exp)
graphics.off()

pdf("graphs/lalonde/sens_nsw_cps.pdf", width = 7, height = 7)
plot(sens.nsw.cps)
graphics.off()

pdf("graphs/lalonde/sens_nsw_psid.pdf", width = 7, height = 7)
plot(sens.nsw.psid)
graphics.off()

gc() # free up memory


                                        