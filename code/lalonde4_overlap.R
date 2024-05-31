# LaLonde (1986) after Nearly Four Decades: Lessons Learned
# Guido Imbens & Yiqing Xu
# 2024

## Plot the overlap in propensity scores

rm(list = ls())
source("code/functions_est.R")
source("code/functions_plot.R")

load("data/lalonde.RData")
load("data/trimmed.RData")

##################################
# Plotting Propensity Score
##################################

covar <- c("age", "education", "black", "hispanic", "married", "nodegree", 
    "re74", "re75", "u74", "u75")

# Propensity Scores (NSW, Dehejia-Wahba Sample)
pdf("graphs/lalonde/ps_ldw_exp.pdf", width = 6, height = 6)
ldw$ps <- probability_forest(X = ldw[, covar], 
    Y = as.factor(ldw$treat), seed = 1234, num.trees = 4000)$predictions[,2]
plot_hist(ldw, "ps", "treat", breaks = 40, density = TRUE)
graphics.off()

# trimmed based on CPS1 controls
pdf("graphs/lalonde/ps_ldw_trim_cps.pdf", width = 6, height = 6)
ldw_trim_cps$ps <- probability_forest(X = ldw_trim_cps[, covar], 
    Y = as.factor(ldw_trim_cps$treat), seed = 1234, num.trees = 4000)$predictions[,2]
plot_hist(ldw_trim_cps, "ps", "treat", breaks = 40, density = TRUE)
graphics.off()

# trimmed based on PSID controls
pdf("graphs/lalonde/ps_ldw_trim_psid.pdf", width = 6, height = 6)
ldw_trim_psid$ps <- probability_forest(X = ldw_trim_psid[, covar], 
    Y = as.factor(ldw_trim_psid$treat), seed = 1234, num.trees = 4000)$predictions[,2]
plot_hist(ldw_trim_psid, "ps", "treat", breaks = 40, density = TRUE)
graphics.off()

################
### LDW-CPS1
################

# Propensity Scores (NSW Treat & CPS1 Controls)
pdf("graphs/lalonde/ps_ldw_cps.pdf", width = 6, height = 6)
ldw_cps$ps <- probability_forest(X = ldw_cps[, covar], 
    Y = as.factor(ldw_cps$treat), seed = 1234, num.trees = 4000)$predictions[,2]
plot_hist(ldw_cps, "ps", "treat", breaks = 40, density = TRUE)
graphics.off()

# Reestimated Propensity Scores (NSW Treat & CPS1 Controls - 1:1 Matched on PS)
pdf("graphs/lalonde/ps_ldw_cps_trim.pdf", width = 6, height = 6)
plot_hist(ldw_cps_trim, "ps_new", "treat", breaks = 40, density = TRUE, main = "")
graphics.off()

################
### LDW-PSID1
################

# Propensity Scores (NSW Treat & PSID1 Controls)
pdf("graphs/lalonde/ps_ldw_psid.pdf", width = 6, height = 6)
ldw_psid$ps <- probability_forest(X = ldw_psid[, covar], 
    Y = as.factor(ldw_psid$treat), seed = 1234, num.trees = 4000)$predictions[,2]
plot_hist(ldw_psid, "ps", "treat", breaks = 40, density = TRUE)
graphics.off()


# Reestimated Propensity Scores (NSW Treat & PSID1 Controls - 1:1 Matched on PS)
pdf("graphs/lalonde/ps_ldw_psid_trim.pdf", width = 6, height = 6)
plot_hist(ldw_psid_trim, "ps_new", "treat", breaks = 40, density = TRUE, main = "")
graphics.off()



##################################
# Log Odds
##################################

# Log Odds (NSW, Dehejia-Wahba Sample)
pdf("graphs/lalonde/odds_ldw_exp.pdf", width = 6, height = 6)
plot_hist(ldw, "ps", "treat", breaks = 40, odds = TRUE, xlim = c(-2, 1.5))
graphics.off()

# trimmed based on CPS1 controls
pdf("graphs/lalonde/odds_ldw_trim_cps.pdf", width = 6, height = 6)
ldw_trim_cps$ps <- probability_forest(X = ldw_trim_cps[, covar], 
    Y = as.factor(ldw_trim_cps$treat), seed = 1234, num.trees = 4000)$predictions[,2]
plot_hist(ldw_trim_cps, "ps", "treat", breaks = 40, odds = TRUE, density = TRUE, xlim = c(-3, 3))
graphics.off()

# trimmed based on PSID controls
pdf("graphs/lalonde/odds_ldw_trim_psid.pdf", width = 6, height = 6)
ldw_trim_psid$ps <- probability_forest(X = ldw_trim_psid[, covar], 
    Y = as.factor(ldw_trim_psid$treat), seed = 1234, num.trees = 4000)$predictions[,2]
plot_hist(ldw_trim_psid, "ps", "treat", breaks = 40, odds = TRUE, density = TRUE, xlim = c(-3, 3))
graphics.off()

### LDW-CPS1

# Log Odds (NSW Treat & CPS1 Controls)
pdf("graphs/lalonde/odds_ldw_cps.pdf", width = 6, height = 6)
plot_hist(ldw_cps, "ps", "treat", breaks = 40, odds = TRUE, xlim = c(-15, 5))
graphics.off()

# Reestimated Log Odds (NSW Treat & CPS1 Controls - 1:1 Matched on PS)
pdf("graphs/lalonde/odds_ldw_cps_trim.pdf", width = 6, height = 6)
plot_hist(ldw_cps_trim, "ps_new", "treat", breaks = 40, odds = TRUE, xlim = c(-3, 3))
graphics.off()

### LDW-PSID1

# Log Odds (NSW Treat & PSID1 Controls)
pdf("graphs/lalonde/odds_ldw_psid.pdf", width = 6, height = 6)
plot_hist(ldw_psid, "ps", "treat", breaks = 40, odds = TRUE, xlim = c(-15, 5))
graphics.off()


# Reestimated Log Odds (NSW Treat & PSID1 Controls - 1:1 Matched on PS)
pdf("graphs/lalonde/odds_ldw_psid_trim.pdf", width = 6, height = 6)
plot_hist(ldw_psid_trim, "ps_new", "treat", breaks = 40, odds = TRUE, xlim = c(-3, 3))
graphics.off()


# $$\                $$\                                $$\                 $$\      $$\           $$\           
# $$ |               $$ |                               $$ |                $$$\    $$$ |          $$ |          
# $$ |      $$$$$$\  $$ |      $$$$$$\  $$$$$$$\   $$$$$$$ | $$$$$$\        $$$$\  $$$$ | $$$$$$\  $$ | $$$$$$\  
# $$ |      \____$$\ $$ |     $$  __$$\ $$  __$$\ $$  __$$ |$$  __$$\       $$\$$\$$ $$ | \____$$\ $$ |$$  __$$\ 
# $$ |      $$$$$$$ |$$ |     $$ /  $$ |$$ |  $$ |$$ /  $$ |$$$$$$$$ |      $$ \$$$  $$ | $$$$$$$ |$$ |$$$$$$$$ |
# $$ |     $$  __$$ |$$ |     $$ |  $$ |$$ |  $$ |$$ |  $$ |$$   ____|      $$ |\$  /$$ |$$  __$$ |$$ |$$   ____|
# $$$$$$$$\\$$$$$$$ |$$$$$$$$\\$$$$$$  |$$ |  $$ |\$$$$$$$ |\$$$$$$$\       $$ | \_/ $$ |\$$$$$$$ |$$ |\$$$$$$$\ 
# \________|\_______|\________|\______/ \__|  \__| \_______| \_______|      \__|     \__| \_______|\__| \_______|
                                                                                                               
covar <- c("age", "education", "black", "hispanic", "married", "nodegree", "re75", "u75")


# Log Odds (NSW, Dehejia-Wahba Sample)
pdf("graphs/lalonde/odds_nsw_exp.pdf", width = 6, height = 6)
nsw$ps <- probability_forest(X = nsw[, covar], 
    Y = as.factor(nsw$treat), seed = 1234, num.trees = 4000)$predictions[,2]
plot_hist(nsw, "ps", "treat", breaks = 40, odds = TRUE, xlim = c(-2, 1.5))
graphics.off()

# trimmed based on CPS1 controls
pdf("graphs/lalonde/odds_nsw_trim_cps.pdf", width = 6, height = 6)
nsw_trim_cps$ps <- probability_forest(X = nsw_trim_cps[, covar], 
    Y = as.factor(nsw_trim_cps$treat), seed = 1234, num.trees = 4000)$predictions[,2]
plot_hist(nsw_trim_cps, "ps", "treat", breaks = 40, odds = TRUE, density = TRUE, xlim = c(-3, 3))
graphics.off()

# trimmed based on PSID controls
pdf("graphs/lalonde/odds_nsw_trim_psid.pdf", width = 6, height = 6)
nsw_trim_psid$ps <- probability_forest(X = nsw_trim_psid[, covar], 
    Y = as.factor(nsw_trim_psid$treat), seed = 1234, num.trees = 4000)$predictions[,2]
plot_hist(nsw_trim_psid, "ps", "treat", breaks = 40, odds = TRUE, density = TRUE, xlim = c(-3, 3))
graphics.off()

### NSW-CPS1

# Log Odds (NSW Treat & CPS1 Controls)
pdf("graphs/lalonde/odds_nsw_cps.pdf", width = 6, height = 6)
ldw_cps$ps <- probability_forest(X = ldw_cps[, covar], 
    Y = as.factor(ldw_cps$treat), seed = 1234, num.trees = 4000)$predictions[,2]
plot_hist(ldw_cps, "ps", "treat", breaks = 40, odds = TRUE, xlim = c(-15, 5))
graphics.off()

# Reestimated Log Odds (NSW Treat & CPS1 Controls - 1:1 Matched on PS)
pdf("graphs/lalonde/odds_nsw_cps_trim.pdf", width = 6, height = 6)
plot_hist(nsw_cps_trim, "ps_new", "treat", breaks = 40, odds = TRUE, xlim = c(-3, 3))
graphics.off()

### NSW-PSID1

# Log Odds (NSW Treat & PSID1 Controls)
pdf("graphs/lalonde/odds_nsw_psid.pdf", width = 6, height = 6)
ldw_psid$ps <- probability_forest(X = ldw_psid[, covar], 
    Y = as.factor(ldw_psid$treat), seed = 1234, num.trees = 4000)$predictions[,2]
plot_hist(ldw_psid, "ps", "treat", breaks = 40, odds = TRUE, xlim = c(-15, 5))
graphics.off()


# Reestimated Log Odds (NSW Treat & PSID1 Controls - 1:1 Matched on PS)
pdf("graphs/lalonde/odds_nsw_psid_trim.pdf", width = 6, height = 6)
plot_hist(nsw_psid_trim, "ps_new", "treat", breaks = 40, odds = TRUE, xlim = c(-3, 3))
graphics.off()

