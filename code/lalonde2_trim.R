# LaLonde (1986) after Nearly Four Decades: Lessons Learned
# Guido Imbens & Yiqing Xu
# 2024

## Trimming the data to improve overlap in propensity scores

rm(list = ls())
source("code/functions_est.R")
source("code/functions_plot.R")
load("data/lalonde.RData")

library(grf)
library(Matching)

# $$\       $$$$$$$\  $$\      $$\ 
# $$ |      $$  __$$\ $$ | $\  $$ |
# $$ |      $$ |  $$ |$$ |$$$\ $$ |
# $$ |      $$ |  $$ |$$ $$ $$\$$ |
# $$ |      $$ |  $$ |$$$$  _$$$$ |
# $$ |      $$ |  $$ |$$$  / \$$$ |
# $$$$$$$$\ $$$$$$$  |$$  /   \$$ |
# \________|\_______/ \__/     \__|
                                 

ldw_co$treat <- 1
ldw_cps.plus <- rbind.data.frame(ldw_cps, ldw_co)
table(ldw_cps.plus$treat)
ldw_psid.plus <- rbind.data.frame(ldw_psid, ldw_co)
table(ldw_psid.plus$treat)


covar <- c("age", "education", "black", "hispanic", "married", "nodegree", 
    "re74", "re75", "u74", "u75")
covar74 <- c("age", "education", "black", "hispanic", "married", "nodegree", "re74", "u74")

                                     
                                     
###################################################
# Trimming LDW-CPS
###################################################


# CPS1: estimate propensity score using GRF
p.forest1 <- probability_forest(X = ldw_cps.plus[, covar], 
    Y = as.factor(ldw_cps.plus$treat), seed = 1234, num.trees = 4000)
ldw_cps.plus$ps <- p.forest1$predictions[,2]
range(ldw_cps.plus$ps)
ldw_cps.plus$ps[which(abs(ldw_cps.plus$ps) <= 1e-7)] <- 1e-7
range(ldw_cps.plus$ps)

round(quantile(ldw_cps.plus$ps[which(ldw_cps.plus$sample == 3 & ldw_cps.plus$ps > 0.6)], probs = seq(0, 1, 0.1)),3)
nrow(subset(ldw_cps.plus, sample == 3 & ps > 0.900)) # only 4 control units have ps > 0.9; choose a cutoff at 0.9

threshold <- 0.9
ldw_trim_cps <- subset(ldw_cps.plus, sample %in% c(1,2) & ps <= threshold)
ldw_trim_cps$treat[which(ldw_trim_cps$sample == 2)] <- 0
ldw_trim_cps$ps <- NULL
table(ldw_trim_cps$treat)
head(ldw_trim_cps)


# propensity score matching without replacement
set.seed(1234) # need to set seed b/c tie-breaking is random
data <- subset(ldw_cps.plus, sample %in% c(1,3) & ps & ps <= threshold) # only use CPS1 controls
data$ps <- probability_forest(X = data[, covar], 
    Y = as.factor(data$treat), seed = 1234, num.trees = 4000)$predictions[,2]
mout <- Match(Y = data$re78, Tr = data$treat, X = data$ps, estimand = "ATT", M = 1,
      BiasAdjust = FALSE, replace=FALSE, ties = FALSE)
ldw_cps_trim <- data[c(mout$index.treated, mout$index.control), ]

# estimate propensity score again
ldw_cps_trim$ps_new <- probability_forest(X = ldw_cps_trim[, covar], 
    Y = as.factor(ldw_cps_trim$treat), 
    seed = 1234, num.trees = 4000)$predictions[,2]
table(ldw_cps_trim$treat)


###################################################
# Trimming LDW-PSID
###################################################

p.forest2 <- probability_forest(X = ldw_psid.plus[, covar], 
    Y = as.factor(ldw_psid.plus$treat), seed = 1234, num.trees = 4000)
ldw_psid.plus$ps <- p.forest2$predictions[,2]
range(ldw_psid.plus$ps)
ldw_psid.plus$ps[which(abs(ldw_psid.plus$ps) <= 1e-7)] <- 1e-7
range(ldw_psid.plus$ps)

# histogram of propensity scores
round(quantile(ldw_psid.plus$ps[which(ldw_psid.plus$sample == 4 & ldw_psid.plus$ps > 0.6)], probs = seq(0, 1, 0.1)),3)
nrow(subset(ldw_psid.plus, sample == 4 & ps > 0.8)) # only 9 control units have ps > 0.8
threshold <- 0.8 

# trim experimental data
ldw_trim_psid <- subset(ldw_psid.plus, sample %in% c(1,2) & ps <= threshold)
ldw_trim_psid$treat[which(ldw_trim_psid$sample == 2)] <- 0
ldw_trim_psid$ps <- NULL
table(ldw_trim_psid$treat)
table(ldw_trim_psid$sample)
head(ldw_trim_psid)

# propensity score matching without replacement
set.seed(1234) # need to set seed b/c tie-breaking is random
data <- subset(ldw_psid.plus, sample %in% c(1,4) & ps <= threshold) # only use psid1 controls
data$ps <- probability_forest(X = data[, covar], 
    Y = as.factor(data$treat), seed = 1234, num.trees = 4000)$predictions[,2]
mout <- Match(Y = data$re78, Tr = data$treat, X = data$ps, estimand = "ATT", M = 1,
      BiasAdjust = FALSE, replace=FALSE, ties = FALSE)
ldw_psid_trim <- data[c(mout$index.treated, mout$index.control), ]

# estimate propensity score again
ldw_psid_trim$ps_new <- probability_forest(X = ldw_psid_trim[, covar], 
    Y = as.factor(ldw_psid_trim$treat), 
    seed = 1234, num.trees = 4000)$predictions[,2]
table(ldw_psid_trim$treat)


# $$$$$$$\  $$\                               $$\                 
# $$  __$$\ $$ |                              $$ |                
# $$ |  $$ |$$ | $$$$$$\   $$$$$$$\  $$$$$$\  $$$$$$$\   $$$$$$\  
# $$$$$$$  |$$ | \____$$\ $$  _____|$$  __$$\ $$  __$$\ $$  __$$\ 
# $$  ____/ $$ | $$$$$$$ |$$ /      $$$$$$$$ |$$ |  $$ |$$ /  $$ |
# $$ |      $$ |$$  __$$ |$$ |      $$   ____|$$ |  $$ |$$ |  $$ |
# $$ |      $$ |\$$$$$$$ |\$$$$$$$\ \$$$$$$$\ $$$$$$$  |\$$$$$$  |
# \__|      \__| \_______| \_______| \_______|\_______/  \______/ 
                                                                

##########################
## CPS1
##########################
                                    
p.forest1a <- probability_forest(X = ldw_cps.plus[, covar74], 
    Y = as.factor(ldw_cps.plus$treat), seed = 1234, num.trees = 4000)
ldw_cps.plus$ps <- p.forest1a$predictions[,2]
range(ldw_cps.plus$ps)
ldw_cps.plus$ps[which(abs(ldw_cps.plus$ps) <= 1e-7)] <- 1e-7
range(ldw_cps.plus$ps)

threshold <- 0.9
ldw_trim_cps_pl <- subset(ldw_cps.plus, sample %in% c(1,2) & ps <= threshold)
ldw_trim_cps_pl$treat[which(ldw_trim_cps_pl$sample == 2)] <- 0
ldw_trim_cps_pl$ps <- NULL
table(ldw_trim_cps_pl$treat)
head(ldw_trim_cps_pl)

# propensity score matching without replacement
set.seed(1234) # need to set seed b/c tie-breaking is random
data <- subset(ldw_cps.plus, sample %in% c(1,3) & ps & ps <= threshold) # only use CPS1 controls
data$ps <- probability_forest(X = data[, covar74], 
    Y = as.factor(data$treat), seed = 1234, num.trees = 4000)$predictions[,2]
mout <- Match(Y = data$re78, Tr = data$treat, X = data$ps, estimand = "ATT", M = 1,
      BiasAdjust = FALSE, replace=FALSE, ties = FALSE)
ldw_cps_trim_pl <- data[c(mout$index.treated, mout$index.control), ]

# estimate propensity score again
ldw_cps_trim_pl$ps_new <- probability_forest(X = ldw_cps_trim_pl[, covar74], 
    Y = as.factor(ldw_cps_trim_pl$treat), 
    seed = 1234, num.trees = 4000)$predictions[,2]
table(ldw_cps_trim_pl$treat)



##########################
## PSID1
##########################


p.forest2a <- probability_forest(X = ldw_psid.plus[, covar74], 
    Y = as.factor(ldw_psid.plus$treat), seed = 1234, num.trees = 4000)
ldw_psid.plus$ps <- p.forest2a$predictions[,2]
range(ldw_psid.plus$ps)
ldw_psid.plus$ps[which(abs(ldw_psid.plus$ps) <= 1e-7)] <- 1e-7
range(ldw_psid.plus$ps)


threshold <- 0.8 # only 9 control units have ps > 0.8

# trim experimental data
ldw_trim_psid_pl <- subset(ldw_psid.plus, sample %in% c(1,2) & ps <= threshold)
ldw_trim_psid_pl$treat[which(ldw_trim_psid_pl$sample == 2)] <- 0
ldw_trim_psid_pl$ps <- NULL
table(ldw_trim_psid_pl$treat)
table(ldw_trim_psid_pl$sample)
head(ldw_trim_psid_pl)

# propensity score matching without replacement
set.seed(1234) # need to set seed b/c tie-breaking is random
data <- subset(ldw_psid.plus, sample %in% c(1,4) & ps <= threshold) # only use psid1 controls
data$ps <- probability_forest(X = data[, covar74], 
    Y = as.factor(data$treat), seed = 1234, num.trees = 4000)$predictions[,2]
mout <- Match(Y = data$re75, Tr = data$treat, X = data$ps, estimand = "ATT", M = 1,
      BiasAdjust = FALSE, replace=FALSE, ties = FALSE)
ldw_psid_trim_pl <- data[c(mout$index.treated, mout$index.control), ]

# estimate propensity score again
ldw_psid_trim_pl$ps_new <- probability_forest(X = ldw_psid_trim_pl[, covar74], 
    Y = as.factor(ldw_psid_trim_pl$treat), 
    seed = 1234, num.trees = 4000)$predictions[,2]
table(ldw_psid_trim_pl$treat)


# $$\   $$\           $$$$$$$$\ $$\   $$\ 
# $$$\  $$ |          \____$$  |$$ |  $$ |
# $$$$\ $$ | $$$$$$\      $$  / $$ |  $$ |
# $$ $$\$$ |$$  __$$\    $$  /  $$$$$$$$ |
# $$ \$$$$ |$$ /  $$ |  $$  /   \_____$$ |
# $$ |\$$$ |$$ |  $$ | $$  /          $$ |
# $$ | \$$ |\$$$$$$  |$$  /           $$ |
# \__|  \__| \______/ \__/            \__|

covar_no74 <- c("age", "education", "black", "hispanic", "married", "nodegree", "re75", "u75")

##########################
## CPS1
##########################
                                    
p.forest1a <- probability_forest(X = ldw_cps.plus[, covar_no74], 
    Y = as.factor(ldw_cps.plus$treat), seed = 1234, num.trees = 4000)
ldw_cps.plus$ps <- p.forest1a$predictions[,2]
range(ldw_cps.plus$ps)
ldw_cps.plus$ps[which(abs(ldw_cps.plus$ps) <= 1e-7)] <- 1e-7
range(ldw_cps.plus$ps)

threshold <- 0.9
ldw_trim_cps_no74 <- subset(ldw_cps.plus, sample %in% c(1,2) & ps <= threshold)
ldw_trim_cps_no74$treat[which(ldw_trim_cps_no74$sample == 2)] <- 0
ldw_trim_cps_no74$ps <- NULL
table(ldw_trim_cps_no74$treat) # 175 treated, 246 controls
head(ldw_trim_cps_no74)

# propensity score matching without replacement
set.seed(1234) # need to set seed b/c tie-breaking is random
data <- subset(ldw_cps.plus, sample %in% c(1,3) & ps & ps <= threshold) # only use CPS1 controls
data$ps <- probability_forest(X = data[, covar_no74], 
    Y = as.factor(data$treat), seed = 1234, num.trees = 4000)$predictions[,2]
mout <- Match(Y = data$re78, Tr = data$treat, X = data$ps, estimand = "ATT", M = 1,
      BiasAdjust = FALSE, replace=FALSE, ties = FALSE)
ldw_cps_trim_no74 <- data[c(mout$index.treated, mout$index.control), ]

# estimate propensity score again
ldw_cps_trim_no74$ps_new <- probability_forest(X = ldw_cps_trim_no74[, covar_no74], 
    Y = as.factor(ldw_cps_trim_no74$treat), 
    seed = 1234, num.trees = 4000)$predictions[,2]
table(ldw_cps_trim_no74$treat)


##########################
## PSID1
##########################


p.forest2a <- probability_forest(X = ldw_psid.plus[, covar_no74], 
    Y = as.factor(ldw_psid.plus$treat), seed = 1234, num.trees = 4000)
ldw_psid.plus$ps <- p.forest2a$predictions[,2]
range(ldw_psid.plus$ps)
ldw_psid.plus$ps[which(abs(ldw_psid.plus$ps) <= 1e-7)] <- 1e-7
range(ldw_psid.plus$ps)


threshold <- 0.8 # only 9 control units have ps > 0.8

# trim experimental data
ldw_trim_psid_no74 <- subset(ldw_psid.plus, sample %in% c(1,2) & ps <= threshold)
ldw_trim_psid_no74$treat[which(ldw_trim_psid_no74$sample == 2)] <- 0
ldw_trim_psid_no74$ps <- NULL
table(ldw_trim_psid_no74$treat) # 79 treated and 82 controls
table(ldw_trim_psid_no74$sample)
head(ldw_trim_psid_no74)

# propensity score matching without replacement
set.seed(1234) # need to set seed b/c tie-breaking is random
data <- subset(ldw_psid.plus, sample %in% c(1,4) & ps <= threshold) # only use psid1 controls
data$ps <- probability_forest(X = data[, covar_no74], 
    Y = as.factor(data$treat), seed = 1234, num.trees = 4000)$predictions[,2]
mout <- Match(Y = data$re75, Tr = data$treat, X = data$ps, estimand = "ATT", M = 1,
      BiasAdjust = FALSE, replace=FALSE, ties = FALSE)
ldw_psid_trim_no74 <- data[c(mout$index.treated, mout$index.control), ]

# estimate propensity score again
ldw_psid_trim_no74$ps_new <- probability_forest(X = ldw_psid_trim_no74[, covar_no74], 
    Y = as.factor(ldw_psid_trim_no74$treat), 
    seed = 1234, num.trees = 4000)$predictions[,2]
table(ldw_psid_trim_no74$treat)


                                                                    

# $$\                $$\                                $$\                 $$\      $$\           $$\           
# $$ |               $$ |                               $$ |                $$$\    $$$ |          $$ |          
# $$ |      $$$$$$\  $$ |      $$$$$$\  $$$$$$$\   $$$$$$$ | $$$$$$\        $$$$\  $$$$ | $$$$$$\  $$ | $$$$$$\  
# $$ |      \____$$\ $$ |     $$  __$$\ $$  __$$\ $$  __$$ |$$  __$$\       $$\$$\$$ $$ | \____$$\ $$ |$$  __$$\ 
# $$ |      $$$$$$$ |$$ |     $$ /  $$ |$$ |  $$ |$$ /  $$ |$$$$$$$$ |      $$ \$$$  $$ | $$$$$$$ |$$ |$$$$$$$$ |
# $$ |     $$  __$$ |$$ |     $$ |  $$ |$$ |  $$ |$$ |  $$ |$$   ____|      $$ |\$  /$$ |$$  __$$ |$$ |$$   ____|
# $$$$$$$$\\$$$$$$$ |$$$$$$$$\\$$$$$$  |$$ |  $$ |\$$$$$$$ |\$$$$$$$\       $$ | \_/ $$ |\$$$$$$$ |$$ |\$$$$$$$\ 
# \________|\_______|\________|\______/ \__|  \__| \_______| \_______|      \__|     \__| \_______|\__| \_______|
                                                                                                               

                                                                                                               
names(nsw)
nsw_tr <- nsw[which(nsw$treat == 1), ] # experimental treated 
nsw_co <- nsw[which(nsw$treat == 0), ] # experimental control
nsw_co$treat <- 1

# drop re74, u74, tau from cps1 and psid1
cps1 <- cps1[, setdiff(names(cps1), c("re74", "u74", "tau"))]
psid1 <- psid1[, setdiff(names(psid1), c("re74", "u74", "tau"))]

nsw_cps <- rbind.data.frame(nsw_tr, cps1)
nsw_cps.plus <- rbind.data.frame(nsw_cps, nsw_co)
table(nsw_cps.plus$treat)

nsw_psid <- rbind.data.frame(nsw_tr, psid1)
nsw_psid.plus <- rbind.data.frame(nsw_psid, nsw_co)
table(nsw_psid.plus$treat)

covar <- c("age", "education", "black", "hispanic", "married", "nodegree", "re75", "u75")

#   $$\               $$\               
#   $$ |              \__|              
# $$$$$$\    $$$$$$\  $$\ $$$$$$\$$$$\  
# \_$$  _|  $$  __$$\ $$ |$$  _$$  _$$\ 
#   $$ |    $$ |  \__|$$ |$$ / $$ / $$ |
#   $$ |$$\ $$ |      $$ |$$ | $$ | $$ |
#   \$$$$  |$$ |      $$ |$$ | $$ | $$ |
#    \____/ \__|      \__|\__| \__| \__|
                                      

###################################################
# Trimming NSW-CPS
###################################################

# CPS1: estimate propensity score using GRF
p.forest1 <- probability_forest(X = nsw_cps.plus[, covar], 
    Y = as.factor(nsw_cps.plus$treat), seed = 1234, num.trees = 4000)
nsw_cps.plus$ps <- p.forest1$predictions[,2]
range(nsw_cps.plus$ps)
nsw_cps.plus$ps[which(abs(nsw_cps.plus$ps) <= 1e-7)] <- 1e-7
range(nsw_cps.plus$ps)

# histogram of propensity scores
round(quantile(nsw_cps.plus$ps[which(nsw_cps.plus$sample == 3 & nsw_cps.plus$ps > 0.6)], probs = seq(0, 1, 0.1)),3)

# only 10 control units have ps > 0.85; choose a cutoff at 0.85
threshold <- 0.85
table(nsw_cps.plus$sample)
nsw_trim_cps <- subset(nsw_cps.plus, sample %in% c(0, 0.5) & ps <= threshold)
nsw_trim_cps$treat[which(nsw_trim_cps$sample == 0.5)] <- 0
table(nsw_trim_cps$treat)
nsw_trim_cps$ps <- NULL
head(nsw_trim_cps)


# propensity score matching without replacement
set.seed(1234) # need to set seed b/c tie-breaking is random
data <- subset(nsw_cps.plus, sample %in% c(0,3) & ps & ps <= threshold) # only use CPS1 controls
data$ps <- probability_forest(X = data[, covar], 
    Y = as.factor(data$treat), seed = 1234, num.trees = 4000)$predictions[,2]
mout <- Match(Y = data$re78, Tr = data$treat, X = data$ps, estimand = "ATT", M = 1,
      BiasAdjust = FALSE, replace=FALSE, ties = FALSE)
nsw_cps_trim <- data[c(mout$index.treated, mout$index.control), ]

# estimate propensity score again
nsw_cps_trim$ps_new <- probability_forest(X = nsw_cps_trim[, covar], 
    Y = as.factor(nsw_cps_trim$treat), 
    seed = 1234, num.trees = 4000)$predictions[,2]
table(nsw_cps_trim$treat)


###################################################
# Trimming NSW-PSID
###################################################

p.forest2 <- probability_forest(X = nsw_psid.plus[, covar], 
    Y = as.factor(nsw_psid.plus$treat), seed = 1234, num.trees = 4000)
nsw_psid.plus$ps <- p.forest2$predictions[,2]
range(nsw_psid.plus$ps)
nsw_psid.plus$ps[which(abs(nsw_psid.plus$ps) <= 1e-7)] <- 1e-7
range(nsw_psid.plus$ps)

# histogram of propensity scores
round(quantile(nsw_psid.plus$ps[which(nsw_psid.plus$sample == 4 & nsw_psid.plus$ps > 0.6)], probs = seq(0, 1, 0.1)),3)
dim(subset(nsw_psid.plus, sample == 4 & ps > 0.85))
threshold <- 0.85 # only 20 control units have ps > 0.85

# trim experimental data
nsw_trim_psid <- subset(nsw_psid.plus, sample %in% c(0,0.5) & ps <= threshold)
nsw_trim_psid$treat[which(nsw_trim_psid$sample == 0.5)] <- 0
nsw_trim_psid$ps <- NULL
table(nsw_trim_psid$treat)
table(nsw_trim_psid$sample)
head(nsw_trim_psid)

# propensity score matching without replacement
set.seed(1234) # need to set seed b/c tie-breaking is random
data <- subset(nsw_psid.plus, sample %in% c(0,4) & ps <= threshold) # only use psid1 controls
data$ps <- probability_forest(X = data[, covar], 
    Y = as.factor(data$treat), seed = 1234, num.trees = 4000)$predictions[,2]
mout <- Match(Y = data$re78, Tr = data$treat, X = data$ps, estimand = "ATT", M = 1,
      BiasAdjust = FALSE, replace=FALSE, ties = FALSE)
nsw_psid_trim <- data[c(mout$index.treated, mout$index.control), ]

# estimate propensity score again
nsw_psid_trim$ps_new <- probability_forest(X = nsw_psid_trim[, covar], 
    Y = as.factor(nsw_psid_trim$treat), 
    seed = 1234, num.trees = 4000)$predictions[,2]
table(nsw_psid_trim$treat)


#  $$$$$$\                                
# $$  __$$\                               
# $$ /  \__| $$$$$$\ $$\    $$\  $$$$$$\  
# \$$$$$$\   \____$$\\$$\  $$  |$$  __$$\ 
#  \____$$\  $$$$$$$ |\$$\$$  / $$$$$$$$ |
# $$\   $$ |$$  __$$ | \$$$  /  $$   ____|
# \$$$$$$  |\$$$$$$$ |  \$  /   \$$$$$$$\ 
#  \______/  \_______|   \_/     \_______|
                                        


save(ldw_trim_cps, ldw_trim_psid, ldw_cps_trim, ldw_psid_trim, 
    ldw_trim_cps_pl, ldw_trim_psid_pl, ldw_cps_trim_pl, ldw_psid_trim_pl, 
    ldw_trim_cps_no74, ldw_trim_psid_no74, ldw_cps_trim_no74, ldw_psid_trim_no74, 
    nsw_trim_cps, nsw_trim_psid, nsw_cps_trim, nsw_psid_trim, 
    file = "data/trimmed.RData")

gc() # free up memory




