# LaLonde (1986) after Nearly Four Decades: Lessons Learned
# Guido Imbens & Yiqing Xu
# 2024

## Imbens-Rubin-Sacerdote Lottery Data
## Estimation

rm(list = ls())
source("code/functions_est.R")
source("code/functions_plot.R")

load("data/irs/lottery.RData")
head(d)
names(d)
str(d)
summary(d)
class(d)
str(d)

#hist(d$agew)
#hist(log10(d$yearlpr), breaks = 50)
table(d$winner, d$bigwinner)

                                       
d$tr <- d$winner                                                 
d$tr1 <- ifelse(d$bigwinner == 1, 1, 0) # big winner
d$tr2 <- ifelse(d$bigwinner == 0 & d$winner == 1, 1, 0) # small winner
d$co <- ifelse(d$winner == 0, 1, 0) # control
d$college <- ifelse(d$educ >= 16, 1, 0)

colnames(d)[9:14] <- paste0("x", 1:6)
colnames(d)[15:21] <- paste0("y", 1:7)
d$earnings_1yr_before <- d$x6

table(d$tr1, d$tr2)
table(d$tr1, d$co)
table(d$tr2, d$co)
d$xearn.avg <- apply(d[, paste0("x", 4:6)], 1, mean) # avg pre outcome
d$yearn.avg <- apply(d[, paste0("y", 1:7)], 1, mean) # avg pst outcome

s1 <- subset(d, tr1 == 1 | co == 1) # big winner
s2 <- subset(d, tr2 == 1 | co == 1) # small winner
table(s1$tr)
table(s2$tr)


covar <- c("tixbot", "male", "workthen", "agew", "educ", "college", 
    "x1", "x2", "x3", "yearw")


# big winners
set.seed(1234)
out1 <- estimate_all(s1, "yearn.avg", "tr", covar)
out2 <- estimate_all(s1, "xearn.avg", "tr", covar)
# small winners
out3 <- estimate_all(s2, "yearn.avg", "tr", covar)
out4 <- estimate_all(s2, "xearn.avg", "tr", covar)


# columns are samples
a <- list(out1, out2, out3, out4)
n <- nrow(out1)   
sav <- matrix("", n, length(a)*3-1)
for (j in 1:length(a)) {
    out <- a[[j]]
    n <- nrow(out)    
    for (i in 1:nrow(out)) {
        sav[i, j*3-2] <- sprintf("%.2f", out[i, 1])
        sav[i, j*3-1] <- paste0("(", sprintf("%.2f", out[i, 2]), ")")    
    }
}
print(sav)
write.csv(sav, file = "tables/irs.csv", row.names = FALSE)


# Trimmed data
# trim
s1$ps <- probability_forest(X = s1[, covar], 
    Y = as.factor(s1$tr), seed = 1234)$predictions[,2]
match.out <- matchit(tr ~ ps, ratio = 1, data = s1, replace = FALSE)
s1t <- s1[which(match.out$weights == 1), ]
table(s1t$tr)

s2$ps <- probability_forest(X = s2[, covar], 
    Y = as.factor(s2$tr), seed = 1234)$predictions[,2]
match.out <- matchit(tr ~ ps, ratio = 1, data = s2, replace = FALSE)
s2t <- s2[which(match.out$weights == 1), ]
table(s2t$tr)
table(s1t$tr)
table(s2t$tr)

set.seed(1234)
out1t <- estimate_all(s1t, "yearn.avg", "tr", covar)
out2t <- estimate_all(s1t, "xearn.avg", "tr", covar)
# small winners
out3t <- estimate_all(s2t, "yearn.avg", "tr", covar)
out4t <- estimate_all(s2t, "xearn.avg", "tr", covar)

# columns are samples
a <- list(out1t, out2t, out3t, out4t)
n <- nrow(out1)   
sav <- matrix("", n, length(a)*3-1)
for (j in 1:length(a)) {
    out <- a[[j]]
    n <- nrow(out)    
    for (i in 1:nrow(out)) {
        sav[i, j*3-2] <- sprintf("%.2f", out[i, 1])
        sav[i, j*3-1] <- paste0("(", sprintf("%.2f", out[i, 2]), ")")    
    }
}
print(sav)
write.csv(sav, file = "tables/irs_trimmed.csv", row.names = FALSE)

#           $$\            $$\     
#           $$ |           $$ |    
#  $$$$$$\  $$ | $$$$$$\ $$$$$$\   
# $$  __$$\ $$ |$$  __$$\\_$$  _|  
# $$ /  $$ |$$ |$$ /  $$ | $$ |    
# $$ |  $$ |$$ |$$ |  $$ | $$ |$$\ 
# $$$$$$$  |$$ |\$$$$$$  | \$$$$  |
# $$  ____/ \__| \______/   \____/ 
# $$ |                             
# $$ |                             
# \__|                             

ylim <- c(-20, 20)

pdf("graphs/irs/irs_coef_big.pdf", width = 12, height = 3)
plot_coef(out1, ylim = ylim, main = "Big Winner vs. Non-Winner", main.pos = 3)
graphics.off()

pdf("graphs/irs/irs_coef_big_pl.pdf", width = 12, height = 3)
plot_coef(out2, ylim = ylim, main = "Big Winner vs. Non-Winner: Placebo Test", main.pos = 3)
graphics.off()

pdf("graphs/irs/irs_coef_small.pdf", width = 12, height = 3)
plot_coef(out3, ylim = ylim, main = "Small Winner vs. Non-Winner", main.pos = 3)
graphics.off()

pdf("graphs/irs/irs_coef_small_pl.pdf", width = 12, height = 3)
plot_coef(out4, ylim = ylim, main = "Small Winner vs. Non-Winner: Placebo Test", main.pos = 3)
graphics.off()

#  $$$$$$\                                
# $$  __$$\                               
# $$ /  \__| $$$$$$\  $$$$$$$\   $$$$$$$\ 
# \$$$$$$\  $$  __$$\ $$  __$$\ $$  _____|
#  \____$$\ $$$$$$$$ |$$ |  $$ |\$$$$$$\  
# $$\   $$ |$$   ____|$$ |  $$ | \____$$\ 
# \$$$$$$  |\$$$$$$$\ $$ |  $$ |$$$$$$$  |
#  \______/  \_______|\__|  \__|\_______/ 

                                        

library(sensemakr)

Y <- "yearn.avg"
treat <- "tr"


covar <- c("tixbot", "male", "workthen", "agew", "educ", "college", 
    "x1", "x2", "x3", "x4", "x5", "earnings_1yr_before", "yearw")


fml <- as.formula(paste(Y, "~", treat, "+", paste(covar, collapse = "+")))
mod1 <- lm(fml, data = s1)
mod2 <- lm(fml, data = s2)
summary(mod1)

bm <- c("earnings_1yr_before")
sens1 <- sensemakr(model = mod1, treatment = treat, benchmark_covariates = bm, kd = 1:3, sensitivity.of = "t-value")
sens2 <- sensemakr(model = mod2, treatment = treat, benchmark_covariates = bm, kd = 1:3, sensitivity.of = "t-value")

pdf("graphs/irs/irs1_sens.pdf", width = 7, height = 7)
plot(sens1)
graphics.off()

pdf("graphs/irs/irs2_sens.pdf", width = 7, height = 7)
plot(sens2)
graphics.off()


