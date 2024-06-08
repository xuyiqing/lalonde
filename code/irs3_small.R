# LaLonde (1986) after Nearly Four Decades: Lessons Learned
# Guido Imbens & Yiqing Xu
# 2024

## Imbens-Rubin-Sacerdote Lottery Data
## Estimating the Effects of Small Prizes on Labor Earnings

rm(list = ls())
source("code/functions_est.R")
source("code/functions_plot.R")

# library("R.matlab")
# d <- as.data.frame(readMat("data/lottery.mat"))
# save(d, file = "data/lottery.RData")

#             $$\                          $$\     
#             $$ |                         $$ |    
#  $$$$$$$\ $$$$$$\    $$$$$$\   $$$$$$\ $$$$$$\   
# $$  _____|\_$$  _|   \____$$\ $$  __$$\\_$$  _|  
# \$$$$$$\    $$ |     $$$$$$$ |$$ |  \__| $$ |    
#  \____$$\   $$ |$$\ $$  __$$ |$$ |       $$ |$$\ 
# $$$$$$$  |  \$$$$  |\$$$$$$$ |$$ |       \$$$$  |
# \_______/    \____/  \_______|\__|        \____/ 



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

table(d$tr1, d$tr2)
table(d$tr1, d$co)
table(d$tr2, d$co)
#s <- subset(d, tr1 == 1 | co == 1)
s <- subset(d, tr2 == 1 | co == 1)

s$xearn.avg <- apply(s[, paste0("xearn.", 4:6)], 1, mean) # avg pre outcome
s$yearn.avg <- apply(s[, paste0("yearn.", 1:7)], 1, mean) # avg pst outcome

table(s$tr)
                                                                                                                                                                                                                                                                                                                                                                                                 
covar <- c("tixbot", "male", "workthen", "agew", "educ", "college", 
    "xearn.1", "xearn.2", "xearn.3", "yearw")
treat <- "tr"


s$ps <- probability_forest(X = s[, covar], 
    Y = as.factor(s$tr), seed = 1234)$predictions[,2]
                                                                                                                                                                                                                                                                                                                                                                                                                                
match.out <- matchit(tr ~ ps, ratio = 1, data = s, replace = FALSE)
s2 <- s[which(match.out$weights == 1), ]
table(s2$tr)

#                                         $$\                                                                                                                                                                                                                                                                                                                           $$\                      $$\                                     
#                                         $$ |                                                                                                                                                                                                                                                                                                                          \$$\                     $$ |                                    
#  $$$$$$\ $$\    $$\  $$$$$$\   $$$$$$\  $$ | $$$$$$\   $$$$$$\                                                                                                                                                                                                                                                                                                         \$$\                    $$ |                                    
# $$  __$$\\$$\  $$  |$$  __$$\ $$  __$$\ $$ | \____$$\ $$  __$$\                                                                                                                                                                                                                                                                                                         \$$\                   \__|                                    
# $$ /  $$ |\$$\$$  / $$$$$$$$ |$$ |  \__|$$ | $$$$$$$ |$$ /  $$ |                                                                                                                                                                                                                                                                                                         \$$\                  $$\                                     
# $$ |  $$ | \$$$  /  $$   ____|$$ |      $$ |$$  __$$ |$$ |  $$ |                                                                                                                                                                                                                                                                                                          \$$\                 $$ |                                    
# \$$$$$$  |  \$  /   \$$$$$$$\ $$ |      $$ |\$$$$$$$ |$$$$$$$  |                                                                                                                                                                                                                                                                                                           \$$\                $$ |                                    
#  \______/    \_/     \_______|\__|      \__| \_______|$$  ____/                                                                                                                                                                                                                                                                                                             \__|$$$$$$\ $$$$$$\\__|                                    
#                                                       $$ |                                                                                                                                                                                                                                                                                                                      \______|\______|                                       
#                                                       $$ |                                                                                                                                                                                                                                                                                                                                                                             
#                                                       \__|                                                                                                                                                                                                                                                                                                                                                                             
          


pdf("graphs/irs/irs2_odds.pdf", width = 5.5, height = 5.5)
plot_hist(s, "ps", "tr", breaks = 30, odds = TRUE, xlim = c(-3, 3), ylim = c(-0.2, 0.2))
graphics.off()


pdf("graphs/irs/irs2_odds_trim.pdf", width = 5.5, height = 5.5)
s2$ps <- probability_forest(X = s2[, covar], 
    Y = as.factor(s2$tr), seed = 1234)$predictions[,2]
plot_hist(s2, "ps", "tr", breaks = 30, odds = TRUE, xlim = c(-3, 3), ylim = c(-0.2, 0.2))
graphics.off()


## PS

pdf("graphs/irs/irs2_ps.pdf", width = 5.5, height = 5.5)
s$ps <- probability_forest(X = s[, covar], 
    Y = as.factor(s$tr), seed = 1234)$predictions[,2]
plot_hist(s, "ps", "tr", breaks = 30, odds = FALSE, xlim = c(0, 1), ylim = c(-0.2, 0.2))
graphics.off()

pdf("graphs/irs/irs2_ps_trim.pdf", width = 5.5, height = 5.5)
s2$ps <- probability_forest(X = s2[, covar], 
    Y = as.factor(s2$tr), seed = 1234)$predictions[,2]
plot_hist(s2, "ps", "tr", breaks = 30, odds = FALSE, xlim = c(0, 1), ylim = c(-0.2, 0.2))
graphics.off()



#                       $$\     $$\                          $$\               
#                       $$ |    \__|                         $$ |              
#  $$$$$$\   $$$$$$$\ $$$$$$\   $$\ $$$$$$\$$$$\   $$$$$$\ $$$$$$\    $$$$$$\  
# $$  __$$\ $$  _____|\_$$  _|  $$ |$$  _$$  _$$\  \____$$\\_$$  _|  $$  __$$\ 
# $$$$$$$$ |\$$$$$$\    $$ |    $$ |$$ / $$ / $$ | $$$$$$$ | $$ |    $$$$$$$$ |
# $$   ____| \____$$\   $$ |$$\ $$ |$$ | $$ | $$ |$$  __$$ | $$ |$$\ $$   ____|
# \$$$$$$$\ $$$$$$$  |  \$$$$  |$$ |$$ | $$ | $$ |\$$$$$$$ | \$$$$  |\$$$$$$$\ 
#  \_______|\_______/    \____/ \__|\__| \__| \__| \_______|  \____/  \_______|
                                                                             


# full dataset
outcomes <- c(paste0("xearn.", 1:6), paste0("yearn.", 1:7))
est <- vector("list", length(outcomes))
names(est) <- outcomes
for (i in 1:length(outcomes)) {
    est[[i]] <- estimate_all(s, outcomes[i], "tr", covar,
    methods = c("diff", "aipw_grf"))
    cat(i, "\n")
}
# matched dataset
est2 <- vector("list", length(outcomes))
names(est2) <- outcomes
for (i in 1:length(outcomes)) {
    est2[[i]] <- estimate_all(s2, outcomes[i], "tr", covar,
    methods = c("diff", "aipw_grf"))
    cat(i, "\n")
}

pdf("graphs/irs/irs2_dyn.pdf", width = 7, height = 5)
par(mar = c(4, 4, 1, 2))
plot(1, xlim = c(3.7, 13.3), ylim = c(-20, 10), type = "n", axes = FALSE, 
    ylab = "Effects on Earnings (thousand USD)", xlab = "Year Relative to Winning")
box(); axis(2)
axis(1, at = 4:13, labels = c(-3:6))
abline(h = 0, v= 6.5, col = "gray60", lty = 2, lwd = 2)
for (i in 4:13) {
    # full dataset with DIM
    lines(c(i-0.075, i-0.075), est[[i]][1,3:4], lty = 1, lwd = 2, col = "grey60") # CI
    points(i-0.075, est[[i]][1,1], pch = 18, col = "grey60", cex = 1.2)  # Coef 
    # full dataset
    lines(c(i+0.075, i+0.075), est[[i]][2,3:4], lwd = 2) # CI
    points(i+0.075, est[[i]][2,1], pch = 16)  # Coef     
}
legend("topright", legend = c("DIM", "AIPW"), lwd = 2, cex = 1.2,
    lty = 1, pch = c(18, 16), col = c("grey60", "black"), bty = "n")
graphics.off()

pdf("graphs/irs/irs2_dyn2.pdf", width = 7, height = 5)
par(mar = c(4, 4, 1, 2))
plot(1, xlim = c(3.7, 13.3), ylim = c(-20, 10), type = "n", axes = FALSE, 
    ylab = "Effects on Earnings (thousand USD)", xlab = "Year Relative to Winning")
box(); axis(2)
axis(1, at = 4:13, labels = c(-3:6))
abline(h = 0, v= 6.5, col = "gray60", lty = 2, lwd = 2)
for (i in 4:13) {
    # full dataset with DIM
    lines(c(i-0.15, i-0.15), est[[i]][1,3:4], lty = 1, lwd = 2, col = "grey60") # CI
    points(i-0.15, est[[i]][1,1], pch = 18, col = "grey60", cex = 1.2)  # Coef 
    # full dataset
    lines(c(i, i), est[[i]][2,3:4], lwd = 2) # CI
    points(i, est[[i]][2,1], pch = 16)  # Coef 
    # matched dataset
    lines(c(i+0.15, i+0.15), est2[[i]][2,3:4], col = "maroon", lwd = 1.5) # CI
    points(i+0.15, est2[[i]][2,1], col = "maroon", pch = 17)  # Coef  
}
legend("topright", legend = c("DIM,  Full (194: 259)", "AIPW, Full (194: 259)", 
    "AIPW, PS Matched (194: 194)"), lwd = 2,
    lty = c(1, 1, 1), pch = c(18, 16, 17), 
    col = c("grey50", "black", "maroon"), bty = "n")
graphics.off()

#                     $$\     $$\     
#                     $$ |    $$ |    
#  $$$$$$$\ $$$$$$\ $$$$$$\ $$$$$$\   
# $$  _____|\____$$\\_$$  _|\_$$  _|  
# $$ /      $$$$$$$ | $$ |    $$ |    
# $$ |     $$  __$$ | $$ |$$\ $$ |$$\ 
# \$$$$$$$\\$$$$$$$ | \$$$$  |\$$$$  |
#  \_______|\_______|  \____/  \____/ 
                                    

library("grf")

## Original Data
data <- s
treat <- "tr"
ntr <- sum(data[, treat] == 1)
tau <- matrix(NA, ntr, length(outcomes))
att <- rep(NA, ntr)
for (i in 1:length(outcomes)) {
    Y <- outcomes[i]
    catt.out <- catt(data, Y, treat, covar)
    tau[, i] <- catt.out$catt
    att[i] <- catt.out$att[1]     
    cat(i, "\n")
}

pdf("graphs/irs/irs2_catt.pdf", width = 7, height = 5)
par(mar = c(4, 4, 1, 2))
plot(1, xlim = c(3.7, 13.3), ylim = c(-20, 10), type = "n", axes = FALSE, 
    ylab = "Effects on Earnings (thousand USD)", xlab = "Year Relative to Winning")
box(); axis(2)
axis(1, at = 4:13, labels = c(-3:6))
abline(h = 0, v= 6.5, col = "gray60", lty = 2, lwd = 1.5)
for (i in 4:length(outcomes)) {
    dens <- density(tau[,i], bw = 0.5)
    polygon(i + dens$y, dens$x, col = "#AAAAAA50", border = NA)
    lines(i + dens$y, dens$x, lwd = 1) 
    points(i+0.01,  att[i], pch = 16, cex = 0.8)  # Coef
}
graphics.off()



## Trimmed Data
data <- s2
treat <- "tr"
ntr <- sum(data[, treat] == 1)
tau <- matrix(NA, ntr, length(outcomes))
att <- rep(NA, ntr)
for (i in 1:length(outcomes)) {
    Y <- outcomes[i]
    catt.out <- catt(data, Y, treat, covar)
    tau[, i] <- catt.out$catt
    att[i] <- catt.out$att[1]     
    cat(i, "\n")
}

pdf("graphs/irs/irs2_catt_trim.pdf", width = 7, height = 5)
par(mar = c(4, 4, 1, 2))
plot(1, xlim = c(3.7, 13.3), ylim = c(-20, 10), type = "n", axes = FALSE, 
    ylab = "Effects on Earnings (thousand USD)", xlab = "Year Relative to Winning")
box(); axis(2)
axis(1, at = 4:13, labels = c(-3:6))
abline(h = 0, v= 6.5, col = "gray60", lty = 2, lwd = 1.5)
for (i in 4:length(outcomes)) {
    dens <- density(tau[,i], bw = 0.5)
    polygon(i + dens$y, dens$x, col = "#AAAAAA50", border = NA)
    lines(i + dens$y, dens$x, lwd = 1) 
    points(i+0.01,  att[i], pch = 16, cex = 0.8)  # Coef
}
graphics.off()


#                                          $$\     $$\ $$\           
#                                          $$ |    \__|$$ |          
#  $$$$$$\  $$\   $$\  $$$$$$\  $$$$$$$\ $$$$$$\   $$\ $$ | $$$$$$\  
# $$  __$$\ $$ |  $$ | \____$$\ $$  __$$\\_$$  _|  $$ |$$ |$$  __$$\ 
# $$ /  $$ |$$ |  $$ | $$$$$$$ |$$ |  $$ | $$ |    $$ |$$ |$$$$$$$$ |
# $$ |  $$ |$$ |  $$ |$$  __$$ |$$ |  $$ | $$ |$$\ $$ |$$ |$$   ____|
# \$$$$$$$ |\$$$$$$  |\$$$$$$$ |$$ |  $$ | \$$$$  |$$ |$$ |\$$$$$$$\ 
#  \____$$ | \______/  \_______|\__|  \__|  \____/ \__|\__| \_______|
#       $$ |                                                         
#       $$ |                                                         
#       \__|                                                         


covar <- c("tixbot", "male", "workthen", "agew", "educ", "college", 
    "xearn.1", "xearn.2", "xearn.3", "yearw")

#hist(s2$xearn.avg, breaks = 50)
#hist(s2$yearn.avg, breaks = 50)

library(qte)

qte.irs.pre.unadj <- est_qte("xearn.avg", treat, NULL, data = s)
qte.irs.pst.unadj <- est_qte("yearn.avg", treat, NULL, data = s)
qte.irs.pre.trim.unadj <- est_qte("xearn.avg", treat, NULL, data = s2)
qte.irs.pst.trim.unadj <- est_qte("yearn.avg", treat, NULL, data = s2)

qte.irs.pre.adj <- est_qte("xearn.avg", treat, covar, data = s)
qte.irs.pst.adj <- est_qte("yearn.avg", treat, covar, data = s)
qte.irs.pre.trim.adj <- est_qte("xearn.avg", treat, covar, data = s2)
qte.irs.pst.trim.adj <- est_qte("yearn.avg", treat,covar, data = s2)

save(qte.irs.pre.unadj, qte.irs.pst.unadj, qte.irs.pre.trim.unadj, qte.irs.pst.trim.unadj,
    qte.irs.pre.adj, qte.irs.pst.adj, qte.irs.pre.trim.adj, qte.irs.pst.trim.adj,
    file = "output/qte_irs2.rds")

####################
# Plot QTE
####################

load("output/qte_irs2.rds")

pdf("graphs/irs/irs2_qte_pre.pdf", width = 6, height = 5)
ylim <- c(-60, 60)
plot_qte(qte.irs.pre.adj, qte.irs.pre.unadj, ylim = ylim, main = "Pre-Winning Avg. Earning")
legend("bottomleft", legend = c("Unadjusted", "Adjusted"), 
    lty = 1, pch = c(17, 16), col = c(2, 1), bty = "n")
graphics.off()

pdf("graphs/irs/irs2_qte_pst.pdf", width = 6, height = 5)
plot_qte(qte.irs.pst.adj, qte.irs.pst.unadj, ylim = ylim, main = "Post-Winning Avg. Earning")
legend("bottomleft", legend = c("Unadjusted", "Adjusted"), 
    lty = 1, pch = c(17, 16), col = c(2, 1), bty = "n")
graphics.off()


pdf("graphs/irs/irs2_qte_pre_trim.pdf", width = 6, height = 5)
plot_qte(qte.irs.pre.trim.adj, qte.irs.pre.unadj, ylim = ylim, main = "Pre-Winning Avg. Earning")
legend("bottomleft", legend = c("Unadjusted", "Adjusted"), 
    lty = 1, pch = c(17, 16), col = c(2, 1), bty = "n")
graphics.off()

pdf("graphs/irs/irs2_qte_pst_trim.pdf", width = 6, height = 5)
plot_qte(qte.irs.pst.trim.adj, qte.irs.pst.unadj, ylim = ylim, main = "Post-Winning Avg. Earning")
legend("bottomleft", legend = c("Unadjusted", "Adjusted"), 
    lty = 1, pch = c(17, 16), col = c(2, 1), bty = "n")
graphics.off()






