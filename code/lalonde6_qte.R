# LaLonde (1986) after Nearly Four Decades: Lessons Learned
# Guido Imbens & Yiqing Xu
# 2024

## Estimating and Visualizing Quantile Treatment Effects

## Note: gc() is for garbage collection to free up memory

rm(list = ls())
source("code/functions_est.R")
source("code/functions_plot.R")

#                       $$\     $$\                          $$\               
#                       $$ |    \__|                         $$ |              
#  $$$$$$\   $$$$$$$\ $$$$$$\   $$\ $$$$$$\$$$$\   $$$$$$\ $$$$$$\    $$$$$$\  
# $$  __$$\ $$  _____|\_$$  _|  $$ |$$  _$$  _$$\  \____$$\\_$$  _|  $$  __$$\ 
# $$$$$$$$ |\$$$$$$\    $$ |    $$ |$$ / $$ / $$ | $$$$$$$ | $$ |    $$$$$$$$ |
# $$   ____| \____$$\   $$ |$$\ $$ |$$ | $$ | $$ |$$  __$$ | $$ |$$\ $$   ____|
# \$$$$$$$\ $$$$$$$  |  \$$$$  |$$ |$$ | $$ | $$ |\$$$$$$$ | \$$$$  |\$$$$$$$\ 
#  \_______|\_______/    \____/ \__|\__| \__| \__| \_______|  \____/  \_______|

load("data/lalonde.RData")
load("data/trimmed.RData")


Y <- "re78"
treat <- "treat"
covar <- c("age", "education", "black", "hispanic", "married", "nodegree", 
    "re74", "re75", "u74", "u75")

## experimental
qte.ldw <- est_qte(Y, treat, NULL, data = ldw)
qte.ldw.cps <- est_qte(Y, treat, NULL, data = ldw_trim_cps)
qte.ldw.psid <- est_qte(Y, treat, NULL, data = ldw_trim_psid); gc()
## non-experimental
qte.ldw_cps <- est_qte(Y, treat, covar, data = ldw_cps); gc() # adjusted
qte.ldw_cps0 <- est_qte(Y, treat, NULL, data = ldw_cps); gc() # unadjusted
qte.ldw_cps.trim <- est_qte(Y, treat, covar, data = ldw_cps_trim) # adjusted
qte.ldw_cps.trim0 <- est_qte(Y, treat, NULL, data = ldw_cps_trim) # unadjusted
qte.ldw_psid <- est_qte(Y, treat, covar, data = ldw_psid); gc() # adjusted
qte.ldw_psid0 <- est_qte(Y, treat, NULL, data = ldw_psid); gc() # unadjusted
qte.ldw_psid.trim <- est_qte(Y, treat, covar, data = ldw_psid_trim) # adjusted
qte.ldw_psid.trim0 <- est_qte(Y, treat, NULL, data = ldw_psid_trim) # unadjusted

save(qte.ldw, qte.ldw.cps, qte.ldw.psid, 
    qte.ldw_cps, qte.ldw_cps0, 
    qte.ldw_cps.trim, qte.ldw_cps.trim0, 
    qte.ldw_psid, qte.ldw_psid0, 
    qte.ldw_psid.trim, qte.ldw_psid.trim0, 
    file = "output/qte_ldw.rds")

gc() # free up memory

############################
## plotting 
############################

load("output/qte_ldw.rds")

                           
pdf("graphs/lalonde/qte_ldw.pdf", width = 6.5, height = 5)
plot_qte(qte.ldw, main = "Experimental", ylim = c(-5000, 10000),
    col = c(4, "#ADD8E670"))
graphics.off()

# CPS
pdf("graphs/lalonde/qte_ldw_cps.pdf", width = 6.5, height = 5)
plot_qte(qte.ldw_cps, qte.ldw_cps0, qte.ldw, main = "LDW-CPS", ylim = c(-25000, 15000))
legend("bottomleft", legend = c("Experimental", "Unadjusted", "Adjusted"), 
    lty = 1, pch = c(16, 17, 16), col = c(4, 2, 1), bty = "n")
graphics.off()

# CPS trimmed
pdf("graphs/lalonde/qte_ldw_cps_trim.pdf", width = 6.5, height = 5)
plot_qte(qte.ldw_cps.trim, qte.ldw_cps.trim0, qte.ldw.cps, main = "LDW-CPS (Trimmed)", ylim = c(-25000, 15000))
legend("bottomleft", legend = c("Experimental", "Unadjusted", "Adjusted"), 
    lty = 1, pch = c(16, 17, 16), col = c(4, 2, 1), bty = "n")
graphics.off()

# PSID
pdf("graphs/lalonde/qte_ldw_psid.pdf", width = 6.5, height = 5)
plot_qte(qte.ldw_psid, qte.ldw_psid0, qte.ldw, main = "LDW-PSID", ylim = c(-25000, 15000))
legend("bottomleft", legend = c("Experimental", "Unadjusted", "Adjusted"), 
    lty = 1, pch = c(16, 17, 16), col = c(4, 2, 1), bty = "n")
graphics.off()

# PSID trimmed
pdf("graphs/lalonde/qte_ldw_psid_trim.pdf", width = 6.5, height = 5)
plot_qte(qte.ldw_psid.trim, qte.ldw_psid.trim0, qte.ldw.psid, main = "LDW-PSID (Trimmed)", ylim = c(-25000, 15000))
legend("bottomleft", legend = c("Experimental", "Unadjusted", "Adjusted"), 
    lty = 1, pch = c(16, 17, 16), col = c(4, 2, 1), bty = "n")
graphics.off()


# $$\                $$\                                $$\                 $$\      $$\           $$\           
# $$ |               $$ |                               $$ |                $$$\    $$$ |          $$ |          
# $$ |      $$$$$$\  $$ |      $$$$$$\  $$$$$$$\   $$$$$$$ | $$$$$$\        $$$$\  $$$$ | $$$$$$\  $$ | $$$$$$\  
# $$ |      \____$$\ $$ |     $$  __$$\ $$  __$$\ $$  __$$ |$$  __$$\       $$\$$\$$ $$ | \____$$\ $$ |$$  __$$\ 
# $$ |      $$$$$$$ |$$ |     $$ /  $$ |$$ |  $$ |$$ /  $$ |$$$$$$$$ |      $$ \$$$  $$ | $$$$$$$ |$$ |$$$$$$$$ |
# $$ |     $$  __$$ |$$ |     $$ |  $$ |$$ |  $$ |$$ |  $$ |$$   ____|      $$ |\$  /$$ |$$  __$$ |$$ |$$   ____|
# $$$$$$$$\\$$$$$$$ |$$$$$$$$\\$$$$$$  |$$ |  $$ |\$$$$$$$ |\$$$$$$$\       $$ | \_/ $$ |\$$$$$$$ |$$ |\$$$$$$$\ 
# \________|\_______|\________|\______/ \__|  \__| \_______| \_______|      \__|     \__| \_______|\__| \_______|
                                                                                                               


Y <- "re78"
treat <- "treat"
covar <- c("age", "education", "black", "hispanic", "married", "nodegree", "re75", "u75")

# experimental
qte.nsw <- est_qte(Y, treat, NULL, data = nsw)
qte.nsw.cps <- est_qte(Y, treat, NULL, data = nsw_trim_cps)
qte.nsw.psid <- est_qte(Y, treat, NULL, data = nsw_trim_psid)
# non-experimental
qte.nsw_cps <- est_qte(Y, treat, covar, data = nsw_cps); gc() # adjusted
qte.nsw_cps0 <- est_qte(Y, treat, NULL, data = nsw_cps); gc() # unadjusted
qte.nsw_cps.trim <- est_qte(Y, treat, covar, data = nsw_cps_trim) # adjusted
qte.nsw_cps.trim0 <- est_qte(Y, treat, NULL, data = nsw_cps_trim) # unadjusted
qte.nsw_psid <- est_qte(Y, treat, covar, data = nsw_psid); gc() # adjusted
qte.nsw_psid0 <- est_qte(Y, treat, NULL, data = nsw_psid); gc() # unadjusted
qte.nsw_psid.trim <- est_qte(Y, treat, covar, data = nsw_psid_trim) # adjusted
qte.nsw_psid.trim0 <- est_qte(Y, treat, NULL, data = nsw_psid_trim) # unadjusted

save(qte.nsw, qte.nsw.cps, qte.nsw.psid, 
    qte.nsw_cps, qte.nsw_cps0, 
    qte.nsw_cps.trim, qte.nsw_cps.trim0, 
    qte.nsw_psid, qte.nsw_psid0, 
    qte.nsw_psid.trim, qte.nsw_psid.trim0, 
    file = "output/qte_nsw.rds")

gc() # free up memory

############################
## plotting 
############################

load("output/qte_nsw.rds")

pdf("graphs/lalonde/qte_nsw_exp.pdf", width = 6.5, height = 5)
plot_qte(qte.nsw, main = "NSW Experimental", ylim = c(-5000, 5000),
    col = c(4, "#ADD8E670"))
graphics.off()

# CPS
pdf("graphs/lalonde/qte_nsw_cps.pdf", width = 6.5, height = 5)
plot_qte(qte.nsw_cps, qte.nsw_cps0, qte.nsw, main = "NSW-CPS", ylim = c(-30000, 12000))
legend("bottomleft", legend = c("Experimental", "Unadjusted", "Adjusted"), 
    lty = 1, pch = c(16, 17, 16), col = c(4, 2, 1), bty = "n")
graphics.off()

# CPS trimmed
pdf("graphs/lalonde/qte_nsw_cps_trim.pdf", width = 6.5, height = 5)
plot_qte(qte.nsw_cps.trim, qte.nsw_cps.trim0, qte.nsw.cps, main = "NSW-CPS (Trimmed)", ylim = c(-30000, 12000))
legend("bottomleft", legend = c("Experimental", "Unadjusted", "Adjusted"), 
    lty = 1, pch = c(16, 17, 16), col = c(4, 2, 1), bty = "n")
graphics.off()

# PSID
pdf("graphs/lalonde/qte_nsw_psid.pdf", width = 6.5, height = 5)
plot_qte(qte.nsw_psid, qte.nsw_psid0, qte.nsw, main = "NSW-PSID", ylim = c(-30000, 12000))
legend("bottomleft", legend = c("Experimental", "Unadjusted", "Adjusted"), 
    lty = 1, pch = c(16, 17, 16), col = c(4, 2, 1), bty = "n")
graphics.off()

# PSID trimmed
pdf("graphs/lalonde/qte_nsw_psid_trim.pdf", width = 6.5, height = 5)
plot_qte(qte.nsw_psid.trim, qte.nsw_psid.trim0, qte.nsw.psid, main = "NSW-PSID (Trimmed)", ylim = c(-30000, 12000))
legend("bottomleft", legend = c("Experimental", "Unadjusted", "Adjusted"), 
    lty = 1, pch = c(16, 17, 16), col = c(4, 2, 1), bty = "n")
graphics.off()