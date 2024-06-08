# LaLonde (1986) after Nearly Four Decades: Lessons Learned
# Guido Imbens & Yiqing Xu
# 2024

## Estimate the ATT using different samples & covariates
### LDW with 74 info
### LDW without 74 info
### LDW using re75 as placebo
### NSW (LaLdonde Male)

rm(list = ls())
source("code/functions_est.R")
source("code/functions_plot.R")

load("data/lalonde.RData")
load("data/trimmed.RData")

data <- ldw_cps
Y <- "re78"
treat <- "treat"
covar <- c("age", "education", "black", "hispanic", "married", "nodegree", "re74", "re75", "u74", "u75")

set.seed(1234)
# experimental
out1 <- estimate_all(ldw, "re78", "treat", covar)
out2 <- estimate_all(ldw_trim_cps, "re78", "treat", covar)
out3 <- estimate_all(ldw_trim_psid, "re78", "treat", covar)
# no experimental
out4 <- estimate_all(ldw_cps, "re78", "treat", covar)
out5 <- estimate_all(ldw_psid, "re78", "treat", covar)
out6 <- estimate_all(ldw_cps_trim, "re78", "treat", covar)
out7 <- estimate_all(ldw_psid_trim, "re78", "treat", covar)

save(out1, out2, out3, out4, out5, out6, out7, file = "output/est_ldw.RData")

######################
## produce table
######################

load("output/est_ldw.RData")
a <- list(out4, out5, out6, out7)
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
sav[1, 1] <- sav[1, 4] <- sprintf("%.0f", out1[1, 1]) # full experimental
sav[1, 7] <- sprintf("%.0f", out2[1, 1]) # trimmed experimental (CPS)
sav[1, 10] <- sprintf("%.0f", out3[1, 1]) # trimmed experimental (PSID)
sav[1, 2] <- sav[1, 5] <- paste0("(", sprintf("%.0f", out1[1, 2]), ")")    
sav[1, 8] <- paste0("(", sprintf("%.0f", out2[1, 2]), ")")    
sav[1, 11] <- paste0("(", sprintf("%.0f", out3[1, 2]), ")")   
print(sav)

write.csv(sav, file = "tables/ldw.csv", row.names = FALSE)

######################
## plot coefficients
######################


load("output/est_ldw.RData")

pdf("graphs/lalonde/coef_ldw_cps.pdf", width = 12, height = 3)
band <- out1[1, 3:4]
est <- out1[1, 1]
plot_coef(out4, band = band, line = est, ylim = c(-15500, 5500), main = "(A) LDW-CPS")
graphics.off()

pdf("graphs/lalonde/coef_ldw_psid.pdf", width = 12, height = 3)
band <- out1[1, 3:4]
est <- out1[1, 1]
plot_coef(out5, band = band, line = est, ylim = c(-15500, 5500), main = "(B) LDW-PSID")
graphics.off()

pdf("graphs/lalonde/coef_ldw_cps_trim.pdf", width = 12, height = 3)
band <- out2[1, 3:4]
est <- out2[1, 1]
plot_coef(out6, band = band, line = est, ylim = c(-15500, 5500), main = "(C) Trimmed LDW-CPS")
graphics.off()


pdf("graphs/lalonde/coef_ldw_psid_trim.pdf", width = 12, height = 3)
band <- out3[1, 3:4]
est <- out3[1, 1]
plot_coef(out7, band = band, line = est, ylim = c(-15500, 5500), main = "(D) Trimmed LDW-PSID")
graphics.off()


# $$\   $$\           $$$$$$$$\ $$\   $$\ 
# $$$\  $$ |          \____$$  |$$ |  $$ |
# $$$$\ $$ | $$$$$$\      $$  / $$ |  $$ |
# $$ $$\$$ |$$  __$$\    $$  /  $$$$$$$$ |
# $$ \$$$$ |$$ /  $$ |  $$  /   \_____$$ |
# $$ |\$$$ |$$ |  $$ | $$  /          $$ |
# $$ | \$$ |\$$$$$$  |$$  /           $$ |
# \__|  \__| \______/ \__/            \__|
                                        
                                        

Y <- "re78"
treat <- "treat"
covar <- c("age", "education", "black", "hispanic", "married", "nodegree", "re75", "u75")

set.seed(1234)

# experimental
out1 <- estimate_all(ldw, "re78", "treat", covar)
out2 <- estimate_all(ldw_trim_cps_no74, "re78", "treat", covar)
out3 <- estimate_all(ldw_trim_psid_no74, "re78", "treat", covar)
# no experimental
out4 <- estimate_all(ldw_cps, "re78", "treat", covar)
out5 <- estimate_all(ldw_psid, "re78", "treat", covar)
out6 <- estimate_all(ldw_cps_trim_no74, "re78", "treat", covar)
out7 <- estimate_all(ldw_psid_trim_no74, "re78", "treat", covar)

save(out1, out2, out3, out4, out5, out6, out7, file = "output/est_ldw_no74.RData")

######################
## produce table
######################

load("output/est_ldw_no74.RData")
a <- list(out4, out5, out6, out7)
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
sav[1, 1] <- sav[1, 4] <- sprintf("%.0f", out1[1, 1]) # full experimental
sav[1, 7] <- sprintf("%.0f", out2[1, 1]) # trimmed experimental (CPS)
sav[1, 10] <- sprintf("%.0f", out3[1, 1]) # trimmed experimental (PSID)
sav[1, 2] <- sav[1, 5] <- paste0("(", sprintf("%.0f", out1[1, 2]), ")")    
sav[1, 8] <- paste0("(", sprintf("%.0f", out2[1, 2]), ")")    
sav[1, 11] <- paste0("(", sprintf("%.0f", out3[1, 2]), ")")   
print(sav)

write.csv(sav, file = "tables/ldw_no74.csv", row.names = FALSE)
                       
                                        


# $$$$$$$\  $$\                               $$\                 
# $$  __$$\ $$ |                              $$ |                
# $$ |  $$ |$$ | $$$$$$\   $$$$$$$\  $$$$$$\  $$$$$$$\   $$$$$$\  
# $$$$$$$  |$$ | \____$$\ $$  _____|$$  __$$\ $$  __$$\ $$  __$$\ 
# $$  ____/ $$ | $$$$$$$ |$$ /      $$$$$$$$ |$$ |  $$ |$$ /  $$ |
# $$ |      $$ |$$  __$$ |$$ |      $$   ____|$$ |  $$ |$$ |  $$ |
# $$ |      $$ |\$$$$$$$ |\$$$$$$$\ \$$$$$$$\ $$$$$$$  |\$$$$$$  |
# \__|      \__| \_______| \_______| \_______|\_______/  \______/ 

Y <- "re75"                                                                
treat <- "treat"
covar <- c("age", "education", "black", "hispanic", "married", "nodegree", "re74", "u74")
                                                                
set.seed(1234)
# experimental
out1 <- estimate_all(ldw, Y, "treat", covar)
out2 <- estimate_all(ldw_trim_cps_pl, Y, "treat", covar)
out3 <- estimate_all(ldw_trim_psid_pl, Y, "treat", covar)
# no experimental
out4 <- estimate_all(ldw_cps, Y, "treat", covar)
out5 <- estimate_all(ldw_psid, Y, "treat", covar)
out6 <- estimate_all(ldw_cps_trim_pl, Y, "treat", covar)
out7 <- estimate_all(ldw_psid_trim_pl, Y, "treat", covar)

save(out1, out2, out3, out4, out5, out6, out7, file = "output/est_ldw_placebo.RData")

######################
## produce table
######################

load("output/est_ldw_placebo.RData")
a <- list(out4, out5, out6, out7)
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
sav[1, 1] <- sav[1, 4] <- sprintf("%.0f", out1[1, 1]) # full experimental
sav[1, 7] <- sprintf("%.0f", out2[1, 1]) # trimmed experimental (CPS)
sav[1, 10] <- sprintf("%.0f", out3[1, 1]) # trimmed experimental (PSID)
sav[1, 2] <- sav[1, 5] <- paste0("(", sprintf("%.0f", out1[1, 2]), ")")    
sav[1, 8] <- paste0("(", sprintf("%.0f", out2[1, 2]), ")")    
sav[1, 11] <- paste0("(", sprintf("%.0f", out3[1, 2]), ")")   
print(sav)
write.csv(sav, file = "tables/ldw_placebo.csv", row.names = FALSE)

#########################
## Plot coefficients
#########################

load("output/est_ldw_placebo.RData")
ylim <- c(-12000, 2000)

pdf("graphs/lalonde/coef_ldwp_cps.pdf", width = 12, height = 3)
band <- out1[1, 3:4]
est <- out1[1, 1]
plot_coef(out4, band = band, line = est, ylim = ylim, main = "(A) LDW-CPS")
graphics.off()

pdf("graphs/lalonde/coef_ldwp_psid.pdf", width = 12, height = 3)
band <- out1[1, 3:4]
est <- out1[1, 1]
plot_coef(out5, band = band, line = est, ylim = ylim, main = "(B) LDW-PSID")
graphics.off()


pdf("graphs/lalonde/coef_ldwp_cps_trim.pdf", width = 12, height = 3)
band <- out2[1, 3:4]
est <- out2[1, 1]
plot_coef(out6, band = band, line = est, ylim = ylim, main = "(C) Trimmed LDW-CPS")
graphics.off()


pdf("graphs/lalonde/coef_ldwp_psid_trim.pdf", width = 12, height = 3)
band <- out3[1, 3:4]
est <- out3[1, 1]
plot_coef(out7, band = band, line = est, ylim = ylim, main = "(D) Trimmed LDW-PSID")
graphics.off()

                                        

# $$\                $$\                                $$\                 $$\      $$\           $$\           
# $$ |               $$ |                               $$ |                $$$\    $$$ |          $$ |          
# $$ |      $$$$$$\  $$ |      $$$$$$\  $$$$$$$\   $$$$$$$ | $$$$$$\        $$$$\  $$$$ | $$$$$$\  $$ | $$$$$$\  
# $$ |      \____$$\ $$ |     $$  __$$\ $$  __$$\ $$  __$$ |$$  __$$\       $$\$$\$$ $$ | \____$$\ $$ |$$  __$$\ 
# $$ |      $$$$$$$ |$$ |     $$ /  $$ |$$ |  $$ |$$ /  $$ |$$$$$$$$ |      $$ \$$$  $$ | $$$$$$$ |$$ |$$$$$$$$ |
# $$ |     $$  __$$ |$$ |     $$ |  $$ |$$ |  $$ |$$ |  $$ |$$   ____|      $$ |\$  /$$ |$$  __$$ |$$ |$$   ____|
# $$$$$$$$\\$$$$$$$ |$$$$$$$$\\$$$$$$  |$$ |  $$ |\$$$$$$$ |\$$$$$$$\       $$ | \_/ $$ |\$$$$$$$ |$$ |\$$$$$$$\ 
# \________|\_______|\________|\______/ \__|  \__| \_______| \_______|      \__|     \__| \_______|\__| \_______|
                                                                                                               


data <- nsw
Y <- "re78"
treat <- "treat"
covar <- c("age", "education", "black", "hispanic", "married", "nodegree", "re75", "u75")

set.seed(1234)
# experimental
out1 <- estimate_all(nsw, "re78", "treat", covar)
out2 <- estimate_all(nsw_trim_cps, "re78", "treat", covar)
out3 <- estimate_all(nsw_trim_psid, "re78", "treat", covar)
# no experimental
out4 <- estimate_all(nsw_cps, "re78", "treat", covar)
out5 <- estimate_all(nsw_psid, "re78", "treat", covar)
out6 <- estimate_all(nsw_cps_trim, "re78", "treat", covar)
out7 <- estimate_all(nsw_psid_trim, "re78", "treat", covar)

save(out1, out2, out3, out4, out5, out6, out7, file = "output/est_nsw.RData")

######################
## produce table
######################

load("output/est_nsw.RData")
# columns are samples
a <- list(out4, out5, out6, out7)
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
sav[1, 1] <- sav[1, 4] <- sprintf("%.0f", out1[1, 1]) # full experimental
sav[1, 7] <- sprintf("%.0f", out2[1, 1]) # trimmed experimental (CPS)
sav[1, 10] <- sprintf("%.0f", out3[1, 1]) # trimmed experimental (PSID)
sav[1, 2] <- sav[1, 5] <- paste0("(", sprintf("%.0f", out1[1, 2]), ")")    
sav[1, 8] <- paste0("(", sprintf("%.0f", out2[1, 2]), ")")    
sav[1, 11] <- paste0("(", sprintf("%.0f", out3[1, 2]), ")")   
print(sav)
write.csv(sav, file = "tables/nsw.csv", row.names = FALSE)


#########################
## Plot coefficients
#########################

load("output/est_nsw.RData")
ylim <- c(-15000, 5000)

pdf("graphs/lalonde/coef_nsw_cps.pdf", width = 12, height = 3)
band <- out1[1, 3:4]
est <- out1[1, 1]
plot_coef(out4, band = band, line = est, ylim = ylim, main = "(A) NSW-CPS")
graphics.off()

pdf("graphs/lalonde/coef_nsw_psid.pdf", width = 12, height = 3)
band <- out1[1, 3:4]
est <- out1[1, 1]
plot_coef(out5, band = band, line = est, ylim = ylim, main = "(B) NSW-PSID")
graphics.off()


pdf("graphs/lalonde/coef_nsw_cps_trim.pdf", width = 12, height = 3)
band <- out2[1, 3:4]
est <- out2[1, 1]
plot_coef(out6, band = band, line = est, ylim = ylim, main = "(C) Trimmed NSW-CPS")
graphics.off()


pdf("graphs/lalonde/coef_nsw_psid_trim.pdf", width = 12, height = 3)
band <- out3[1, 3:4]
est <- out3[1, 1]
plot_coef(out7, band = band, line = est, ylim = ylim, main = "(D) Trimmed NSW-PSID")
graphics.off()

gc() # free up memory