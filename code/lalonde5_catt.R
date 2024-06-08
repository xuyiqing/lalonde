# LaLonde (1986) after Nearly Four Decades: Lessons Learned
# Guido Imbens & Yiqing Xu
# 2024

## Estimating and Visualizing Conditional Average Treatment Effects

rm(list = ls())
source("code/functions_est.R")
source("code/functions_plot.R")

load("data/lalonde.RData")
load("data/trimmed.RData")

Y <- "re78"
treat <- "treat"
covar <- c("age", "education", "black", "hispanic", "married", "nodegree", "re74", "re75", "u74", "u75")

set.seed(1234)
catt.ldw <- catt(ldw, Y, treat, covar)
catt.cps <- catt(ldw_cps, Y, treat, covar)
catt.psid <- catt(ldw_psid, Y, treat, covar)
catt.cps.trim <- catt(ldw_cps_trim, Y, treat, covar)
catt.psid.trim <- catt(ldw_psid_trim, Y, treat, covar)
# trimmed experimental data
catt.ldw.cps <- catt(ldw_trim_cps, Y, treat, covar)
catt.ldw.psid <- catt(ldw_trim_psid, Y, treat, covar)

range(catt.ldw$catt)
range(catt.cps$catt)
range(catt.psid$catt)
 
pdf("graphs/lalonde/catt_cps.pdf", width = 5.5, height = 5.5)
par(mar = c(4, 4, 1, 1))
catt1 <- catt.ldw$catt
att1 <- catt.ldw$att[1]
catt2 <- catt.cps$catt
att2 <- catt.cps$att[1]
plot_catt(catt1, catt2, att1, att2, "CATT (Experimental)", "CATT (CPS-Full)",
    main = "", c(-8000, 8000))
graphics.off()

pdf("graphs/lalonde/catt_psid.pdf", width = 5.5, height = 5.5)
par(mar = c(4, 4, 1, 1))
catt1 <- catt.ldw$catt
att1 <- catt.ldw$att[1]
catt2 <- catt.psid$catt
att2 <- catt.psid$att[1]
plot_catt(catt1, catt2, att1, att2, "CATT (Experimental)", "CATT (PSID-Full)",
    main = "", c(-8000, 8000))
graphics.off()

## Trimmed sample

pdf("graphs/lalonde/catt_cps_trim.pdf", width = 5.5, height = 5.5)
par(mar = c(4, 4, 1, 1))
catt1 <- catt.ldw.cps$catt
att1 <- catt.ldw.cps$att[1]
catt2 <- catt.cps.trim$catt
att2 <- catt.cps.trim$att[1]
plot_catt(catt1, catt2, att1, att2, "CATT (Experimental)", "CATT (CPS-Trimmed)",
    main = "", c(-8000, 8000))
graphics.off()

pdf("graphs/lalonde/catt_psid_trim.pdf", width = 5.5, height = 5.5)
par(mar = c(4, 4, 1, 1))
catt1 <- catt.ldw.psid$catt
att1 <- catt.ldw.psid$att[1]
catt2 <- catt.psid.trim$catt
att2 <- catt.psid.trim$att[1]
plot_catt(catt1, catt2, att1, att2, "CATT (Experimental)", "CATT (PSID-Trimmed)",
    main = "", c(-8000, 8000))
graphics.off()

#                     $$$$$$$$\ $$\   $$\ 
#                     \____$$  |$$ |  $$ |
# $$$$$$$\   $$$$$$\      $$  / $$ |  $$ |
# $$  __$$\ $$  __$$\    $$  /  $$$$$$$$ |
# $$ |  $$ |$$ /  $$ |  $$  /   \_____$$ |
# $$ |  $$ |$$ |  $$ | $$  /          $$ |
# $$ |  $$ |\$$$$$$  |$$  /           $$ |
# \__|  \__| \______/ \__/            \__|
                                        

Y <- "re78"
treat <- "treat"
covar <- c("age", "education", "black", "hispanic", "married", "nodegree", "re75", "u75")

set.seed(1234)
catt.ldw <- catt(ldw, Y, treat, covar)
catt.cps <- catt(ldw_cps, Y, treat, covar)
catt.psid <- catt(ldw_psid, Y, treat, covar)
catt.cps.trim <- catt(ldw_cps_trim_no74, Y, treat, covar)
catt.psid.trim <- catt(ldw_psid_trim_no74, Y, treat, covar)
# trimmed experimental data
catt.ldw.cps <- catt(ldw_trim_cps_no74, Y, treat, covar)
catt.ldw.psid <- catt(ldw_trim_psid_no74, Y, treat, covar)


pdf("graphs/lalonde/catt_no74_cps.pdf", width = 5.5, height = 5.5)
par(mar = c(4, 4, 1, 1))
catt1 <- catt.ldw$catt
att1 <- catt.ldw$att[1]
catt2 <- catt.cps$catt
att2 <- catt.cps$att[1]
plot_catt(catt1, catt2, att1, att2, "CATT (Experimental)", "CATT (CPS-Full)",
    main = "", c(-8000, 8000))
graphics.off()

pdf("graphs/lalonde/catt_no74_psid.pdf", width = 5.5, height = 5.5)
par(mar = c(4, 4, 1, 1))
catt1 <- catt.ldw$catt
att1 <- catt.ldw$att[1]
catt2 <- catt.psid$catt
att2 <- catt.psid$att[1]
plot_catt(catt1, catt2, att1, att2, "CATT (Experimental)", "CATT (PSID-Full)",
    main = "", c(-8000, 8000))
graphics.off()

## Trimmed sample

pdf("graphs/lalonde/catt_no74_cps_trim.pdf", width = 5.5, height = 5.5)
par(mar = c(4, 4, 1, 1))
catt1 <- catt.ldw.cps$catt
att1 <- catt.ldw.cps$att[1]
catt2 <- catt.cps.trim$catt
att2 <- catt.cps.trim$att[1]
plot_catt(catt1, catt2, att1, att2, "CATT (Experimental)", "CATT (CPS-Trimmed)",
    main = "", c(-8000, 8000))
graphics.off()

pdf("graphs/lalonde/catt_no74_psid_trim.pdf", width = 5.5, height = 5.5)
par(mar = c(4, 4, 1, 1))
catt1 <- catt.ldw.psid$catt
att1 <- catt.ldw.psid$att[1]
catt2 <- catt.psid.trim$catt
att2 <- catt.psid.trim$att[1]
plot_catt(catt1, catt2, att1, att2, "CATT (Experimental)", "CATT (PSID-Trimmed)",
    main = "", c(-8000, 8000))
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

Y <- "re75"
treat <- "treat"
covar <- c("age", "education", "black", "hispanic", "married", "nodegree", "re74", "u74")

set.seed(1234)
catt.ldw <- catt(ldw, Y, treat, covar)
catt.cps <- catt(ldw_cps, Y, treat, covar)
catt.psid <- catt(ldw_psid, Y, treat, covar)
catt.cps.trim <- catt(ldw_cps_trim_pl, Y, treat, covar)
catt.psid.trim <- catt(ldw_psid_trim_pl, Y, treat, covar)
# trimmed experimental data
catt.ldw.cps <- catt(ldw_trim_cps_pl, Y, treat, covar)
catt.ldw.psid <- catt(ldw_trim_psid_pl, Y, treat, covar)


pdf("graphs/lalonde/catt_placebo_cps.pdf", width = 5.5, height = 5.5)
par(mar = c(4, 4, 1, 1))
catt1 <- catt.ldw$catt
att1 <- catt.ldw$att[1]
catt2 <- catt.cps$catt
att2 <- catt.cps$att[1]
plot_catt(catt1, catt2, att1, att2, "CATT (Experimental)", "CATT (CPS-Full)",
    main = "", c(-6000, 2000))
graphics.off()

pdf("graphs/lalonde/catt_placebo_psid.pdf", width = 5.5, height = 5.5)
par(mar = c(4, 4, 1, 1))
catt1 <- catt.ldw$catt
att1 <- catt.ldw$att[1]
catt2 <- catt.psid$catt
att2 <- catt.psid$att[1]
plot_catt(catt1, catt2, att1, att2, "CATT (Experimental)", "CATT (PSID-Full)",
    main = "", c(-6000, 2000))
graphics.off()

## Trimmed sample

pdf("graphs/lalonde/catt_placebo_cps_trim.pdf", width = 5.5, height = 5.5)
par(mar = c(4, 4, 1, 1))
catt1 <- catt.ldw.cps$catt
att1 <- catt.ldw.cps$att[1]
catt2 <- catt.cps.trim$catt
att2 <- catt.cps.trim$att[1]
plot_catt(catt1, catt2, att1, att2, "CATT (Experimental)", "CATT (CPS-Trimmed)",
    main = "", c(-6000, 2000))
graphics.off()

pdf("graphs/lalonde/catt_placebo_psid_trim.pdf", width = 5.5, height = 5.5)
par(mar = c(4, 4, 1, 1))
catt1 <- catt.ldw.psid$catt
att1 <- catt.ldw.psid$att[1]
catt2 <- catt.psid.trim$catt
att2 <- catt.psid.trim$att[1]
plot_catt(catt1, catt2, att1, att2, "CATT (Experimental)", "CATT (PSID-Trimmed)",
    main = "", c(-6000, 2000))
graphics.off()


# $$\                $$\                                $$\                 $$\      $$\           $$\           
# $$ |               $$ |                               $$ |                $$$\    $$$ |          $$ |          
# $$ |      $$$$$$\  $$ |      $$$$$$\  $$$$$$$\   $$$$$$$ | $$$$$$\        $$$$\  $$$$ | $$$$$$\  $$ | $$$$$$\  
# $$ |      \____$$\ $$ |     $$  __$$\ $$  __$$\ $$  __$$ |$$  __$$\       $$\$$\$$ $$ | \____$$\ $$ |$$  __$$\ 
# $$ |      $$$$$$$ |$$ |     $$ /  $$ |$$ |  $$ |$$ /  $$ |$$$$$$$$ |      $$ \$$$  $$ | $$$$$$$ |$$ |$$$$$$$$ |
# $$ |     $$  __$$ |$$ |     $$ |  $$ |$$ |  $$ |$$ |  $$ |$$   ____|      $$ |\$  /$$ |$$  __$$ |$$ |$$   ____|
# $$$$$$$$\\$$$$$$$ |$$$$$$$$\\$$$$$$  |$$ |  $$ |\$$$$$$$ |\$$$$$$$\       $$ | \_/ $$ |\$$$$$$$ |$$ |\$$$$$$$\ 
# \________|\_______|\________|\______/ \__|  \__| \_______| \_______|      \__|     \__| \_______|\__| \_______|
                                                                                                               

load("data/lalonde.RData")
load("data/trimmed.RData")
                                    

Y <- "re78"
treat <- "treat"
covar <- c("age", "education", "black", "hispanic", "married", "nodegree", "re75", "u75")

set.seed(1234)
catt.nsw <- catt(nsw, Y, treat, covar) # nsw experimental
catt.cps <- catt(nsw_cps, Y, treat, covar)
catt.psid <- catt(nsw_psid, Y, treat, covar)
catt.cps.trim <- catt(nsw_cps_trim, Y, treat, covar)
catt.psid.trim <- catt(nsw_psid_trim, Y, treat, covar)
# trimmed experimental data
catt.nsw.cps <- catt(nsw_trim_cps, Y, treat, covar)
catt.nsw.psid <- catt(nsw_trim_psid, Y, treat, covar)

pdf("graphs/lalonde/catt_nsw_cps.pdf", width = 5.5, height = 5.5)
par(mar = c(4, 4, 1, 1))
catt1 <- catt.nsw$catt
att1 <- catt.nsw$att[1]
catt2 <- catt.cps$catt
att2 <- catt.cps$att[1]
plot_catt(catt1, catt2, att1, att2, "CATT (Experimental)", "CATT (CPS-Full)",
    main = "", c(-8000, 8000))
graphics.off()

pdf("graphs/lalonde/catt_nsw_psid.pdf", width = 5.5, height = 5.5)
par(mar = c(4, 4, 1, 1))
catt1 <- catt.nsw$catt
att1 <- catt.nsw$att[1]
catt2 <- catt.psid$catt
att2 <- catt.psid$att[1]
plot_catt(catt1, catt2, att1, att2, "CATT (Experimental)", "CATT (PSID-Full)",
    main = "", c(-8000, 8000))
graphics.off()

## Trimmed sample

pdf("graphs/lalonde/catt_nsw_cps_trim.pdf", width = 5.5, height = 5.5)
par(mar = c(4, 4, 1, 1))
catt1 <- catt.nsw.cps$catt
att1 <- catt.nsw.cps$att[1]
catt2 <- catt.cps.trim$catt
att2 <- catt.cps.trim$att[1]
plot_catt(catt1, catt2, att1, att2, "CATT (Experimental)", "CATT (CPS-Trimmed)",
    main = "", c(-8000, 8000))
graphics.off()

pdf("graphs/lalonde/catt_nsw_psid_trim.pdf", width = 5.5, height = 5.5)
par(mar = c(4, 4, 1, 1))
catt1 <- catt.nsw.psid$catt
att1 <- catt.nsw.psid$att[1]
catt2 <- catt.psid.trim$catt
att2 <- catt.psid.trim$att[1]
plot_catt(catt1, catt2, att1, att2, "CATT (Experimental)", "CATT (PSID-Trimmed)",
    main = "", c(-8000, 8000))
graphics.off()

gc() # free up memory