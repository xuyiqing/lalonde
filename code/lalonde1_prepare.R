# LaLonde (1986) after Nearly Four Decades: Lessons Learned
# Guido Imbens & Yiqing Xu
# 2024

## Process the original data files from Dehejia and Wahba (1999)

library(haven)
library(labelled)


process.data  <- function(filename) {
    d <- haven::read_dta(filename)
    d <- as.data.frame(zap_label(d))
    for (i in 1:ncol(d)) {
        if (is.numeric(d[, i])) {
            d[, i] <- as.vector(d[, i])
        }
    }
    if ("re74" %in% colnames(d)) {d$u74 <- ifelse(d$re74 == 0, 1, 0)}
    d$u75 <- ifelse(d$re75 == 0, 1, 0)
    return(d)
}


nsw <- process.data("data/lalonde/nsw.dta")
table(nsw$treat)
nsw$sample <- NA
nsw$sample[which(nsw$treat == 1)] <- 0
nsw$sample[which(nsw$treat == 0)] <- 0.5
table(nsw$treat, nsw$sample)
nsw_tr <- subset(nsw, treat == 1)
nsw_co <- subset(nsw, treat == 0)

ldw <- process.data("data/lalonde/nsw_dw.dta")
ldw$sample <- NA
ldw$sample[which(ldw$treat == 1)] <- 1
ldw$sample[which(ldw$treat == 0)] <- 2
table(ldw$treat, ldw$sample)
ldw_tr <- ldw[which(ldw$treat == 1), ] # experimental treated 
ldw_co <- ldw[which(ldw$treat == 0), ] # experimental control
cps1 <- process.data("data/lalonde/cps_controls.dta")
psid1 <- process.data("data/lalonde/psid_controls.dta")
                                                      
                                                      
# $$$$$$\$$$$\   $$$$$$\   $$$$$$\   $$$$$$\   $$$$$$\  
# $$  _$$  _$$\ $$  __$$\ $$  __$$\ $$  __$$\ $$  __$$\ 
# $$ / $$ / $$ |$$$$$$$$ |$$ |  \__|$$ /  $$ |$$$$$$$$ |
# $$ | $$ | $$ |$$   ____|$$ |      $$ |  $$ |$$   ____|
# $$ | $$ | $$ |\$$$$$$$\ $$ |      \$$$$$$$ |\$$$$$$$\ 
# \__| \__| \__| \_______|\__|       \____$$ | \_______|
#                                   $$\   $$ |          
#                                   \$$$$$$  |          
#                                    \______/           


cps1$sample <- 3
ldw_cps <- rbind.data.frame(ldw_tr, cps1)
table(ldw_cps$treat)

psid1$sample <- 4
ldw_psid <- rbind.data.frame(ldw_tr, psid1)
table(ldw_psid$treat)

cps1a <- subset(cps1, select =  -c(re74, u74))
names(nsw)
names(cps1a)
nsw_cps <- rbind.data.frame(nsw_tr, cps1a)
table(nsw_cps$treat)

psid1a <- subset(psid1, select =  -c(re74, u74))
nsw_psid <- rbind.data.frame(nsw_tr, psid1a)
table(nsw_psid$treat)


save(nsw, nsw_tr, nsw_co, ldw, ldw_tr, ldw_co, 
    cps1, psid1, ldw_cps, ldw_psid, nsw_cps, nsw_psid, file = "data/lalonde.RData")

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

load("data/lalonde.RData")

dim(nsw_tr)
dim(nsw_co)
dim(ldw_tr)
dim(ldw_co)
dim(cps1)
dim(psid1)

X0 <- c("age", "education", "nodegree", "married", "black", "hispanic",  
    "re75", "u75")

X <- c("age", "education", "nodegree", "married", "black", "hispanic", 
    "re75", "u75", "re74", "u74")

# mean
nsw.tr <- c(apply(nsw_tr[, X0], 2, mean), rep(NA, 2))
nsw.co <- c(apply(nsw_co[, X0], 2, mean), rep(NA, 2))
ldw.tr <- c(apply(ldw_tr[, X], 2, mean))
ldw.co <- apply(ldw_co[, X], 2, mean)
cps <- apply(cps1[, X], 2, mean)
psid <- apply(psid1[, X], 2, mean)
out <- cbind.data.frame(nsw.tr, nsw.co, cps, psid, ldw.tr, ldw.co)
out[c("re75", "re74"), ] <- out[c("re75", "re74"), ]/1000

# sd
nsw.tr <- c(apply(nsw_tr[, X0], 2, sd), rep(NA, 2))
nsw.co <- c(apply(nsw_co[, X0], 2, sd), rep(NA, 2))
ldw.tr <- c(apply(ldw_tr[, X], 2, sd))
ldw.co <- apply(ldw_co[, X], 2, sd)
cps <- apply(cps1[, X], 2, sd)
psid <- apply(psid1[, X], 2, sd)
out2 <- cbind.data.frame(nsw.tr, nsw.co, cps, psid, ldw.tr, ldw.co)
out2[c("re75", "re74"), ] <- out2[c("re75", "re74"), ]/1000

# columns are samples
n <- nrow(out)   
sav <- matrix("", n*2, ncol(out))
for (j in 1:n) {
    a <- out[j, ]
    b <- out2[j, ]
    sav[j*2-1, ] <- sprintf("%.2f", a)
    sav[j*2, ] <- paste0("(", sprintf("%.2f",b), ")")       
}
print(sav)

write.csv(sav, file = "tables/stats.csv", row.names = FALSE)

gc() # free up memory