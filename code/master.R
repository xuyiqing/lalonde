# LaLonde (1986) after Nearly Four Decades: Lessons Learned
# Guido Imbens & Yiqing Xu
# 2024

## Master script to run all the analyses
## Note: gc() is for garbage collection to free up memory

rm(list = ls())
path <- "~/Dropbox/ProjectZ/lalonde/replication/" # change to the root directory of the project
setwd(path)

# install packages
install_all <- function(packages) {
  installed_pkgs <- installed.packages()[, "Package"]
  for (pkg in packages) {
    if (!pkg %in% installed_pkgs) {
      install.packages(pkg)
    }
  }
}
packages <- c("haven", "labelled", "Matching", "grf", "sensemakr", "qte",
    "estimatr", "CBPS", "hbal", "DoubleML", "mlr3learners", "fixest", "ggplot2")
install_all(packages)

# load all pakcages
packages <- c("haven", "labelled", "Matching", "grf", "sensemakr", "qte",
    "estimatr", "CBPS", "hbal", "DoubleML", "mlr3learners", "fixest", "ggplot2")
for (pkg in packages) {
  library(pkg, character.only = TRUE)
}


# run R scripts sequentially

## LaLonde data
source("code/lalonde1_prepare.R") # data preparation
source("code/lalonde2_trim.R") # trimming
source("code/lalonde3_overlap.R") # overlap in propensity scores
source("code/lalonde4_estimate.R") # ATT estimates
source("code/lalonde5_catt.R") # conditional average treatment effects
source("code/lalonde6_qte.R") # quantile treatment effects
source("code/lalonde7_sens.R") # sensitivity analysis
source("code/lalonde8_lcs.R") # LaLonde female data reconstrcted by Calonico & Smith (2017)

## IRS data
source("code/irs1_est.R") # ATT estimates
source("code/irs2_big.R") # Dynamic effects for big winners & plots
source("code/irs3_small.R") # Dynamic effects for small winners & plots


