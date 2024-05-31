Replications Files for Imbens & Xu (2024)
================

This Github repo contains the data and code necessary to replicate
**Imbens and Xu (2024)**: “LaLonde (1986) After Nearly Four Decades:
Lessons Learned.” For a more detialed tutorial, click
[here](https://yiqingxu.org/papers/english/2024_lalonde/tutorial.html).

## Folder Structure

The folder structure of this repo is as follows:

| folder | usage                                                |
|:-------|:-----------------------------------------------------|
| code   | R scripts                                            |
| data   | Data files, include two subfolders “lalonde” & “irs” |
| graphs | To store graphics                                    |
| output | To store estimation results                          |
| tables | To store tables                                      |

## Data Files

The paper uses the following datasets, which are based on LaLonde
(1986), Dehejia and Wahba (1999), and Imbens, Rubin, and Sacerdote
(2001).

| Data.files        | Details                                                          | File_Type | Experimental |
|:------------------|:-----------------------------------------------------------------|:----------|:-------------|
| nsw.dta           | NSW experimental data, used in LaLonde (1986)                    | Stata     | Yes          |
| nsw_dw.dta        | Subset of NSW experimental data, used in Dehejia & Wahba (1999)  | Stata     | Yes          |
| cps_controls.dta  | CPS-SSA-1 controls, used in both papers                          | Stata     | No           |
| psid_controls.dta | PSID-1 controls, used in both papers                             | Stata     | No           |
| lottery.RData     | Data of lottery winners, used in Imbens, Rubin & Sacerdote (201) | R         | No           |

## R Scripts

To replicate all findings, set the directory to the root folder of the
replications files and execute `master.R`. Below are explanations for
the usage of each R script:

| Data.files          | Usage                                                  |
|:--------------------|:-------------------------------------------------------|
| master.R            | Install necessary R packages and excute all R scripts. |
| functions_est.R     | Store functions for estimation                         |
| functions_plot.R    | Store functions for making plots                       |
| lalonde1_prepare.R  | Preprocess LaLonde datasets                            |
| lalonde2_trim.R     | Trim datasets to improve overlap                       |
| lalonde3_estimate.R | Estimate the ATT                                       |
| lalonde4_overlap.R  | Visualize overlap in propensity scores                 |
| lalonde5_catt.R     | Estimate and visualize CATT                            |
| lalonde6_qte.R      | Estimate and visualize quantile treatment effects      |
| laldone7_sens.R     | Conduct sensitivity analyses                           |
| irs1_est.R          | Estimate the ATT using the IRS data                    |
| irs2_big.R          | Additional analyses for winning big prizes             |
| irs3_small.R        | Additional analyses for winning small prizes           |

## Install R Packages

For successful replication, the following R packages need to be
installed:

``` r
# required packages
packages <- c("haven", "labelled", "Matching", "grf", "sensemakr", "qte",
    "estimatr", "CBPS", "hbal", "DoubleML", "mlr3learners", "fixest", "ggplot2")

# install packages
install_all <- function(packages) {
  installed_pkgs <- installed.packages()[, "Package"]
  for (pkg in packages) {
    if (!pkg %in% installed_pkgs) {
      install.packages(pkg)
    }
  }
}
install_all(packages)
```

## Report Errors

To report errors, please contact <yiqingxu@stanford.edu>. Comments and
suggestions are welcome.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-dehejiawahba" class="csl-entry">

Dehejia, Rajeev H, and Sadek Wahba. 1999. “Causal Effects in
Nonexperimental Studies: Reevaluating the Evaluation of Training
Programs.” *Journal of the American Statistical Association* 94 (448):
1053–62.

</div>

<div id="ref-imbensrubinsacerdote" class="csl-entry">

Imbens, Guido W, Donald B Rubin, and Bruce I Sacerdote. 2001.
“Estimating the Effect of Unearned Income on Labor Earnings, Savings,
and Consumption: Evidence from a Survey of Lottery Players.” *American
Economic Review* 91 (4): 778–94.

</div>

<div id="ref-imbensxu" class="csl-entry">

Imbens, Guido W, and Yiqing Xu. 2024. “LaLonde (1986) After Nearly Four
Decades: Lessons Learned.” arXiv:TBD.

</div>

<div id="ref-LaLonde" class="csl-entry">

LaLonde, Robert J. 1986. “Evaluating the Econometric Evaluations of
Training Programs with Experimental Data.” *The American Economic
Review* 76 (4): 604–20.

</div>

</div>
