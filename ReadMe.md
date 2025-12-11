Replications Files for Imbens & Xu (2024)
================

This Github repo contains the data and code necessary to replicate
**Imbens and Xu (2024)**: “LaLonde (1986) After Nearly Four Decades:
Lessons Learned.” [\[arXiv\]](https://arxiv.org/abs/2406.00827)

**Tutorial**: For a detailed bookdown tutorial, see
[here](https://yiqingxu.org/tutorials/lalonde/). We thank Zihan Xie and
Jinwen Wu for their excellent research assistance, which makes this
tutorial possible.

## Folder Structure

The folder structure of this repo is as follows:

| folder | usage                                                      |
|:-------|:-----------------------------------------------------------|
| code   | R scripts                                                  |
| data   | Data files, include two subfolders “lalonde”, “cs” & “irs” |
| graphs | To store graphics                                          |
| output | To store estimation results                                |
| tables | To store tables                                            |

## Data Files

Imbens and Xu (2024) uses the following datasets, which are based on
LaLonde (1986), Dehejia and Wahba (1999), Calónico and Smith (2017), and
Imbens, Rubin, and Sacerdote (2001).

| Data.files        | Details                                                          | File_Type | Experimental |
|:------------------|:-----------------------------------------------------------------|:----------|:-------------|
| nsw.dta           | NSW experimental data, used in LaLonde (1986)                    | Stata     | Yes          |
| nsw_dw.dta        | Subset of NSW experimental data, used in Dehejia & Wahba (1999)  | Stata     | Yes          |
| cps_controls.dta  | CPS-SSA-1 controls, used in both papers                          | Stata     | No           |
| psid_controls.dta | PSID-1 controls, used in both papers                             | Stata     | No           |
| lottery.RData     | Data of lottery winners, used in Imbens, Rubin & Sacerdote (201) | R         | No           |
| NSW_AFDC_CS.dta   | Reconstructed NSW AFDC female samples                            | Stata     | Both         |

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
| lalonde3_overlap.R  | Visualize overlap in propensity scores                 |
| lalonde4_estimate.R | Estimate the ATT                                       |
| lalonde5_catt.R     | Estimate and visualize CATT                            |
| lalonde6_qte.R      | Estimate and visualize quantile treatment effects      |
| laldone7_sens.R     | Conduct sensitivity analyses                           |
| lalonde8_lcs.R      | Analyze the female samples by Calonico & Smith (2017)  |
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

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-calonico2017women" class="csl-entry">

Calónico, Sebastian, and Jeffrey Smith. 2017. “The Women of the National
Supported Work Demonstration.” *Journal of Labor Economics* 35 (S1):
S65–97.

</div>

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
Decades: Lessons Learned.” arXiv:2406.00827.

</div>

<div id="ref-LaLonde" class="csl-entry">

LaLonde, Robert J. 1986. “Evaluating the Econometric Evaluations of
Training Programs with Experimental Data.” *The American Economic
Review* 76 (4): 604–20.

</div>

</div>
## Student Replication Contribution

This fork was created as part of an econometrics replication assignment.
I reviewed the repository structure and documentation for the replication
of LaLonde (1986) as discussed in Imbens & Xu (2024).

My contribution focuses on clarifying the replication workflow and
documenting how the data, code, and outputs are organized. Future
extensions may include running alternative specifications or robustness
checks.

