# LaLonde-Dehejia-Wahba (LDW) Data

LaLonde (1986) assessed the impact of the National Supported Work Demonstration (NSW) program on both female and male participants, with females drawn from the Aid to Families with Dependent Children (AFDC) program and males from three other target groups: ex-drug addicts, ex-criminal offenders, and high-school dropouts. LaLonde used two primary data sources:

- CPS-SSA-1, from Westat’s Matched Current Population Survey–Social Security Administration File for individuals under 55 matching specific criteria.
- PSID-1, from the Panel Study of Income Dynamics for household heads under 55 from specific years who were not retired in 1975, adjusted for factors like employment status and poverty level, resulting in four additional comparison groups.

Dehejia and Wahba (1999) subsampled 62% of LaLonde’s original dataset focusing on 1974 earnings and unemployment status of male participants. Since the construction relies solely on pretreatment information like month of assignment and employment history to ensure treatment assignment remains orthogonal to all pretreatment variables, Dehejia and Wahba argue the refined dataset, LaLonde-Dehejia-Wahba (LDW) data, is a valid experimental sample.

Built upon LDW data, our analysis examines three samples: (1) LDW-Experimental, with 185 treated and 280 control participants from the experimental data; (2) LDW-CPS1, featuring the same treated individuals alongside 15,992 controls from CPS-SSA-1; and (3) LDW-PSID1, including the same treated participants and 2,490 controls from PSID-1. Then, we apply the same set of statistical tools to analyze the original male sample from LaLonde (1986) in section 4 as an additional demonstration. 


```r
load("data/lalonde.RData")
ldw_co$treat <- 1
ldw_cps.plus <- rbind.data.frame(ldw_cps, ldw_co)
ldw_psid.plus <- rbind.data.frame(ldw_psid, ldw_co)

# define variables
Y <- "re78"
treat <- "treat"
covar <- c("age", "education", "black", "hispanic", "married",
           "nodegree", "re74", "re75", "u74", "u75")
```

## Assessing Overlap
To identify the average causal effect under unconfoundedness, we need to ensure that we can estimate the average effect at every value for the covariates. Thus, we require overlaps between the treated and control units. Using the `assess_overlap` function, we can assess overlaps in propensity scores. To test the overlap assumption, we plot histograms using the log-odds of propensity scores, i.e., $\log(\frac{\hat{e}}{1-\hat{e}})$.

Ideally, for a well-balanced experimental design, the distributions of the treated (red) and the control (gray) groups should overlap.

### LDW-Experimental



































