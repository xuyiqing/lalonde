##### "The Women of the National Supported Work Demonstration"
##### Sebastian Calonico and Jeffrey Smith
##### Journal of Labor Economics 
##### April 2017
##### Data replication file

The analysis can be replicated using two main Stata do-files:

1) "1_generate_datasets.do" generates the NSW and PSID datasets from the original files included in the data folder.
2) This do-file generates one final dataset called "NSW_AFDC_CS.dta" in the data folder.
3) "2_generate_tables.do" generates all the tables and log-files used for the article in the output folder.

The variables included in the final dataset are:
- treated: treatment indicator for the experimental sample.
- age: age in years
- educ: years of education
- nodegree: indicator for educ<12
- married: married indicator
- black: black indicator
- hisp: hispanic indicator
- moa: month of assignment for the experimental sample.
- re75: real earnings in 1975
- re76: real earnings in 1976    
- re77: real earnings in 1977
- re78: real earnings in 1978
- re79: real earnings in 1979 
- re74: real earnings in "1974"
- afdc75: afdc recipient indicator
- nchildren75: number of children in 1975
- zero74: indicator for zero real earnings in "1974"
- zero75: indicator for zero real earnings in 1975 
- redif: re79-re75   
- lalonde: indicator for the final experimental sample used in the analysis.
- psid1: indicator for PSID-1 sample
- psid2: indicator for PSID-2 sample
- sample_lalonde: Indicator for Lalonde Sample
- sample_dw: Indicator for DW Sample     
- sample_early: Indicator for Early RA Sample  

