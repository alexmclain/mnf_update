# Preparing Primary Data

This folder cleans the JME survey data and merges it with the imputed
covariates. The scripts impute missing standard errors, adjust for
partial age ranges, merge the survey data with the covariate data and
regional groupings, and add approximate sex-specific values and annual
rows.

## Files and usage

Run the scripts in order, once per marker (`Wasting`, `SevereWasting`):

```bash
Rscript "2 - Preparing Primary Data/1 - SE_clean_and_impute.R" Wasting
Rscript "2 - Preparing Primary Data/2 - Age_range_analysis.R" Wasting
Rscript "2 - Preparing Primary Data/3 - Merging Survey_Covariate_Data.R" Wasting
Rscript "2 - Preparing Primary Data/4 - Sex_cross_walk.R" Wasting
```

| Script | Reads | Writes |
|----|----|----|
| `1 - SE_clean_and_impute.R` | `Data/JME/<year>/Raw/JME_Country_Level_Input_<marker>.xlsx` (or `.xls`) | `Data/JME/<year>/Cleaned/<marker>_SE_clean.rds`; `<marker>_smallN.csv` (estimates dropped because N < 6) |
| `2 - Age_range_analysis.R` | `<marker>_SE_clean.rds` | `Data/JME/<year>/Cleaned/<marker>_SE_Age_clean.rds` |
| `3 - Merging Survey_Covariate_Data.R` | `<marker>_SE_Age_clean.rds`, `Data/IHME_covs/Multiple_Imputed_<label>.rds`, `Single_Impute_<label>.rds`, `Data/Country/Crosswalk_Oct_2026.xlsx` | `Data/Merged/<year>/<marker>_<month>_reg_all_multiple_impute.rds` and `..._reg_all_mean_multiple.rds` |
| `4 - Sex_cross_walk.R` | the two `reg_all` files | `Data/Merged/<year>/<marker>_<month>_final_multiple_impute.rds` and `..._final_mean_impute.rds` |

The `final_multiple_impute` file (20 imputations, column `.imp`) is the
input to `3 - Model/2 - Analysis_MI.R`; the `final_mean_impute` file
(mean-imputed covariates, `.imp = 0`) is used by the exploratory analysis
and model comparison scripts. Scripts 3 and 4 loop over both versions.

### Arguments (top of each script)

- **marker**: command-line argument, `Wasting` or `SevereWasting`.
- **year**: year folder of the JME data (`"2026"`).
- **month**: scripts 3 and 4 only; appended to the output names
  (`"Oct"`).
- **imputed_label**: script 3 only; label of the covariate files made by
  `1 - Preparing Covariates/02 - Imputation.qmd` (`"Oct2026"`).

## Survey input data

`Data/JME/<year>/Raw/JME_Country_Level_Input_<marker>.xlsx` are made by
`Data/JME/<year>/Raw/Initial_cleaning.R` (run from that folder). It
combines the UNICEF SDMX extract (`data.csv`) with the CMRS extract
(`cmrs2-ant-accepted_jme2025_wasting-national-sex_fieldwork_20261021.csv`),
which adds the survey fieldwork dates, formats them as in
`desired_data_format.csv`, and removes estimates from the previous JME
modelling. The CMRS fieldwork dates give `middle_year`, the continuous
mid-point of the survey fieldwork, which is carried through all scripts
and is the time variable in the final models.

## Details

### 1 - SE_clean_and_impute.R

The survey sample standard error (SSE) is used in the model to account
for survey uncertainty, but it is not reported for all data sources.
Some sources report a 95% confidence interval (CI) instead. Since the
scale on which the CI was calculated varies and is not always known, the
script tries several transformations (none, log, logit) of the
prevalence and CI limits and uses the one that gives the most symmetric
CI to back out the SSE. For sources with neither an SSE nor a CI, the
SSE is predicted from a linear mixed model of log(SSE) fitted to the
sources with complete SSE, prevalence and sample size.

Sections: (1) loading and formatting the data, and dropping estimates
with N < 6; (2) SEs from confidence intervals; (3) identifying outlying
SEs; (4) predicting missing SEs; (5) exporting the data.

### 2 - Age_range_analysis.R

Some sources do not cover the full 0-59 month age range (e.g. 0-5 or
0-11 months missing). A linear mixed model of the difference between
the 0-59 month prevalence and the age-group prevalences (0-5, 6-11,
12-23, 24-35, 36-47, 48-59 months), as a function of the full
prevalence, age group and their interaction, is fitted to the sources
that report both. It is used to impute the missing age groups, and the
full-range prevalence is then recomputed from the observed and imputed
age groups for sources with at least one missing age group.

### 3 - Merging Survey_Covariate_Data.R

Merges the survey data with the imputed covariates (once with the 20
multiple imputations, once with the mean imputation) and adds the
modelling regions from `Data/Country/Crosswalk_Oct_2026.xlsx`. Countries
with no region in the crosswalk give a warning.

### 4 - Sex_cross_walk.R

1. Predicts missing sex-specific prevalences (surveys that only report
   the overall value) with linear mixed models. These predictions are
   **not** used in the model fit; they only fill the sex-specific
   figures, so that a survey without a sex breakdown does not look like
   a drop in the female or male trend.
2. Adds one row with no survey (covariates only) at the integer year
   for every country-year-sex that has a survey. Surveys sit at their
   `middle_year`, so without these rows the annual estimate for a survey
   year would be at a different time point than the other years. Every
   country-year-sex therefore has exactly one annual row
   (`UNICEFSurveyID = NA`, `middle_year = year`).

## Contact

For questions or issues regarding this folder, please contact the
repository maintainer.
