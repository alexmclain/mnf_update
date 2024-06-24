
# Preparing Primary Data

This folder contains the code used to clean and merge the data for the
Global Health Estimates of Stunting and Overweight. The files included
in this folder perform essential data preparation tasks, such as
imputing missing standard error information, adjusting for partial age
ranges, merging survey data with covariate data, and addressing partial
sex coverage.

## Files

1.  **SE_clean_and_impute.R**: Impute missing standard error (SE)
    information.
2.  **Age_range_analysis.R**: Perform a cross-walk to account for
    partial age ranges.
3.  **Merging Survey_Covariate_Data.R**: Merge survey data with
    covariate data.
4.  **Sex_cross_walk.R**: Cross-walk for partial sex coverage.

## Data Sources

The data sources are located in the `Data/JME/year/Raw/` folder (e.g.,
year = 2024):

- Raw survey data for Stunting: `JME_Country_Level_Input_Stunting.csv`
- Raw survey data for Overweight:
  `JME_Country_Level_Input_Overweight.csv`

## Usage

To run the data cleaning process, set the working directory (`wd`) to
the folder that contains the **“2 - Preparing Primary Data”** folder.

Execute the programs in the following order:

1.  **SE_clean_and_impute.R**
2.  **Age_range_analysis.R**
3.  **Merging Survey_Covariate_Data.R**
4.  **Sex_cross_walk.R**

Each program has modifiable arguments for finding data or appending file
names. The arguments are as follows:

- **marker**: Used for all files and should be set to `"Stunting"` or
  `"Overweight"`.
- **year**: Used for all files and should correspond to the year value
  in the folder containing the raw data.
- **month**: Used for files 3 and 4 only. This value will append the
  file name.

Each file should be run once for “Stunting” and once for “Overweight”.

## Details

### SE_clean_and_impute.R

#### Background

An essential component of the statistical methods applied to generate
the country prevalence estimates is the incorporation of the sample
standard error (SSE) of the survey to adjust for the survey uncertainty.
However, SSE’s were not reported for all surveys. Some surveys reported
95% confidence intervals (CI), but not the SSE. The CIs can be used to
estimate the SSE. The scale of CI calculation varies by study and is not
always known. To identify the scale of calculation, we try various
transformations (no transformation, log, logit) of the survey prevalence
and 95% CI limits, to identify which transformation resulted in the most
symmetric 95% CI. Once the transformation was identified, the SSE can be
estimated.

For surveys without SSE’s or 95% CI’s, they were predicted using a
linear mixed model on the log transformed SSE values. The SSE model was
fitted using the surveys that had complete data on SSE, survey type,
prevalence and sample size. The fitted model was then used to estimate
the SSE for surveys that were missing SSE. To limit the impact studies
with missing SSE on the analysis, a conservative prediction of SSE was
used where one standard error was added to the predicted SSE values.

##### Script Sections

This script imputes missing SSE information. The sections include:

1.  Loading and formatting the data (lines 12 - 60)
2.  Calculating missing SE values from confidence interval estimates
    (lines 62 - 121)
3.  Identifying outlying SE values (lines 126 - 202)
4.  Predicting with standard errors (lines 208 - 278)
5.  Exporting the final data (lines 280 - 307)

### Age_range_analysis.R

#### Background

Some surveys do not cover the entire age interval 0 to 59 months and
thus are not aligned with the standard definition for the child
malnutrition indicators (e.g., 0-5 or 0-12 months not covered). To
incorporate these surveys, this script runs a linear mixed model on the
difference between the 0-59-month prevalence estimate and the estimates
at 0-5-, 6-11-, 12-23-, 24-35-, 36-47- and 48-59-month age-groups, using
data from surveys with both the 0-59-month prevalence and separate
age-group prevalence values. Specifically, this difference was modeled
as a function of the full prevalence, age-group and a full prevalence by
age-group interaction. Model diagnostics showed that the linearity
assumption was upheld. With the estimated mixed model, the data for
missing age groups were then imputed using the data from the observed
age groups. The prevalence estimate for the full age range was then
aggregated using the estimated and observed age-group prevalence rates,
for sources with at least one missing age group.

##### Script Sections

This script performs a cross-walk to account for partial age ranges. The
sections include:

1.  Loading and formatting the data (lines 12 - 195)
2.  Running linear mixed models to predict missing age groups (lines
    200 - 249)
3.  Recalculating the national estimate if the previous national
    estimate had a partial age range (lines 252 - 296)
4.  Inspecting results and outputting estimates (lines 298 - 326)

### Merging Survey_Covariate_Data.R

This script merges the imputed covariate data with the survey data and
adds regional groupings to the data. For both indicators, the nine
regions used in the modeling were Eastern and Central Africa, Southern
Africa, Western Africa, the Middle East and North Africa, East Asia and
Pacific, South Asia, Europe and Central Asia, High-Income Western
Countries, and Latin America and the Caribbean.

### Sex_cross_walk.R

This script addresses partial sex coverage. The sections include:

1.  Loading and formatting the data (lines 20 - 131)
2.  Running linear mixed models to predict missing sex groups (lines
    134 - 430)
3.  Merging and outputting estimates (lines 439 - 500)

## Contact

For questions or issues regarding this folder, please contact the
repository maintainer.
