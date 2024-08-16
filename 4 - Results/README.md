
# Results

The scripts in this folder are used to summarize the imputed analyses,
produce figures of the results, and compare different model fits.

## Files

- **0 - Summarizing_imputed_results.R**: Programs for pooling the
  estimates from the imputed datasets.
- **1 - Plotting_estimates.R**: Programs for plotting the estimates.
- **2 - Comparison Plots.R**: Programs for producing plots to compare
  multiple versions of the estimates.

## Usage

For each file, set the working directory (`wd`) to the folder that
contains the **“4 - Results”** folder.

### Arguments

- **marker**: Should be set to `"Stunting"` or `"Overweight"`.
- **year/month**: Needed to find the data. This should correspond to the
  `year` and `month` values used in the data cleaning process.
- **marker_f**: Needed to find the analysis data. This should match what
  was used in the `Analysis_MI.R` script.
- **date**: Used for appending the file names, e.g., `"May2024"`.

## Details

### 0 - Summarizing_imputed_results.R

This script reads the results from each of the imputed analyses, pools
them together, and outputs a dataset with the final predictions. The
dataset with the final predictions will be written to
`Data/Analysis files/marker` with the filename
`marker results data.csv`.

### 1 - Plotting_estimates.R

This script produces country-level plots by region. For each sex, the
following figures will be produced:

- Predicted values with 95% confidence and prediction intervals.
- Comparisons of predicted values using only fixed effects; only fixed
  and penalized effects; and fixed, penalized, and random effects (i.e.,
  the full estimates). These plots help determine what is driving the
  results.
- Comparisons between female, male, and overall.

The figures will be written to `Figures/marker/monthyear/`, where
`monthyear` is the concatenated `month` and `year` arguments. **Make
sure this folder exists before running.**

### 2 - Comparison Plots.R

This script compares the model fit of two separate analyses. The
following figures will be produced for overall only:

- Predicted values with survey estimates by whether the survey estimates
  are the same (any differences $<0.5\%$ are considered equivalent).

The figures will be written to `Figures/marker/monthyear/`, where
`monthyear` is the concatenated `month` and `year` arguments. **Make
sure this folder exists before running.**

## Contact

For questions or issues regarding this folder, please contact the
repository maintainer.
