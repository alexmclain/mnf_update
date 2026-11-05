# Global Estimates of Child Malnutrition (JME)

This repository contains the R code used to produce the country-level
model-based estimates for the UNICEF-WHO-World Bank Joint Child
Malnutrition Estimates (JME). The current round (October 2026) produces
estimates of **Wasting** and **Severe Wasting**; the same pipeline has
been used in earlier rounds for Stunting, Overweight and Severe
Overweight.

The model is a heterogeneous penalized longitudinal mixed model
(McLain et al. 2019), fitted separately to each of 20 multiply imputed
covariate datasets and pooled with Rubin's rules. See also Saraswati et
al. (2022) and the editorial by Finaret (2022).

## Pipeline

The numbered folders are run in order. Each stage reads the previous
stage's output from `Data/`. Every folder has its own README with the
arguments, inputs and outputs of each script.

| Stage | Folder / script | What it does | Main output |
|----|----|----|----|
| 0 | `Data/JME/<year>/Raw/Initial_cleaning.R` | Builds the JME survey input files from the UNICEF SDMX and CMRS extracts | `JME_Country_Level_Input_<marker>.xlsx` |
| 1 | `1 - Preparing Covariates` | Cleans and merges the covariates (SDI, war/terror, shock, GDP, food prices, DPT, WPP) and multiply imputes them | `Data/IHME_covs/Multiple_Imputed_Oct2026.rds`, `Single_Impute_Oct2026.rds` |
| 2 | `2 - Preparing Primary Data` | Imputes missing survey SEs, adjusts non-standard age ranges, merges surveys with covariates and regions, sex cross-walk and annual rows | `Data/Merged/<year>/<marker>_<month>_final_*.rds` |
| 3 | `3 - Model` | Exploratory plots, hold-out model comparison, and the final multiply imputed analysis | `Data/Analysis files/<marker>/Mi_files/Estimation <marker>.rds` |
| 4 | `4 - Results` | Pools the imputations (Rubin's rules), plots the estimates, compares runs, checks Wasting vs Severe Wasting | `Data/Analysis files/<marker>/<marker> results <tag> <month> <year>.csv`, `Figures/` |

Typical run for one round (from the repository root):

```bash
# 1. Covariates (Quarto; render both)
quarto render "1 - Preparing Covariates/01 - Covariate Cleaning.qmd"
quarto render "1 - Preparing Covariates/02 - Imputation.qmd"

# 2. Primary data (once per marker)
for m in Wasting SevereWasting; do
  Rscript "2 - Preparing Primary Data/1 - SE_clean_and_impute.R" $m
  Rscript "2 - Preparing Primary Data/2 - Age_range_analysis.R" $m
  Rscript "2 - Preparing Primary Data/3 - Merging Survey_Covariate_Data.R" $m
  Rscript "2 - Preparing Primary Data/4 - Sex_cross_walk.R" $m
done

# 3. Model (optional exploration / model selection first; see 3 - Model/README.md)
Rscript "3 - Model/2 - Analysis_MI.R" Wasting          # Wasting, model without fpi
Rscript "3 - Model/2 - Analysis_MI.R" Wasting fpi      # Wasting, model with fpi
Rscript "3 - Model/2 - Analysis_MI.R" SevereWasting

# 4. Results (pass the same model tag as in step 3)
Rscript "4 - Results/0 - Summarizing_imputed_results.R" Wasting [fpi]
Rscript "4 - Results/1 - Plotting_estimates.R" Wasting [max_year] [fpi]
Rscript "4 - Results/3 - Wasting_SevereWasting_ratio.R" [fpi]

# Compare any two runs (e.g. Wasting without vs with fpi)
Rscript "4 - Results/2 - Comparison Plots.R" Wasting "Oct 2026" "fpi Oct 2026" labels="No FPI,FPI"
```

## Conventions

- **Paths.** Every script finds its own location with `this.path` and
  sets the working directory to the repository root, so it can be run
  from anywhere. All paths in the code are relative to the root
  (`Data/...`, `Utils/...`, `Figures/...`). The one exception is
  `Data/JME/<year>/Raw/Initial_cleaning.R`, which must be run from its
  own folder.
- **Marker.** Scripts in stages 2-4 take the indicator as the first
  command-line argument: `Wasting` or `SevereWasting` (earlier rounds also
  used `Stunting`, `Overweight`, `SevereOverweight`). It must match the
  folder names under `Data/Analysis files/`.
- **Round keys.** `year` / `month` (currently `"2026"` / `"Oct"`) are set
  at the top of each script and are used to build the input and output
  file names. Stage 4 also has `year_o` / `month_o` (keys used to *find*
  the data) separate from `year` / `month` (keys used to *name* the
  output). Update them in all stage 2-4 scripts at the start of a new
  round. The covariate label (`Oct2026`) is set in `02 - Imputation.qmd`
  (`out_label`) and `3 - Merging Survey_Covariate_Data.R`
  (`imputed_label`).
- **Model tag.** `2 - Analysis_MI.R` takes an optional second argument
  (e.g. `fpi`) that selects an alternative model and is appended to all
  output names. Pass the same tag to the stage 4 scripts.

## Folders

- **`1 - Preparing Covariates`**, **`2 - Preparing Primary Data`**,
  **`3 - Model`**, **`4 - Results`**: the pipeline (see each README).
- **`Utils`**: functions sourced by the pipeline.
  - `Programs_Oct_2026.R`: core model code (`cmnpe()`, prediction and
    output functions). Used by the imputation, model and results
    scripts. Changes here affect every downstream stage.
  - `Programs_Cleaning_SE.R`: helper functions for the sex cross-walk.
- **`Data`** (not on GitHub; kept in OneDrive):
  - `JME/<year>/Raw`, `JME/<year>/Cleaned`: survey input and cleaned
    survey data.
  - `IHME_covs`: raw covariate sources and the imputed covariate files.
  - `Country/Crosswalk_Oct_2026.xlsx`: country to modelling-region
    crosswalk.
  - `Merged/<year>`: survey data merged with covariates.
  - `Analysis files/<marker>`: model output, model comparison tables
    and pooled results. `Output data dictionary.xlsx` describes the
    columns of the results files.
- **`Figures`** (not on GitHub): figures written by stages 3 and 4.
- **`Write-up`**: documentation of the current round (background annex,
  description of the model changes).
- **`Round_changes_Oct2026.txt`**: detailed notes on what changed in the
  October 2026 round (covariates, bug fixes, model selection, final
  models, open issues).

Data, model output and figures (`*.rds`, `*.csv`, `*.xls(x)`, `*.RData`,
`Figures/`) are excluded by `.gitignore`, so a fresh clone cannot rerun
the pipeline without the OneDrive `Data/` folder. Scripts, data and
figures from earlier rounds that the current code no longer uses are in
`R_programs/Archive/` on OneDrive (the November 2026 clean-up is in
`Archive/mnf_update_cleanup_2026-11-03/`, which keeps the original
folder structure).

## Software

R (with `tidyverse`, `nlme`, `mice`, `readxl`, `writexl`, `this.path`,
`janitor`, `slider`, `doParallel`) and Quarto for the covariate step.
The parallel code uses forked processes (`mclapply`), so it runs on
macOS or Linux.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-FINARET20221595" class="csl-entry">

Finaret, Amelia B. 2022. “Advancing Nutritional Epidemiology by Linking
Datasets and Addressing Data Quality.” *The Journal of Nutrition* 152
(7): 1595–96. <a href="https://doi.org/10.1093/jn/nxac092">doi.org/10.1093/jn/nxac092</a>

</div>

<div id="ref-McLetal19" class="csl-entry">

McLain, Alexander C, Edward A Frongillo, Juan Feng, and Elaine Borghi.
2019. “Prediction Intervals for Penalized Longitudinal Models with
Multisource Summary Measures: An Application to Childhood Malnutrition.”
*Statistics in Medicine* 38 (6): 1002–12. <a href="https://doi.org/10.1002/sim.8024">doi.org/10.1002/sim.8024</a>

</div>

<div id="ref-Saretal22" class="csl-entry">

Saraswati, Chitra M, Elaine Borghi, João JR da Silva Breda, Monica C
Flores-Urrutia, Julianne Williams, Chika Hayashi, Edward A Frongillo,
and Alexander C McLain. 2022. “Estimating Childhood Stunting and
Overweight Trends in the European Region from Sparse Longitudinal Data.”
*The Journal of Nutrition* 152 (7): 1773–82. <a href="https://doi.org/10.1093/jn/nxac072">doi.org/10.1093/jn/nxac072</a>

</div>

</div>

## Contact

For questions or issues, please contact the repository maintainer.
