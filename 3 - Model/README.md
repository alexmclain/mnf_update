# Model

The statistical model is the heterogeneous penalized longitudinal mixed
model of McLain et al. (2019). It uses covariates together with country,
regional and global trends to predict the outcome for countries and
years with no data. Non-linear trends are captured with penalized
B-splines, with country-specific random intercepts (and slopes); the
link between penalized spline smoothing and linear mixed models (Currie
and Durban 2002) is used to estimate the penalized spline coefficients.
The heterogeneous error term, Var = sigma² (1 + gamma · SE²), accounts
for the survey standard errors, which differ by survey. The outcome is
modelled on the logit scale (SEs transformed with the delta method).

The model functions (`cmnpe()` and the prediction and output functions)
are in `Utils/Programs_Oct_2026.R`.

## Files

| Script | Purpose | Main output |
|----|----|----|
| `0 - Exploratory Analysis.R` | Plots of each covariate against the logit outcome by region, and the outcome over time (mean-imputed data, observed outcomes only) | `Figures/Exploratory/` |
| `1 - Compare_Models.R` | Hold-out comparison of candidate models (grid of fixed effects, penalized covariates, random slope, etc.); optional TOR comparisons with bootstrap intervals | `Data/Analysis files/<marker>/Model comparison <marker> <month> <year> <out_tag>.csv`, `TOR model comparison <marker> <month> <year>.xlsx` |
| `1b - Compare_FPI_interp.R` | Check of the food price index in the Wasting fpi model (interpolated or not; linear / quadratic / cubic; middle_year or integer year) | `... fpi_interp_Oct2026.csv` |
| `2 - Analysis_MI.R` | **Final analysis.** Fits the chosen model to each of the 20 imputed datasets | `Data/Analysis files/<marker>/Mi_files/` |

## Usage

```bash
Rscript "3 - Model/0 - Exploratory Analysis.R" Wasting
Rscript "3 - Model/1 - Compare_Models.R" Wasting
Rscript "3 - Model/2 - Analysis_MI.R" <marker> [model]
```

All scripts read `Data/Merged/<year>/<marker>_<month>_final_*.rds`
(made by `2 - Preparing Primary Data`). `year` and `month` are set at the
top of each script (`"2026"`, `"Oct"`). The marker defaults to `Wasting`
if no argument is given.

### 2 - Analysis_MI.R

- **marker**: `Wasting` or `SevereWasting` (branches for `Stunting` and
  `Overweight` are kept but use MCI, which is no longer in the covariate
  data).
- **model** (optional, Wasting only): `fpi` = the model with the food
  price index. The tag is appended to every output name (e.g.
  `Estimation Wasting_fpi.rds`) and must be passed to the scripts in
  `4 - Results`.
- `B <- 20` imputations. For each imputation the script fits the model,
  predicts every country-year-sex row and saves
  `Mi_files/Plot data for <marker>[_tag] imputation <j>.rds`. After the
  last imputation it saves `Mi_files/Estimation <marker>[_tag].rds`,
  which is read by `4 - Results/0 - Summarizing_imputed_results.R`.
- Section 1 of the loop sets the model for each marker: `model_formula`
  (fixed effects), `Pcov_data` / `referent_lev` (penalized covariates),
  `DF_P` (penalty knots), `DF_R` (random spline knot), `cov_mat`,
  `slope` and the time variable.

### Final models, October 2026

All models: logit outcome, penalized spline (`q.order = 2`), country
random intercept and slope, `cov_mat = "VC"`, time variable
`middle_year`, SMART set to 0 for prediction.

| Model | Fixed effects | Penalized covariates | Knots (`DF_P`) | Estimates |
|----|----|----|----|----|
| Wasting (`2 - Analysis_MI.R Wasting`) | Sex + Region + Region:Sex + SMART + SDI + log(war_terror) | none | 1994, 2002, 2010, 2018 | 1990-2026 |
| Wasting fpi (`... Wasting fpi`) | as above + poly(asinh(fpi_avg), 2) | region-specific (`P_Region`) | 2002, 2010, 2018 | 2001-2026 (FPI starts 2001) |
| Severe Wasting | Sex + Region + Region:Sex + SMART + poly(SDI, 3) + log(war_terror) | none | 1994, 2003, 2012, 2021 | 1990-2026 |

### Model selection (1 - Compare_Models.R)

Models are compared on hold-out prediction error, using the
mean-imputed data: the outcomes for 2021-2025 are set to missing, the
model is fitted to the remaining rows, and the held-out surveys are
predicted. The criterion is the RMSE on the prevalence (original) scale;
logit RMSE, bias and 95% prediction interval coverage are also reported.
Which candidate sets are run is controlled by switches near the top of
the script (`run_main_grid`, `run_year_top`, `run_tor`, ...), and each
set has its own `out_tag`. With `keep_old_results = TRUE`, results
saved by earlier runs with the same tag are added to the table. Models
are fitted in parallel with `mclapply` (`n_cores`).

The current comparison tables are the `grid2_Oct2026` (main grid),
`TOR_Oct2026` and `fpi_interp_Oct2026` files. See
`Round_changes_Oct2026.txt` (sections 3-5) for the results.

## Contact

For questions or issues regarding this folder, please contact the
repository maintainer.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-currie2002flexible" class="csl-entry">

Currie, Iain D, and Maria Durban. 2002. “Flexible Smoothing with
p-Splines: A Unified Approach.” *Statistical Modelling* 2 (4): 333–49.
<a href="https://doi.org/10.1191/1471082x02st039ob">doi.org/10.1191/1471082x02st039ob</a>.

</div>

<div id="ref-McLetal19" class="csl-entry">

McLain, Alexander C, Edward A Frongillo, Juan Feng, and Elaine Borghi.
2019. “Prediction Intervals for Penalized Longitudinal Models with
Multisource Summary Measures: An Application to Childhood Malnutrition.”
*Statistics in Medicine* 38 (6): 1002–12.
<a href="https://doi.org/10.1002/sim.8024">doi.org/10.1002/sim.8024</a>

</div>

</div>
