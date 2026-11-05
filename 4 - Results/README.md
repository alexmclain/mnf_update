# Results

The scripts in this folder pool the imputed analyses, plot the
estimates, and check the consistency of the Wasting and Severe Wasting
estimates.

## Files and usage

```bash
Rscript "4 - Results/0 - Summarizing_imputed_results.R" <marker> [model]
Rscript "4 - Results/1 - Plotting_estimates.R" <marker> [max_year] [model]
Rscript "4 - Results/2 - Comparison Plots.R" <marker> <runA> <runB> [key=value ...]
Rscript "4 - Results/3 - Wasting_SevereWasting_ratio.R" [model]
```

| Script | Reads | Writes |
|----|----|----|
| `0 - Summarizing_imputed_results.R` | `Data/Analysis files/<marker>/Mi_files/Estimation <marker>[_model].rds` and the per-imputation plot data; `Data/Merged/<year>/<marker>_<month>_final_multiple_impute.rds` | `Data/Analysis files/<marker>/<marker> results <model> <month> <year>.csv` |
| `1 - Plotting_estimates.R` | the results file above and the merged data | `Figures/<marker>/<month><year>[_<model>]/` |
| `2 - Comparison Plots.R` | the results files of two runs | `Figures/<marker>/Comparisons/<marker> <A> vs <B> <sex>.pdf`, `Data/Analysis files/<marker>/Comparisons/<marker> <A> vs <B>.csv` |
| `3 - Wasting_SevereWasting_ratio.R` | the Wasting and SevereWasting results files | `Figures/Wasting/<month><year>[_<model>]/log Wasting to SevereWasting ratio ....pdf`, `Data/Analysis files/Wasting/SevereWasting greater than Wasting ....xlsx` |

Run `0` and `1` for each marker (and each Wasting model), and `3` after
`0` has been run for both markers.

### Arguments

- **marker**: `Wasting` or `SevereWasting`.
- **model** (optional): the model tag used in
  `3 - Model/2 - Analysis_MI.R` (e.g. `fpi`). Omit it for the default
  model. In `3 - Wasting_SevereWasting_ratio.R` it selects the Wasting
  model; Severe Wasting always uses the default model.
- **max_year** (optional, `1 - Plotting_estimates.R` only): plot only the
  years up to and including `max_year`, e.g.
  `Rscript "4 - Results/1 - Plotting_estimates.R" Wasting 2025`.
  `through <year>` is then appended to the figure names so the
  full-range figures are not overwritten. The two optional arguments can
  be given in either order.
- At the top of each script: `month` / `year` name the output
  (`"Oct"`, `"2026"`), and `month_o` / `year_o` find the input data.

## Details

### 0 - Summarizing_imputed_results.R

Reads the predictions from each of the 20 imputed analyses and pools
them with Rubin's rules: the total variance is the mean within-imputation
variance plus (B + 1)/B times the between-imputation variance. It
outputs, for every country, year and sex, the point estimates and the
95% confidence and prediction intervals (back-transformed from the logit
scale), along with the survey data. The columns of the results file are
described in `Data/Analysis files/Output data dictionary.xlsx`.

### 1 - Plotting_estimates.R

Country-level plots by region. For each sex (Overall, Female, Male):

- Predicted values with 95% confidence and prediction intervals and the
  survey estimates.
- Predictions from the fixed effects only; the fixed and penalized
  effects; and the fixed, penalized and random effects (the full
  estimates). These show what is driving the results.
- Comparison of female, male and overall estimates.

Estimates are plotted against `middle_year`. The output folder is
created if it does not exist.

### 2 - Comparison Plots.R

Compares the pooled estimates of any two runs, e.g. two models in the
same round or the same model in two rounds. Each run is given as it
appears in the results file name, `"[model] <month> <year>"`, or as the
path to any results `.csv` (e.g. an archived run). Example, the Wasting
model without vs with the food price index:

```bash
Rscript "4 - Results/2 - Comparison Plots.R" Wasting "Oct 2026" "fpi Oct 2026" labels="No FPI,FPI" max_year=2025
```

Options (`key=value`): `labels=A,B` (names used in the figures and file
names; default: the run strings), `sex=` (`Overall`, `Female`, `Male`;
default `Overall`), `max_year=` (default: all years), `ci=` (draw 95%
confidence bands; default `TRUE`), `diff_pp=` (a survey whose value
differs by more than this many percentage points is flagged as changed;
default 0.5).

The PDF has a page with the distribution over countries of the
difference B − A by year, a page with the 30 country-years with the
largest differences, and one page per region with both runs' estimates
by country and the survey data. A survey counts as used by a run if
that run has a prediction for it (e.g. the fpi model does not use surveys
before 2001). Survey points are marked as used by both runs, value
changed, or used by one run only (new or dropped surveys, or surveys
outside a run's years). The summary
is also printed to the console. The CSV has the annual estimates and
confidence limits of both runs and their difference (percentage points)
for every country, year and sex.

### 3 - Wasting_SevereWasting_ratio.R

Plots log(Wasting / Severe Wasting) of the pooled Overall estimates by
country (annual rows only), one page per region, with a dashed line at 0
(Wasting = Severe Wasting). It also exports every country-year where the
Severe Wasting estimate is larger than the Wasting estimate to Excel.

## Contact

For questions or issues regarding this folder, please contact the
repository maintainer.
