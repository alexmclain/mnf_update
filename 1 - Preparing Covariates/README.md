# Preparing Covariates

This folder builds the country-by-year covariate data used in the model
and multiply imputes the missing values (20 imputations). It has two
Quarto documents, run in order. Rendering each one also writes an HTML
report next to it (`*.html`, not tracked in git).

| File | Input | Output (in `Data/IHME_covs/`) |
|----|----|----|
| `01 - Covariate Cleaning.qmd` | raw covariate sources (below) | `FPI_GDP_IHMESHOCK_WPP_SDI.csv` (plus `FPI_GDP_IHMESHOCK.csv` and the `country_crosswalk*.csv` merge checks) |
| `02 - Imputation.qmd` | `FPI_GDP_IHMESHOCK_WPP_SDI.csv`, `Tidy_World_Devel_Indic.rds`, the JME raw files (country list) | `Multiple_Imputed_Oct2026.rds`, `Single_Impute_Oct2026.rds`, `Imputation_Oct2026.RData` |

```bash
quarto render "1 - Preparing Covariates/01 - Covariate Cleaning.qmd"
quarto render "1 - Preparing Covariates/02 - Imputation.qmd"
```

Both documents set their own working directory (`01` works inside
`Data/IHME_covs`, `02` at the repository root), so no `wd` needs to be
edited. `02 - Imputation.qmd` runs `mice` in parallel with forked
processes (macOS/Linux). Setting the environment variable `IMP_B` to a
small number (e.g. `IMP_B=2`) gives a quick test run.

## Covariates

| Variable | Description | Source file | Observed years |
|----|----|----|----|
| `SDI` | Socio-demographic index (GBD 2023) | `sdi_gbd_2023.csv` | through 2023 |
| `war_terror` | IHME war and terrorism mortality covariate (GBD 2023) | `IHME_GBD_2023_COV_1980_2023_MX_WARTERROR_10YEARS_Y2025M01D08.xlsx` | through 2023 |
| `shock` | IHME shock mortality covariate (GBD 2023) | `IHME_GBD_2023_COV_1980_2023_MX_SHOCK_LAST_10YEARS_Y2025M01D08.xlsx` | through 2023 |
| `log_gdp_pc` / `gdp_pc` | GDP per capita, current US$ (World Bank `NY.GDP.PCAP.CD`); imputed on the log scale | `API_NY.GDP.PCAP.CD_DS2_en_csv_v2_468810.csv` | through 2025 |
| `gdp` | GDP per capita growth, annual % (World Bank `NY.GDP.PCAP.KD.ZG`); predictor in the imputation only, not imputed | `API_NY.GDP.PCAP.KD.ZG_DS2_en_excel_v2_51.xls` | |
| `fpi_avg` | FAO food consumer price index, yearly average (`fpi_prev5yr_avg`, `fpi_anom` are derived from it) | `ConsumerPriceIndices_E_All_Data.csv`, `ConsumerPriceIndices_E_All_Data_(Normalized).csv` | 2001-2026 |
| `dpt_imm` | DPT immunization, % of children 12-23 months (World Bank `SH.IMM.IDPT`) | `Immun_skilled_WB_data_Oct26.csv` | through 2024 |
| WPP indicators | Population density, median age, CBR, TFR, births 15-19, CDR, life expectancy, IMR, Q5 (UN WPP 2024); predictors in the imputation only | `WPP2024_Demographic_Country_Indicators_Medium.csv` | |

Linking files: `Country_connectors.xlsx` (country names and codes across
sources) and `GBD_2021_GBD_LOCATION_HIERARCHY_Y2024M05D16.xlsx` (GBD
location ids). The IHME files are joined to the location hierarchy on
`location_id` only, because some country names in the IHME files have
broken encoding.

MCI (used in earlier rounds) is no longer part of the covariate data.
Of these covariates, the October 2026 Wasting and Severe Wasting models
use `SDI`, `war_terror` and (one Wasting model) `fpi_avg`; the others are
available for model comparison or used as predictors in the imputation.

## Imputation method (`02 - Imputation.qmd`)

Each variable is imputed on a scale where it is roughly symmetric
(`SDI`: identity, bounded to [0.001, 1]; `shock`, `war_terror`: log;
`fpi_avg`: asinh; `log_gdp_pc`: log; `dpt_imm`: logit of the
percentage). For each variable:

1. **Country-level means.** Impute the variable together with GDP and
   the WPP indicators with `mice`, and compute each country's mean over
   the observed years in every imputation.
2. **Mean-centered imputation.** Center the variable by country, reshape
   to wide format (one column per year), impute again with `mice`; the
   imputed value is the country mean plus the centered value.
3. **Years after the last observed year.** Fit the penalized
   longitudinal model (`cmnpe()` from `Utils/Programs_Oct_2026.R`) to the
   recent trend and draw the future values (through `last_year = 2026`)
   from its predictive distribution. Skipped for `fpi_avg`.

Observed values are always kept. The single-imputation file is the
average over the imputations of each numeric variable and is used for
model comparison and exploratory analysis.

### Settings (top of `02 - Imputation.qmd`)

- `last_year`: last year imputed (2026).
- `jme_year`: year folder of the JME raw data used for the country list.
- `out_label`: appended to the output names (`"Oct2026"`). It must match
  `imputed_label` in `2 - Preparing Primary Data/3 - Merging
  Survey_Covariate_Data.R`.
- `B` (20 imputations), `seed_base`.

## Contact

For questions or issues regarding this folder, please contact the
repository maintainer.
