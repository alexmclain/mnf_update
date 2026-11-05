## Compare alternative model formulations on the mean-imputed dataset.
## Only rows with an observed Y are used (for fitting AND for prediction).
##
## Usage:  Rscript "3 - Model/1 - Compare_Models.R" [marker]      (default: Wasting)
## Edit the MODELS list below to add/remove candidates. Each candidate can
## differ in fixed effects (formula), penalized covariates (pcov / referent_lev),
## penalty knots (DF_P), random knot (DF_R), covariance (cov_mat) and slope.
##
## Each model is fitted once: outcomes for `holdout_years` are set to missing,
## the model is fitted to the remaining (training) rows, and cmnpe() predicts
## the held-out rows. Models are compared on their held-out prediction error.
##
## Output: a table sorted by held-out RMSE on the prevalence scale, printed + saved to
##   Data/Analysis files/<marker>/Model comparison <marker> <month> <year> <out_tag>.csv)
## With run_tor = TRUE (TOR deliverables), also an Excel workbook
##   Data/Analysis files/<marker>/TOR model comparison <marker> <month> <year>.xlsx
## with bootstrap intervals, prediction-interval coverage and subgroup errors.

remove(list=ls())

library(this.path)
wd <- dirname(this.path::here())
setwd(wd)
source("Utils/Programs_Oct_2026.R")
library(tidyverse)

marker <- as.character(commandArgs(trailingOnly = TRUE))
if(length(marker)==0) marker <- "Wasting"
year  <- "2026"
month <- "Oct"
TRANS   <- FALSE
q.order <- 2
min_year <- 1990          # keep year > min_year, as in the main analysis
holdout_years <- c(2021:2025)   # validation: outcomes set missing for these years
## holdout label saved with the results; written as "first to last" because Excel
## reads "2021,2022,..." as a number when the CSV is opened and saved there
hold_key <- paste(min(holdout_years), "to", max(holdout_years))
keep_old_results <- TRUE   # add results saved by earlier runs (same marker/month/year) to this one
n_cores <- 6               # models are fitted in parallel (parallel::mclapply)
path_out <- paste0("Data/Analysis files/", marker, "/")

##############################################################################
#### 1. Candidate models  ####################################################
##############################################################################
## Fields per model:
##   formula       fixed effects (one-sided formula; Sex is coded 0/1/-1)
##   pcov          NULL (default) = no penalized covariates, or "yes" to use P_Region
##                 (region-specific penalized splines = random-effect structure)
##   referent_lev  referent Region level for P_Region (used when pcov="yes")
##   DF_P          knots of the penalized spline
##   cov_mat       "VC" (diagonal) / "UN" (unstructured) / "CS" (comp. symm.)
##   slope         TRUE = random-slope versions of the fitting functions
##   zero_covs     covariates zeroed for prediction (only affects plot_data)
##   time_var      time variable for the splines: "year" (integer survey year) or
##                 "middle_year" (continuous mid-point of the survey)
##   B_off         upper boundary-knot offset: the upper boundary knot is
##                 last training time + B_off, but never below the
##                 last time to be predicted (so held-out rows are inside the
##                 knots). Only used without pcov (pcov models use + 10).
##                 The lower boundary knot is always floor(first observed time) - 2.
##
## Defaults (below) are filled in for any field you leave out.
defaults <- list(referent_lev = 2,
                 DF_P = seq(1993, 2023, 7), cov_mat = "VC", slope = TRUE,
                 zero_covs = "SMART", time_var = "year", B_off = 2)

## Tag added to the output file names; a new tag keeps earlier results out of
## this comparison. "midyear_lb2" files were made with Programs_Feb_2020.R,
## whose predictions used the wrong residual variance, so they are not reused.
out_tag <- "grid_Oct2026"

## ---- SevereWasting first run: full grid of the four big questions ----------
##   pcov   : "yes" (region-specific penalized splines) vs NULL
##   fpi    : fpi_avg (quadratic poly of asinh) in the model or not
##   sexReg : Region:Sex interaction or not
##   knots  : k5 = every 5 years (seq(1993,2023,5)), k9 = every 9 years (seq(1993,2022,9))
## Common terms: Sex + Region + SMART + cubic SDI + quadratic log(war_terror)
## (the covariates that ended up in the Wasting model; MCI_5_yr, used in the old
##  SevereWasting block of 1 - Analysis_MI_sexf.R, is no longer in the merged data).
## Models are named  <pcov|nopcov>_<fpi|nofpi>_<sexReg|nosexReg>_<k5|k9>.
base_terms <- "Sex + Region + SMART + I(poly(SDI,3)) + I(poly(log(war_terror),2))"
make_formula <- function(fpi, sexReg){
  as.formula(paste("~", base_terms,
                   if(sexReg) "+ Region:Sex",
                   if(fpi)    "+ I(poly(asinh(fpi_avg),2))"))
}
knot_opts <- list(k7 = seq(1993, 2023, 7), k9 = seq(1993, 2022, 9))

MODELS <- list()
run_old_grid <- FALSE      # the SevereWasting grid above (already run on the earlier data)
if(run_old_grid) for(pc in c(TRUE, FALSE)) for(fpi in c(FALSE, TRUE)) for(sr in c(TRUE, FALSE)) for(k in names(knot_opts)){
  nm <- paste(if(pc) "pcov" else "nopcov", if(fpi) "fpi" else "nofpi",
              if(sr) "sexReg" else "nosexReg", k, sep = "_")
  MODELS[[nm]] <- list(formula = make_formula(fpi, sr),
                       pcov = if(pc) "yes" else NULL,
                       DF_P = knot_opts[[k]])
}

## The earlier SevereWasting specification, minus MCI_5_yr (not in the data now):
if(run_old_grid) MODELS$earlier_script_noMCI <- list(
  formula = ~ Sex + Region + Region:Sex + SDI,
  pcov = "yes", DF_P = seq(1993, 2023, 7))

## ---- Time variable and boundary knots (final models of 2 - Analysis_MI.R) ----
## The final Wasting / SevereWasting specifications, each fitted with
##   time : year (integer) vs middle_year (continuous survey mid-point)
##   bk   : upper boundary-knot offset; u2 is the current setting, the others
##          move the knot further from the data to soften the boundary
##          (end-of-series) behaviour of the splines.
## Models are named  <year|mid>_<bk>, e.g. mid_u2.
## The penalized-spline knots (DF_P) used here are the ones set in final_spec
## below (per marker); they override DF_P in `defaults`.
final_spec <- if(grepl("SevereWasting", marker, ignore.case = TRUE)){
  list(formula = ~ Sex + Region + SMART + I(poly(SDI,3)) + I(poly(log(war_terror),2)),
       DF_P = seq(1993, 2022, 8))
} else if(grepl("Wasting", marker, ignore.case = TRUE)){
  list(formula = ~ Sex + Region + Region:Sex + SMART + I(poly(SDI,3)) + I(poly(log(war_terror),2)),
       DF_P = seq(1993, 2024, 8))
}
bk_opts <- list(u4 = 4, u6 = 6, u8 = 8, u10 = 10)
run_bk_grid <- FALSE       # the time-variable / boundary-knot comparison above (already run)
if(run_bk_grid && !is.null(final_spec)) for(tv in c("year", "middle_year")) for(bk in names(bk_opts)){
  nm <- paste(if(tv == "year") "year" else "mid", bk, sep = "_")
  MODELS[[nm]] <- c(final_spec, list(time_var = tv, B_off = bk_opts[[bk]]))
}

## ---- Oct 2026 grid: covariate forms and model structure --------------------
## Start from the final model in 2 - Analysis_MI.R for each marker (middle_year,
## upper boundary offset 10 for Wasting / 4 for SevereWasting, cov_mat "VC",
## SMART zeroed for prediction), with DF_P = seq(1994, 2023, 8) for both, and
## fit every combination of
##   sdi    : SDI linear / quadratic / cubic (orthogonal poly)
##   war    : log(war_terror) linear / quadratic / cubic (orthogonal poly)
##   pcov   : region-specific penalized splines (P_Region) or not
##   fpi    : + quadratic poly of asinh(fpi_avg) or not
##   slope  : random country slope or not
##   sexReg : + Region:Sex or not
## Models are named  sdi<k>_war<k>_<pcov|nopcov>_<fpi|nofpi>_<slope|noslope>_<sexReg|nosexReg>.
poly_term <- function(x, k) if(k == 1) x else paste0("I(poly(", x, ",", k, "))")
## Rerun 26 Oct 2026 (tag grid2_Oct2026) on the CIV/TUR-fixed covariates, with the
## final DF_P / boundary knots of 2 - Analysis_MI.R for each marker (not varied),
## no knot shift, and SMART not zeroed for prediction (held-out surveys are
## predicted as the survey they were).
grid_sw <- grepl("SevereWasting", marker, ignore.case = TRUE)
grid_B_off <- if(grid_sw) 4 else 10
grid_DF_P  <- if(grid_sw) seq(1994, 2023, 9) else seq(1994, 2023, 8)
run_main_grid <- FALSE     # the Oct 2026 grid (already run; results in the grid2_Oct2026 file)
## Top 20 grid models (by RMSE_orig in the grid2_Oct2026 file) refitted with the
## integer survey year instead of middle_year; named <grid model>_year and added
## to the same file.
## year_top_fpi: which grid models to take the top n_year_top from:
## "fpi" / "nofpi" (models with / without fpi_avg) or "all".
## (26 Oct: the top 20 overall, all fpi models, were run first; then the top 20 nofpi.)
run_year_top <- TRUE
n_year_top   <- 20
year_top_fpi <- "nofpi"
if(run_main_grid || run_year_top){
  out_tag <- "grid2_Oct2026"
  keep_old_results <- run_year_top   # add to the existing grid results
}
grid_specs <- list()
for(sdi in 1:3) for(war in 1:3) for(pc in c(FALSE, TRUE)) for(fpi in c(FALSE, TRUE))
  for(sl in c(TRUE, FALSE)) for(sr in c(TRUE, FALSE)){
  nm <- paste0("sdi", sdi, "_war", war, "_", if(pc) "pcov" else "nopcov", "_",
               if(fpi) "fpi" else "nofpi", "_", if(sl) "slope" else "noslope", "_",
               if(sr) "sexReg" else "nosexReg")
  grid_specs[[nm]] <- list(
    formula  = as.formula(paste("~ Sex + Region + SMART +", poly_term("SDI", sdi), "+",
                                poly_term("log(war_terror)", war),
                                if(sr) "+ Region:Sex", if(fpi) "+ I(poly(asinh(fpi_avg),2))")),
    pcov     = if(pc) "yes" else NULL,
    slope    = sl,
    DF_P     = grid_DF_P,
    time_var = "middle_year",
    B_off    = grid_B_off,
    zero_covs = NULL)
}
if(run_main_grid) MODELS <- c(MODELS, grid_specs)
if(run_year_top){
  grid_res <- read.csv(paste0(path_out, "Model comparison ", marker, " ", month, " ", year,
                              " grid2_Oct2026.csv"), stringsAsFactors = FALSE)
  top_nm <- grid_res |> filter(time_var == "middle_year") |>
    filter(year_top_fpi == "all" | grepl("_fpi_", model) == (year_top_fpi == "fpi")) |>
    arrange(RMSE_orig) |>
    head(n_year_top) |> pull(model)
  for(nm in top_nm) MODELS[[paste0(nm, "_year")]] <- modifyList(grid_specs[[nm]], list(time_var = "year"))
}

## ---- TOR comparison: one change at a time from the final model -------------
## For the TOR deliverables (seasonality, conflict/shocks, model flexibility).
## `final` is the model in 2 - Analysis_MI.R for each marker; every other model
## changes one thing. SMART is NOT zeroed (zero_covs = NULL), so each held-out
## survey is predicted as the survey it was (SMART or not).
## Extra outputs (section 5): bootstrap CI of the RMSE difference from the
## final model, 95% prediction-interval coverage, and error by subgroup,
## written to an Excel workbook.
run_tor <- FALSE    # TOR comparison (already run; results in the TOR workbooks)
if(run_tor){
  out_tag <- "TOR_Oct2026"
  keep_old_results <- FALSE   # a model that fails now must not show an earlier run's result
  sw <- grepl("SevereWasting", marker, ignore.case = TRUE)
  tor_terms <- "Sex + Region + Region:Sex + SMART + I(poly(SDI,3))"
  tor_f <- function(...) as.formula(paste("~", tor_terms, ...))
  tor_final <- list(
    formula = tor_f("+ log(war_terror)"),
    pcov = if(sw) NULL else "yes",
    slope = TRUE,
    DF_P = if(sw) seq(1994, 2023, 9) else seq(1994, 2023, 8),
    time_var = "middle_year",
    B_off = if(sw) 4 else 10,
    zero_covs = NULL,
    knot_shift = TRUE)   # validation knots keep the full-analysis distance to the end of the data (fit_core)
  tor_mod <- function(...){ s <- tor_final; ch <- list(...); for(nm in names(ch)) s[nm] <- list(ch[[nm]]); s }

  ## TOR item and a short description for each model (used in the workbook)
  tor_info <- tribble(
    ~model,                ~tor_item,        ~description,
    "final",               "Final model",    "Final model of 2 - Analysis_MI.R",
    "season_year",         "Seasonality",    "Integer survey year instead of middle_year (survey mid-point)",
    "season_harmonic",     "Seasonality",    "+ annual harmonic of survey mid-month (sin + cos), common to all regions",
    "season_harmonic_reg", "Seasonality",    "+ annual harmonic of survey mid-month by Region (region-specific seasonality)",
    "conflict_war2",       "Conflict/shocks","log(war_terror) quadratic instead of linear",
    "conflict_warlag1",    "Conflict/shocks","log(war_terror) lagged 1 year instead of current year",
    "conflict_war_lag1",   "Conflict/shocks","+ log(war_terror) lagged 1 year (current and lag)",
    "conflict_shock",      "Conflict/shocks","+ log(shock) (IHME natural disaster / shock mortality)",
    "conflict_none",       "Conflict/shocks","No conflict term (war_terror removed)",
    "flex_noslope",        "Flexibility",    "No random country slope (random intercept only)",
    "flex_pcov_switch",    "Flexibility",    if(sw) "+ region-specific penalized splines (pcov)" else "No region-specific penalized splines (no pcov)",
    "flex_nosexReg",       "Flexibility",    "No Region:Sex interaction",
    "flex_nocovs",         "Flexibility",    "No SDI or war_terror (Sex, Region, Region:Sex, SMART and splines only)"
  )

  MODELS <- list(
    final               = tor_final,
    season_year         = tor_mod(time_var = "year"),
    season_harmonic     = tor_mod(formula = tor_f("+ log(war_terror) + season_s + season_c")),
    season_harmonic_reg = tor_mod(formula = tor_f("+ log(war_terror) + Region:season_s + Region:season_c")),
    conflict_war2       = tor_mod(formula = tor_f("+ I(poly(log(war_terror),2))")),
    conflict_warlag1    = tor_mod(formula = tor_f("+ log(war_lag1)")),
    conflict_war_lag1   = tor_mod(formula = tor_f("+ log(war_terror) + log(war_lag1)")),
    conflict_shock      = tor_mod(formula = tor_f("+ log(war_terror) + log(shock)")),
    conflict_none       = tor_mod(formula = tor_f("")),
    flex_noslope        = tor_mod(slope = FALSE),
    flex_pcov_switch    = tor_mod(pcov = if(sw) "yes" else NULL),
    flex_nosexReg       = tor_mod(formula = ~ Sex + Region + SMART + I(poly(SDI,3)) + log(war_terror)),
    flex_nocovs         = tor_mod(formula = ~ Sex + Region + Region:Sex + SMART)
  )
}

## Add hand-made candidates here, e.g.
# MODELS$my_model <- list(formula = ~ Sex + Region + SMART + SDI,
#                         pcov = "yes", referent_lev = 3, DF_P = seq(1993, 2023, 5))
## To re-run only some models, subset, e.g.  MODELS <- MODELS[c("pcov_nofpi_sexReg_k5")]

##############################################################################
#### 2. Data (mean-imputed, observed Y only) ##################################
##############################################################################
raw <- readRDS(paste0("Data/Merged/", year, "/", marker, "_", month,
                      "_final_mean_impute.rds"))

## war_terror of the previous year (from the annual rows, one per country-year)
war_lag <- raw |>
  filter(is.na(UNICEFSurveyID)) |>
  distinct(ISO.code, year, war_terror) |>
  transmute(ISO.code, year = year + 1, war_lag1 = war_terror)

all_data <- raw |>
  left_join(war_lag, by = c("ISO.code", "year")) |>
  mutate(
    ## Annual harmonic of the survey mid-point's time of year. Surveys whose
    ## middle_year equals the integer year have no month information, so they
    ## get 0 (no seasonal shift).
    season_frac = middle_year - year,
    season_s = if_else(season_frac == 0, 0, sin(2*pi*season_frac)),
    season_c = if_else(season_frac == 0, 0, cos(2*pi*season_frac))
  ) |>
  mutate(
    Sex = case_when(Sex == "Both" ~ 0, Sex == "Female" ~ 1, Sex == "Male" ~ -1),
    Region = factor(Region),
    SMART = case_when(ShortSource == "SMART" ~ 1, TRUE ~ 0),
    Surveillance = case_when(ShortSource == "Surveillance" ~ 1, TRUE ~ 0)
  ) |>
  rename("Y" = "Point.Estimate.NS", "Y_all" = "Point.Estimate.Imp",
         "SE_var" = "SE_val") |>
  filter(year > min_year, !is.na(Y)) |>       # <- observed Y only
  arrange(country, year) |>
  mutate(rid = row_number())                  # unique row id (used to match rows across models)

cat("Rows with observed Y:", nrow(all_data), " Countries:",
    length(unique(all_data$country)), "\n")

##############################################################################
#### 3. Helpers ###############################################################
##############################################################################
clean_design <- function(cov_data, y){
  ## Same checks as the main analysis: rank, zero variance, X'X deficiency
  if(qr(cov_data)$rank != ncol(cov_data)){
    qrX <- qr(cov_data)
    dep <- qrX$pivot[(qrX$rank + 1):ncol(cov_data)]
    cat("   Design matrix not full rank; dropping", colnames(cov_data)[dep], "\n")
    cov_data <- cov_data[, -dep, drop = FALSE]
  }
  zv <- apply(cov_data, 2, function(x) var(x, na.rm = TRUE) == 0)
  if(any(zv)){
    cat("   Zero variance; dropping", colnames(cov_data)[zv], "\n")
    cov_data <- cov_data[, !zv, drop = FALSE]
  }
  fit <- lm(y ~ cov_data)
  bad <- is.na(coef(fit))[-1]
  if(any(bad)){
    cat("   X'X deficient; dropping", colnames(cov_data)[bad], "\n")
    cov_data <- cov_data[, !bad, drop = FALSE]
  }
  cov_data
}

## Builds the data for one candidate model and fits it with cmnpe().
## Rows kept: observed outcome AND non-missing values of every variable the
## model uses (so a model with fpi_avg can have fewer rows than one without).
## holdout = vector of years whose outcome is set to missing for fitting; those
## rows stay in the data so cmnpe() returns predictions for them.
fit_core <- function(s, holdout = NULL){
  need <- intersect(all.vars(s$formula), names(all_data))
  dat <- all_data[complete.cases(all_data[, c(need, "Y", s$time_var)]), ]
  ## time used by the splines; `year` itself is kept for the holdout and output
  dat$time <- dat[[s$time_var]]
  ## same (locale-aware, stable) ordering that cmnpe() applies, so rows line up
  dat <- dat[order(dat$country, dat$time), ]
  dat$Y_obs <- dat$Y                                   # untouched outcome
  if(!is.null(holdout)) dat$Y[dat$year %in% holdout] <- NA

  Pcov_data <- NULL
  if(!is.null(s$pcov)){
    dat$P_Region <- relevel(dat$Region, ref = s$referent_lev)
    Pcov_data <- model.matrix(~P_Region, data = dat,
                  contrasts.arg = list(P_Region = diag(nlevels(dat$P_Region))))[,-1]
  }

  data_w_out <- dat |>
    dplyr::select(country, year = time, Y, SE_var) |>
    mutate(SE_pred = 0,
           SE_var = SE_var*((1/Y) + 1/(1-Y)),
           Y = log(Y/(1-Y)))
  ## SE_var for held-out rows is computed from their (hidden) Y above; cmnpe only
  ## uses SE_var of rows with an outcome when fitting, but blank it to be safe.
  data_w_out$SE_var[is.na(data_w_out$Y)] <- NA

  yr_fit <- data_w_out$year[!is.na(data_w_out$Y)]       # training years only
  if(is.null(Pcov_data)){
    DF_R <- quantile(yr_fit, probs = 0.5)
    B.knots <- c(floor(min(yr_fit)) - 2, max(max(yr_fit) + s$B_off, max(data_w_out$year)))
  } else {
    DF_R <- NULL
    B.knots <- c(floor(min(yr_fit)) - 2, max(yr_fit) + 10)
  }

  ## Penalized-spline knots. With knot_shift = TRUE the knots of a validation fit
  ## are moved back by the length of the held-out period (last observed time
  ## minus last training time), so the last knot is as far before the end of the
  ## training data as it is before the end of the data in the full analysis.
  ## Knots that land within 1 year of the lower boundary knot are dropped.
  DF_P <- s$DF_P
  if(!is.null(holdout) && isTRUE(s$knot_shift)){
    DF_P <- DF_P - (max(dat$time) - max(yr_fit))
    DF_P <- DF_P[DF_P > B.knots[1] + 1]
  }

  cov_data <- as.matrix(data.frame(model.matrix(s$formula, data = dat)[,-1]))
  cov_data <- clean_design(cov_data, data_w_out$Y)   # lm() ignores held-out (NA) Y

  ## cmnpe() needs a Sex column for plot_data. If Sex is already a covariate,
  ## cmnpe cbinds it from cov_data; adding it here too would duplicate the
  ## column (cmnpe only de-duplicates when Pcov_data is non-NULL).
  if(!"Sex" %in% colnames(cov_data)) data_w_out$Sex <- dat$Sex

  zc <- s$zero_covs[s$zero_covs %in% colnames(cov_data)]
  if(length(zc) == 0) zc <- NULL

  est <- try(cmnpe(data_w_out, DF_P, DF_R, B.knots, q.order,
                   cov_data = cov_data, Pcov_data = Pcov_data,
                   cov_mat = s$cov_mat, plots = FALSE, TRANS = TRANS,
                   zero_covs = zc, slope = s$slope), silent = TRUE)
  if(inherits(est, "try-error")){
    cat("   Model error:", conditionMessage(attr(est, "condition")), "\n")
    return(NULL)
  }
  list(est = est, dat = dat, B.knots = B.knots, DF_P = DF_P)
}

get_spec <- function(spec){
  s <- defaults
  for(nm in names(spec)) s[nm] <- list(spec[[nm]])     # list() keeps explicit NULLs
  s
}

## Validation: hide the outcome for `holdout` years, refit, predict those rows.
## Returns one row per held-out observation (observed and predicted, both
## on the logit and the original scale).
validate_one <- function(name, spec, holdout = holdout_years){
  s <- get_spec(spec)
  cat("\n== Validating:", name, "(holdout years:", holdout, ") ==\n")
  core <- fit_core(s, holdout = holdout)
  if(is.null(core)) return(NULL)
  pd  <- core$est$plot_data
  dat <- core$dat
  ## cmnpe sorts by country, year (stable); dat is already sorted that way.
  stopifnot(nrow(pd) == nrow(dat), all(pd$country == dat$country), all(pd$year == dat$time))
  hold <- dat$year %in% holdout
  if(!any(hold)){ cat("   No held-out observations.\n"); return(NULL) }
  ## Predictive SD for a new survey (logit scale): uncertainty of the mean
  ## (sigma_T) plus the residual variance with the survey's own sampling SE,
  ## sigma2*(1 + gamma*SE^2). (plot_data's sigma_Y_est uses SE = 0 for rows
  ## without an outcome, so it is rebuilt here.)
  mod <- core$est$result$model
  sig2 <- mod$sigma^2
  gam <- exp(c(coef(mod$modelStruct$varStruct)))
  y_h <- dat$Y_obs[hold]
  se_logit <- dat$SE_var[hold]*(1/y_h + 1/(1 - y_h))
  data.frame(
    model = name, rid = dat$rid[hold], country = dat$country[hold], year = dat$year[hold],
    Sex = dat$Sex[hold],
    obs_logit  = qlogis(dat$Y_obs[hold]),
    pred_logit = pd$pred[hold],
    obs  = dat$Y_obs[hold],
    pred = plogis(pd$pred[hold]),
    pred_sd_logit = sqrt(pd$sigma_T_est[hold]^2 + sig2*(1 + gam*se_logit^2)),
    Region = as.character(dat$Region[hold]),
    ShortSource = dat$ShortSource[hold],
    B_knots = paste(round(core$B.knots, 2), collapse = ", "),
    DF_P_used = paste(round(core$DF_P, 2), collapse = ", "),
    n_train = sum(!is.na(dat$Y)),
    stringsAsFactors = FALSE
  )
}

summarise_validation <- function(preds){
  ## Metrics for one model's held-out predictions
  if(!"pred_sd_logit" %in% names(preds)) preds$pred_sd_logit <- NA_real_
  preds |>
    summarise(n_holdout = n(),
              RMSE_logit = sqrt(mean((obs_logit - pred_logit)^2)),
              MAE_logit  = mean(abs(obs_logit - pred_logit)),
              bias_logit = mean(pred_logit - obs_logit),
              RMSE_orig  = sqrt(mean((obs - pred)^2)),
              MAE_orig   = mean(abs(obs - pred)),
              bias_orig  = mean(pred - obs),
              ## share of held-out surveys inside the 95% prediction interval
              ## (NA for predictions saved before pred_sd_logit was added)
              PI95_coverage = mean(abs(obs_logit - pred_logit) <= 1.96*pred_sd_logit))
}

##############################################################################
#### 4. Run every model: held-out prediction error ############################
##############################################################################
options(width = 200)

## For each model: hide the outcomes for `holdout_years`, fit to the training
## rows, predict the held-out rows. `info` describes the model; `preds` holds
## one row per held-out observation.
run_model <- function(name, spec){
  sp <- get_spec(spec)
  preds <- validate_one(name, spec)
  if(is.null(preds)) return(list(info = NULL, preds = NULL))
  info <- data.frame(
    model    = name,
    n_train  = preds$n_train[1],
    formula  = paste(deparse(sp$formula, width.cutoff = 500), collapse = " "),
    pcov     = if(is.null(sp$pcov)) "NULL" else sp$pcov,
    slope    = sp$slope,
    DF_P     = paste(sp$DF_P, collapse = ", "),
    time_var = sp$time_var,
    B_off    = paste(sp$B_off, collapse = ", "),
    B_knots  = preds$B_knots[1],
    knot_shift = isTRUE(sp$knot_shift),
    DF_P_used  = preds$DF_P_used[1],
    holdout  = hold_key,
    min_year = min_year,
    run      = format(Sys.time(), "%Y-%m-%d %H:%M"),
    stringsAsFactors = FALSE)
  list(info = info, preds = preds |> dplyr::select(-B_knots, -DF_P_used, -n_train))
}
runs  <- parallel::mclapply(seq_along(MODELS), function(i) run_model(names(MODELS)[i], MODELS[[i]]),
                            mc.cores = n_cores, mc.preschedule = FALSE)
## a crashed worker returns a try-error instead of a list
runs  <- lapply(runs, function(r) if(inherits(r, "try-error")) list(info = NULL, preds = NULL) else r)
cat("\nFitted", sum(!sapply(map(runs, "info"), is.null)), "of", length(MODELS), "models\n")
info  <- map(runs, "info")  |> bind_rows()
preds <- map(runs, "preds") |> bind_rows()
if(nrow(preds) > 0) preds$holdout <- hold_key

#### Add results from previous runs (models re-run now replace their old rows) ####
## Old results are only kept if they were made with the same min_year and
## holdout_years; otherwise the held-out rows (row ids) would not line up.
file_comp  <- paste0(path_out, "Model comparison ", marker, " ", month, " ", year, " ", out_tag, ".csv")
new_names  <- unique(info$model)

## read.csv() guesses column types (e.g. B_off "2" comes back as integer), so
## give the old columns the same types as the new ones before bind_rows().
match_types <- function(old, new){
  for(v in intersect(names(old), names(new)))
    old[[v]] <- methods::as(old[[v]], class(new[[v]])[1])
  old
}

## Results saved by earlier runs (same min_year / holdout_years) are added with
## their saved validation columns; models re-run now replace their old rows.
## The held-out predictions themselves are not saved, so old and new models are
## only comparable when they share the same held-out rows (check n_holdout).
old_results <- NULL
if(keep_old_results && file.exists(file_comp)){
  old_results <- read.csv(file_comp, stringsAsFactors = FALSE)
  if(all(c("holdout", "min_year") %in% names(old_results))){
    old_results <- old_results |>
      filter(!model %in% new_names, min_year == !!min_year, holdout == hold_key)
    cat("\nKept", nrow(old_results), "model(s) from earlier runs\n")
  } else old_results <- NULL
}

if(nrow(preds) > 0){
  ## Models fitted in this run are scored on the held-out rows common to all of them.
  key <- "rid"   # unique row id; country/year/Sex is not unique (multiple surveys)
  n_models <- n_distinct(preds$model)
  common <- preds |> count(across(all_of(key))) |> filter(n == n_models) |> dplyr::select(-n)
  validation <- inner_join(preds, common, by = key) |>
    group_by(model) |> summarise_validation() |> ungroup()

  results <- info |> left_join(validation, by = "model")
  if(!is.null(old_results)) results <- bind_rows(match_types(old_results, results), results)
  results <- results |> arrange(RMSE_orig)   # RMSE on the prevalence scale is the main criterion

  cat("\n\n===== ", marker, ": held-out prediction error (mean-imputed, observed Y only) =====\n", sep = "")
  cat("Outcomes for ", paste(holdout_years, collapse = ", "),
      " hidden; models fitted to the remaining rows and the hidden rows predicted.\n",
      "n_holdout common rows = ", validation$n_holdout[1],
      ". bias = mean(pred - obs). Sorted by RMSE_orig (prevalence scale).\n\n", sep = "")
  print(as.data.frame(results |> dplyr::select(-any_of(c("formula", "run", "min_year", "holdout")))),
        digits = 4, row.names = FALSE)

  write.csv(results, file_comp, row.names = FALSE)
}

##############################################################################
#### 5. TOR workbook: bootstrap, coverage and subgroups #######################
##############################################################################
## Scored on the same common held-out rows as the table above.
## - RMSE difference from the final model with a 95% bootstrap interval:
##   countries are resampled with replacement (paired: the same draw for every
##   model), so surveys from one country stay together.
## - Error and 95% prediction-interval coverage by region, survey type and
##   conflict level (high conflict = held-out surveys in the top quarter of
##   war_terror among held-out surveys).
if(run_tor && nrow(preds) > 0){
  set.seed(20261023)
  B_boot <- 2000
  scored <- inner_join(preds, common, by = key) |>
    left_join(all_data |> dplyr::select(rid, war_terror), by = "rid")

  ## Paired cluster bootstrap of RMSE_orig
  sse <- scored |> group_by(model, country) |>
    summarise(sse = sum((obs - pred)^2), n = n(), .groups = "drop")
  SSE <- sse |> dplyr::select(-n) |> pivot_wider(names_from = model, values_from = sse)
  N   <- sse |> filter(model == "final") |> dplyr::select(country, n)
  SSE <- SSE |> left_join(N, by = "country")
  n_c <- nrow(SSE)
  W <- t(rmultinom(B_boot, n_c, rep(1/n_c, n_c)))             # B x countries draw counts
  mods <- setdiff(names(SSE), c("country", "n"))
  rmse_b <- sqrt((W %*% as.matrix(SSE[, mods])) / as.vector(W %*% SSE$n))
  diff_b <- rmse_b - rmse_b[, "final"]
  boot_tab <- tibble(
    model = mods,
    RMSE_diff_vs_final = (results$RMSE_orig[match(mods, results$model)] -
                            results$RMSE_orig[results$model == "final"]),
    diff_CI_lower = apply(diff_b, 2, quantile, 0.025),
    diff_CI_upper = apply(diff_b, 2, quantile, 0.975),
    prob_better_than_final = colMeans(diff_b < 0))
  boot_tab[boot_tab$model == "final", c("diff_CI_lower", "diff_CI_upper", "prob_better_than_final")] <- NA

  summary_tab <- tor_info |>
    left_join(results |> dplyr::select(model, n_train, n_holdout, RMSE_orig, MAE_orig, bias_orig,
                                       PI95_coverage, RMSE_logit, bias_logit, formula, pcov, slope,
                                       DF_P, time_var, B_knots), by = "model") |>
    left_join(boot_tab, by = "model") |>
    mutate(RMSE_pct_change = 100*RMSE_diff_vs_final/RMSE_orig[model == "final"]) |>
    relocate(RMSE_diff_vs_final, RMSE_pct_change, diff_CI_lower, diff_CI_upper,
             prob_better_than_final, .after = RMSE_orig)

  ## Subgroups
  war_cut <- quantile(scored$war_terror[scored$model == "final"], 0.75)
  scored <- scored |>
    mutate(source_type = case_when(ShortSource %in% c("SMART", "Surveillance") ~ ShortSource,
                                   TRUE ~ "Other survey types"),
           conflict = if_else(war_terror > war_cut, "High conflict (top 25% war_terror)", "Other"))
  by_group <- function(var, label){
    scored |> group_by(model, group = .data[[var]]) |> summarise_validation() |> ungroup() |>
      mutate(grouping = label, .before = 1)
  }
  subgroup_tab <- bind_rows(by_group("Region", "Region"),
                            by_group("source_type", "Survey type"),
                            by_group("conflict", "Conflict level")) |>
    left_join(tor_info |> dplyr::select(model, tor_item), by = "model") |>
    relocate(tor_item, .after = model) |>
    mutate(model = factor(model, levels = tor_info$model)) |>
    arrange(grouping, group, model) |>
    mutate(model = as.character(model))

  notes <- tibble(Note = c(
    paste0("Marker: ", marker, ". Mean-imputed data, observed surveys only, years > ", min_year, "."),
    paste0("Hold-out: outcomes for ", paste(range(holdout_years), collapse = "-"),
           " hidden; each model fitted to the remaining surveys and the hidden surveys predicted."),
    paste0("All models scored on the same ", results$n_holdout[1], " held-out surveys (",
           n_distinct(scored$country), " countries)."),
    "Main criterion: RMSE on the prevalence scale (RMSE_orig). bias = mean(predicted - observed).",
    "Each model changes one thing from the final model (2 - Analysis_MI.R); see the description column.",
    paste0("RMSE_diff_vs_final: positive = worse than the final model. 95% interval and prob_better_than_final from a paired ",
           "bootstrap over countries (", B_boot, " draws)."),
    "PI95_coverage: share of held-out surveys inside the 95% prediction interval (logit scale; mean uncertainty + residual variance with the survey's own SE).",
    "SMART is not set to 0 here: held-out SMART surveys are predicted as SMART surveys.",
    "Knots: in each validation fit the penalized-spline knots are moved back by the length of the held-out period, so the last knot is as far before the end of the training data as it is before the end of the data in the full analysis (DF_P_used column). Knots within 1 year of the lower boundary knot are dropped.",
    "Seasonality harmonic uses the survey mid-point's time of year; surveys with middle_year equal to the integer year (no month information) get no seasonal shift.",
    paste0("High conflict: held-out surveys with war_terror above ", signif(war_cut, 3), " (top 25% of held-out surveys)."),
    paste0("Run: ", format(Sys.time(), "%Y-%m-%d %H:%M"), ", 3 - Model/1 - Compare_Models.R (run_tor = TRUE).")))

  file_xlsx <- paste0(path_out, "TOR model comparison ", marker, " ", month, " ", year, ".xlsx")
  writexl::write_xlsx(list(Summary = summary_tab, Subgroups = subgroup_tab,
                           Predictions = scored |> dplyr::select(-holdout), Notes = notes),
                      file_xlsx)
  cat("\nTOR workbook written to", file_xlsx, "\n")
  print(as.data.frame(summary_tab |> dplyr::select(tor_item, model, RMSE_orig, RMSE_pct_change,
                                                   diff_CI_lower, diff_CI_upper, bias_orig, PI95_coverage)),
        digits = 3, row.names = FALSE)
}
