## Quick check of the food price index (FPI) in the Wasting model with FPI:
##   interp : FPI linearly interpolated to each row's time (yes/no)
##   fpi    : form of asinh(fpi_avg): linear / quadratic / cubic (orthogonal poly)
##   time   : middle_year (survey mid-point) or integer year for the splines
## 2 x 3 x 2 = 12 models, nothing else varied. Same validation as
## 1 - Compare_Models.R (hold out 2021-2025 on the mean-imputed data, score on
## the held-out surveys, RMSE on the prevalence scale is the criterion).
##
## Interpolation: the annual fpi_avg is the mean over the calendar year, so it is
## placed at the middle of the year (year + 0.5) and linearly interpolated to the
## survey mid-point (middle_year). Surveys with no month information
## (middle_year == year) are placed at year + 0.5, i.e. they keep the annual value.
## Times before the first or after the last FPI year + 0.5 keep the nearest annual
## value; FPI stays missing before the country's first FPI year (2001).
##
## Usage:  Rscript "3 - Model/1b - Compare_FPI_interp.R" [marker]   (default: Wasting)
## Output: Data/Analysis files/<marker>/Model comparison <marker> <month> <year> fpi_interp_Oct2026.csv

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
hold_key <- paste(min(holdout_years), "to", max(holdout_years))
keep_old_results <- FALSE
n_cores <- 6               # models are fitted in parallel (parallel::mclapply)
path_out <- paste0("Data/Analysis files/", marker, "/")
out_tag <- "fpi_interp_Oct2026"

##############################################################################
#### 1. Candidate models  ####################################################
##############################################################################
## Fields as in 1 - Compare_Models.R. Base model: the Wasting model with FPI in
## 2 - Analysis_MI.R (linear SDI, linear log(war_terror), Region:Sex, SMART,
## region-specific penalized splines, random slope, DF_P = seq(1994, 2023, 8),
## upper boundary + 10). SMART is not zeroed when predicting held-out surveys.
defaults <- list(referent_lev = 2,
                 DF_P = seq(1994, 2023, 8), cov_mat = "VC", slope = TRUE,
                 zero_covs = NULL, time_var = "middle_year", B_off = 10)

poly_term <- function(x, k) if(k == 1) x else paste0("I(poly(", x, ",", k, "))")
MODELS <- list()
for(ip in c(FALSE, TRUE)) for(k in 1:3) for(tv in c("middle_year", "year")){
  fv <- if(ip) "asinh(fpi_interp)" else "asinh(fpi_avg)"
  nm <- paste0(if(ip) "interp" else "nointerp", "_fpi", k, "_", if(tv == "year") "year" else "mid")
  MODELS[[nm]] <- list(
    formula  = as.formula(paste("~ Sex + Region + Region:Sex + SMART + SDI + log(war_terror) +",
                                poly_term(fv, k))),
    pcov     = "yes",
    time_var = tv)
}

##############################################################################
#### 2. Data (mean-imputed, observed Y only) ##################################
##############################################################################
raw <- readRDS(paste0("Data/Merged/", year, "/", marker, "_", month,
                      "_final_mean_impute.rds"))

## Annual FPI (one value per country-year, from the annual rows), placed at mid-year
fpi_annual <- raw |>
  filter(is.na(UNICEFSurveyID), !is.na(fpi_avg)) |>
  distinct(ISO.code, year, fpi_avg) |>
  mutate(t = year + 0.5)

interp_fpi <- function(iso, time){
  a <- fpi_annual[fpi_annual$ISO.code == iso[1], ]
  if(nrow(a) < 1) return(rep(NA_real_, length(time)))
  out <- if(nrow(a) == 1) rep(a$fpi_avg, length(time)) else
    approx(a$t, a$fpi_avg, xout = time, rule = 2)$y
  out[time < min(a$year)] <- NA        # no FPI before the country's first FPI year
  out
}

all_data <- raw |>
  mutate(fpi_time = if_else(middle_year == year, year + 0.5, middle_year)) |>
  group_by(ISO.code) |>
  mutate(fpi_interp = interp_fpi(ISO.code, fpi_time)) |>
  ungroup() |>
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
cat("Rows with FPI: annual", sum(!is.na(all_data$fpi_avg)),
    " interpolated", sum(!is.na(all_data$fpi_interp)), "\n")
cat("Interpolated minus annual asinh(FPI), survey rows with a mid-point:\n")
print(summary(with(all_data[all_data$middle_year != all_data$year, ],
                   asinh(fpi_interp) - asinh(fpi_avg))))

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
