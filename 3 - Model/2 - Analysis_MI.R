remove(list=ls())

library(this.path)
wd <- dirname(this.path::here())
print(wd)
setwd(wd)
source("Utils/Programs_Oct_2026.R")
library(tidyverse)

# Usage: Rscript "3 - Model/2 - Analysis_MI.R" <marker> [model]
#   model (Wasting only): "fpi" = model with fpi_avg (estimates from 2001 on, as
#   fpi_avg is missing before then); omitted = model without fpi.
#   The model tag is added to the output file names (e.g. "Estimation Wasting_fpi.rds").
args <- as.character(commandArgs(trailingOnly = TRUE))
marker <- if(length(args) >= 1) args[1] else "Wasting"
model_tag <- if(length(args) >= 2) args[2] else ""
year <- "2026"
month <- "Oct" #for finding the data
marker_f <- paste0(marker, if(model_tag != "") paste0("_", model_tag))

## Path to output folder
path = paste0("Data/Analysis files/",marker,"/")

TRANS <- FALSE
q.order <- 2
plots <- FALSE
B <- 20
boot_vals <- 1:B
for(j in 1:B){
  
  #### 1. Setting Model Parameters ####
  if (grepl("Stunt", marker, ignore.case = TRUE)) {
    model_formula <- ~Sex + Region + Region:Sex + 
      MCI_5_yr+ I(MCI_5_yr^2)  + 
      MCI_5_yr:Sex+ Sex:I(MCI_5_yr^2) + 
      SDI
    
    # Penalized covariates (optional). NULL means none.
    Pcov_data <- NULL
    referent_lev <- NULL
    
    DF_P <- seq(1993, 2023, 1)
    bk <- 2
    year_var <- "year"
    cov_mat <- "VC"
    slope <- TRUE
    
  } else if (grepl("Over", marker, ignore.case = TRUE)) {
    model_formula <- ~ Sex + P_Region + MCI_5_yr
    
    DF_P <- seq(1993, 2022, 5)
    cov_mat <- "VC"
    slope <- TRUE
    year_var <- "year"
    
    Pcov_data <- "yes"
    referent_lev <- 2
    
  } else if (grepl("SevereWasting", marker, ignore.case = TRUE)) {
    # Chosen model: sdi3_war1_nopcov_nofpi_slope_sexReg (lowest held-out RMSE on
    # the prevalence scale in the Oct 2026 grid, see 1 - Compare_Models.R)
    model_formula <- ~ Sex + Region + Region:Sex +
      SMART + I(poly(SDI,3)) + log(war_terror)
    # Previous specification:
    # model_formula <- ~ Sex + Region + SMART + I(poly(SDI,3)) + I(poly(log(war_terror),2))

    Pcov_data <- NULL
    referent_lev <- NULL
    bk <- 4
    year_var <- "middle_year"

    DF_P <- seq(1994, 2023, 9)
    cov_mat <- "VC"
    slope <- TRUE
    
  } else if (model_tag == "fpi") {
    #Wasting, model with fpi
    # Chosen model: sdi1_war1_pcov_fpi_slope_sexReg (lowest held-out RMSE on the
    # prevalence scale in the grid2_Oct2026 grid, see 1 - Compare_Models.R).
    # fpi_avg is missing for 1990-2000, so those rows are dropped (estimates from 2001).
    model_formula <- ~ Sex + Region + Region:Sex +
      SMART + SDI + log(war_terror) + I(poly(asinh(fpi_avg),2))

    # Region-specific penalized splines
    Pcov_data <- "yes"
    referent_lev <- 2
    bk <- 10
    year_var <- "middle_year"

    DF_P = seq(2002, 2023, 8)
    cov_mat <- "VC"
    slope <- TRUE

  } else {
    #Wasting, model without fpi
    # Chosen model: sdi1_war1_nopcov_nofpi_slope_sexReg (lowest held-out RMSE on the
    # prevalence scale among models without fpi in the grid2_Oct2026 grid; the
    # integer year was 0.3-0.5% worse than middle_year for the top 20).
    model_formula <- ~ Sex + Region + Region:Sex +
      SMART + SDI + log(war_terror)

    Pcov_data <- NULL
    referent_lev <- NULL
    bk <- 10
    year_var <- "middle_year"

    DF_P = seq(1994, 2023, 8)
    cov_mat <- "VC"
    slope <- TRUE
  }
  
  
  #### 2. Running and Outputting the Models #### 
  all_data <- readRDS(paste0("Data/Merged/",year,"/",marker,"_",
                             month,"_final_multiple_impute.rds"))  |> 
    filter(.imp == j | .imp == 0) |> 
    mutate(
      SexF = case_when(
        Sex == "Female" ~ 1,
        TRUE ~ 0
      ),
      SexM = case_when(
        Sex == "Male" ~ 1,
        TRUE ~ 0
      ),
      Sex = case_when(
        Sex == "Both" ~ 0,
        Sex == "Female" ~ 1,
        Sex == "Male" ~ -1
      ),
      Region = factor(Region), 
      "SMART" = case_when(
        ShortSource=="SMART" ~ 1,
        TRUE ~ 0
      ),
      "Surveillance" = case_when(
        ShortSource=="Surveillance" ~ 1,
        TRUE ~ 0
      )
    ) |> 
    dplyr::select(c(".imp","country","year","middle_year", "Point.Estimate.NS", 
                    "Point.Estimate.Imp","SE_val", "ShortSource",
                    "Region", "Sex", "SMART", #, "SexF","SexM"
                    "Surveillance", "SDI", everything())) |> 
    rename("Y" = "Point.Estimate.NS", 
           "Y_all" = "Point.Estimate.Imp",
           "SE_var" = "SE_val") |> 
    arrange(country, year) #|>  filter(!is.na(Y))
  
  # Drop rows with a missing model variable (before model.frame: poly() fails on NAs)
  model_vars <- intersect(all.vars(model_formula), names(all_data))
  all_data <- all_data[complete.cases(all_data[, model_vars]),]
  mf <- model.frame(model_formula, data = all_data, na.action = na.pass)
  all_data <- all_data[complete.cases(mf),]
  
  if(year_var == "middle_year"){
    all_data <- all_data |> 
      mutate(
        int_year = year,
        year = middle_year)
  }
  
  
  cat(j,length(unique(all_data$country)), 
      length(c(all_data$Y)),
      length(c(all_data$Y[!is.na(all_data$Y)])), 
      table(all_data$Region),"\n") 
  
  if(!is.null(Pcov_data)){
    all_data$P_Region <- all_data$Region
    levels(all_data$P_Region)
    all_data$P_Region <- relevel(all_data$P_Region,ref = referent_lev)
    levels(all_data$P_Region)
    
    Pcov_data <- model.matrix(~P_Region,data=all_data,contrasts.arg = list(P_Region=diag(nlevels(all_data$P_Region))))[,-1]
  }
  
  ## Transforming outcome ##
  data_w_out <- all_data |> 
    dplyr::select(c("country", "year", "Y", "SE_var")) |> 
    mutate(
      SE_pred = 0, 
      SE_var = SE_var*((1/Y) + 1/(1-Y)), 
      Y = log(Y/(1-Y))
    )
  if(grepl("SexF", as.character(model_formula)[2])){
    data_w_out <- all_data |> 
      dplyr::select(c("country", "year", "Y", "SE_var", "Sex")) |>
      mutate(
        SE_pred = 0, 
        SE_var = SE_var*((1/Y) + 1/(1-Y)), 
        Y = log(Y/(1-Y))
      )
  }
  
  if(is.null(Pcov_data)){
    DF_R <- quantile(data_w_out$year[!is.na(data_w_out$Y)],probs = c(0.5))
    B.knots <- c(floor(min(data_w_out$year[!is.na(data_w_out$Y)]))-2, 
                 max(data_w_out$year[!is.na(data_w_out$Y)])+bk)
  }else{
    DF_R <- NULL  
    # Lower knot as in 1 - Compare_Models.R: first observed year rounded down, minus 2
    B.knots <- range(data_w_out$year[!is.na(data_w_out$Y)])
    B.knots[1] <- floor(B.knots[1]) - 2
    B.knots[2] <- B.knots[2] + 10
  }
  
  
  cov_data <- as.matrix(data.frame(model.matrix(
    model_formula,
    data=all_data)[,-1]
  ))
  
  ## Checking for issues in the design matrix
  # Not full rank
  if(qr(cov_data)$rank !=  ncol(cov_data)){
    qrX <- qr(cov_data)
    dep_cols <- qrX$pivot[(qrX$rank + 1):ncol(cov_data)]
    dep_cols <- dep_cols[!is.na(dep_cols)]
    colnames(cov_data)[dep_cols]
    cat("Design matrix not full rank dropping", colnames(cov_data)[dep_cols], "\n")
    cov_data <- cov_data[,-dep_cols]
  }
  # Zero Variance columns
  zero_var <- apply(cov_data, 2, function(x) var(x, na.rm = TRUE) == 0)
  if(any(zero_var)){
    cat("Zero variance columns. Dropping", colnames(cov_data)[zero_var], "\n")
    
    cov_data <- cov_data[, !zero_var, drop = FALSE]
  }
  # Checking X'X
  test <- lm(data_w_out$Y ~ data_w_out$year + cov_data)
  beta_hat <- summary(test)$coefficients
  if(ncol(cov_data)+2 != nrow(beta_hat) |
     any(is.na(beta_hat[,1]))){
    good_cols <- rownames(beta_hat[!is.na(beta_hat[,1]),])
    good_cols <- sub("cov_data","",good_cols)
    
    cat("X'X deficient. Dropping", 
        colnames(cov_data)[!(colnames(cov_data) %in% 
                               good_cols)], "\n")
    cov_data <- cov_data[,(colnames(cov_data) %in% 
                             good_cols)]
  }
  
  zero_covs <- c("SMART")
  
  remove(all_data)
  
  ##################### Covariate analysis with multiple penalized functions #################################
  t1 <- try(Estimation <- cmnpe(data_w_out, DF_P, DF_R, B.knots, q.order, 
                                cov_data = cov_data, Pcov_data = Pcov_data, 
                                cov_mat = cov_mat, plots = plots, TRANS=TRANS,
                                zero_covs = zero_covs, slope = slope))
  

  if(is.null(attr(t1,"class"))){
    cat(j,2*Estimation$df - 2*c(summary(Estimation$result$model)$logLik),"\n")
    
    t_plot_data <- Estimation$plot_data
    saveRDS(t_plot_data, 
            file = paste0(path,"MI_files/Plot data for ",marker_f," imputation ",j,".rds"))
    
    outfile <- list(
      gamma = c(Estimation$result$model$modelStruct$varStruct[1]),
      sigma2 = Estimation$result$model$sigma^2,
      smart_cov = Estimation$result$model$coefficients$fixed[which(colnames(cov_data) %in% zero_covs) +2] ,
      zero_covs = zero_covs,
      year_var = year_var,   # time variable of plot_data$year; output_function() joins on it
      boot_vals = boot_vals[boot_vals<=j])
    
    saveRDS(outfile, file = paste0(path,"MI_files/Estimation ",marker_f,".rds"))
    remove(Estimation)
    
  }else{
    boot_vals <- boot_vals[boot_vals != j]
    cat(j,"Model Error.\n")
  }
}



