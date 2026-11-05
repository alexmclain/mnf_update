

library(this.path)
wd <- dirname(this.path::here())
print(wd)
setwd(wd)

source("Utils/Programs_Cleaning_SE.R")
library(readxl)
library(nlme)
library(tidyverse)
library(dplyr)


marker <- as.character(commandArgs(trailingOnly = TRUE))
month <- "Oct" #for appending filename
year <- "2026"

### If doing for multiple imputation set as TRUE, otherwise FALSE
multiple_imputation_vec <- c(TRUE,FALSE)

#### 1. Loading and formatting the data (lines 20 - 131) ####
for(multiple_imputation in multiple_imputation_vec){
  if(multiple_imputation){
    surv_data <- read_rds(paste0("Data/Merged/",year,"/",marker,"_",month,"_reg_all_multiple_impute.rds"))
    keys <- c("ISO.code","Sex", ".imp")
  }else{
    surv_data <- read_rds(paste0("Data/Merged/",year,"/",marker,"_",month,"_reg_all_mean_multiple.rds")) 
    keys <- c("ISO.code","Sex")
  }
  
  ## Every country/sex(/imputation) should have a row for each year.
  n_years <- n_distinct(surv_data$year)
  short_keys <- surv_data %>%
    count(across(all_of(keys))) %>%
    filter(n < n_years)
  if(nrow(short_keys) > 0){
    warning(nrow(short_keys), " combinations of ", paste(keys, collapse = "/"),
            " have fewer than ", n_years, " rows.")
    print(short_keys)
  }
  
  # View(test %>% arrange(across(all_of(keys))) %>% select(all_of(keys), everything()))
  
  
  
  ##### Creating a dataset where both is missing when male and female are observed.
  
  surv_data <- surv_data %>% 
    mutate(UNICEFSurveyID = case_when(
      UNICEFSurveyID == 0 ~ NA_real_,
      TRUE ~ UNICEFSurveyID)
    ) %>% 
    group_by(ISO.code, year,.imp) %>%
    fill(UNICEFSurveyID,
         .direction = "updown")

  ## Continuous year: sex rows added for a survey take the survey's middle_year,
  ## rows with no survey use the integer year.
  if(!"middle_year" %in% names(surv_data)){surv_data$middle_year <- NA_real_}
  surv_data <- surv_data %>%
    group_by(ISO.code, year, UNICEFSurveyID, .imp) %>%
    fill(middle_year, .direction = "updown") %>%
    ungroup() %>%
    mutate(middle_year = coalesce(middle_year, as.numeric(year)))

  
  surv_data <- surv_data %>% 
    group_by(UNICEFSurveyID,.imp) %>% 
    mutate(check = all(!is.na(Point.Estimate)) & n()==3) %>% 
    ungroup() %>% 
    mutate(Point.Estimate.NS = case_when(
      (check==TRUE & Sex == "Both") ~ NA_real_,
      TRUE ~ Point.Estimate
    ))
  
  ########### Formatting the data.
  
  surv_wide_one <- surv_data %>% 
    dplyr::select(
      c(ISO.code, country,  year, Sex, UNICEFSurveyID,
        Point.Estimate, SE_val, All_africa_HIW,.imp)
    ) %>% 
    filter(.imp <= 1) %>% 
    mutate(
      year_ID = paste0(year, UNICEFSurveyID, sep = "_"), 
      UNICEFSurveyID = case_when(
        is.na(UNICEFSurveyID) ~ 0,
        !is.na(UNICEFSurveyID) ~ UNICEFSurveyID
      )) %>% 
    group_by(ISO.code, country, year, UNICEFSurveyID, All_africa_HIW, year_ID, Sex) %>%
    filter(row_number() == 1) %>%  
    pivot_wider(
      names_from = Sex, 
      values_from = c(Point.Estimate, SE_val)
    ) %>% 
    mutate(
      SE_val_Female = case_when(
        is.na(SE_val_Female) ~ SE_val_Both, 
        TRUE ~ SE_val_Female
      ), 
      SE_val_Male = case_when(
        is.na(SE_val_Male) ~ SE_val_Both, 
        TRUE ~ SE_val_Male
      )
    ) %>% 
    filter(!is.na(Point.Estimate_Both))
  
  surv_wide_B <- surv_data %>% 
    filter(Sex == "Both") %>% 
    dplyr::select(
      c(.imp, ISO.code, country,  year, UNICEFSurveyID, ShortSource)
    ) %>% 
    right_join(surv_wide_one) 
  
  
  ################# Look at variable relationships
  if(FALSE){
    ggplot(data=surv_wide_B, aes(x=Point.Estimate_Both,y=Point.Estimate_Female)) + 
      geom_point() + 
      theme(text=element_text(family="Garamond", size=14))
    
    ggplot(data=surv_wide_B, aes(x=log(Point.Estimate_Both),y=log(Point.Estimate_Female))) + 
      geom_point() + 
      theme(text=element_text(family="Garamond", size=14))
    
    ggplot(data=surv_wide_B, aes(x=sqrt(Point.Estimate_Both),y=sqrt(Point.Estimate_Female))) + 
      geom_point() + 
      theme(text=element_text(family="Garamond", size=14))
    
    
    ggplot(data=surv_wide_B, aes(x=Point.Estimate_Both,y=Point.Estimate_Male)) + 
      geom_point() + 
      theme(text=element_text(family="Garamond", size=14)) 
    
    ggplot(data=surv_wide_B, aes(x=log(Point.Estimate_Both),y=log(Point.Estimate_Male))) + 
      geom_point() + 
      theme(text=element_text(family="Garamond", size=14)) 
    ggplot(data=surv_wide_B, aes(x=sqrt(Point.Estimate_Both),y=sqrt(Point.Estimate_Male))) + 
      geom_point() + 
      theme(text=element_text(family="Garamond", size=14)) 
    
    ggplot(data=surv_wide_B, aes(y = sqrt(Point.Estimate_Female) - sqrt(Point.Estimate_Both),
                                 x = sqrt(Point.Estimate_Both))) + 
      geom_point() +
      geom_smooth(method = "lm") +
      theme(text=element_text(family="Garamond", size=14))
    
    
    ggplot(data=surv_wide_B, aes(y = sqrt(Point.Estimate_Male) - sqrt(Point.Estimate_Both),
                                 x = sqrt(Point.Estimate_Both))) + 
      geom_point() +
      geom_smooth(method = "lm") +
      theme(text=element_text(family="Garamond", size=14))
  }
  
  #### 2. Running linear mixed models to predict missing sex groups ####
  ### Predicting for Females
  ### Rows must be contiguous by country (and ordered the same way as the 
  ### prediction data) for the random effects design matrix (make_Z_noPcov).
  surv_wide <-  surv_wide_B %>%  
    filter(.imp <= 1) %>% 
    arrange(country, year, UNICEFSurveyID) %>% 
    mutate(Source = relevel(factor(All_africa_HIW), ref = "Southern Asia")) %>% 
    dplyr::select(-c(".imp"))
  
  formula <- sqrt(Point.Estimate_Female) ~ 
    sqrt(Point.Estimate_Both) 
  formula_x <- ~ 
    sqrt(Point.Estimate_Both) 
  if(grepl("St",marker)){
    cnt_list = list(maxIter = 5000)
  }else{
    cnt_list = list(maxIter = 5000, opt = "optim")
  }
  
  
  
  #### Fitting the model
  fitted_model <- lme(formula,
                      random=~1|country,
                      weights = varIdent(form=~1|Source),
                      data = surv_wide, na.action = na.omit, 
                      control = cnt_list)
  
  all_data <- surv_wide
  data_frame <- surv_wide %>% 
    mutate(Y = sqrt(Point.Estimate_Female)) %>% 
    filter(!is.na(Y))
  
  Xi.matrix <- cbind(model.matrix(formula_x ,data=data_frame))
  SE.matrix <- cbind(model.matrix(~Source,data=data_frame)[,-1])
  N <- length(unique(data_frame$country))
  X_orig <- Xi.matrix
  Z_orig <- make_Z_noPcov(data_frame,data_frame)
  
  # Estimate EBLUPs, predicted means, and variances of the predicted means.
  pred_mat <- pred_fun_noPcov(X=X_orig,Z=Z_orig,Y=data_frame$Y,fitted_model,N,SE_var=SE.matrix,full_b = 1)
  S <- pred_mat$S
  b <- pred_mat$b
  
  
  # Extracting only those countries that were used in the original analysis
  cov_data_used_only <- all_data[all_data$country %in% data_frame$country,]
  # Ordering the data so the random effects align.
  div=c(cov_data_used_only$country)
  cov_data_used_only <- cov_data_used_only[order(div,cov_data_used_only$year),]
  # Making the X and Z matrices for the new data.
  
  X <- model.matrix(formula_x,data=cov_data_used_only)
  SE<- cbind(model.matrix(~Source,data=cov_data_used_only)[,-1])
  Z <- make_Z_noPcov(cov_data_used_only,data_frame)
  
  # Predicting the mean and variance for each new observation.
  pred_mat_new <- pred_fun_noPcov(X,Z,Y=0,fitted_model,N,SE_var=SE,full_b = b, S = S)
  
  pred_all  <- pred_mat_new$mu_T^2
  sigma_Y_all <- diag(pred_mat_new$sigma_Y)*4*pred_all
  
  pred_new_data <- data.frame(cov_data_used_only,pred_all, sigma_Y_all)
  all_data <- left_join(all_data, pred_new_data)
  plot(all_data$Point.Estimate_Both, all_data$pred_all)
  lines(c(0,100),c(0,100))
  plot(sqrt(all_data$Point.Estimate_Both), sqrt(all_data$pred_all) )
  lines(c(0,100),c(0,100))
  
  
  # Extracting countries that were not used in the original analysis
  cov_data_not_used <- all_data[not_in(all_data$country,data_frame$country),]
  
  if(nrow(cov_data_not_used)>0){
    # Predicting for them
    Xi.full <- cbind(model.matrix(formula_x ,data=all_data))
    SE.full <- cbind(model.matrix(~Source,data=all_data)[,-1])
    X <- Xi.full[not_in(all_data$country,data_frame$country),]
    SE<- SE.full[not_in(all_data$country,data_frame$country),]
    
    N <- length(unique(cov_data_not_used$country))
    pred_mat_new_noC <- pred_fun_noPcov_noC(X,Y,fitted_model,N,SE_var=SE,full_b=b,S=S)
    pred_all_noC  <- pred_mat_new_noC$mu_T^2
    sigma_Y_all_noC <- diag(pred_mat_new_noC$sigma_Y)*4*pred_all_noC
    
    
    # Predicting the standard errors.
    pred_new_data_noC <- data.frame(cov_data_not_used,pred_all_noC, sigma_Y_all_noC)
    all_data <- left_join(all_data, pred_new_data_noC)
    plot(all_data$Point.Estimate_Both, all_data$pred_all_noC)
    lines(c(0,100),c(0,100))
  }else{
    ## All countries were used in the fit; columns are needed for the final merge.
    all_data$pred_all_noC <- NA_real_
    all_data$sigma_Y_all_noC <- NA_real_
  }
  
  ### Final data for Female
  surv_wide_Female <- surv_wide_B %>% 
    left_join( all_data) %>%   
    mutate(
      Sex = "Female",
      pred_all = case_when(
        is.na(pred_all) ~ pred_all_noC, 
        TRUE ~ pred_all
      ),
      sigma_Y_all = case_when(
        is.na(sigma_Y_all) ~ sigma_Y_all_noC, 
        TRUE ~ sigma_Y_all
      )
    ) %>% 
    dplyr::select(-c(pred_all_noC, sigma_Y_all_noC)) %>%   
    mutate(
      Point.Estimate_pred = case_when(
        is.na(Point.Estimate_Female) ~ pred_all, 
        TRUE ~ Point.Estimate_Female
      ),
      SE_pred = case_when(
        is.na(Point.Estimate_Female) ~ sqrt(SE_val_Female^2 + sigma_Y_all), 
        TRUE ~ SE_val_Female
      )
    ) 
  
  plot(surv_wide_Female$Point.Estimate_Both, surv_wide_Female$Point.Estimate_pred)
  lines(c(0,100),c(0,100))
  
  if(FALSE){
    ggplot(data=surv_wide_Female, aes(y = SE_pred,
                                      x = SE_val_Female)) + 
      geom_point() +
      geom_smooth(method = "lm") +
      facet_grid(~is.na(Point.Estimate_Female)) +
      geom_abline(intercept = 0, slope = 1)
    
    ggplot(data=surv_wide_Female, aes(y = Point.Estimate_pred,
                                      x = Point.Estimate_Both)) + 
      geom_point() +
      geom_smooth(method = "lm") +
      facet_grid(~is.na(Point.Estimate_Female)) +
      geom_abline(intercept = 0, slope = 1)
    # 
    # surv_wide_Female <- surv_wide_Female %>% 
    #   dplyr::select(
    #     c(ISO.code, country,  year, Sex,  UNICEFSurveyID,
    #       Point.Estimate_pred, SE_pred, ShortSource)
    #   )
  }
  
  test_merge <- surv_data %>% 
    left_join(surv_wide_Female, 
              by = c(".imp","ISO.code", "year", "Sex", "country",
                     "UNICEFSurveyID", "All_africa_HIW")
    ) %>% 
    mutate(ShortSource = ShortSource.x) %>% 
    mutate(
      ShortSource = case_when(
        is.na(ShortSource) ~ ShortSource.y,
        TRUE ~ ShortSource
      )
    ) %>% 
    dplyr::select(-c(ShortSource.x,ShortSource.y))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ### Predicting for Males
  # Fitting the model
  fitted_model <- lme(formula,
                      random=~1|country,
                      weights = varIdent(form=~1|Source),
                      data = surv_wide, na.action = na.omit, 
                      control = cnt_list)
  
  all_data <- surv_wide
  data_frame <- surv_wide %>% 
    mutate(Y = sqrt(Point.Estimate_Male)) %>% 
    filter(!is.na(Y))
  
  Xi.matrix <- cbind(model.matrix(formula_x ,data=data_frame))
  SE.matrix <- cbind(model.matrix(~Source,data=data_frame)[,-1])
  N <- length(unique(data_frame$country))
  X_orig <- Xi.matrix
  Z_orig <- make_Z_noPcov(data_frame,data_frame)
  
  # Estimate EBLUPs, predicted means, and variances of the predicted means.
  pred_mat <- pred_fun_noPcov(X=X_orig,Z=Z_orig,Y=data_frame$Y,fitted_model,N,SE_var=SE.matrix,full_b = 1)
  S <- pred_mat$S
  b <- pred_mat$b
  
  
  # Extracting only those countries that were used in the original analysis
  cov_data_used_only <- all_data[all_data$country %in% data_frame$country,]
  # Ordering the data so the random effects align.
  div=c(cov_data_used_only$country)
  cov_data_used_only <- cov_data_used_only[order(div,cov_data_used_only$year),]
  
  # Making the X and Z matrices for the new data.
  X <- cbind(model.matrix(formula_x,data=cov_data_used_only))
  SE<- cbind(model.matrix(~Source,data=cov_data_used_only)[,-1])
  Z <- make_Z_noPcov(cov_data_used_only,data_frame)
  
  # Predicting the mean and variance for each new observation.
  pred_mat_new <- pred_fun_noPcov(X,Z,Y=0,fitted_model,N,SE_var=SE,full_b = b, S = S)
  pred_all  <- pred_mat_new$mu_T^2
  sigma_Y_all <- diag(pred_mat_new$sigma_Y)*4*pred_all
  
  pred_new_data <- data.frame(cov_data_used_only,pred_all, sigma_Y_all)
  all_data <- left_join(all_data,pred_new_data)
  plot(all_data$Point.Estimate_Both, all_data$pred_all)
  lines(c(0,100),c(0,100))
  
  
  # Extracting countries that were not used in the original analysis
  cov_data_not_used <- all_data[not_in(all_data$country,data_frame$country),]
  
  if(nrow(cov_data_not_used)>0){
    # Predicting for them
    Xi.full <- cbind(model.matrix(formula_x,data=all_data))
    SE.full <- cbind(model.matrix(~Source,data=all_data)[,-1])
    X <- Xi.full[not_in(all_data$country,data_frame$country),]
    SE<- SE.full[not_in(all_data$country,data_frame$country),]
    
    N <- length(unique(cov_data_not_used$country))
    pred_mat_new_noC <- pred_fun_noPcov_noC(X,Y,fitted_model,N,SE_var=SE,full_b=b,S=S)
    pred_all_noC  <- pred_mat_new_noC$mu_T^2
    sigma_Y_all_noC <- diag(pred_mat_new_noC$sigma_Y)*4*pred_all_noC
    
    
    # Predicting the standard errors.
    pred_new_data_noC <- data.frame(cov_data_not_used,pred_all_noC, sigma_Y_all_noC)
    all_data <- merge(all_data,pred_new_data_noC,all.x=TRUE)
    plot(all_data$Point.Estimate_Both, all_data$pred_all_noC)
    lines(c(0,100),c(0,100))
  }else{
    ## All countries were used in the fit; columns are needed for the final merge.
    all_data$pred_all_noC <- NA_real_
    all_data$sigma_Y_all_noC <- NA_real_
  }
  
  surv_wide_Male <- surv_wide_B %>% 
    left_join( all_data) %>%   
    mutate(
      Sex = "Male",
      pred_all = case_when(
        is.na(pred_all) ~ pred_all_noC, 
        TRUE ~ pred_all
      ),
      sigma_Y_all = case_when(
        is.na(sigma_Y_all) ~ sigma_Y_all_noC, 
        TRUE ~ sigma_Y_all
      )
    ) %>% 
    dplyr::select(-c(pred_all_noC, sigma_Y_all_noC)) %>%   
    mutate(
      Point.Estimate_pred = case_when(
        is.na(Point.Estimate_Male) ~ pred_all, 
        TRUE ~ Point.Estimate_Male
      ),
      SE_pred = case_when(
        is.na(Point.Estimate_Male) ~ sqrt(SE_val_Male^2 + sigma_Y_all), 
        TRUE ~ SE_val_Male
      )
    ) 
  
  plot(surv_wide_Male$Point.Estimate_Both, surv_wide_Male$Point.Estimate_pred)
  lines(c(0,100),c(0,100))
  
  if(FALSE){
    ggplot(data=surv_wide_Male, aes(y = SE_pred,
                                    x = SE_val_Male)) + 
      geom_point() +
      geom_smooth(method = "lm") +
      facet_grid(~is.na(Point.Estimate_Male)) +
      geom_abline(intercept = 0, slope = 1)
    
    ggplot(data=surv_wide_Male, aes(y = Point.Estimate_pred,
                                    x = Point.Estimate_Both)) + 
      geom_point() +
      geom_smooth(method = "lm") +
      facet_grid(~is.na(Point.Estimate_Male)) +
      geom_abline(intercept = 0, slope = 1)
  }
  surv_wide_Male <- surv_wide_Male %>% 
    dplyr::select(
      c(.imp,ISO.code, country,  year, Sex, UNICEFSurveyID,  
        Point.Estimate_pred, SE_pred, ShortSource)
    )
  
  
  
  
  
  
  
  #### 3. Merging and outputting estimates ####
  
  ### Final merge of all data
  final_merge <- test_merge %>% 
    left_join(surv_wide_Male, by = c(".imp","ISO.code", "country",  "year", "Sex", "UNICEFSurveyID")) %>%   
    mutate(ShortSource = ShortSource.x) %>% 
    mutate(
      ShortSource = case_when(
        is.na(ShortSource) ~ ShortSource.y,
        TRUE ~ ShortSource
      )
    ) %>% 
    dplyr::select(-c(ShortSource.x,ShortSource.y)) %>% 
    mutate(
      Point.Estimate = case_when(
        !is.na(Point.Estimate) ~ Point.Estimate, 
        !is.na(Point.Estimate_pred.x) ~ Point.Estimate_pred.x, 
        !is.na(Point.Estimate_pred.y) ~ Point.Estimate_pred.y, 
        TRUE ~ NA_real_
      ),
      SE_pred = case_when(
        !is.na(SE_val) ~ SE_val, 
        !is.na(SE_pred.x) ~ SE_pred.x, 
        !is.na(SE_pred.y) ~ SE_pred.y, 
        TRUE ~ NA_real_
      )
    ) %>% 
    dplyr::select(
      -c(Point.Estimate_pred.x, SE_pred.x, Point.Estimate_pred.y, SE_pred.y, Region)
    ) %>% 
    rename(
      Point.Estimate.Orig = PointEstimate,
      Region = All_africa_HIW,
      Point.Estimate.Imp = Point.Estimate
    ) %>% 
    dplyr::select(
      .imp,ISO.code, country, Region, year, middle_year, Sex, Point.Estimate.Orig,
      Point.Estimate.Imp, Point.Estimate.NS, SE_pred, SE_val, ShortSource,
      SDI, WB.Income.Group, UNICEFSurveyID,Standard.Error,
      everything()
    ) %>% 
    arrange(.imp,country,year,Sex)

  ## Annual prediction rows. Country-years with a survey only have survey rows,
  ## which sit at the survey's middle_year. Add a row with no survey at the
  ## integer year (country-year covariates only) so every country-year has an
  ## annual estimate at the same time point as the years without surveys.
  cy_cols <- c(".imp", "ISO.code", "country", "Region", "year", "Sex", "SDI",
               "WB.Income.Group", "Country", "gdp", "log_gdp_pc", "gdp_pc",
               "dpt_imm", "fpi_avg", "fpi_prev5yr_avg", "fpi_anom", "shock",
               "war_terror")
  annual_rows <- final_merge %>%
    group_by(.imp, ISO.code, year, Sex) %>%
    filter(all(!is.na(UNICEFSurveyID))) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(any_of(cy_cols)) %>%
    mutate(middle_year = as.numeric(year), check = FALSE)
  cat("Adding", nrow(annual_rows), "annual rows for country-years with surveys\n")
  final_merge <- bind_rows(final_merge, annual_rows) %>%
    arrange(.imp,country,year,middle_year,Sex)


  ## Checking for duplicates
  final_merge_nodup <- final_merge %>% 
    distinct()
  if(nrow(final_merge_nodup) < nrow(final_merge)){
    warning(nrow(final_merge) - nrow(final_merge_nodup), 
            " duplicate rows were removed before exporting.")
  }
  final_merge <- final_merge_nodup
  
  # Exporting
  if(multiple_imputation){
    saveRDS(final_merge,file = paste("Data/Merged/",year,"/",marker,"_",month,"_final_multiple_impute.rds",sep=""))
  }else{
    final_merge <- final_merge %>% 
      dplyr::select(-".imp")
    saveRDS(final_merge,file = paste("Data/Merged/",year,"/",marker,"_",month,"_final_mean_impute.rds",sep=""))
  }
  
}

