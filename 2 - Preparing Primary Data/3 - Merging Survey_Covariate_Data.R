

library(this.path)
wd <- dirname(this.path::here())
print(wd)
setwd(wd)

library(tidyverse)
library(readxl)
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

marker <- as.character(commandArgs(trailingOnly = TRUE))

month <- "Oct" #for appending filename
year <- "2026"

#### Note: "1 - Preparing Covariates/02 - Imputation.qmd" must be run before you can
#### commence. It creates both the multiple imputation (Multiple_Imputed_*.rds) and
#### the single (mean) imputation (Single_Impute_*.rds) files.
imputed_label <- "Oct2026" # label of the files created by 02 - Imputation.qmd

### If doing for multiple imputation set as TRUE, otherwise FALSE
multiple_imputation_vec <- c(TRUE,FALSE)

for(multiple_imputation in multiple_imputation_vec){
  if(multiple_imputation){
    cov_data <- readRDS(paste0("Data/IHME_covs/Multiple_Imputed_",imputed_label,".rds")) %>% 
      filter(!is.na(.imp)) # countries that were not imputed
  }else{
    cov_data <- readRDS(paste0("Data/IHME_covs/Single_Impute_",imputed_label,".rds")) %>% 
      filter(!is.na(SDI)) %>% # countries that were not imputed
      mutate(.imp = 0)
  }
  
  ## SEV is not used
  cov_data <- cov_data %>% 
    dplyr::select(-any_of("SEV"))
  
  #### One row per country/year/sex/imputation ####
  ## Rows are kept for every imputation, even where covariates are identical 
  ## across imputations (observed years); the model needs the full grid for 
  ## each .imp.
  cov_data <- cov_data %>% 
    distinct(ISO.code, year, Sex, .imp, .keep_all = TRUE)
  
  ## Reading in covariate and Cleaned (by Age and SE) data.
  stunt_res <- read_rds(
    paste0("Data/JME/",year,"/Cleaned/",marker,"_SE_Age_clean.rds")
  ) %>% 
    as_tibble() %>% 
    rename("ISO.code" = "ISO3Code") %>% 
    mutate(ISO.code = as.character(ISO.code),
           Point.Estimate = Adj_PointEstimate*100)%>% 
    filter(Age_range == "National") %>% 
    dplyr::select(-c(Age_range,Adj_PointEstimate)) %>% 
    filter(year > 1989)
  
  if(!any(names(stunt_res) == "M49")){stunt_res$M49 <- NA}
  
  ### Get the total number of primary data sources before merging.
  length(stunt_res$Point.Estimate[!is.na(stunt_res$Point.Estimate)])
  
  
  # 1. Ensure covariates are unique per .imp + ISO + year + Sex.
  cov_data_keyed <- cov_data %>%
    distinct(ISO.code, year, Sex, .imp, .keep_all = TRUE)
  
  # 2. Expand surveys across .imp if needed.
  if (multiple_imputation) {
    stunt_res_expanded <- stunt_res %>%
      tidyr::crossing(.imp = sort(unique(cov_data_keyed$.imp)))
  } else {
    stunt_res_expanded <- stunt_res %>%
      mutate(.imp = 0)
  }
  
  test <- cov_data_keyed %>%
    left_join(
      stunt_res_expanded,
      by = c("ISO.code", "year", "Sex", ".imp"),
      relationship = "one-to-many"
    )
  
  keys <- c("ISO.code", "year", "Sex", ".imp", "UNICEFSurveyID")
  # View(test %>% arrange(across(all_of(keys))) %>% select(all_of(keys), everything()))
  
  test %>%
    count(across(all_of(keys))) %>%
    filter(n > 1)
  
  
  # 3. Keep cov-only rows too
  ### Merge with covariate data
  ### Surveys are merged on the integer year, so covariates are from the
  ### integer year and a survey with a non-integer middle_year fills the
  ### existing country-year row (no extra row). middle_year is NA for
  ### covariate-only rows here; it is filled in "4 - Sex_cross_walk.R".
  Stunt_data_w_cov <- cov_data_keyed %>%
    left_join(
      stunt_res_expanded,
      by = c("ISO.code", "year", "Sex", ".imp"),
      relationship = "one-to-many"
    ) %>% 
    dplyr::select(-c("Country")) %>% 
    dplyr::rename("Country"="location_name") %>% 
    filter(year>1989) %>% 
    arrange(.imp, Country, year, Sex) %>% 
    dplyr::select(-c(M49, country)) %>% 
    dplyr::select(ISO.code, Country, year, Sex, everything())

  ## The model identifies countries by name, so each ISO code must have exactly
  ## one name in every year (otherwise those years become a separate country).
  name_check <- Stunt_data_w_cov %>% distinct(ISO.code, Country) %>% count(ISO.code) %>% filter(n > 1)
  if (nrow(name_check) > 0) {
    stop("More than one country name for: ", paste(name_check$ISO.code, collapse = ", "),
         ". Fix the names in 1 - Preparing Covariates/02 - Imputation.qmd.")
  }

  ### Ensuring only 1 observation per survey/year/sex/.imp
  Stunt_data_w_cov <- Stunt_data_w_cov %>% 
    arrange(ISO.code, Country, year, Sex, UNICEFSurveyID) %>% 
    mutate(ISO.code  = as.factor(ISO.code),
           year_ID = paste0(year, UNICEFSurveyID, sep = "_"), 
           UNICEFSurveyID = case_when(
             is.na(UNICEFSurveyID) ~ 0,
             !is.na(UNICEFSurveyID) ~ UNICEFSurveyID
           )) %>% 
    group_by(ISO.code, Country, year, UNICEFSurveyID, year_ID, Sex,.imp) %>%
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    dplyr::select(-"year_ID") 
  
  #View(Stunt_data_w_cov)
  
  ### Get the total number of primary data sources and countries (should be 206) after merging.
  B <- max(Stunt_data_w_cov$.imp)
  length(unique(Stunt_data_w_cov$ISO.code))
  
  
  ### Load in and edit regional groupings ###
  reg_groups <- read_xlsx("Data/Country/Crosswalk_Oct_2026.xlsx") %>% 
    select(-"CountryorArea") %>% 
    rename(
      ISO.code = MNF_Country_Code,
      All_africa_HIW = Modelling_Group
      )
  
  ### Merge regional groupings with data ###
  ### Only keeping countries with covariate data (all.x), crosswalk-only countries are dropped.
  Stunt_data_w_cov <- merge(Stunt_data_w_cov,reg_groups,by="ISO.code",all.x = TRUE)
  data_miss <- Stunt_data_w_cov[is.na(Stunt_data_w_cov$All_africa_HIW),]
  if(nrow(data_miss) > 0){
    warning("No region in the crosswalk for: ",
            paste(unique(as.character(data_miss$ISO.code)), collapse = ", "))
  }
  ### Capitalizing the region names (missing regions stay NA).
  region_chr <- tolower(as.character(Stunt_data_w_cov$All_africa_HIW))
  Stunt_data_w_cov$All_africa_HIW <- ifelse(
    is.na(region_chr), NA_character_, vapply(region_chr, simpleCap, character(1), USE.NAMES = FALSE)
  )
  Stunt_data_w_cov$country <- as.character(Stunt_data_w_cov$Country)
  ### Shorten "Eastern Asia, South Eastern Asia And Oceania Excluding Australia & New Zealand" region
  Stunt_data_w_cov$All_africa_HIW[Stunt_data_w_cov$All_africa_HIW=="Eastern Asia, South Eastern Asia And Oceania Excluding Australia & New Zealand"] <- "Eastern Asia, South Eastern Asia And Oceania"
  
  
  ### Check data to see if n' are correct
  length(unique(Stunt_data_w_cov$ISO.code))
  unique(Stunt_data_w_cov$All_africa_HIW)
  
  ### Doing some cleaning of the outcome data.
  if(marker == "Overweight"){
    ## Changing datapoint for India consultation
    Stunt_data_w_cov$Point.Estimate[Stunt_data_w_cov$ISO.code=="IND" & Stunt_data_w_cov$year == 1997] <- NA
    
    
    # DEMOCRATIC PEOPLE'S REP. OF KOREA (THE) MICS 2009 has 0 for overweight.  Here putting in the 1/(2*n).
    Stunt_data_w_cov$SE_val[Stunt_data_w_cov$Point.Estimate ==0 &
                              !is.na(Stunt_data_w_cov$Point.Estimate) ] <- 0.1
    Stunt_data_w_cov$Point.Estimate[Stunt_data_w_cov$Point.Estimate ==0 &
                                      !is.na(Stunt_data_w_cov$Point.Estimate) ] <- 
      min(Stunt_data_w_cov$Point.Estimate[Stunt_data_w_cov$Point.Estimate>0],na.rm=TRUE)
  }
  
  # Changing estimates=0 to the minimum value
  min_val_pr <- min(Stunt_data_w_cov$Point.Estimate[Stunt_data_w_cov$Point.Estimate>0],na.rm=TRUE)
  min_val_SE <- min(Stunt_data_w_cov$SE_val[Stunt_data_w_cov$SE_val>0],na.rm=TRUE)
  Stunt_data_w_cov <- as_tibble(Stunt_data_w_cov) %>% 
    arrange(Country,year) %>% 
    mutate(
      SE_val = case_when(
        SE_val == 0 ~ min_val_SE/100,
        TRUE ~ SE_val/100
      ),
      Point.Estimate = case_when(
        (Point.Estimate==0 & !is.na(Point.Estimate)) ~ min_val_pr/100,
        TRUE ~ Point.Estimate/100
      )
    ) 
  
  
  # Exporting data
  if(multiple_imputation){
    saveRDS(Stunt_data_w_cov,file = paste0("Data/Merged/",year,"/",marker,"_",month,"_reg_all_multiple_impute.rds"))
  }else{
    saveRDS(Stunt_data_w_cov,file = paste0("Data/Merged/",year,"/",marker,"_",month,"_reg_all_mean_multiple.rds"))
  }
  
}



