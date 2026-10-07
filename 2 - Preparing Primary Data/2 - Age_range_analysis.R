

library(this.path)
wd <- dirname(this.path::here())
print(wd)
setwd(wd)

library(tidyverse)
library(dplyr)
library(nlme)

marker <- as.character(commandArgs(trailingOnly = TRUE))
year <- "2026"

stunt_data <- readRDS(paste0("Data/JME/",year,"/Cleaned/",marker,"_SE_clean.rds"))

# Max UNICEF ID
max_unicefid <- max(stunt_data$UNICEFSurveyID, na.rm = TRUE) +1

stunt_data_slim <- stunt_data %>% 
  mutate(Notes=ExtendedDisplayFootnotes) %>% 
  rename(
    Age_range = StandardDisaggregations
  ) %>% 
  mutate(Sex = Age_range, 
         # Identifying unadjusted surveys.
         unadj_ind = case_when(
           grepl("nadjust",Notes) ~ 1,
           grepl("nadjust",StandardFootnotes) ~ 1,
           TRUE ~ 0
         )
  ) %>% 
  # Creating a fake UNICEF ID when one is missing. All rows (sexes and age 
  # ranges) from the same country, year, and source share the same fake ID.
  group_by(country, year, ShortSource) %>% 
  mutate(
    UNICEFSurveyID = case_when(
      !is.na(UNICEFSurveyID) ~ UNICEFSurveyID,
      TRUE ~ max_unicefid + as.numeric(cur_group_id())
    )
  ) %>% 
  ungroup()

##### Clean up the notes variable #####
stunt_data_slim <- stunt_data_slim %>% 
  mutate(
    Notes = case_when(
      grepl("months",Notes) ~ Notes,
      grepl("Months",Notes) ~ Notes,
      grepl("Age",Notes) ~ Notes,
      TRUE ~ NA_character_
    )
  )
sort(unique(stunt_data_slim$Notes))

## Change those that are close to full age range to full age range
stunt_data_slim <- stunt_data_slim %>% 
  mutate(
    Notes = case_when(
      Notes == "0-71 months" ~ NA_character_,
      Notes == "2-60 months" ~ NA_character_,
      grepl("Age 0-2 months",Notes) ~ NA_character_,
      grepl("Age 0-5 months",Notes) ~ NA_character_,
      TRUE ~ Notes
    )
  )
sort(unique(stunt_data_slim$Notes))

## Combining like categories
# Patterns are anchored so that e.g. "0-23 months" does not match "10-23 months".
anch <- function(x) paste0("(^|[^0-9])", x)
stunt_data_slim <- stunt_data_slim %>% 
  mutate(
    Notes = case_when(
      grepl(anch("0-23 months"),Notes) ~ "Age interval 0-23 months",
      grepl(anch("0-35 months"),Notes) ~ "Age interval 0-36 months",
      grepl(anch("0-36 months"),Notes) ~ "Age interval 0-36 months",
      grepl(anch("0-47 months"),Notes) ~ "Age interval 0-48 months",
      grepl(anch("0-48 months"),Notes) ~ "Age interval 0-48 months",
      grepl(anch("0-52 months"),Notes) ~ "Age interval 0-52 months",
      grepl("1-4;",Notes)        ~ "Age interval 12-60 months",
      grepl(anch("12-60 months"),Notes) ~ "Age interval 12-60 months",
      grepl(anch("12-59 months"),Notes) ~ "Age interval 12-60 months",
      grepl(anch("24-59 months"),Notes) ~ "Age interval 24-60 months",
      grepl(anch("3-36 months"),Notes) ~ "Age interval 3-36 months",
      grepl(anch("36-59 months"),Notes) ~ "Age interval 36-60 months",
      grepl(anch("5-47 months"),Notes) ~ "Age interval 6-48 months",
      grepl(anch("6-36 months"),Notes) ~ "Age interval 6-36 months",
      TRUE ~ Notes
    )
  )
stunt_data_slim$Notes <- gsub("; unadjusted","",stunt_data_slim$Notes)
sort(unique(stunt_data_slim$Notes))


##### Clean up Age_range and Sex variables

stunt_data_slim %>% distinct(Age_range)
stunt_data_slim %>% distinct(Sex)

stunt_data_slim <- stunt_data_slim %>% 
  mutate(
    Sex = case_when(
      grepl("Female",Sex) ~ "Female",
      grepl("Male",Sex) ~ "Male",
      TRUE ~ "Both"
    ),
    Age_range = case_when(
      Age_range == "Male" ~ "National",
      Age_range == "Female" ~ "National",
      TRUE ~ Age_range
    )
  ) %>% 
  mutate(
    Age_range = str_remove(Age_range, "Male ")
  ) %>% 
  mutate(
    Age_range = str_remove(Age_range, "Female ")
  )  %>% 
  mutate(
    Sex = as.factor(Sex),
    Age_range = as.factor(Age_range)
  )

stunt_data_slim %>% distinct(Age_range)
stunt_data_slim %>% distinct(Sex)


## Only national estimates
national_only <- stunt_data_slim %>% 
  rename(                        
    "nat"="Point.Estimate"
  ) %>% 
  mutate(nat=nat/100) %>% 
  group_by(Sex , UNICEFSurveyID) %>% 
  filter(Age_range == "National")  %>% 
  select(c(country, year, nat, Sex, UNICEFSurveyID)) 

## Checking the uniqueness of the keys
keys <- c("country", "year", "UNICEFSurveyID", "Sex", "Age_range")

dup_x <- stunt_data_slim %>%
  count(across(all_of(keys))) %>%
  filter(n > 1)

dup_y <- national_only %>%
  count(across(all_of(keys[-5]))) %>%
  filter(n > 1)

dup_x
dup_y


### Transforming data to difference from national estimate
all_data <- stunt_data_slim %>% 
  rename(                          
    "Y"="Point.Estimate"
  ) %>% 
  mutate(Y=Y/100) %>% 
  group_by(Sex , UNICEFSurveyID) %>% 
  left_join(
    national_only, 
    by = c("country", "year", "UNICEFSurveyID", "Sex")
  ) %>% 
  mutate(Y2 = Y-nat) %>%  
  arrange(UNICEFSurveyID) %>% 
  mutate(Age_range=as.character(Age_range)) %>% 
  ungroup()

### Create extra rows for the missing age ranges (so they can be predicted)
### Only done if Sex is in the data.

all_data <- all_data %>% 
  # Removing sparse age ranges
  filter(Age_range != "6 to 8 months" & 
           Age_range != "9 to 11 months") %>% 
  complete(UNICEFSurveyID, Sex, Age_range) %>% 
  # Fill within survey (and survey-by-sex for the sex-specific national value)
  group_by(UNICEFSurveyID) %>% 
  fill(country, year, unadj_ind, .direction = "updown") %>% 
  group_by(UNICEFSurveyID, Sex) %>% 
  fill(nat, .direction = "updown") %>% 
  filter(!all(is.na(Y))) %>% 
  ungroup() %>% 
  # Flagging missing age groups, the national estimate and estimates with a small 
  # sample size for removal. National estimates are not used in the analysis 
  # (the difference is 0 by definition). Estimates with a small sample size 
  # are treated as missing (and predicted).
  mutate(flag = case_when(
    is.na(Y) ~ 0,
    Age_range == "National" ~ 0,
    !is.na(unweighted_N) & unweighted_N < 30 ~ 0,
    TRUE ~ 1
  ))

### Setting flagged rows to NA.
all_data <- all_data %>% 
  mutate(
    Y2 = case_when(
      flag==0 ~ NA_real_,
      Age_range=="National" ~ NA_real_,
      TRUE ~ Y2
    )
  ) %>% 
  # Create new age range variable that will not have "national" and be a factor.
  arrange(country,year) %>% 
  mutate( 
    Age_range2 = if_else( 
      Age_range=="National", 
      "0 to 5 months", 
      Age_range
    )
  ) %>%
  mutate( 
    Age_range2 = relevel( 
      as.factor(Age_range2), 
      ref = "36 to 47 months"
    )
  )





### Running the model

## Setting the regression form based on if multiple sex's are included.
n_sex <- length(unique(all_data$Sex))
if(n_sex > 1){
  mm_form <- Y2 ~ Age_range2 + nat + nat:Age_range2 + year + Sex + Sex:Age_range2
}else{
  mm_form <- Y2 ~ Age_range2 + nat + nat:Age_range2 + year
}

fitted_model <- lme(
  mm_form,
  random=~1|country,
  weights = varExp(form=~SE_val),
  data = all_data,
  na.action = na.omit
)
summary(fitted_model)
anova.lme(fitted_model)

### Getting the predicted difference between national and age level prevalence
### and the predicted age level prevalence by adding the national prevalence
pred <- predict( fitted_model, newdata = all_data)
stunt_data_w_pred <- all_data %>% 
  add_column( pred = pred) 

## Age groups that must be imputed but could not be predicted by the model 
## (e.g., countries not in the model fit). These are set equal to the national 
## prevalence (difference of 0). Reporting them here.
fallback_ind <- stunt_data_w_pred$flag == 0 & 
  stunt_data_w_pred$Age_range != "National" & 
  is.na(stunt_data_w_pred$pred)
message(sum(fallback_ind), " imputed age-group rows had no model prediction ",
        "and were set to the national prevalence.")
if(any(fallback_ind)){
  print(
    stunt_data_w_pred[fallback_ind, ] %>% 
      count(country, UNICEFSurveyID, Sex, name = "n_fallback")
  )
}

stunt_data_w_pred <- stunt_data_w_pred %>% 
  mutate(
    pred = case_when(
      Age_range == "National" ~ 0, 
      is.na(pred) ~ 0, 
      TRUE ~ pred
    ),
    Y2 = case_when(
      flag == 0 ~ pred, 
      TRUE ~ Y2
    )
  ) %>% 
  rename(
    "PointEstimate"="Y"
  ) %>% 
  mutate(
    Adj_PointEstimate = case_when(
      Age_range == "National" ~ PointEstimate, 
      TRUE ~ Y2+nat
    ),
    Adj_PointEstimate = case_when(
      Adj_PointEstimate < 0 ~ 0, 
      TRUE ~ Adj_PointEstimate
    )
  )  %>% 
  select(-c(Age_range2,Y2,pred,nat))


### Using all of the predicted age level prevalence to predict the 
### national level prevalence (if adjustement is necessary)
# Proportion of the 0-59 month period covered by each age group.
q_vec <- c(
  "0 to 5 months" = 6, "6 to 11 months" = 6, "12 to 23 months" = 12,
  "24 to 35 months" = 12, "36 to 47 months" = 12, "48 to 59 months" = 12
)/60
UN_ID <- unique(stunt_data_w_pred$UNICEFSurveyID)
for(j in UN_ID){
  t_data_surv <- stunt_data_w_pred %>% 
    filter(UNICEFSurveyID==j) 
  sex_vals <- unique(t_data_surv$Sex)
  for(k in sex_vals){
    # All rows (including national) for this survey and sex
    t_data_sex <- t_data_surv %>% filter(Sex == k)
    # Standard age groups only, in the order of q_vec
    t_data <- t_data_sex %>% 
      filter(Age_range %in% names(q_vec)) %>% 
      arrange(match(Age_range, names(q_vec)))
    # Partial age range or unadjusted survey?
    partial <- max(t_data_sex$unadj_ind, na.rm = TRUE) == 1 | 
      any(!is.na(t_data_sex$Notes))
    is_nat <- stunt_data_w_pred$Age_range=="National" & 
      stunt_data_w_pred$UNICEFSurveyID==j &
      stunt_data_w_pred$Sex == k
    if(!partial || nrow(t_data) == 0){
      stunt_data_w_pred$Adj_PointEstimate[is_nat] <- 
        stunt_data_w_pred$PointEstimate[is_nat]
    }else{
      t_data$weighted_N[t_data$flag==0] <- NA
      #### Calculating the sample size in each category (desired N is from the weighted N of the study)
      q_k <- q_vec[t_data$Age_range]
      weights_vec <- q_k
      if(any(!is.na(t_data$weighted_N))){weights_vec <- t_data$weighted_N}
      if(any(is.na(weights_vec))){
        total_N <- sum(weights_vec[!is.na(weights_vec)])/(sum(q_k[!is.na(weights_vec)]))
        weights_vec[is.na(weights_vec)] <- total_N*q_k[is.na(weights_vec)]
      }
      weights_vec <- weights_vec/sum(weights_vec)
      weighted_est <- sum(t_data$Adj_PointEstimate*weights_vec)
      stunt_data_w_pred$Adj_PointEstimate[is_nat] <- weighted_est
    }
  }
}

### Inspecting the results
nat_only <- stunt_data_w_pred %>%
 filter(Age_range=="National") %>%
 mutate(Diff = PointEstimate - Adj_PointEstimate)
# nat_only %>% View()

## Checking the uniqueness of the keys
keys <- c("country", "year", "UNICEFSurveyID", "Sex")

dup_x <- nat_only %>%
  count(across(all_of(keys))) %>%
  filter(n > 1)

dup_x



ggplot(data=nat_only, aes(x=PointEstimate,y=Diff)) +
 geom_point() +
 facet_grid(~Sex,scales = "fixed")



# Removing rows that were added for age adjustment.
# Creating an indicator for age adjustment.
stunt_data_w_pred <- stunt_data_w_pred %>% 
  filter(!is.na(PointEstimate)) %>% 
  mutate(Adj_indicator = case_when(
    round(PointEstimate,6) == round(Adj_PointEstimate,6) ~ 0,
    TRUE ~ 1
  ))


# Export Data
saveRDS(
  stunt_data_w_pred,
  paste0("Data/JME/",year,"/Cleaned/",marker,"_SE_Age_clean.rds")
  )
