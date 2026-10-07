
library(this.path)
wd <- dirname(this.path::here())
print(wd)
setwd(wd)

source("Utils/Programs_Feb_2020.R")
library(tidyverse)
library(mvtnorm)
library(readxl)
library(mice)
library(parallel)
library(doParallel)

###### 0. Settings   ###############

first_year    <- 1984   # first year of covariate data
last_obs_year <- 2022   # last year with observed SDI (IHME GBD 2022)
last_year     <- 2025   # last year to impute for
trend_start_year <- 2014 # first year used in the SDI trend model (for years after last_obs_year)
jme_year      <- "2026" # year folder of the JME raw data (only used for the country list)
gdp_file      <- "Data/IHME_covs/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_4701247.csv"
## ^ update to the new World Bank GDP file (must include years up to last_year)

B <- 20 # Number of imputations
seed_base <- 238746

## Number of cores: the largest divisor of B that is <= the available cores - 1.
## This keeps n.core * n.imp.core == B (parlmice overrides m otherwise).
max_cores <- max(1, min(B, parallel::detectCores() - 1))
n_core <- max(Filter(function(d) B %% d == 0, seq_len(max_cores)))
n_imp_core <- B / n_core

###### 1. Read in and Merge Data   ###############

## Read in IHME covariate data
IHME_dat_2022 <- read_csv("Data/IHME_covs/GBD 2022 MCI and SDI.csv") %>%
  select(c(location_name, year, SDI, ISO.code))


## Get all the countries that there are data for
## From JME
jme_file <- paste0("Data/JME/", jme_year, "/Raw/JME_Country_Level_Input_Stunting.xlsx")
if(!file.exists(jme_file)){
  stop("JME file not found: ", jme_file, ". Update `jme_year` or add the file.")
}
surv_data <- read_excel(jme_file)
surv_data <- surv_data %>%
  group_by(ISO3Code) %>%
  filter(row_number() == 1) %>%
  select(c(ISO3Code, Country)) %>%
  ungroup()
# From IHME data
IHME_countries <- IHME_dat_2022 %>%
  rename(ISO3Code = ISO.code, Country = location_name) %>%
  group_by(ISO3Code) %>%
  filter(row_number() == 1) %>%
  select(c(ISO3Code)) %>%
  ungroup()
# Full merge
All_countries <- surv_data %>%
  full_join(IHME_countries, by = "ISO3Code") %>%
  bind_rows(
    tibble(ISO3Code = "XKX", Country = "Kosovo")
  ) %>%
  filter(!is.na(ISO3Code)) %>%
  distinct(ISO3Code, .keep_all = TRUE)

## Merge with Fertility and GDP data
wpp_dat <- read_csv("Data/IHME_covs/WPP2022_Demographic_Indicators_Medium.csv",
                    show_col_types = FALSE) %>%
  filter(!is.na(ISO3_code)) %>%
  rename(year = Time, ISO3Code = ISO3_code) %>%
  filter(year >= first_year & year <= last_year)

## Complete country-by-year grid, so that every country has a row for every year
## (countries without WPP/IHME data, e.g., Kosovo, would otherwise get one row
## with a missing year).
country_year_grid <- expand_grid(
  ISO3Code = All_countries$ISO3Code,
  year = first_year:last_year
)
big_wpp_dat <- country_year_grid %>%
  left_join(All_countries, by = "ISO3Code") %>%
  left_join(wpp_dat, by = c("ISO3Code", "year")) %>%
  left_join(IHME_dat_2022, by = c("ISO3Code" = "ISO.code", "year")) %>%
  mutate(location_name = case_when(
    is.na(location_name) ~ Country,
    TRUE ~ location_name
  )
  )


GDP_wide <- read_csv(gdp_file, show_col_types = FALSE)
if(!all(c("Country Code", as.character(last_year)) %in% names(GDP_wide))){
  stop("The GDP file must have a `Country Code` column and a column for ", last_year, ".")
}
GDP_long <- GDP_wide %>%
  pivot_longer(
    matches("^[0-9]{4}$"), names_to = "year",
    names_transform = list(year = as.numeric),
    values_to = "GDP") %>%
  filter(year >= first_year & year <= last_year) %>%
  rename(ISO3Code = `Country Code`) %>%
  select(c(ISO3Code, year, GDP))

all_cov <- big_wpp_dat %>%
  left_join(GDP_long, by=c("ISO3Code", "year")) %>%
  mutate(lGDP = log(GDP),
         lpop = log(TPopulation1Jan),
         c_factor = factor(LocID)) %>%
  select(c("ISO3Code","c_factor", "location_name", "year", "SDI", "lGDP",
           "lpop","PopDensity", "MedianAgePop", "CBR", "TFR",
           "Births1519", "CDR", "LEx", "IMR", "Q5")) %>%
  ## Country names are missing after the last IHME year for IHME-only countries.
  group_by(ISO3Code) %>%
  fill(location_name, .direction = "downup") %>%
  ungroup() %>%
  filter(!is.na(ISO3Code)) %>%
  group_by(ISO3Code, year) %>%
  filter(row_number() == 1) %>%
  ungroup()

stopifnot(!anyNA(all_cov$year))

##### 2. Imputation to Create Country-Level Means by Imputation   ###############
##### This first imputation will be used to generate the country level means by imputation.
ini <- mice(all_cov, maxit = 0)

pred <- ini$pred
meth <- ini$method
pred["SDI",] <- pred["lGDP",] <- c(0,0,0, rep(1,length(pred["SDI",]) - 3))
diag(pred) <- 0

imp <- parlmice(data = all_cov, n.core = n_core, n.imp.core = n_imp_core,
                pred = pred, meth = meth, print = TRUE,
                m = B, maxit = 20, cluster.seed = seed_base)

all_cov_comp <- mice::complete(imp, action = "long")

all_cov_comp <- all_cov_comp %>%
  as_tibble() %>%
  group_by(.imp,ISO3Code) %>%
  filter(year <= last_year) %>%
  mutate(mn_SDI = mean(SDI),
         mn_lGDP = mean(lGDP)) %>%
  ungroup() %>%
  select(-c("SDI","lGDP"))



##### 3. Perform Imputation on Mean Centered Data  ###############
##### Mean center the data by country and re-impute using wide format.
all_cov_center_SDI_wide <- all_cov %>%
  group_by(ISO3Code) %>%
  mutate(Z_SDI = SDI - mean(SDI, na.rm = TRUE)) %>%
  ungroup() %>%
  select(c("ISO3Code","c_factor", "year", "Z_SDI")) %>%
  pivot_wider(names_from = year, values_from = Z_SDI,
              names_glue = "{.value}_{year}",
              values_fn = {mean})

ini <- mice(all_cov_center_SDI_wide, maxit = 0)

pred <- ini$pred
pred[1:3,] <- 0
pred[,1:3] <- 0
meth <- ini$method
meth[names(meth)=="Z_SDI_2022"] = "pmm"

imp_sdi <- parlmice(data = all_cov_center_SDI_wide, n.core = n_core,
                    n.imp.core = n_imp_core,
                    pred = pred, meth = meth, print = TRUE,
                    m = B, maxit = 20, cluster.seed = seed_base + 1)

all_cov_center_SDI_wide <- mice::complete(imp_sdi, action = "long")
all_cov_comp_wide_long <- all_cov_center_SDI_wide %>%
  pivot_longer(
    starts_with("Z_SDI_"), names_prefix = "Z_SDI_",
    names_to = "year",
    names_transform = list(year = as.numeric),
    values_to = "Z_SDI") %>%
  ## There are no observations after the last observed year. The values for these
  ## years are estimated in Section 4, so the (uninformative) imputed values are
  ## removed here.
  mutate(
    Z_SDI = case_when(
      year > last_obs_year ~ NA_real_,
      TRUE ~ Z_SDI
    )
  )

save(
  all_cov, all_cov_comp, all_cov_comp_wide_long,
  file = "Data/IHME_covs/Imputation_1Aug24.RData"
)



############### 4. Perform Imputation for after the last observed year for All Countries ###############
P_all_data <- all_cov_comp %>%
  left_join(
    all_cov_comp_wide_long %>%
      select(".imp","ISO3Code","year", "Z_SDI"),
    by = c(".imp","ISO3Code","year")
  ) %>%
  mutate(imp_SDI = mn_SDI + Z_SDI)

## The additive reconstruction can fall outside the valid range of SDI, which
## would give NaN/Inf on the logit scale below. Values are clamped to [sdi_min, sdi_max].
sdi_min <- 0.001
sdi_max <- 1
n_out_of_range <- sum(
  !is.na(P_all_data$imp_SDI) &
    (P_all_data$imp_SDI < sdi_min | P_all_data$imp_SDI > sdi_max)
)
message(n_out_of_range, " imputed SDI values were outside [", sdi_min, ", ",
        sdi_max, "] and were clamped.")
P_all_data <- P_all_data %>%
  mutate(imp_SDI = pmin(pmax(imp_SDI, sdi_min), sdi_max))

if(anyNA(P_all_data$imp_SDI[P_all_data$year > 1989 & P_all_data$year <= last_obs_year])){
  stop("Missing imputed SDI values up to ", last_obs_year, ".")
}

P_all_data <- P_all_data %>%
  mutate(
    country = ISO3Code,
    Y= log(imp_SDI/1.15/(1-imp_SDI/1.15)),
    SE_var = 1) %>%
  group_by(.imp,ISO3Code,year) %>%
  filter(row_number()==1) %>%
  ungroup()

registerDoParallel(cores = n_core)

settings_to_try <- 1:B
foreach(set_i = settings_to_try,
        .packages = (.packages()))  %dopar% {
          source("Utils/Programs_Feb_2020.R")

  j <- set_i
  all_data <- P_all_data %>%
    filter(.imp == j & year >= trend_start_year)
  data_w_out <- all_data %>%
    select(c("country", "year", "Y", "SE_var"))
  data_w_out$SE_pred <- data_w_out$SE_var

  DF_R <- quantile(data_w_out$year,probs = c(0.5))
  B.knots <- range(data_w_out$year[!is.na(data_w_out$Y)])
  B.knots[1] <- B.knots[1] - 1
  B.knots[2] <- B.knots[2] + 10
  DF_P <- seq(min(data_w_out$year),last_obs_year,2)
  cov_data <- as.matrix(data.frame(Sex = all_data[,c("lpop")], lpop2 = all_data[,c("lpop")]^2))
  colnames(cov_data)[1] <- "Sex"
  zero_covs <- NULL
  cov_mat <- "VC"
  q.order <- 2

  ######### Covariate analysis with multiple penalized functions
  Estimation <- cmnpe(data_w_out, DF_P, DF_R, B.knots, q.order,
                      cov_data=cov_data, Pcov_data = NULL,
                      cov_mat = cov_mat, plots=FALSE, TRANS=FALSE,
                      zero_covs = zero_covs, slope = TRUE)
  cat(j,2*Estimation$df - 2*c(summary(Estimation$result$model)$logLik) + summary(Estimation$result$model)$AIC+2*c(summary(Estimation$result$model)$logLik),"\n")

  t_plot_data <- Estimation$plot_data
  saveRDS(t_plot_data, file = paste0("Data/IHME_covs/Plot data for SDI imputation ",j,".rds"))

}

stopImplicitCluster()

save(
  all_cov, all_cov_comp, all_cov_comp_wide_long, P_all_data,
  file = "Data/IHME_covs/Imputation_1Aug24.RData"
)

## Draw the values after the last observed year from the predictive distribution.
## A single standard normal draw is used per country and imputation (scaled by the
## predictive SD of each year) so the draws are not independent noise from year to
## year. The seed makes the draws reproducible.
plot_dat <- tibble()
for(j in 1:B){
  set.seed(seed_base + j)

  t_plot_data <- readRDS(file = paste0("Data/IHME_covs/Plot data for SDI imputation ",j,".rds"))
  t_plot_data <- t_plot_data %>%
    mutate(.imp = j) %>%
    filter(year > last_obs_year) %>%
    mutate(ISO3Code = country) %>%
    group_by(ISO3Code,year) %>%
    filter(row_number()==1) %>%
    ungroup() %>%
    select(.imp,ISO3Code, year,pred,sigma_Y_est) %>%
    group_by(ISO3Code) %>%
    mutate(z_draw = rnorm(1)) %>%
    ungroup()

  t_plot_data$SDI_imp <- exp(t_plot_data$pred +
                               t_plot_data$z_draw*t_plot_data$sigma_Y_est)
  t_plot_data$SDI_imp <- 1.15*t_plot_data$SDI_imp/(1 + t_plot_data$SDI_imp)

  t_plot_data <- t_plot_data %>%
    select(.imp,ISO3Code, year,SDI_imp)
  plot_dat <- plot_dat %>%
    bind_rows(t_plot_data)
}


P_all_data <- P_all_data %>%
  left_join(plot_dat, by = c(".imp","ISO3Code","year")) %>%
  mutate(
    imp_SDI = case_when(
      year > last_obs_year ~ SDI_imp,
      TRUE ~ imp_SDI
    ))

if(anyNA(P_all_data$imp_SDI[P_all_data$year > 1989])){
  stop("Missing imputed SDI values after 1989 (check the trend model output).")
}





##### 5. Merge Data and Create Final Variables  ###############
### Final data
fin_all_cov <- P_all_data %>%
  select(c(".imp","ISO3Code","year","imp_SDI", "location_name")) %>%
  left_join(
    all_cov,
    by = c("ISO3Code", "year", "location_name")
  ) %>%
  select(
    c(".imp","ISO3Code","year","SDI","imp_SDI","lGDP", "location_name")
  )

### Checking model fit
samp_coun <- fin_all_cov %>% filter(ISO3Code %in% sample(unique(fin_all_cov$ISO3Code),20))

ggplot(samp_coun, aes(x = year, y = imp_SDI, group = .imp)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~location_name)


here_coun <- fin_all_cov %>%
  filter(ISO3Code == "XKX" | ISO3Code == "TCA" )

ggplot(here_coun,
       aes(x = year, y = imp_SDI, group = .imp, color = .imp)) +
  geom_line(show.legend = FALSE) +
  facet_grid(~ISO3Code)

all_cov <- fin_all_cov %>%
  mutate(
    SDI = imp_SDI) %>%
  select(
    c(".imp","ISO3Code","year","SDI","lGDP", "location_name")
  ) %>%
  group_by(.imp,ISO3Code,year) %>%
  filter(row_number()==1) %>%
  ungroup()



cov_data <- readRDS("Data/IHME_covs/Tidy_World_Devel_Indic.rds")

all_cov_f <- all_cov %>%
  mutate(Sex = "Female")
all_cov_m <- all_cov %>%
  mutate(Sex = "Male")
all_cov <- all_cov %>%
  mutate(Sex = "Both") %>%
  bind_rows(all_cov_m) %>%
  bind_rows(all_cov_f) %>%
  arrange(ISO3Code, year, Sex)

P_cov_data <- cov_data %>%
  full_join(all_cov, by = c("ISO.code"="ISO3Code", "year", "Sex")) %>%
  mutate(location_name = case_when(
    !is.na(location_name.x) ~ location_name.x,
    !is.na(location_name.y) ~ location_name.y,
    TRUE ~ as.character(Country)
  )) %>%
  select(-c("location_name.x", "location_name.y"))

cov_data <- P_cov_data %>%
  arrange(.imp,ISO.code, Sex, year) %>%
  filter(year > 1989) %>%
  group_by(.imp,ISO.code, Sex) %>%
  fill(Region:Country) %>%
  ungroup()

here_coun <- cov_data %>%
  filter(ISO.code == "XKX" | ISO.code == "TCA" )

samp_coun <- cov_data %>% filter(ISO.code %in% sample(unique(cov_data$ISO.code),36))

ggplot(samp_coun, aes(x = year, y = SDI, group = .imp)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~location_name)

saveRDS(cov_data,"Data/IHME_covs/Multiple_Imputed_Mar2023.rds")


### Single imputation file: the average over the imputations of every numeric
### variable (NA if all are missing). Non-numeric variables (e.g., Region, Country,
### income group) are carried over.
mean_na <- function(x){
  if(all(is.na(x))){NA_real_}else{mean(x, na.rm = TRUE)}
}
cov_data_mean <- cov_data %>%
  group_by(ISO.code, Sex, year) %>%
  summarise(
    across(where(is.numeric) & !any_of(".imp"), mean_na),
    across(!where(is.numeric), first),
    .groups = "drop"
  )

saveRDS(cov_data_mean,"Data/IHME_covs/Single_Impute_Mar2023.rds")
