remove(list=ls())

library(this.path)
wd <- dirname(this.path::here())
print(wd)
setwd(wd)
library(tidyverse)

marker <- as.character(commandArgs(trailingOnly = TRUE))
if(length(marker) == 0){marker <- "Wasting"}
year <- "2026"
month <- "Oct" #for finding the data

## Exploratory plots of each covariate against the logit-transformed outcome,
## by region. Uses the mean-imputed covariates (one row per country/year/sex)
## and only the observed outcomes.

## Path to output folder
path <- paste0("Figures/Exploratory/")
dir.create(path, recursive = TRUE, showWarnings = FALSE)

#### 1. Reading and formatting the data (as in 1 - Analysis_MI_sexf.R) ####
all_data <- readRDS(paste0("Data/Merged/",year,"/",marker,"_",
                           month,"_final_mean_impute.rds")) |>
  mutate(
    Sex_code = case_when(
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
  dplyr::select(c("country","year","middle_year", "Point.Estimate.NS", "Point.Estimate.Imp",
                  "SE_val", "ShortSource", "Region", "Sex", "Sex_code",
                  "SMART", "Surveillance",
                  "SDI", "gdp", "gdp_pc", "log_gdp_pc", "dpt_imm",
                  "fpi_avg",
                  "fpi_prev5yr_avg",
                  "fpi_anom", "shock", "war_terror")) |>
  rename("Y" = "Point.Estimate.NS",
         "Y_all" = "Point.Estimate.Imp",
         "SE_var" = "SE_val") |>
  arrange(country, year) |>
  filter(!is.na(Y))

cat(length(unique(all_data$country)), "countries,", nrow(all_data),
    "observed outcomes\n")
print(table(all_data$Region, all_data$Sex))

## Outcome on the logit scale, as in the model.
all_data <- all_data |>
  mutate(
    # Y = pmin(pmax(Y, 1e-6), 1 - 1e-6),
    logit_Y = log(Y/(1-Y))
  )

#### 2. Covariates and the transformation used for plotting ####
## Skewed covariates are plotted on a transformed scale (axis label says so).
covariates <- tribble(
  ~var,              ~label,                        ~trans,
  "year",            "Year",                        identity,
  "SDI",             "SDI",                         identity,
  "gdp",             "GDP per capita growth (%)",   identity,
  "gdp_pc",          "log(GDP per capita)",         log,
  "dpt_imm",         "logit(DPT immunization %)",   function(x) qlogis(pmin(pmax(x, 1), 99)/100),
  "fpi_avg",         "asinh(FPI average)",          asinh,
  "fpi_prev5yr_avg", "asinh(FPI 5-yr average)",     asinh,
  "fpi_anom",        "asinh(FPI anomaly)",          asinh,
  "shock",           "log(shock)",                  log,
  "war_terror",      "log(war/terror)",             log
) |>
  filter(var %in% names(all_data))

#### 3. Plots: for each covariate, by region, by sex and overall ####
## Each covariate gets a by-region page, a by-sex page and an overall page. Covariates that
## are transformed for plotting also get both on the original (non-transformed)
## scale.
plot_cov <- function(x, x_label, by = c("region", "sex", "overall")){
  by <- match.arg(by)
  plot_data <- all_data |>
    mutate(x = x) |>
    filter(is.finite(x))
  n_miss <- nrow(all_data) - nrow(plot_data)

  p <- ggplot(plot_data, aes(x = x, y = logit_Y)) +
    geom_point(aes(colour = Sex), alpha = 0.35, size = 0.8) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                colour = "black", linewidth = 0.6) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                colour = "red", linewidth = 0.6, linetype = "dashed") +
    labs(
      title = paste0(marker, ": ", x_label, " vs. logit(outcome)",
                     switch(by, region = ", by region", sex = ", by sex", overall = ", overall")),
      subtitle = paste0("Black = linear fit, red dashed = loess. ",
                        n_miss, " observations with a missing covariate not shown."),
      x = x_label, y = "logit(outcome)"
    ) +
    theme_bw() +
    theme(legend.position = "bottom", strip.text = element_text(size = 7))
  if(by == "region"){
    p <- p + facet_wrap(~Region, scales = "fixed")
  }
  if(by == "sex"){
    p <- p + facet_wrap(~Sex)
  }
  print(p)
}

pdf(paste0(path, marker, " covariates by logit outcome.pdf"),
    width = 12, height = 9)
for(i in seq_len(nrow(covariates))){
  v <- covariates$var[i]
  x_trans <- covariates$trans[[i]](all_data[[v]])

  # Plotting scale (the original scale for untransformed covariates)
  plot_cov(x_trans, covariates$label[i], by = "region")
  plot_cov(x_trans, covariates$label[i], by = "sex")
  plot_cov(x_trans, covariates$label[i], by = "overall")

  # Original (non-transformed) scale
  if(!identical(covariates$trans[[i]], identity)){
    plot_cov(all_data[[v]], paste0(v, " (non-transformed)"), by = "region")
    plot_cov(all_data[[v]], paste0(v, " (non-transformed)"), by = "sex")
    plot_cov(all_data[[v]], paste0(v, " (non-transformed)"), by = "overall")
  }
}
dev.off()

#### 4. Covariate correlation with the outcome, by region ####
cor_table <- map_dfr(seq_len(nrow(covariates)), function(i){
  v <- covariates$var[i]
  trans_fun <- covariates$trans[[i]]
  all_data |>
    mutate(x = trans_fun(.data[[v]])) |>
    filter(is.finite(x)) |>
    group_by(Region) |>
    summarise(
      covariate = covariates$label[i],
      n = n(),
      cor = if(n() > 2 && sd(x) > 0) cor(x, logit_Y, method = "spearman") else NA_real_,
      .groups = "drop"
    )
})
cor_wide <- cor_table |>
  select(-n) |>
  pivot_wider(names_from = covariate, values_from = cor)
print(cor_wide, width = Inf)
write_csv(cor_wide, paste0(path, marker, " covariate correlations by region.csv"))

#### 5. Outcome over time by region ####
p_time <- ggplot(all_data, aes(x = year, y = logit_Y, group = country)) +
  geom_line(alpha = 0.3) +
  geom_point(aes(colour = Sex), size = 0.6, alpha = 0.5) +
  facet_wrap(~Region) +
  labs(title = paste0(marker, ": observed logit(outcome) over time, by region"),
       x = "Year", y = "logit(outcome)") +
  theme_bw() + theme(legend.position = "bottom", strip.text = element_text(size = 7))
ggsave(paste0(path, marker, " outcome over time by region.pdf"), p_time,
       width = 12, height = 9)
