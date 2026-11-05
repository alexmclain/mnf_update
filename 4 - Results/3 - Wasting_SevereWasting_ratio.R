remove(list=ls())

library(tidyverse)
library(this.path)
wd <- dirname(this.path::here())
print(wd)
setwd(wd)

# Plots log(Wasting / SevereWasting) of the pooled Overall (both sexes) estimates
# by country, one page per region. Reads the output of 0 - Summarizing_imputed_results.R
# for both markers.
# Usage: Rscript "4 - Results/3 - Wasting_SevereWasting_ratio.R" [model]
#   model (optional): Wasting model tag used in 2 - Analysis_MI.R (e.g. "fpi").
#   The SevereWasting results are always the default model.

wast_tag <- as.character(commandArgs(trailingOnly = TRUE))
wast_tag <- if (length(wast_tag) >= 1) wast_tag[1] else ""
month <- "Oct" #Month Key to find data / name file
year <- "2026" #Year Key to find data / name file
marker_f <- paste0("")
date <- paste(marker_f,month,year)
date_w <- paste(wast_tag,month,year)   # Wasting results / output names

path_fig = paste0("Figures/Wasting/",month,year,if (wast_tag != "") paste0("_",wast_tag),"/")
if (!dir.exists(path_fig)) {
  dir.create(path_fig, recursive = TRUE, showWarnings = FALSE)
}

read_est <- function(m, d = date){
  filename <- paste0("Data/Analysis files/",m,"/",m," results ",d,".csv")
  if (!file.exists(filename)) {
    stop("Missing results file: ", filename)
  }
  read.csv(filename) %>%
    filter(Sex == "Overall") %>%
    # Group on ISO.code: survey rows can carry a different country name
    # (e.g. Turkey/Türkiye, Nauru/Naoero).
    group_by(ISO.code) %>%
    mutate(country = names(which.max(table(country)))) %>%
    ungroup() %>%
    # Annual estimates: the row with no survey in each country-year
    # (at the integer year; added in 4 - Sex_cross_walk.R).
    filter(is.na(UNICEFSurveyID)) %>%
    dplyr::select(ISO.code, country, Region, year, pred = Prediction)
}

ratio_data <- inner_join(
  read_est("Wasting", date_w) %>% rename(wast = pred),
  read_est("SevereWasting") %>% rename(sev = pred) %>% dplyr::select(ISO.code, year, sev),
  by = c("ISO.code", "year")
) %>%
  mutate(log_ratio = log(wast/sev)) %>%
  arrange(Region, country, year)

cat("Countries:", length(unique(ratio_data$country)), "\n")
print(summary(ratio_data$log_ratio))

reg_vals <- sort(unique(ratio_data$Region))
# Country-years where the SevereWasting estimate exceeds the Wasting estimate
sev_gt_wast <- ratio_data %>%
  filter(sev > wast) %>%
  dplyr::select(ISO.code, country, Region, year, Wasting = wast, SevereWasting = sev, log_ratio)
cat("Country-years with SevereWasting > Wasting:", nrow(sev_gt_wast),
    "in", length(unique(sev_gt_wast$country)), "countries\n")
writexl::write_xlsx(sev_gt_wast,
                    paste0("Data/Analysis files/Wasting/SevereWasting greater than Wasting ",date_w,".xlsx"))

pdf(paste0(path_fig,"log Wasting to SevereWasting ratio ",date_w,".pdf"), width = 15, height = 8)
for(k in reg_vals){
  plot_data <- ratio_data %>% filter(Region == k)
  # y-axis fixed within a region (not across regions: a few countries with
  # severe wasting near zero, e.g. the USA, would flatten every other panel)
  Lims <- range(c(plot_data$log_ratio, 0), na.rm = TRUE)

  p <- ggplot(plot_data, aes(x = year, y = log_ratio)) +
    geom_hline(yintercept = 0, color = "grey50", linetype = 2) +
    geom_line(color = "blue") +
    facet_wrap(~country, scales = "fixed") +
    ylim(Lims) +
    labs(x = "Year", y = "log(Wasting / Severe Wasting)",
         title = paste("log ratio of Wasting to Severe Wasting estimates for", k),
         caption = "Dashed line: Wasting = Severe Wasting") +
    theme_bw() +
    theme(axis.text = element_text(family = "Helvetica", color = "#666666", size = 10),
          axis.title = element_text(family = "Helvetica", color = "#666666", face = "bold", size = 22))
  print(p)
}
dev.off()
