remove(list=ls())

library(this.path)
wd <- dirname(this.path::here())
print(wd)
setwd(wd)
source("Utils/Programs_Oct_2026.R")

library(tidyverse)

# Usage: Rscript "4 - Results/0 - Summarizing_imputed_results.R" <marker> [model]
#   model: model tag used in 2 - Analysis_MI.R (e.g. "fpi"); omitted = default model.
args <- as.character(commandArgs(trailingOnly = TRUE))
if (!length(args) %in% 1:2) {
  stop("Expected a marker argument and an optional model tag.")
}
marker <- args[1]
model_tag <- if (length(args) == 2) args[2] else ""

month <- "Oct" #Month Key to name file
year <- "2026" #Year Key to name file

month_o <- "Oct" #Month Key to find data
year_o <- "2026"#Year Key to find data
marker_f <- model_tag

date <- paste(marker_f,month,year) #for appending filename


path = paste0("Data/Analysis files/",marker,"/")

mi_name <- paste0(marker, if (model_tag != "") paste0("_", model_tag))
estimation_path <- paste0(path,"/Mi_files/","Estimation ",mi_name,".rds")
if (!file.exists(estimation_path)) {
  stop("Missing Estimation file: ", estimation_path)
}
Estimation <- readRDS(estimation_path)

### Summarizing imputed samples:
boot_vals <- Estimation$boot_vals
if (length(boot_vals) == 0) {
  stop("boot_vals is empty; cannot summarize imputations.")
}
Point.Est_mat <- NULL
Point.Est_fixed_mat <- NULL
Point.Est_fixpen_mat <- NULL
Var_mean_mat <- NULL
Var_pred_mat <- NULL
B <- length(boot_vals)
for(j in boot_vals){
  plot_path <- paste0(path,"/Mi_files/","/Plot data for ",mi_name," imputation ",j,".rds")
  if (!file.exists(plot_path)) {
    stop("Missing plot data file: ", plot_path)
  }
  plot_data <- read_rds(plot_path)
  cat(dim(plot_data),"\n")
  Point.Est_mat <- cbind(Point.Est_mat,plot_data$pred)
  Point.Est_fixed_mat <- cbind(Point.Est_fixed_mat,plot_data$pred_fixed)
  Point.Est_fixpen_mat <- cbind(Point.Est_fixpen_mat,plot_data$pred_fixpen)
  Var_mean_mat  <- cbind(Var_mean_mat ,plot_data$sigma_T_est^2)
  Var_pred_mat  <- cbind(Var_pred_mat ,plot_data$sigma_Y_est^2)
}

plot_data$pred <- apply(Point.Est_mat,1,mean)
plot_data$pred_fixed <- apply(Point.Est_fixed_mat,1,mean)
plot_data$pred_fixpen <- apply(Point.Est_fixpen_mat,1,mean)
B_Var <- apply(Point.Est_mat,1,var)
W_mean <- apply(Var_mean_mat,1,mean)
W_pred <- apply(Var_pred_mat,1,mean)
plot_data$SE_mean_pred <- sqrt(W_mean + (B+1)/B*B_Var)
plot_data$SE_pred <- sqrt(W_pred + (B+1)/B*B_Var)
plot_data$lower_CI <- plot_data$pred - 1.96*plot_data$SE_mean_pred
plot_data$upper_CI <- plot_data$pred + 1.96*plot_data$SE_mean_pred
plot_data$lower_CI2 <- plot_data$pred - 1.96*plot_data$SE_pred
plot_data$upper_CI2 <- plot_data$pred + 1.96*plot_data$SE_pred

remove(list = c("Point.Est_mat","Point.Est_fixed_mat", 
                "Point.Est_fixpen_mat", "Var_mean_mat",
                "Var_pred_mat"))


data_one <- readRDS(paste0("Data/Merged/",year_o,"/",marker,"_",month_o,"_final_multiple_impute.rds")) %>% 
  filter(.imp == B | .imp == 0) %>% 
  ### Getting rid of a Morocco survey for severe overweight that appears to be incorrect.
  # mutate(
  #   Point.Estimate.NS = case_when(
  #     UNICEFSurveyID == 8918 ~ NA_real_,
  #     TRUE ~ Point.Estimate.NS
  #   ), 
  #   Point.Estimate.Imp = case_when(
  #     UNICEFSurveyID == 8918 ~ NA_real_,
  #     TRUE ~ Point.Estimate.Imp
  #   )
  # ) %>% 
  
  dplyr::select(-".imp")

### Outputting the data to "path" folder.
output_function(plot_data, Estimation, data_one, marker, date, path)
