remove(list=ls())

library(this.path)
library(tidyverse)
wd <- dirname(this.path::here())
print(wd)
setwd(wd)

marker <- as.character(commandArgs(trailingOnly = TRUE))
month <- "Jul" #for finding the data
year <- "2025"

marker_f <- paste0(marker)
date <- paste(marker_f,"results ",month,year)
date2 <- paste(marker_f,"results ","Jul 2025_nlmb")
# date2 <- paste(marker_f,"results","25February2025")

name <- "Jul2025"
name2 <- "Jul2025_1knot"


date_file <- "Jul2025"
measure <- marker
if(grepl("Over",marker)){
  y_high = 0.35
  if(grepl("Sev",marker)){
    y_high = 0.25
  }
}

path_data = paste0("Data/Analysis files/",measure,"/")
path_fig = paste0("Figures/",measure,"/",date_file,"/")


P_plot_data <- read.csv(paste0(path_data,date,".csv"))
P_plot_data <- P_plot_data %>% mutate(Y=Point.Estimate.Imp) %>% 
  rename("SE_var"="Total_SSE","pred"="Prediction","lower_CI2"="lower_PI","upper_CI2"="upper_PI")


P_plot_data_Mar <- read.csv(paste0(path_data,date2,".csv"))
P_plot_data_Mar <- P_plot_data_Mar %>% mutate(Y=Point.Estimate.Imp) %>% 
  rename("SE_var"="Total_SSE","pred"="Prediction","lower_CI2"="lower_PI","upper_CI2"="upper_PI") 

reg_vals <- sort(as.character(unique(P_plot_data$Region)))


#### Comparison of model fits noting new data (relaxed version that says they are different if the imputed prevalence
#### or SSE are different)

pdf(paste0(path_fig,"Model comparison of ",name," versus ",name2," diff data.pdf"),width = 15, height = 8)

for(k in reg_vals){
  reg_nice <- reg_vals[k==reg_vals]
  t_country <- as.character(unique(P_plot_data$country[as.character(P_plot_data$Region)==k]))
  plot_data <- P_plot_data[P_plot_data$country %in% t_country,] 
  plot_data_mar <- P_plot_data_Mar[P_plot_data_Mar$country %in% t_country,]
  
  plot_data_all <- plot_data %>% 
    full_join(plot_data_mar, 
              by = c("ISO.code","country","year","Sex", "UNICEFSurveyID"),
              relationship = "many-to-many"
    ) %>% 
    select(ISO.code:year,Sex, UNICEFSurveyID,
           Point.Estimate.Imp.x,Point.Estimate.Imp.y,
           SSE_imp.x,SSE_imp.y,
           WHOSurveyID.x,WHOSurveyID.y) %>% 
    mutate(
      new.y = case_when(
        is.na(Point.Estimate.Imp.x) & !is.na(Point.Estimate.Imp.y) ~ 1,
        !is.na(Point.Estimate.Imp.x) & is.na(Point.Estimate.Imp.y) ~ 1,
        abs(Point.Estimate.Imp.x - Point.Estimate.Imp.y) > 5e-3 ~ 1,
        TRUE ~ 0
      )
    )
  
  plot_data_all.x <- plot_data_all %>% 
    select(ISO.code:year,Sex, UNICEFSurveyID,
           Point.Estimate.Imp.x,
           SSE_imp.x,
           WHOSurveyID.x, new.y) %>% 
    rename(
      Point.Estimate.Imp = Point.Estimate.Imp.x,
      SSE_imp = SSE_imp.x,
      WHOSurveyID = WHOSurveyID.x,
      Type = new.y
    )
  
  
  plot_data <- plot_data %>% 
    left_join(plot_data_all.x) %>% 
    mutate(
      Group = paste(name,sep = "")
    )
  plot_data$Type[is.na(plot_data$Type)] <- 1
  
  plot_data_all.y <- plot_data_all %>% 
    select(ISO.code:year,Sex, UNICEFSurveyID,
           Point.Estimate.Imp.y,
           SSE_imp.y,
           WHOSurveyID.y, new.y) %>% 
    rename(
      Point.Estimate.Imp = Point.Estimate.Imp.y,
      SSE_imp = SSE_imp.y,
      WHOSurveyID = WHOSurveyID.y,
      Type = new.y
    )
  
  plot_data_mar <- plot_data_mar %>% 
    left_join(plot_data_all.y) %>% 
    mutate(
      Group = paste(name2,sep = "")
    )
  plot_data_mar$Type[is.na(plot_data_mar$Type)] <- 1
  
  all_est <- plot_data %>%  
    bind_rows(plot_data_mar) %>% 
    filter(Sex == "Overall") %>% 
    mutate(
      Type = factor(Type, levels = c(0,1),labels = c("Used previously","New survey"))
    )
  
  if(grepl("St",marker)){
    p <- ggplot(data=all_est,aes(x=year,y=pred,col=Group)) + 
      geom_line() + 
      geom_point(aes(x=year,y=Y,col=Type))  + 
      facet_wrap(~country,scales="fixed")
  }else{
    p <- ggplot(data=all_est,aes(x=year,y=pred,col=Group)) + 
      geom_line() + 
      ylim(0,y_high) +
      geom_point(aes(x=year,y=Y,col=Type))  + 
      facet_wrap(~country,scales="fixed")
  }
  
  p <- p +  labs(
    x="Year", 
    y=paste(measure,"Prevalence"),
    title = paste(measure,"estimates for", reg_nice, marker, name, "versus", name2), 
    size=60
  ) + 
    theme_bw() + 
    theme(
      axis.text = element_text(family = "Helvetica", color="#666666",size=10), 
      axis.title = element_text(family = "Helvetica", color="#666666", face="bold", size=22)
    ) 
  
  print(p)
  # This will give a Warning message "Removed XXX rows containing...." ignore this.
}

dev.off()

