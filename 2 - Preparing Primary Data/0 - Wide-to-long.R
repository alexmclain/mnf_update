library(tidyverse)
library(readxl)
library(this.path)
wd <- dirname(this.path::here())
print(wd)
setwd(wd)

handle <- "EXPDB_ANT_WHZ_NE3.xlsx"
year <- "2025"

(raw_data <- read_excel(paste0("Data/JME/",year,"/Raw/",handle)))

raw_data %>%
  mutate_at(
    vars(
      ends_with("_r")|
      ends_with("_ll")|
      ends_with("_ul")|
      ends_with("_N")
      ),
    \(x) as.numeric(x)
  ) |> 
  pivot_longer(
    cols = -c(UNICEF_Survey_ID:Estimate_Type),
    names_to = c("grouping", ".value"),
    names_pattern = "(.*)_(r|ll|ul|weighted_N|Footnote)"
  ) %>%
  rename(
    PointEstimate = `r`,
    LowerLimit = `ll`,
    UpperLimit = `ul` 
  ) |> 
  filter(
    str_detect(grouping, "^male") | 
      str_detect(grouping, "^female") | 
      str_detect(grouping, "^National") | 
      str_detect(grouping, "^month")
    ) |>
  mutate(
    age_range_str = str_extract(grouping, "\\d+_\\d+"),
    age_ll = as.numeric(str_extract(age_range_str, "^(\\d+)")),
    age_ul = as.numeric(str_extract(age_range_str, "(\\d+)$"))
  ) |> 
  mutate(
    .before = ISO3Code,
    Indicator = "Severe Overweight",
    StandardDisaggregations = case_when(
      grouping == "National" ~ "National",
      grouping == "male" ~ "Male",
      grouping == "female" ~ "Female",
      startsWith(grouping, "male_month") ~ paste("Male", age_ll, "to", age_ul, "months"),
      startsWith(grouping, "female_month") ~ paste("Female",  age_ll, "to", age_ul, "months"),
      startsWith(grouping, "month") ~ paste(age_ll, "to", age_ul, "months"),
      TRUE ~ NA_character_
    ) 
  ) |> 
  filter(
    !is.na(PointEstimate) &
      PointEstimate > 0 ## There is one negative PointEstimate: UNICEFSurveyID 9132 age 0 to 5 months.
  ) |> 
  rename(
    Year = CMRS_year,
    ShortSource = DataSourceTypeGlobal,
    Country = CountryName,
    UNICEFSurveyID = UNICEF_Survey_ID,
    StandardFootnotes = Footnote,
  ) |> 
  select(-c(grouping, age_range_str:age_ul)) -> long_data


### Severe Overweight Data is missing unweighted N. Getting these from the Overweight data.

surv_data <- read_excel(
  paste0("Data/JME/",year,"/Raw/JME_Country_Level_Input_Overweight.xlsx")
  ) %>% 
  select(
    ISO3Code,Year,UNICEFSurveyID,StandardDisaggregations, weighted_N, unweighted_N
  ) %>% 
  rename(
    weighted_N_over = weighted_N
  )

### Merging Severe Overweight with Overweight N's
long_data_w_unweighted_N <- long_data %>% 
  left_join(surv_data)


long_data_w_unweighted_N %>% 
  mutate(
    prop_ww_N = (weighted_N-weighted_N_over)/weighted_N,
    prop_wu_N = (weighted_N-unweighted_N)/weighted_N,
    unweighted_N_SO = case_when(
      ### If the weighted N's are close or the overweight weighted N is smaller, 
      ### use the unweighted N from overweight.
      prop_ww_N > (-0.01) ~ unweighted_N,
      ### If the weighted N is close to the unweighted N then use the minimum of 
      ### the severe overweight Weighted N and overweight Unweighted N
      abs(prop_wu_N) < 0.1 & (weighted_N >= unweighted_N) ~ unweighted_N,
      abs(prop_wu_N) < 0.1 & (weighted_N < unweighted_N) ~ weighted_N,
      ### If the overweight weighted N is equal to the overweight unweighted N, 
      ### use the severe overweight weighted N
      weighted_N_over == unweighted_N ~ weighted_N,
      ### If the overweight weighted N is larger then the severe overweight 
      ### weighted N, use the overweight unweighted N  the scaled by the
      ### severe overweight to overweight weighted N ratio.
      !is.na(weighted_N) & !is.na(weighted_N_over) & !is.na(unweighted_N) ~ weighted_N*unweighted_N/weighted_N_over,
      TRUE ~ NA_real_
    ),
    ### Data are missing starndard errors, setting to NA, estimated in next program
    StandardError = NA_real_,
    ExtendedDisplayFootnotes = NA_character_
  ) -> long_data_w_N



long_data_w_N %>% 
  select(
    -c(weighted_N_over, unweighted_N, prop_ww_N, prop_wu_N,)# StandardError_LO)
  ) %>% 
  rename(
    unweighted_N = unweighted_N_SO
  )  -> final_long_data_w_N

writexl::write_xlsx(final_long_data_w_N, 
                    paste0("Data/JME/",year,"/Raw/","JME_Country_Level_Input_SevereOverweight.xlsx")
)





## Checking the overweight versus servere overweight prevalence


### Severe Overweight Data is missing unweighted N. Getting these from the Overweight data.
long_data <- read_excel(
  paste0("Data/JME/",year,
         "/Raw/JME_Country_Level_Input_SevereOverweight.xlsx")
)

surv_data <- read_excel(
  paste0("Data/JME/",2024,"/Raw/JME_Country_Level_Input_Overweight.xlsx")
) 

surv_data_sm <- surv_data %>% 
  rename(PointEstimate_over = PointEstimate) %>% 
  select(
    ISO3Code,Year,UNICEFSurveyID,StandardDisaggregations, PointEstimate_over
  ) 

### Merging Severe Overweight with Overweight
long_data_w_unweighted_N <- long_data %>% 
  left_join(surv_data_sm) %>% 
  mutate(
    diff = PointEstimate_over - PointEstimate
  )

summary(long_data_w_unweighted_N$diff)

View(long_data_w_unweighted_N %>% 
       filter(diff < 0))

IDs = long_data_w_unweighted_N %>% 
  filter(diff < 0) %>% distinct(UNICEFSurveyID)

surv_data %>% 
  filter(
    UNICEFSurveyID %in% IDs$UNICEFSurveyID
  ) %>% 
  View()


long_data %>% 
  filter(
    UNICEFSurveyID %in% IDs$UNICEFSurveyID
  ) %>% 
  View()

long_data_w_unweighted_N %>% 
  filter(
    UNICEFSurveyID %in% IDs$UNICEFSurveyID
  ) %>% 
  View()
