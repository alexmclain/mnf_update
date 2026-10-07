suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
  library(writexl)
})

# --- helpers ------------------------------------------------------------

infer_short_source <- function(title) {
  t <- str_to_lower(coalesce(title, ""))
  case_when(
    str_detect(t, "demographic and health survey|\\bdhs\\b") ~ "DHS",
    str_detect(t, "multiple indicator cluster survey|\\bmics\\b") ~ "MICS",
    str_detect(t, "nutrition survey|\\bnns\\b") ~ "NNS",
    str_detect(t, "papfam") ~ "PAPFAM",
    TRUE ~ "Other"
  )
}

sex_to_label <- function(sex) {
  sex <- as.character(sex)
  case_when(
    sex == "F" ~ "Female",
    sex == "M" ~ "Male",
    # sex == "_T" ~ "Both",
    TRUE ~ ""  # includes _T and anything unknown
  )
}

age_to_label <- function(age) {
  age <- as.character(age)
  
  # default blank (meaning "overall")
  out <- rep("", length(age))
  
  # handle common "overall" codes
  overall_idx <- is.na(age) | age %in% c("_T", "TOTAL", "ALL", "Y0T4")
  out[overall_idx] <- ""
  
  # month ranges like M12T23
  m <- str_match(age, "^M(\\d+)T(\\d+)$")
  a <- suppressWarnings(as.integer(m[, 2]))
  b <- suppressWarnings(as.integer(m[, 3]))
  idx <- !is.na(a) & !is.na(b)
  
  
  # exact-range mapping (no collapsing)
  range_map <- c(
    "Y0T4"  = "0 to 4 years",
    "M0T11" = "0 to 11 months",
    "M12T23" = "12 to 23 months",
    "M48T53" = "48 to 53 months",
    "M36T47" = "36 to 47 months",
    "M48T59" = "48 to 59 months",
    "M6T11"  = "6 to 11 months",
    "M24T59" = "24 to 59 months",
    "M0T23"  = "0 to 23 months",
    "M24T29" = "24 to 29 months",
    "M12T17" = "12 to 17 months",
    "M30T35" = "30 to 35 months",
    "M0T5"   = "0 to 5 months",
    "M42T47" = "42 to 47 months",
    "M18T23" = "18 to 23 months",
    "M36T41" = "36 to 41 months",
    "M54T59" = "54 to 59 months",
    "M24T35" = "24 to 35 months",
    "M9T11"  = "9 to 11 months",
    "M6T8"   = "6 to 8 months"
  )
  
  out[idx] <- dplyr::recode(out[idx], !!!range_map, .default = out[idx])
  
  # treat 0-59 months as "overall"
  out[idx & a == 0 & b >= 59] <- ""
  
  # # if the range is fully inside a standard bucket, collapse to that bucket
  # out[idx & a >= 0  & b <= 5 ]  <- "0 to 5 months"
  # out[idx & a >= 6  & b <= 11]  <- "6 to 11 months"
  # out[idx & a >= 12 & b <= 23]  <- "12 to 23 months"
  # out[idx & a >= 24 & b <= 35]  <- "24 to 35 months"
  # out[idx & a >= 36 & b <= 47]  <- "36 to 47 months"
  # out[idx & a >= 48 & b <= 59]  <- "48 to 59 months"
  
  # for any remaining MxTy not captured above, use literal "a to b months"
  remaining <- idx & out == ""
  out[remaining] <- paste0(a[remaining], " to ", b[remaining], " months")
  
  out
}

collapse_nonempty <- function(...) {
  x <- c(...)
  x <- str_squish(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) NA_character_ else paste(x, collapse = "; ")
}

build_standard_disagg <- function(sex_lab, age_lab) {
  case_when(
    sex_lab == "" & age_lab == "" ~ "National",
    sex_lab != "" & age_lab == "" ~ sex_lab,
    sex_lab == "" & age_lab != "" ~ age_lab,
    TRUE ~ paste(sex_lab, age_lab)
  )
}

# optional: append extra disaggregations to the label (OFF by default)
append_extra_disaggs <- function(base, wealth, residence, maternal_edu) {
  wealth <- as.character(wealth)
  residence <- as.character(residence)
  maternal_edu <- as.character(maternal_edu)
  
  wealth_lab <- case_when(
    wealth %in% c(NA, "_T") ~ NA_character_,
    wealth %in% paste0("Q", 1:5) ~ paste0("Wealth=", wealth),
    wealth %in% c("B20","B40","B60","B80") ~ paste0("Wealth=Bottom ", str_sub(wealth, 2, 3), "%"),
    wealth %in% c("R20","R40","R60","R80") ~ paste0("Wealth=Top ", str_sub(wealth, 2, 3), "%"),
    TRUE ~ paste0("Wealth=", wealth)
  )
  
  res_lab <- case_when(
    residence %in% c(NA, "_T") ~ NA_character_,
    residence == "U" ~ "Residence=Urban",
    residence == "R" ~ "Residence=Rural",
    TRUE ~ paste0("Residence=", residence)
  )
  
  medu_lab <- case_when(
    maternal_edu %in% c(NA, "_T") ~ NA_character_,
    TRUE ~ paste0("MaternalEdu=", maternal_edu)
  )
  
  extras <- purrr::pmap_chr(list(wealth_lab, res_lab, medu_lab),
                            ~ collapse_nonempty(..1, ..2, ..3))
  
  if_else(is.na(extras), base, paste0(base, "; ", extras))
}

# --- main transform ------------------------------------------------------

transform_current_to_desired <- function(current_path,
                                         include_extra_disaggs = FALSE) {
  
  cur <- read_csv(current_path, show_col_types = FALSE, col_types = cols(.default = col_character())) %>% 
    filter(
      WEALTH_QUINTILE == "_T" &
      RESIDENCE == "_T" &
      MATERNAL_EDU_LVL == "_T" &
      HEAD_OF_HOUSE == "_T" &
        REPORTING_LVL == "C"
    )
  
  cur2 <- cur %>%
    filter(!is.na(REF_AREA), !is.na(INDICATOR), !is.na(OBS_VALUE)) %>%
    mutate(
      ISO3Code = REF_AREA,
      IndicatorCode = str_remove(INDICATOR, "^NT_"),
      Indicator = case_when(
        IndicatorCode == "ANT_WHZ_NE2" ~ "Wasting",
        IndicatorCode == "ANT_WHZ_NE3" ~ "Severe wasting",
        TRUE ~ NA_character_
      ),
      
      Survey_years = as.character(TIME_PERIOD),
      Year = suppressWarnings(as.integer(str_sub(Survey_years, 1, 4))),
      
      ShortSource = infer_short_source(DATA_SOURCE),
      FullSourceTitle = DATA_SOURCE,
      ReportAuthorProvider = CUSTODIAN,
      
      EstimateType = recode(OBS_STATUS,
                            "RA" = "Reanalyzed",
                            "ER" = "External Reanalysis",
                            "RP" = "Reported",
                            "U"  = "Unreliable",
                            .default = NA_character_),
      
      PointEstimate = suppressWarnings(as.numeric(OBS_VALUE)),
      StandardError = suppressWarnings(as.numeric(STD_ERR)),
      LowerLimit = suppressWarnings(as.numeric(LOWER_BOUND)),
      UpperLimit = suppressWarnings(as.numeric(UPPER_BOUND)),
      weighted_N = suppressWarnings(as.numeric(WGTD_SAMPL_SIZE)),
      unweighted_N = NA_real_,
      
      JMEStatus = "Accepted and Free from Confidentiality",
      UNICEFSurveyID = NA_character_,
      WHOSurveyID = NA_character_,
      StandardFootnotes = NA_character_,
      
      ExtendedDisplayFootnotes = purrr::pmap_chr(
        list(OBS_FOOTNOTE, SERIES_FOOTNOTE),
        ~ collapse_nonempty(..1, ..2)
      ),
      
      sex_lab = sex_to_label(SEX),
      age_lab = age_to_label(AGE),
      StandardDisaggregations = build_standard_disagg(sex_lab, age_lab)
    ) %>%
    { if (include_extra_disaggs)
      mutate(., StandardDisaggregations = append_extra_disaggs(
        StandardDisaggregations, WEALTH_QUINTILE, RESIDENCE, MATERNAL_EDU_LVL
      ))
      else .
    } %>%
    select(-sex_lab, -age_lab)
  
  # add Country + M49 if countrycode is available
  if (requireNamespace("countrycode", quietly = TRUE)) {
    cc <- countrycode::codelist %>%
      transmute(
        ISO3Code = iso3c,
        M49 = suppressWarnings(as.integer(un)),
        Country = country.name.en
      )
    
    cur2 <- cur2 %>%
      left_join(cc, by = "ISO3Code")
  } else {
    cur2 <- cur2 %>%
      mutate(M49 = NA_integer_, Country = NA_character_)
  }
  
  out <- cur2 %>%
    select(
      JMEStatus,
      IndicatorCode,
      Indicator,
      StandardDisaggregations,
      ISO3Code,
      M49,
      Country,
      ShortSource,
      Survey_years,
      Year,
      UNICEFSurveyID,
      WHOSurveyID,
      EstimateType,
      PointEstimate,
      StandardError,
      LowerLimit,
      UpperLimit,
      weighted_N,
      unweighted_N,
      StandardFootnotes,
      ExtendedDisplayFootnotes,
      ReportAuthorProvider,
      FullSourceTitle
    )
  
  out
}

# --- run it --------------------------------------------------------------
desired_like <- transform_current_to_desired(
  current_path = "data.csv",
  include_extra_disaggs = FALSE
)

template_cols <- names(read_csv("desired_data_format.csv", n_max = 0, show_col_types = FALSE))
desired_like <- desired_like %>% select(any_of(template_cols))

## Make final wasting dataset and create numeric survey id.
desired_wasting <- desired_like %>% 
  filter(Indicator == "Wasting") %>%
  # Remove modeled estimates
  filter(ExtendedDisplayFootnotes != "Estimate from 2025 JME modeling")%>% 
  arrange(Country, Year, StandardDisaggregations) %>% 
  mutate(
    unique_code = paste(Country, Survey_years, FullSourceTitle, sep = " | "),
    UNICEFSurveyID = as.integer(factor(unique_code))
  ) 
  
keys <- c("Country", "Survey_years", "Year","UNICEFSurveyID", "StandardDisaggregations", "ExtendedDisplayFootnotes")
# View(desired_wasting %>% arrange(across(all_of(keys))) %>% select(all_of(keys), everything()))

desired_wasting %>%
  count(across(all_of(keys))) %>%
  filter(n > 1)

## Make final severe wasting dataset and create numeric survey id.
desired_severe_wasting <- desired_like %>% 
  filter(Indicator == "Severe wasting") %>%
  # Remove modeled estimates
  filter(ExtendedDisplayFootnotes != "Estimate from 2025 JME modeling")%>% 
  arrange(Country, Year, StandardDisaggregations) %>% 
  mutate(
    unique_code = paste(Country, Survey_years, FullSourceTitle, sep = " | "),
    UNICEFSurveyID = as.integer(factor(unique_code))
  ) 

desired_severe_wasting %>%
  count(across(all_of(keys))) %>%
  filter(n > 1)


write_xlsx(desired_wasting, "JME_Country_Level_Input_Wasting.xlsx")
write_xlsx(desired_severe_wasting, "JME_Country_Level_Input_SevereWasting.xlsx")
