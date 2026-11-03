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
    str_detect(t, "smart|standardi[sz]ed monitoring") ~ "SMART",
    str_detect(t, "surveillance|sentinel") ~ "Surveillance",
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

# --- CMRS fieldwork extract (national + sex, with continuous year) --------
# The CMRS extract has a continuous year (middle_year = middle of fieldwork)
# and estimates on the proportion scale. Output keeps the integer Year
# (CMRS_year = floor(middle_year)) and middle_year, with estimates in percent.

transform_cmrs_to_desired <- function(cmrs_path) {

  cur <- read_csv(cmrs_path, show_col_types = FALSE, col_types = cols(.default = col_character())) %>%
    filter(
      WEALTH == "_T" &
        RESIDENCE == "_T" &
        EDUCATION == "_T" &
        HEAD_OF_HOUSEHOLD == "_T" &
        REGION == "_T" &
        AGE == "_T"
    )

  num <- function(x) suppressWarnings(as.numeric(x))

  cur %>%
    mutate(
      # Adjusted values (r, se, ll, ul) are missing for some sex-specific rows;
      # the raw values are used for those (matches the previous JME input).
      use_raw = is.na(r),
      PointEstimate = 100 * if_else(use_raw, num(r_raw), num(r)),
      StandardError = 100 * if_else(use_raw, num(se_raw), num(se)),
      LowerLimit = 100 * if_else(use_raw, num(ll_raw), num(ll)),
      UpperLimit = 100 * if_else(use_raw, num(ul_raw), num(ul)),

      # middle_year is missing for a few surveys without fieldwork dates; fall
      # back to the calculated fieldwork midpoint, then the integer year.
      middle_year = coalesce(num(middle_year),
                             num(CalculatedFieldworkMidPoint_DEC),
                             num(TIME_PERIOD)),
      Year = as.integer(coalesce(num(CMRS_year), floor(middle_year))),

      JMEStatus = DataSourceDecision,
      Indicator = case_when(
        IndicatorCode == "ANT_WHZ_NE2" ~ "Wasting",
        IndicatorCode == "ANT_WHZ_NE3" ~ "Severe wasting",
        TRUE ~ NA_character_
      ),
      M49 = as.integer(M49),
      Country = CountryName,
      ShortSource = DataSourceTypeGlobal,
      Survey_years = DataSourceYears,
      UNICEFSurveyID = as.integer(UNICEF_Survey_ID),
      WHOSurveyID = NA_character_,
      EstimateType = Estimate_Type,
      weighted_N = num(weighted_N),
      unweighted_N = num(unweighted_N),
      ReportAuthorProvider = NA_character_
    ) %>%
    filter(!is.na(PointEstimate)) %>%
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
      middle_year,
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
}

# --- merge SDMX (age groups) with CMRS (middle_year) ---------------------
# The SDMX extract has the age-disaggregated estimates but only an integer
# year; the CMRS extract has middle_year. Surveys are matched (1) on country,
# integer year and source title (punctuation/case ignored), then (2) on
# country and year when only one unmatched survey is left on each side (e.g.,
# "[1999 Extract]" titles). Matched surveys take the CMRS middle_year and
# survey ID; unmatched SDMX surveys use the integer year for middle_year.
# CMRS surveys not in the SDMX extract (from min_year on) are appended.

norm_title <- function(x) {
  str_squish(str_to_lower(str_replace_all(coalesce(x, ""), "[[:punct:]]", " ")))
}

merge_sdmx_cmrs <- function(sdmx, cmrs, min_year = 1990) {

  sdmx_surv <- sdmx %>%
    distinct(ISO3Code, Year, Survey_years, FullSourceTitle) %>%
    mutate(title_key = norm_title(FullSourceTitle))
  cmrs_surv <- cmrs %>%
    distinct(ISO3Code, Year, middle_year, UNICEFSurveyID, FullSourceTitle) %>%
    mutate(title_key = norm_title(FullSourceTitle))

  # (1) country + year + title
  match1 <- sdmx_surv %>%
    inner_join(cmrs_surv %>% select(ISO3Code, Year, title_key, middle_year, UNICEFSurveyID),
               by = c("ISO3Code", "Year", "title_key"))

  # (2) country + year, when there is a single unmatched survey on each side
  match2 <- sdmx_surv %>%
    anti_join(match1, by = c("ISO3Code", "Survey_years", "FullSourceTitle")) %>%
    add_count(ISO3Code, Year, name = "n_sdmx") %>%
    filter(n_sdmx == 1) %>%
    inner_join(
      cmrs_surv %>%
        filter(!UNICEFSurveyID %in% match1$UNICEFSurveyID) %>%
        add_count(ISO3Code, Year, name = "n_cmrs") %>%
        filter(n_cmrs == 1) %>%
        select(ISO3Code, Year, middle_year, UNICEFSurveyID, cmrs_title = FullSourceTitle),
      by = c("ISO3Code", "Year")
    )
  message(nrow(match1), " surveys matched on title; ", nrow(match2),
          " matched on country-year only:")
  print(match2 %>% select(ISO3Code, Year, FullSourceTitle, cmrs_title), n = Inf)

  surv_key <- bind_rows(match1, match2) %>%
    select(ISO3Code, Survey_years, FullSourceTitle, middle_year, UNICEFSurveyID)

  out <- sdmx %>%
    select(-UNICEFSurveyID) %>%
    left_join(surv_key, by = c("ISO3Code", "Survey_years", "FullSourceTitle"),
              relationship = "many-to-one")

  unmatched <- out %>% filter(is.na(UNICEFSurveyID)) %>%
    distinct(ISO3Code, Year, Survey_years, FullSourceTitle)
  message(nrow(unmatched), " SDMX surveys not in CMRS (middle_year = Year).")
  print(unmatched, n = Inf)

  # New survey IDs (above the CMRS IDs) for unmatched SDMX surveys
  max_id <- max(cmrs$UNICEFSurveyID, na.rm = TRUE)
  out <- out %>%
    mutate(
      unique_code = if_else(is.na(UNICEFSurveyID),
                            paste(ISO3Code, Survey_years, FullSourceTitle, sep = " | "),
                            NA_character_),
      UNICEFSurveyID = coalesce(UNICEFSurveyID,
                                max_id + as.integer(factor(unique_code))),
      middle_year = coalesce(middle_year, as.numeric(Year))
    ) %>%
    select(-unique_code)

  # CMRS-only surveys. unweighted_N is dropped since the SDMX extract does
  # not have it (the SE program uses weighted_N when unweighted_N is all NA).
  cmrs_only <- cmrs %>%
    filter(!UNICEFSurveyID %in% surv_key$UNICEFSurveyID, Year >= min_year) %>%
    mutate(unweighted_N = NA_real_)
  message(n_distinct(cmrs_only$UNICEFSurveyID), " CMRS surveys not in SDMX were added:")
  print(cmrs_only %>% distinct(ISO3Code, Year, middle_year, FullSourceTitle), n = Inf)

  bind_rows(out, cmrs_only)
}

# --- run it --------------------------------------------------------------
# SDMX extract (age-disaggregated, integer year only), downloaded Feb 2026:
# data link: https://sdmx.data.unicef.org/sdmx/v2/data/dataflow/UNICEF/NUTRITION/1.0/?c%5BINDICATOR%5D=NT_ANT_WHZ_NE2,NT_ANT_WHZ_NE3&format=csv&labels=id&startPeriod=1990&endPeriod=2025
sdmx_data <- transform_current_to_desired(
  current_path = "data.csv",
  include_extra_disaggs = FALSE
)

# CMRS extract (national + sex, with middle_year)
cmrs_data <- transform_cmrs_to_desired(
  "cmrs2-ant-accepted_jme2025_wasting-national-sex_fieldwork_20261021.csv"
)

desired_like <- merge_sdmx_cmrs(sdmx_data, cmrs_data)

template_cols <- names(read_csv("desired_data_format.csv", n_max = 0, show_col_types = FALSE))
desired_like <- desired_like %>% select(any_of(c(template_cols, "middle_year")))

## Make final wasting dataset. The UNICEF survey ID comes from the CMRS
## extract (new IDs for SDMX surveys not in CMRS).
desired_wasting <- desired_like %>%
  filter(Indicator == "Wasting") %>%
  # Remove modeled estimates (NA footnotes are kept)
  filter(is.na(ExtendedDisplayFootnotes) | 
           ExtendedDisplayFootnotes != "Estimate from 2025 JME modeling") %>% 
  arrange(Country, Year, StandardDisaggregations)
  
keys <- c("Country", "Survey_years", "Year", "middle_year", "UNICEFSurveyID", "StandardDisaggregations")
# View(desired_wasting %>% arrange(across(all_of(keys))) %>% select(all_of(keys), everything()))

desired_wasting %>%
  count(across(all_of(keys))) %>%
  filter(n > 1)

## Make final severe wasting dataset.
desired_severe_wasting <- desired_like %>% 
  filter(Indicator == "Severe wasting") %>%
  # Remove modeled estimates (NA footnotes are kept)
  filter(is.na(ExtendedDisplayFootnotes) | 
           ExtendedDisplayFootnotes != "Estimate from 2025 JME modeling") %>% 
  arrange(Country, Year, StandardDisaggregations)

desired_severe_wasting %>%
  count(across(all_of(keys))) %>%
  filter(n > 1)


write_xlsx(desired_wasting, "JME_Country_Level_Input_Wasting.xlsx")
write_xlsx(desired_severe_wasting, "JME_Country_Level_Input_SevereWasting.xlsx")
