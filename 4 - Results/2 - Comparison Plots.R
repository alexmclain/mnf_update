remove(list=ls())

library(tidyverse)
library(this.path)
wd <- dirname(this.path::here())
print(wd)
setwd(wd)

# Compares the pooled estimates of two runs (two models, two rounds, ...) made
# by 0 - Summarizing_imputed_results.R.
#
# Usage:
#   Rscript "4 - Results/2 - Comparison Plots.R" <marker> <runA> <runB> [key=value ...]
#
# A run is given as it appears in the results file name,
#   Data/Analysis files/<marker>/<marker> results <tag> <month> <year>.csv
# i.e. "[tag] <month> <year>": "Oct 2026" (default model) or "fpi Oct 2026".
# A run can also be the path to any results .csv file (e.g. an archived run).
#
# Options (key=value):
#   labels=A,B     names of the two runs in the figures (default: the run strings)
#   sex=Overall    Overall, Female or Male
#   max_year=2025  only plot years <= max_year (default: all years)
#   ci=TRUE        draw the 95% confidence bands of both runs
#   diff_pp=0.5    a survey is "changed" if its value differs by more than this
#                  many percentage points between the runs
#
# Example (Wasting model without vs with the food price index):
#   Rscript "4 - Results/2 - Comparison Plots.R" Wasting "Oct 2026" "fpi Oct 2026" labels="No FPI,FPI" max_year=2025
#
# Output (<A> and <B> are the labels):
#   Figures/<marker>/Comparisons/<marker> <A> vs <B> <sex>.pdf
#     page 1: differences B - A by year (annual estimates, all countries)
#     page 2: the 30 country-years with the largest absolute differences
#     then one page per region: both runs by country, with the survey data
#   Data/Analysis files/<marker>/Comparisons/<marker> <A> vs <B>.csv
#     annual estimates of both runs and their difference, all sexes

#### 1. Arguments ####
args <- as.character(commandArgs(trailingOnly = TRUE))
opt_ind <- grepl("^[A-Za-z_]+=", args)
pos <- args[!opt_ind]
if (length(pos) != 3) {
  stop('Expected: <marker> <runA> <runB> [key=value ...], e.g.\n',
       '  Wasting "Oct 2026" "fpi Oct 2026" labels="No FPI,FPI"')
}
marker <- pos[1]
runs <- pos[2:3]

opts <- list(labels = NULL, sex = "Overall", max_year = Inf, ci = TRUE, diff_pp = 0.5)
for (a in args[opt_ind]) {
  key <- sub("=.*", "", a)
  val <- sub("^[^=]*=", "", a)
  if (!key %in% names(opts)) stop("Unknown option: ", key)
  opts[[key]] <- val
}
labels <- if (is.null(opts$labels)) str_squish(runs) else str_squish(strsplit(opts$labels, ",")[[1]])
if (length(labels) != 2 || labels[1] == labels[2]) stop("labels must be two different names, e.g. labels=\"No FPI,FPI\"")
sex <- opts$sex
max_year <- as.numeric(opts$max_year)
show_ci <- as.logical(opts$ci)
diff_tol <- as.numeric(opts$diff_pp) / 100

measure <- case_when(
  marker == "SevereWasting" ~ "Severe Wasting",
  marker == "SevereOverweight" ~ "Severe Overweight",
  TRUE ~ marker
)

path_data <- paste0("Data/Analysis files/", marker, "/")
path_fig  <- paste0("Figures/", marker, "/Comparisons/")
path_out  <- paste0(path_data, "Comparisons/")
dir.create(path_fig, recursive = TRUE, showWarnings = FALSE)
dir.create(path_out, recursive = TRUE, showWarnings = FALSE)

#### 2. Reading the two runs ####
run_file <- function(run) {
  if (grepl("\\.csv$", run)) return(run)
  tokens <- strsplit(str_squish(run), " ")[[1]]
  if (length(tokens) < 2) stop("A run must be \"[tag] <month> <year>\" or a .csv path: ", run)
  n <- length(tokens)
  tag <- paste(tokens[seq_len(n - 2)], collapse = " ")
  paste0(path_data, marker, " results ", paste(tag, tokens[n - 1], tokens[n]), ".csv")
}

read_run <- function(run, label) {
  file <- run_file(run)
  if (!file.exists(file)) stop("Missing results file: ", file)
  cat("Run", label, ":", file, "\n")
  dat <- read.csv(file)
  # Older results files have no middle_year (surveys at the integer year)
  if (!"middle_year" %in% names(dat)) dat$middle_year <- dat$year
  dat %>%
    mutate(
      Run = label,
      middle_year = coalesce(middle_year, as.numeric(year)),
      # survey key: UNICEF survey ID when available, otherwise the year
      survey_key = if_else(is.na(UNICEFSurveyID), NA_character_,
                           paste(UNICEFSurveyID, round(middle_year, 3)))
    )
}

dat_list <- map2(runs, labels, read_run)

## Annual estimates: one row per country-year-sex. Every country-year-sex has
## a row with no survey (UNICEFSurveyID = NA) in the current results; older
## results only have the survey rows in survey years, so fall back to those.
annual <- function(dat) {
  dat %>%
    filter(!is.na(Prediction)) %>%
    arrange(ISO.code, year, Sex, !is.na(UNICEFSurveyID), abs(middle_year - year)) %>%
    distinct(ISO.code, year, Sex, .keep_all = TRUE) %>%
    select(ISO.code, country, Region, year, Sex, Run, Prediction, lower_CI, upper_CI)
}
ann <- map(dat_list, annual)

## Use the country names and regions of run A for both runs
ctry <- ann[[1]] %>% distinct(ISO.code, country, Region) %>% distinct(ISO.code, .keep_all = TRUE)
ann <- map(ann, \(d) d %>% select(-country, -Region) %>% left_join(ctry, by = "ISO.code"))

#### 3. Differences between the runs ####
diff_dat <- full_join(
  ann[[1]] %>% select(ISO.code, year, Sex, A = Prediction, A_lower = lower_CI, A_upper = upper_CI),
  ann[[2]] %>% select(ISO.code, year, Sex, B = Prediction, B_lower = lower_CI, B_upper = upper_CI),
  by = c("ISO.code", "year", "Sex")
) %>%
  left_join(ctry, by = "ISO.code") %>%
  mutate(
    diff_pp = 100 * (B - A),
    ratio_CI_width = (B_upper - B_lower) / (A_upper - A_lower)
  ) %>%
  relocate(ISO.code, country, Region, year, Sex) %>%
  arrange(ISO.code, Sex, year)

out_name <- paste(marker, labels[1], "vs", labels[2])
write.csv(
  diff_dat %>% rename_with(\(x) str_replace(x, "^A", labels[1]) %>% str_replace("^B", labels[2]),
                           c(A, A_lower, A_upper, B, B_lower, B_upper)),
  paste0(path_out, out_name, ".csv"), row.names = FALSE
)

d_sex <- diff_dat %>% filter(Sex == sex, year <= max_year, !is.na(diff_pp))
if (nrow(d_sex) == 0) stop("The two runs have no country-years in common for Sex = ", sex)

cat("\nDifference ", labels[2], " - ", labels[1], " (percentage points), ", sex,
    ", ", min(d_sex$year), "-", max(d_sex$year), ", ", n_distinct(d_sex$ISO.code),
    " countries:\n", sep = "")
print(summary(d_sex$diff_pp))
cat("Share within +/-0.5 points:", round(mean(abs(d_sex$diff_pp) <= 0.5), 3),
    "; within +/-1 point:", round(mean(abs(d_sex$diff_pp) <= 1), 3), "\n")
yearly <- d_sex %>%
  group_by(year) %>%
  summarise(mean_diff = mean(diff_pp), median_diff = median(diff_pp),
            mean_abs_diff = mean(abs(diff_pp)), max_abs_diff = max(abs(diff_pp)), .groups = "drop")
print(as.data.frame(yearly %>% mutate(across(-year, \(x) round(x, 3)))))

top <- d_sex %>% slice_max(abs(diff_pp), n = 30, with_ties = FALSE)
cat("\nLargest absolute differences:\n")
print(as.data.frame(top %>% transmute(country, year, A = round(100 * A, 2), B = round(100 * B, 2),
                                      diff_pp = round(diff_pp, 2))))

#### 4. Survey data used by each run, flagged by whether the other run used it ####
## A survey counts as used by a run only if that run has a prediction for its
## row (e.g. the fpi model has no predictions before 2001, so earlier surveys
## are in its results file but were not used in the fit).
surveys <- function(dat) {
  dat %>%
    filter(!is.na(Point.Estimate.Imp), !is.na(Prediction)) %>%
    select(ISO.code, Sex, year, middle_year, survey_key, Run, Y = Point.Estimate.Imp)
}
sv <- map(dat_list, surveys)
## match on survey ID + time when available, otherwise on the time
match_key <- function(d) d %>% mutate(key = coalesce(survey_key, paste("t", round(middle_year, 3))))
sv <- map(sv, match_key)
sv_join <- full_join(
  sv[[1]] %>% select(ISO.code, Sex, key, YA = Y, tA = middle_year),
  sv[[2]] %>% select(ISO.code, Sex, key, YB = Y, tB = middle_year),
  by = c("ISO.code", "Sex", "key"), relationship = "many-to-many"
) %>%
  mutate(
    Survey = case_when(
      is.na(YB) ~ paste("Used by", labels[1], "only"),
      is.na(YA) ~ paste("Used by", labels[2], "only"),
      abs(YA - YB) > diff_tol ~ "Value changed",
      TRUE ~ "Used by both"
    ),
    middle_year = coalesce(tA, tB),
    Y = coalesce(YB, YA)   # the value in run B where both have the survey
  ) %>%
  left_join(ctry, by = "ISO.code") %>%
  filter(Sex == sex, middle_year < max_year + 1) %>%
  mutate(Survey = factor(Survey, levels = c("Used by both", "Value changed",
                                            paste("Used by", labels[1], "only"),
                                            paste("Used by", labels[2], "only"))))
cat("\nSurveys (", sex, "):\n", sep = "")
print(table(sv_join$Survey))

#### 5. Figures ####
run_cols <- setNames(c("#1b6ca8", "#d1495b"), labels)
survey_shapes <- setNames(c(16, 17, 1, 2), levels(sv_join$Survey))

est <- bind_rows(ann) %>%
  filter(Sex == sex, year <= max_year) %>%
  mutate(Run = factor(Run, levels = labels))

base_theme <- theme_bw() +
  theme(
    axis.text = element_text(color = "#666666", size = 10),
    axis.title = element_text(color = "#666666", face = "bold", size = 18),
    plot.title = element_text(size = 16),
    legend.position = "bottom"
  )
yr_txt <- if (is.finite(max_year)) paste0(" (through ", max_year, ")") else ""
sex_txt <- if (sex == "Overall") "" else paste0(" ", sex)

pdf(paste0(path_fig, out_name, " ", sex, ".pdf"), width = 15, height = 8)

## Page 1: distribution of the differences by year
p <- ggplot(d_sex, aes(x = factor(year), y = diff_pp)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_boxplot(outlier.size = 0.8, fill = "grey90") +
  geom_point(data = yearly, aes(y = mean_diff), colour = "#d1495b", size = 2) +
  labs(
    x = "Year", y = "Difference (percentage points)",
    title = paste0(measure, sex_txt, ": ", labels[2], " minus ", labels[1],
                   ", annual country estimates", yr_txt),
    subtitle = "Boxes: distribution over countries. Red points: mean over countries."
  ) +
  base_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
print(p)

## Page 2: largest differences
p <- top %>%
  mutate(lab = fct_reorder(paste0(country, " ", year), abs(diff_pp))) %>%
  pivot_longer(c(A, B), names_to = "Run", values_to = "Est") %>%
  mutate(Run = factor(if_else(Run == "A", labels[1], labels[2]), levels = labels)) %>%
  ggplot(aes(x = 100 * Est, y = lab, colour = Run)) +
  geom_line(aes(group = lab), colour = "grey70") +
  geom_point(size = 2.5) +
  scale_colour_manual(values = run_cols) +
  labs(x = paste(measure, "prevalence (%)"), y = NULL, colour = NULL,
       title = paste0(measure, sex_txt, ": the 30 country-years with the largest differences", yr_txt)) +
  base_theme
print(p)

## One page per region
for (reg in sort(unique(est$Region))) {
  e_reg <- est %>% filter(Region == reg)
  s_reg <- sv_join %>% filter(Region == reg)
  p <- ggplot(e_reg, aes(x = year, y = 100 * Prediction, colour = Run))
  if (show_ci) {
    p <- p + geom_ribbon(aes(ymin = 100 * lower_CI, ymax = 100 * upper_CI, fill = Run),
                         colour = NA, alpha = 0.15)
  }
  p <- p +
    geom_line(linewidth = 0.7) +
    geom_point(data = s_reg, aes(x = middle_year, y = 100 * Y, shape = Survey),
               inherit.aes = FALSE, size = 1.6, colour = "grey15") +
    facet_wrap(~country, scales = "fixed") +
    scale_colour_manual(values = run_cols) +
    scale_fill_manual(values = run_cols, guide = "none") +
    scale_shape_manual(values = survey_shapes, drop = TRUE) +
    labs(
      x = "Year", y = paste(measure, "prevalence (%)"), colour = NULL, shape = NULL,
      title = paste0(measure, sex_txt, " estimates for ", reg, ": ", labels[1], " versus ", labels[2], yr_txt),
      subtitle = if (show_ci) "Lines: estimates; bands: 95% confidence intervals; points: survey data" else NULL
    ) +
    base_theme
  print(p)
}

dev.off()
cat("\nWrote", paste0(path_fig, out_name, " ", sex, ".pdf"), "\nand",
    paste0(path_out, out_name, ".csv"), "\n")
