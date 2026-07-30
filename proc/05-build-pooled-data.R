library(dplyr)
library(readr)

source("proc/01-build-cived-1999.R")
source("proc/02-build-iccs-2009.R")
source("proc/03-build-iccs-2016.R")
source("proc/04-build-iccs-2022.R")

normalize_country_name <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- trimws(x)
  dplyr::recode(
    x,
    "hong kong sar" = "hong kong",
    "russian federation" = "russia",
    "slovak republic" = "slovakia",
    "chinese taipei" = "taiwan",
    "korea republic of" = "korea",
    "belgium flemish" = "belgium",
    "england" = "united kingdom",
    "germany north rhine westphalia" = "germany",
    "germany schleswig holstein" = "germany",
    "germany schleswig holstein land" = "germany",
    "dominic republic" = "dominican republic",
    .default = x
  )
}

read_swiid_lookup <- function(path = "raw_data/swiid9_92/swiid9_92_summary.csv") {
  read_csv(path, show_col_types = FALSE) |>
    transmute(
      country_key = normalize_country_name(as.character(country)),
      study = as.numeric(year),
      gini = as.numeric(gini_disp)
    ) |>
    filter(study %in% c(1999, 2009, 2016, 2022)) |>
    distinct(country_key, study, .keep_all = TRUE)
}

clean_item_1_4 <- function(x) {
  x <- as.numeric(x)
  case_when(
    x %in% 1:4 ~ x,
    x %in% c(0, 8, 9, 98, 99) ~ NA_real_,
    TRUE ~ NA_real_
  )
}

build_pooled_data <- function(
  cived_data_dir = "raw_data/CivED1999_IDB_SPSS_G8/Data",
  iccs2009_data_dir = "raw_data/ICCS2009_IDB_SPSS/Data_G8",
  iccs2016_data_dir = "raw_data/ICCS2016_IDB_SPSS/Data",
  iccs2022_data_dir = "raw_data/ICCS2022_IDB_R/Data",
  swiid_summary_path = "raw_data/swiid9_92/swiid9_92_summary.csv",
  output_dir = "proc/output"
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  d1999 <- build_cived_1999(cived_data_dir)
  d2009 <- build_iccs_2009(iccs2009_data_dir)
  d2016 <- build_iccs_2016(iccs2016_data_dir)
  d2022 <- build_iccs_2022(iccs2022_data_dir)

  pooled <- bind_rows(d1999, d2009, d2016, d2022)

  swiid_lookup <- read_swiid_lookup(swiid_summary_path)

  pooled <- pooled |>
    mutate(
      country_key = normalize_country_name(label_country)
    ) |>
    left_join(swiid_lookup, by = c("country_key", "study")) |>
    mutate(
      contact = clean_item_1_4(contact),
      peaceful = clean_item_1_4(peaceful),
      sign = clean_item_1_4(sign),
      spray = clean_item_1_4(spray),
      trafic = clean_item_1_4(trafic),
      occupy = clean_item_1_4(occupy),
      civic_test = as.numeric(civic_test),
      weights = as.numeric(weights),
      parents_hisced = as.numeric(parents_hisced),
      gender = as.numeric(gender),
      idstudent = as.numeric(idstudent),
      idschool = as.numeric(idschool),
      idcountry = as.numeric(idcountry)
    )

  write_rds(d1999, file.path(output_dir, "cived_1999_raw_processed.rds"))
  write_rds(d2009, file.path(output_dir, "iccs_2009_raw_processed.rds"))
  write_rds(d2016, file.path(output_dir, "iccs_2016_raw_processed.rds"))
  write_rds(d2022, file.path(output_dir, "iccs_2022_raw_processed.rds"))
  write_rds(pooled, file.path(output_dir, "pooled_data.rds"))

  diagnostics <- pooled |>
    group_by(study) |>
    summarise(
      n = n(),
      n_countries = n_distinct(idcountry, na.rm = TRUE),
      gini_missing_pct = round(100 * mean(is.na(gini)), 2),
      .groups = "drop"
    )

  message("Build complete. Study-level diagnostics:")
  print(diagnostics)

  pooled
}

