library(dplyr)
library(purrr)
library(readr)

source("proc/01_build_multilevel_processing.R")

# Select one ISG file per country, preferring C4 over B4 when both exist.
select_iccs2022_isg_files <- function(data_dir = "raw_data/ICCS2022_IDB_R/Data") {
  all_files <- list.files(
    data_dir,
    pattern = "^ISG[A-Z]{3}[BC]4\\.Rdata$",
    full.names = TRUE
  )

  if (length(all_files) == 0) {
    stop("No ICCS 2022 ISG country files found in: ", data_dir)
  }

  meta <- tibble::tibble(
    path = all_files,
    file = basename(all_files),
    country_code = substr(file, 4, 6),
    form = substr(file, 7, 8),
    form_priority = dplyr::case_when(
      form == "C4" ~ 1,
      form == "B4" ~ 2,
      TRUE ~ 9
    )
  )

  meta |>
    arrange(country_code, form_priority) |>
    group_by(country_code) |>
    slice(1) |>
    ungroup() |>
    pull(path)
}

load_rdata_object <- function(path) {
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  obj_names <- ls(env)

  if (length(obj_names) == 0) {
    stop("No object found in file: ", path)
  }

  obj <- env[[obj_names[[1]]]]

  if (!inherits(obj, "data.frame")) {
    stop("Object in file is not a data.frame: ", path)
  }

  tibble::as_tibble(obj)
}

process_iccs_2022 <- function(
  data_dir = "raw_data/ICCS2022_IDB_R/Data",
  country_lookup,
  swiid_lookup) {
  files <- select_iccs2022_isg_files(data_dir)

  required_vars <- c(
    "IDCNTRY", "IDSCHOOL", "IDSTUD", "TOTWGTS", "PV1CIV",
    "IS4G30C", "IS4G30D", "IS4G31B", "IS4G31C", "IS4G31I",
    "IS4G31K", "IS4G31L", "IS4G31M", "IS4G14G", "S_HISCED", "SD_GENDER"
  )

  country_frames <- purrr::map(files, function(f) {
    df <- load_rdata_object(f)
    file_name <- basename(f)
    country_code <- substr(file_name, 4, 6)

    missing_required <- setdiff(required_vars, names(df))
    if (length(missing_required) > 0) {
      warning(
        "Skipping ", file_name,
        " because required variables are missing: ",
        paste(missing_required, collapse = ", ")
      )
      return(NULL)
    }

    df |>
      mutate(
        source_file = file_name,
        country_code = country_code
      )
  }) |>
    purrr::compact()

  if (length(country_frames) == 0) {
    stop("No valid ICCS 2022 country files could be processed.")
  }

  df_2022 <- bind_rows(country_frames) |>
    rename(
      idcntry = IDCNTRY,
      idschool = IDSCHOOL,
      idstud = IDSTUD,
      totwgts = TOTWGTS
    ) |>
    add_common_design_vars() |>
    mutate(
      marcha = binary_12_yes(IS4G31C),
      ocupa = binary_12_yes(IS4G31M),
      trafico = binary_12_yes(IS4G31L),
      rayar = binary_12_yes(IS4G31K),
      radical = row_mean_safe(
        reverse_1_4(IS4G31I),
        reverse_1_4(IS4G31L),
        reverse_1_4(IS4G31M)
      ),
      pacific = row_mean_safe(
        reverse_1_4(IS4G31B),
        reverse_1_4(IS4G30C),
        reverse_1_4(IS4G30D)
      ),
      S_GENDER = case_when(
        SD_GENDER == 1 ~ 1,
        SD_GENDER == 0 ~ 0,
        SD_GENDER %in% c(8, 9) ~ NA_real_,
        TRUE ~ NA_real_
      ),
      S_HISCED = case_when(
        S_HISCED %in% c(8, 9) ~ NA_real_,
        TRUE ~ as.numeric(S_HISCED)
      ),
      University = case_when(
        S_HISCED == 4 ~ 1,
        S_HISCED %in% 0:3 ~ 0,
        TRUE ~ NA_real_
      ),
      internet = as.numeric(IS4G14G),
      PV1CIV = as.numeric(PV1CIV),
      study = 2022,
      country_name_from_file = map_iccs2022_country_code(country_code),
      country_key = normalize_country_name(country_name_from_file)
    ) |>
    join_swiid_gini(
      study_year = 2022,
      country_lookup = country_lookup,
      swiid_lookup = swiid_lookup,
      country_lookup_extra = NULL
    ) |>
    group_by(idcntry) |>
    mutate(pct_know = dplyr::ntile(PV1CIV, 100)) |>
    ungroup() |>
    harmonize_ids() |>
    select_standard()

  df_2022
}

build_multilevel_dataset_with_iccs2022 <- function(
    file_1999 = "raw_data/subset_CIVED_1999.dta",
    file_2009 = "raw_data/subset_ICCS_2009.dta",
    file_2016 = "raw_data/subset_ICCS_2016.dta",
    swiid_summary_path = "raw_data/swiid9_92/swiid9_92_summary.csv",
    iccs2022_data_dir = "raw_data/ICCS2022_IDB_R/Data",
    output_dir = "proc/output") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  country_lookup <- build_country_lookup_from_files(file_1999, file_2009, file_2016)
  swiid_lookup <- read_swiid_lookup(swiid_summary_path)

  cived_1999 <- process_cived_1999(file_1999, country_lookup = country_lookup, swiid_lookup = swiid_lookup)
  iccs_2009 <- process_iccs_2009(file_2009, country_lookup = country_lookup, swiid_lookup = swiid_lookup)
  iccs_2016 <- process_iccs_2016(file_2016, country_lookup = country_lookup, swiid_lookup = swiid_lookup)
  iccs_2022 <- process_iccs_2022(iccs2022_data_dir, country_lookup = country_lookup, swiid_lookup = swiid_lookup)

  pooled <- bind_rows(cived_1999, iccs_2009, iccs_2016, iccs_2022)

  write_rds(cived_1999, file.path(output_dir, "cived_1999_processed.rds"))
  write_rds(iccs_2009, file.path(output_dir, "iccs_2009_processed.rds"))
  write_rds(iccs_2016, file.path(output_dir, "iccs_2016_processed.rds"))
  write_rds(iccs_2022, file.path(output_dir, "iccs_2022_processed.rds"))
  write_rds(pooled, file.path(output_dir, "iccs_cived_1999_2009_2016_2022_pooled.rds"))

  pooled
}

if (sys.nframe() == 0) {
  build_multilevel_dataset_with_iccs2022()
}

data <- readRDS("proc/output/iccs_cived_1999_2009_2016_2022_pooled.rds")

View(data)
