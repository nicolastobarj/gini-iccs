library(dplyr)
library(haven)
library(readr)

standard_vars <- c(
  "idcntry", "idschool", "study", "year", "marcha", "ocupa", "trafico", "rayar", "radical", "pacific",
  "gini", "PV1CIV", "pct_know", "S_GENDER", "S_HISCED", "University", "internet", "totwgts"
)

swiid_study_years <- c(1999, 2009, 2016, 2022)

# ICCS includes a few study-specific country IDs that should map to standard SWIID names.
idcntry_country_overrides <- tibble::tribble(
  ~idcntry, ~country,
  36, "Australia",
  76, "Brazil",
  158, "Taiwan",
  200, "Czech Republic",
  250, "France",
  276, "Germany",
  344, "Hong Kong",
  348, "Hungary",
  410, "Korea",
  620, "Portugal",
  642, "Romania",
  643, "Russia",
  688, "Serbia",
  703, "Slovakia",
  826, "United Kingdom",
  840, "United States",
  926, "United Kingdom",
  956, "Belgium",
  957, "Belgium",
  9642, "Palestinian Territories",
  276001, "Germany",
  276005, "Germany"
)

# Harmonize legacy country labels from ICCS/CIVED files to SWIID naming.
country_key_aliases <- c(
  "hong kong sar" = "hong kong",
  "russian federation" = "russia",
  "slovak republic" = "slovakia",
  "chinese taipei" = "taiwan",
  "korea republic of" = "korea",
  "belgium flemish" = "belgium",
  "england" = "united kingdom",
  "germany north rhine westphalia" = "germany",
  "germany schleswig holstein" = "germany",
  "germany schleswig holstein land" = "germany"
)

binary_12_yes <- function(x, na_vals = c(0, 8, 9, 99)) {
  dplyr::case_when(
    x %in% c(1, 2) ~ 1,
    x %in% c(3, 4) ~ 0,
    x %in% na_vals ~ NA_real_,
    TRUE ~ NA_real_
  )
}

binary_34_yes <- function(x, na_vals = c(0, 8, 9, 99)) {
  dplyr::case_when(
    x %in% c(3, 4) ~ 1,
    x %in% c(1, 2) ~ 0,
    x %in% na_vals ~ NA_real_,
    TRUE ~ NA_real_
  )
}

reverse_1_4 <- function(x, na_vals = c(0, 8, 9, 99)) {
  dplyr::case_when(
    x %in% 1:4 ~ 5 - as.numeric(x),
    x %in% na_vals ~ NA_real_,
    TRUE ~ NA_real_
  )
}

row_mean_safe <- function(...) {
  m <- cbind(...)
  out <- rowMeans(m, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

add_common_design_vars <- function(df) {
  if (!"totwgts" %in% names(df) && "totwgt" %in% names(df)) {
    df <- df |> mutate(totwgts = as.numeric(totwgt))
  }

  if (!"idcntry" %in% names(df) && "cntryid" %in% names(df)) {
    df <- df |> mutate(idcntry = as.numeric(cntryid))
  }

  df |>
    mutate(
      idcntry = as.numeric(idcntry),
      idschool = as.numeric(idschool),
      totwgts = as.numeric(totwgts)
    )
}

normalize_country_name <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- trimws(x)
  dplyr::recode(x, !!!country_key_aliases, .default = x)
}

map_iccs2022_country_code <- function(code) {
  dplyr::recode(
    code,
    "BGR" = "Bulgaria",
    "BRA" = "Brazil",
    "COL" = "Colombia",
    "CYP" = "Cyprus",
    "DNK" = "Denmark",
    "DNW" = "Germany",
    "DSH" = "Germany",
    "ESP" = "Spain",
    "EST" = "Estonia",
    "FRA" = "France",
    "HRV" = "Croatia",
    "ITA" = "Italy",
    "LTU" = "Lithuania",
    "LVA" = "Latvia",
    "MLT" = "Malta",
    "NLD" = "Netherlands",
    "NOR" = "Norway",
    "POL" = "Poland",
    "ROU" = "Romania",
    "SRB" = "Serbia",
    "SVK" = "Slovakia",
    "SVN" = "Slovenia",
    "SWE" = "Sweden",
    "TWN" = "Taiwan",
    .default = NA_character_
  )
}

read_swiid_lookup <- function(
    path = "raw_data/swiid9_92/swiid9_92_summary.csv",
    study_years = swiid_study_years) {
  read_csv(path, show_col_types = FALSE) |>
    transmute(
      country = as.character(country),
      study = as.numeric(year),
      gini = as.numeric(gini_disp)
    ) |>
    filter(study %in% study_years) |>
    mutate(country_key = normalize_country_name(country)) |>
    select(country_key, study, gini) |>
    distinct(country_key, study, .keep_all = TRUE)
}

extract_idcntry_country_lookup <- function(raw_df) {
  id_raw <- NULL

  if ("idcntry" %in% names(raw_df)) {
    id_raw <- raw_df$idcntry
  }
  if (is.null(id_raw) && "cntryid" %in% names(raw_df)) {
    id_raw <- raw_df$cntryid
  }

  if (is.null(id_raw)) {
    return(tibble::tibble(idcntry = numeric(), country = character()))
  }

  country_lbl <- suppressWarnings(as.character(haven::as_factor(id_raw, levels = "labels")))

  out <- tibble::tibble(
    idcntry = as.numeric(id_raw),
    country = country_lbl
  ) |>
    filter(!is.na(idcntry), !is.na(country), country != "")

  # If labels are unavailable, as_factor can return numeric text; drop those pseudo-labels.
  out |>
    filter(!grepl("^[0-9.]+$", country)) |>
    distinct(idcntry, country)
}

build_country_lookup_from_files <- function(file_1999, file_2009, file_2016) {
  from_files <- bind_rows(
    extract_idcntry_country_lookup(read_dta(file_1999)),
    extract_idcntry_country_lookup(read_dta(file_2009)),
    extract_idcntry_country_lookup(read_dta(file_2016))
  )

  # Put overrides first so they take precedence when labels are inconsistent.
  bind_rows(idcntry_country_overrides, from_files) |>
    distinct(idcntry, .keep_all = TRUE) |>
    mutate(country_key = normalize_country_name(country)) |>
    select(idcntry, country_key)
}

join_swiid_gini <- function(df, study_year, country_lookup, swiid_lookup, country_lookup_extra = NULL) {
  lookup_all <- bind_rows(country_lookup_extra, country_lookup) |>
    distinct(idcntry, .keep_all = TRUE)

  if (!"country_key" %in% names(df)) {
    df$country_key <- NA_character_
  }

  out <- df |>
    mutate(study = if_else(is.na(study), study_year, as.numeric(study))) |>
    select(-any_of("gini")) |>
    rename(country_key_input = country_key) |>
    left_join(lookup_all, by = "idcntry") |>
    mutate(country_key = dplyr::coalesce(country_key_input, country_key)) |>
    select(-country_key_input) |>
    left_join(swiid_lookup, by = c("country_key", "study")) |>
    select(-country_key)

  missing_ids <- out |>
    filter(is.na(gini)) |>
    distinct(idcntry) |>
    pull(idcntry)

  if (length(missing_ids) > 0) {
    warning(
      "SWIID gini missing for study ", study_year,
      " and idcntry: ", paste(sort(missing_ids), collapse = ", ")
    )
  }

  out
}

harmonize_ids <- function(df) {
  df |>
    mutate(
      year = dplyr::case_when(
        study == 1999 ~ 1,
        study == 2009 ~ 2,
        study == 2016 ~ 3,
        study == 2022 ~ 4,
        TRUE ~ NA_real_
      )
    )
}

select_standard <- function(df) {
  missing_cols <- setdiff(standard_vars, names(df))
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      df[[col]] <- NA_real_
    }
  }
  df |>
    mutate(across(any_of(c("S_HISCED", "S_GENDER", "gini", "PV1CIV")), as.numeric)) |>
    select(all_of(standard_vars))
}

process_iccs_2016 <- function(path, country_lookup, swiid_lookup) {
  df <- read_dta(path) |> as_tibble() |> add_common_design_vars()

  if (all(c("IS3G30C", "IS3G30K", "IS3G30J", "IS3G30I", "IS3G31I", "IS3G31B", "IS3G30D") %in% names(df))) {
    df <- df |>
      mutate(
        marcha = binary_12_yes(IS3G30C),
        ocupa = binary_12_yes(IS3G30K),
        trafico = binary_12_yes(IS3G30J),
        rayar = binary_12_yes(IS3G30I),
        radical = row_mean_safe(reverse_1_4(IS3G31I), reverse_1_4(IS3G30J), reverse_1_4(IS3G30K)),
        pacific = row_mean_safe(reverse_1_4(IS3G31B), reverse_1_4(IS3G30C), reverse_1_4(IS3G30D)),
        University = case_when(S_HISCED == 4 ~ 1, S_HISCED %in% 0:3 ~ 0, TRUE ~ NA_real_),
        internet = as.numeric(IS3G14G),
        study = 2016
      )
  }

  if (!"pacific" %in% names(df)) {
    warning("Study 2016: pacific not found in input file and required raw items are unavailable.")
  }

  df |>
    join_swiid_gini(study_year = 2016, country_lookup = country_lookup, swiid_lookup = swiid_lookup) |>
    group_by(idcntry) |>
    mutate(pct_know = dplyr::ntile(PV1CIV, 100)) |>
    ungroup() |>
    harmonize_ids() |>
    select_standard()
}

process_iccs_2009 <- function(path, country_lookup, swiid_lookup) {
  df <- read_dta(path) |> as_tibble() |> add_common_design_vars()

  if (all(c("is2p31d", "is2p31g", "is2p31h", "is2p31i", "is2p31c", "is2p31e", "sgender", "hisced") %in% names(df))) {
    df <- df |>
      mutate(
        marcha = binary_12_yes(is2p31d),
        rayar = binary_12_yes(is2p31g),
        trafico = binary_12_yes(is2p31h),
        ocupa = binary_12_yes(is2p31i),
        radical = row_mean_safe(reverse_1_4(is2p31g), reverse_1_4(is2p31h), reverse_1_4(is2p31i)),
        pacific = row_mean_safe(reverse_1_4(is2p31c), reverse_1_4(is2p31d), reverse_1_4(is2p31e)),
        S_GENDER = as.numeric(sgender),
        S_HISCED = if_else(hisced %in% c(0, 1), 1, as.numeric(hisced)),
        University = case_when(S_HISCED == 5 ~ 1, S_HISCED %in% 1:4 ~ 0, TRUE ~ NA_real_),
        internet = as.numeric(is2g13e),
        study = 2009
      )
  }

  if (!"pacific" %in% names(df)) {
    warning("Study 2009: pacific not found in input file and required raw items are unavailable.")
  }

  df |>
    join_swiid_gini(study_year = 2009, country_lookup = country_lookup, swiid_lookup = swiid_lookup) |>
    group_by(idcntry) |>
    mutate(pct_know = dplyr::ntile(PV1CIV, 100)) |>
    ungroup() |>
    harmonize_ids() |>
    select_standard()
}

process_cived_1999 <- function(path, country_lookup, swiid_lookup) {
  df <- read_dta(path) |> as_tibble() |> add_common_design_vars()

  if (all(c("bs5m9", "bs5m10", "bs5m11", "bs5m12", "bs5m4", "bs5m8", "itsex", "bsgedum", "bsgeduf", "knowlmle", "bsgp1") %in% names(df))) {
    hisced_m <- if_else(df$bsgedum %in% c(0, 8), NA_real_, as.numeric(df$bsgedum))
    hisced_f <- if_else(df$bsgeduf %in% c(0, 8), NA_real_, as.numeric(df$bsgeduf))
    hisced_raw <- pmax(hisced_m, hisced_f, na.rm = TRUE)
    hisced_raw[is.infinite(hisced_raw)] <- NA_real_

    df <- df |>
      mutate(
        marcha = binary_34_yes(bs5m9),
        rayar = binary_34_yes(bs5m10),
        trafico = binary_34_yes(bs5m11),
        ocupa = binary_34_yes(bs5m12),
        radical = row_mean_safe(
          if_else(bs5m10 %in% c(0, 8), NA_real_, as.numeric(bs5m10)),
          if_else(bs5m11 %in% c(0, 8), NA_real_, as.numeric(bs5m11)),
          if_else(bs5m12 %in% c(0, 8), NA_real_, as.numeric(bs5m12))
        ),
        pacific = row_mean_safe(
          if_else(bs5m4 %in% c(0, 8), NA_real_, as.numeric(bs5m4)),
          if_else(bs5m8 %in% c(0, 8), NA_real_, as.numeric(bs5m8)),
          if_else(bs5m9 %in% c(0, 8), NA_real_, as.numeric(bs5m9))
        ),
        S_GENDER = case_when(itsex == 1 ~ 1, itsex == 2 ~ 0, itsex %in% c(8, 9) ~ NA_real_, TRUE ~ NA_real_),
        S_HISCED = case_when(
          hisced_raw == 1 ~ 1,
          hisced_raw %in% c(2, 3) ~ 2,
          hisced_raw == 4 ~ 3,
          hisced_raw %in% c(5, 6) ~ 4,
          hisced_raw == 7 ~ 5,
          TRUE ~ NA_real_
        ),
        University = case_when(S_HISCED == 5 ~ 1, S_HISCED %in% 1:4 ~ 0, TRUE ~ NA_real_),
        internet = if_else(bsgp1 %in% c(0, 8), NA_real_, as.numeric(bsgp1)),
        PV1CIV = as.numeric(knowlmle),
        study = 1999
      )
  }

  if (!"pacific" %in% names(df)) {
    warning("Study 1999: pacific not found in input file and required raw items are unavailable.")
  }

  df |>
    join_swiid_gini(study_year = 1999, country_lookup = country_lookup, swiid_lookup = swiid_lookup) |>
    group_by(idcntry) |>
    mutate(pct_know = dplyr::ntile(PV1CIV, 100)) |>
    ungroup() |>
    harmonize_ids() |>
    select_standard()
}

build_multilevel_dataset <- function(
    file_1999 = "raw_data/subset_CIVED_1999.dta",
    file_2009 = "raw_data/subset_ICCS_2009.dta",
    file_2016 = "raw_data/subset_ICCS_2016.dta",
    swiid_summary_path = "raw_data/swiid9_92/swiid9_92_summary.csv",
    output_dir = "proc/output") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  country_lookup <- build_country_lookup_from_files(file_1999, file_2009, file_2016)
  swiid_lookup <- read_swiid_lookup(swiid_summary_path)

  cived_1999 <- process_cived_1999(file_1999, country_lookup = country_lookup, swiid_lookup = swiid_lookup)
  iccs_2009 <- process_iccs_2009(file_2009, country_lookup = country_lookup, swiid_lookup = swiid_lookup)
  iccs_2016 <- process_iccs_2016(file_2016, country_lookup = country_lookup, swiid_lookup = swiid_lookup)

  pooled <- bind_rows(cived_1999, iccs_2009, iccs_2016)

  write_rds(cived_1999, file.path(output_dir, "cived_1999_processed.rds"))
  write_rds(iccs_2009, file.path(output_dir, "iccs_2009_processed.rds"))
  write_rds(iccs_2016, file.path(output_dir, "iccs_2016_processed.rds"))
  write_rds(pooled, file.path(output_dir, "iccs_cived_1999_2009_2016_pooled.rds"))

  pooled
}

if (sys.nframe() == 0) {
  build_multilevel_dataset()
}
