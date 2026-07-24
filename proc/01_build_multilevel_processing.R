library(dplyr)
library(haven)
library(readr)

standard_vars <- c(
  "idcntry", "idschool", "study", "year", "marcha", "ocupa", "trafico", "rayar", "radical", "pacific",
  "gini", "PV1CIV", "pct_know", "S_GENDER", "S_HISCED", "University", "internet", "totwgts"
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

gini_2016 <- tibble::tribble(
  ~idcntry, ~gini,
  956, 25.3, 100, 38.7, 152, 45.8, 170, 47.7, 208, 25.7, 276001, 28.8,
  214, 43.8, 233, 30.8, 246, 25.7, 344, 44.2, 191, 31.7, 380, 35.3,
  410, 29.9, 440, 35.5, 428, 36.6, 484, 43.9, 470, 27.0, 528, 27.8,
  578, 25.9, 604, 44.7, 643, 35.0, 705, 25.2, 752, 27.2, 158, 30.0
)

gini_2009 <- tibble::tribble(
  ~idcntry, ~gini,
  40, 28.9, 100, 35.2, 152, 48.5, 158, 31.8, 170, 52.7, 196, 28.9,
  203, 24.3, 208, 24.3, 214, 47.7, 233, 31.9, 246, 24.4, 300, 32.7,
  320, 47.0, 344, 42.8, 360, 46.8, 372, 30.1, 380, 33.0, 410, 31.7,
  428, 36.6, 438, NA_real_, 440, 34.8, 442, 28.3, 470, 27.2, 484, 44.8,
  528, 25.8, 554, 31.9, 578, 24.5, 600, 49.5, 616, 30.7, 643, 35.6,
  703, 25.8, 705, 25.5, 724, 32.4, 752, 26.9, 756, 29.7, 764, 40.7,
  926, 33.7, 956, 26.8
)

gini_1999 <- tibble::tribble(
  ~idcntry, ~gini,
  36, 31.5, 957, 28.3, 100, 30.8, 152, 50.5, 170, 51.3, 196, 27.4,
  200, 22.9, 208, 22.8, 826, 35.1, 233, 33.3, 246, 24.2, 276, 26.0,
  300, 33.9, 344, 43.1, 348, 29.0, 380, 32.7, 428, 33.9, 440, 32.3,
  578, 24.7, 616, 29.5, 620, 30.5, 642, 29.8, 643, 34.7, 703, 25.0,
  705, 22.8, 752, 25.4, 756, 26.2, 840, 36.8
)

harmonize_ids <- function(df) {
  df |>
    mutate(
      year = dplyr::case_when(
        study == 1999 ~ 1,
        study == 2009 ~ 2,
        study == 2016 ~ 3,
        study == 2023 ~ 4,
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

process_iccs_2016 <- function(path) {
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

  df |>
    select(-any_of("gini")) |>
    left_join(gini_2016, by = "idcntry") |>
    group_by(idcntry) |>
    mutate(pct_know = dplyr::ntile(PV1CIV, 100)) |>
    ungroup() |>
    mutate(study = if_else(is.na(study), 2016, as.numeric(study))) |>
    harmonize_ids() |>
    select_standard()
}

process_iccs_2009 <- function(path) {
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

  df |>
    select(-any_of("gini")) |>
    left_join(gini_2009, by = "idcntry") |>
    group_by(idcntry) |>
    mutate(pct_know = dplyr::ntile(PV1CIV, 100)) |>
    ungroup() |>
    mutate(study = if_else(is.na(study), 2009, as.numeric(study))) |>
    harmonize_ids() |>
    select_standard()
}

process_cived_1999 <- function(path) {
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

  df |>
    select(-any_of("gini")) |>
    left_join(gini_1999, by = "idcntry") |>
    group_by(idcntry) |>
    mutate(pct_know = dplyr::ntile(PV1CIV, 100)) |>
    ungroup() |>
    mutate(study = if_else(is.na(study), 1999, as.numeric(study))) |>
    harmonize_ids() |>
    select_standard()
}

build_multilevel_dataset <- function(
    file_1999 = "raw_data/subset_CIVED_1999.dta",
    file_2009 = "raw_data/subset_ICCS_2009.dta",
    file_2016 = "raw_data/subset_ICCS_2016.dta",
    output_dir = "proc/output") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  cived_1999 <- process_cived_1999(file_1999)
  iccs_2009 <- process_iccs_2009(file_2009)
  iccs_2016 <- process_iccs_2016(file_2016)

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
