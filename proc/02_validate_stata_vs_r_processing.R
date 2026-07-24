library(dplyr)
library(haven)
library(readr)
library(tidyr)
library(purrr)

normalize_for_compare <- function(df, numeric_digits = 10) {
  df |>
    mutate(
      across(
        everything(),
        \(x) {
          if (inherits(x, "haven_labelled")) {
            x <- haven::zap_labels(x)
          }

          if (is.factor(x)) {
            return(as.character(x))
          }

          if (is.numeric(x)) {
            return(round(as.numeric(x), numeric_digits))
          }

          x
        }
      )
    )
}

column_mismatch_counts <- function(reference, candidate, cols, tolerance = 1e-8) {
  tibble(column = cols) |>
    mutate(
      n_mismatch = map_int(
        column,
        \(col) {
          x <- reference[[col]]
          y <- candidate[[col]]

          both_na <- is.na(x) & is.na(y)

          if (is.numeric(x) && is.numeric(y)) {
            sum(!both_na & (is.na(x) != is.na(y) | abs(x - y) > tolerance), na.rm = TRUE)
          } else {
            sum(!both_na & (is.na(x) != is.na(y) | x != y), na.rm = TRUE)
          }
        }
      )
    ) |>
    arrange(desc(n_mismatch), column)
}

row_multiset_diff <- function(reference, candidate, cols) {
  ref_n <- reference |>
    count(across(all_of(cols)), name = "n_reference")

  cand_n <- candidate |>
    count(across(all_of(cols)), name = "n_candidate")

  full_join(ref_n, cand_n, by = cols) |>
    mutate(
      n_reference = coalesce(n_reference, 0L),
      n_candidate = coalesce(n_candidate, 0L),
      delta = n_reference - n_candidate
    ) |>
    filter(delta != 0L)
}

validate_pair <- function(reference_dta, candidate_rds, study_label, out_dir) {
  reference <- read_dta(reference_dta) |> as_tibble()
  candidate <- read_rds(candidate_rds) |> as_tibble()

  reference <- normalize_for_compare(reference)
  candidate <- normalize_for_compare(candidate)

  reference_names <- names(reference)
  candidate_names <- names(candidate)

  columns_identical_order <- identical(reference_names, candidate_names)
  columns_same_set <- setequal(reference_names, candidate_names)

  only_reference <- setdiff(reference_names, candidate_names)
  only_candidate <- setdiff(candidate_names, reference_names)

  common_cols <- intersect(reference_names, candidate_names)

  if (length(common_cols) == 0) {
    comparison_cols <- character()
    mismatches <- tibble(column = character(), n_mismatch = integer())
    row_diff <- tibble()
  } else {
    comparison_cols <- if (columns_same_set) reference_names else common_cols

    ref_cmp <- reference |> select(all_of(comparison_cols))
    cand_cmp <- candidate |> select(all_of(comparison_cols))

    key_candidates <- c("idcntry", "idschool", "study", "year_study")
    key_cols <- key_candidates[key_candidates %in% comparison_cols]

    if (length(key_cols) > 0) {
      ref_cmp <- ref_cmp |> arrange(across(all_of(key_cols)))
      cand_cmp <- cand_cmp |> arrange(across(all_of(key_cols)))
    }

    if (nrow(ref_cmp) == nrow(cand_cmp)) {
      mismatches <- column_mismatch_counts(ref_cmp, cand_cmp, comparison_cols)
    } else {
      mismatches <- tibble(column = comparison_cols, n_mismatch = NA_integer_)
    }

    row_diff <- row_multiset_diff(ref_cmp, cand_cmp, comparison_cols)
  }

  rows_equal_count <- nrow(reference) == nrow(candidate)
  cols_equal_count <- ncol(reference) == ncol(candidate)
  values_equal <- rows_equal_count && nrow(row_diff) == 0 && all(coalesce(mismatches$n_mismatch, 0L) == 0L)

  summary_row <- tibble(
    study = study_label,
    reference_file = reference_dta,
    candidate_file = candidate_rds,
    nrow_reference = nrow(reference),
    nrow_candidate = nrow(candidate),
    ncol_reference = ncol(reference),
    ncol_candidate = ncol(candidate),
    rows_equal_count = rows_equal_count,
    cols_equal_count = cols_equal_count,
    columns_identical_order = columns_identical_order,
    columns_same_set = columns_same_set,
    values_equal = values_equal,
    exact_match = rows_equal_count && cols_equal_count && columns_identical_order && values_equal
  )

  write_csv(
    tibble(column = only_reference),
    file.path(out_dir, paste0("columns_only_in_stata_", study_label, ".csv"))
  )

  write_csv(
    tibble(column = only_candidate),
    file.path(out_dir, paste0("columns_only_in_r_", study_label, ".csv"))
  )

  write_csv(
    mismatches,
    file.path(out_dir, paste0("column_mismatch_counts_", study_label, ".csv"))
  )

  write_csv(
    row_diff |> slice_head(n = 5000),
    file.path(out_dir, paste0("row_multiset_diff_", study_label, ".csv"))
  )

  summary_row
}

run_stata_r_validation <- function(
    stata_dir = "2022_INVALSI",
    processed_dir = "proc/output",
    out_dir = "proc/output/validation") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  required_r <- c(
    file.path(processed_dir, "cived_1999_processed.rds"),
    file.path(processed_dir, "iccs_2009_processed.rds"),
    file.path(processed_dir, "iccs_2016_processed.rds")
  )

  if (!all(file.exists(required_r))) {
    source("proc/01_build_multilevel_processing.R")
    build_multilevel_dataset(output_dir = processed_dir)
  }

  file_map <- tibble::tribble(
    ~study, ~reference_file, ~candidate_file,
    "cived_1999", file.path(stata_dir, "subset_CIVED_1999.dta"), file.path(processed_dir, "cived_1999_processed.rds"),
    "iccs_2009", file.path(stata_dir, "subset_ICCS_2009.dta"), file.path(processed_dir, "iccs_2009_processed.rds"),
    "iccs_2016", file.path(stata_dir, "subset_ICCS_2016.dta"), file.path(processed_dir, "iccs_2016_processed.rds")
  )

  summary_tbl <- pmap_dfr(
    file_map,
    \(study, reference_file, candidate_file) {
      validate_pair(
        reference_dta = reference_file,
        candidate_rds = candidate_file,
        study_label = study,
        out_dir = out_dir
      )
    }
  )

  write_csv(summary_tbl, file.path(out_dir, "stata_vs_r_validation_summary.csv"))

  summary_tbl
}

if (sys.nframe() == 0) {
  run_stata_r_validation()
}
