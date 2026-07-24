library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)

build_validation_report_tables <- function(
    validation_dir = "proc/output/validation",
    refresh_validation = TRUE) {
  if (refresh_validation) {
    source("proc/02_validate_stata_vs_r_processing.R")
    run_stata_r_validation(out_dir = validation_dir)
  }

  summary_path <- file.path(validation_dir, "stata_vs_r_validation_summary.csv")
  summary_tbl <- read_csv(summary_path, show_col_types = FALSE)

  mismatch_files <- list.files(
    validation_dir,
    pattern = "^column_mismatch_counts_.*\\.csv$",
    full.names = TRUE
  )

  mismatch_tbl <- mismatch_files |>
    set_names() |>
    map_dfr(
      \(path) {
        study <- str_remove(basename(path), "^column_mismatch_counts_") |>
          str_remove("\\.csv$")

        read_csv(path, show_col_types = FALSE) |>
          mutate(study = study)
      }
    ) |>
    left_join(summary_tbl |> select(study, nrow_reference), by = "study") |>
    mutate(
      mismatch_rate = n_mismatch / nrow_reference,
      mismatch_rate_pct = 100 * mismatch_rate
    ) |>
    relocate(study, column)

  columns_presence_files <- list.files(
    validation_dir,
    pattern = "^columns_only_in_(stata|r)_.*\\.csv$",
    full.names = TRUE
  )

  columns_presence_tbl <- columns_presence_files |>
    set_names() |>
    map_dfr(
      \(path) {
        fname <- basename(path)
        side <- if_else(str_detect(fname, "stata"), "stata_only", "r_only")
        study <- fname |>
          str_remove("^columns_only_in_(stata|r)_") |>
          str_remove("\\.csv$")

        tbl <- read_csv(path, show_col_types = FALSE)

        if (nrow(tbl) == 0) {
          tibble(study = study, side = side, column = NA_character_)
        } else {
          tbl |>
            transmute(study = study, side = side, column = column)
        }
      }
    )

  row_diff_files <- list.files(
    validation_dir,
    pattern = "^row_multiset_diff_.*\\.csv$",
    full.names = TRUE
  )

  row_diff_tbl <- row_diff_files |>
    set_names() |>
    map_dfr(
      \(path) {
        study <- str_remove(basename(path), "^row_multiset_diff_") |>
          str_remove("\\.csv$")

        n_diff_rows <- suppressWarnings(nrow(read_csv(path, show_col_types = FALSE)))

        tibble(study = study, n_row_multiset_differences = n_diff_rows)
      }
    )

  overview_tbl <- summary_tbl |>
    left_join(row_diff_tbl, by = "study") |>
    mutate(
      n_row_multiset_differences = coalesce(n_row_multiset_differences, 0L),
      status = case_when(
        exact_match ~ "Exact match",
        rows_equal_count & columns_same_set & values_equal ~ "Equivalent values (order differs)",
        rows_equal_count & !columns_same_set ~ "Rows match; column set differs",
        rows_equal_count & columns_same_set & !values_equal ~ "Rows/columns match; some values differ",
        TRUE ~ "Not equivalent"
      )
    )

  key_vars_tbl <- mismatch_tbl |>
    filter(column %in% c("idcntry", "idschool", "study", "year_study", "marcha", "ocupa", "trafico", "rayar", "radical", "PV1CIV", "S_GENDER", "S_HISCED", "University", "internet", "totwgts", "gini", "pct_know")) |>
    arrange(study, desc(n_mismatch), column)

  readr::write_csv(overview_tbl, file.path(validation_dir, "validation_report_overview.csv"))
  readr::write_csv(mismatch_tbl, file.path(validation_dir, "validation_report_mismatch_by_column.csv"))
  readr::write_csv(key_vars_tbl, file.path(validation_dir, "validation_report_key_vars.csv"))
  readr::write_csv(columns_presence_tbl, file.path(validation_dir, "validation_report_columns_presence.csv"))
  readr::write_csv(row_diff_tbl, file.path(validation_dir, "validation_report_row_diff_counts.csv"))

  out <- list(
    overview = overview_tbl,
    mismatches = mismatch_tbl,
    key_vars = key_vars_tbl,
    columns_presence = columns_presence_tbl,
    row_diff = row_diff_tbl
  )

  saveRDS(out, file.path(validation_dir, "validation_report_objects.rds"))

  out
}

if (sys.nframe() == 0) {
  build_validation_report_tables()
}
