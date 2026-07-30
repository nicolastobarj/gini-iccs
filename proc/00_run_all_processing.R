source("proc/05_build_multilevel_with_iccs2022.R")

pooled <- build_multilevel_dataset_with_iccs2022(
  file_1999 = "raw_data/subset_CIVED_1999.dta",
  file_2009 = "raw_data/subset_ICCS_2009.dta",
  file_2016 = "raw_data/subset_ICCS_2016.dta",
  swiid_summary_path = "raw_data/swiid9_92/swiid9_92_summary.csv",
  iccs2022_data_dir = "raw_data/ICCS2022_IDB_R/Data",
  output_dir = "proc/output"
)

if (requireNamespace("quarto", quietly = TRUE)) {
  quarto::quarto_render("proc/06_pooled_data_quality_report.qmd")
} else {
  message("Package 'quarto' is not installed; skipping report render.")
}

invisible(pooled)
