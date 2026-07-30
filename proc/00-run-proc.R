source("proc/05-build-pooled-data.R")

pooled <- build_pooled_data(
  cived_data_dir = "raw_data/CivED1999_IDB_SPSS_G8/Data",
  iccs2009_data_dir = "raw_data/ICCS2009_IDB_SPSS/Data_G8",
  iccs2016_data_dir = "raw_data/ICCS2016_IDB_SPSS/Data",
  iccs2022_data_dir = "raw_data/ICCS2022_IDB_R/Data",
  swiid_summary_path = "raw_data/swiid9_92/swiid9_92_summary.csv",
  output_dir = "proc/output"
)

if (requireNamespace("quarto", quietly = TRUE)) {
  quarto::quarto_render("proc/06_pooled_data_quality_report.qmd")
} else {
  message("Package 'quarto' is not installed; skipping report render.")
}

invisible(pooled)
