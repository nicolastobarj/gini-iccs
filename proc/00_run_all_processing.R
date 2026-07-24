source("proc/01_build_multilevel_processing.R")

pooled <- build_multilevel_dataset(
  file_1999 = "raw_data/subset_CIVED_1999.dta",
  file_2009 = "raw_data/subset_ICCS_2009.dta",
  file_2016 = "raw_data/subset_ICCS_2016.dta",
  output_dir = "proc/output"
)

invisible(pooled)
