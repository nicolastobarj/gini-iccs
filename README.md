# Gini-ICCS longitudinal project

This repository migrates the original Stata workflow (`2022_INVALSI/INVALSI_2022.do`) to reproducible R scripts, prioritizing data processing and multilevel harmonization.

## Repository structure

- `raw_data/`: raw input datasets used by scripts.
- `proc/`: data processing and harmonization scripts.
- `analyses/`: models, tables, plots, and analysis outputs (`.rds`).
- `presentations/`: `.Rmd` / `.qmd` presentation files.
- `article/`: manuscript sources.
- `bib/`: bibliography files.

## Current implementation status

### Completed
- Created reproducible folder structure.
- Added `proc/01_build_multilevel_processing.R` as first Stata-to-R processing translation.
- Added `proc/00_run_all_processing.R` orchestration script.
- Processing currently builds harmonized pooled data for:
  - CIVED 1999
  - ICCS 2009
  - ICCS 2016

### Outputs generated
Running `source("proc/00_run_all_processing.R")` writes:

- `proc/output/cived_1999_processed.rds`
- `proc/output/iccs_2009_processed.rds`
- `proc/output/iccs_2016_processed.rds`
- `proc/output/iccs_cived_1999_2009_2016_pooled.rds`

## Next migration steps

1. Add `raw_data/ICCS_2023` inputs and create `process_iccs_2023()`.
2. Validate every recode against `INVALSI_2022.do` with side-by-side QA tables.
3. Move all modeling/plotting to dedicated scripts under `analyses/`.
4. Externalize Gini mappings into a maintained country-year file in `raw_data/`.
