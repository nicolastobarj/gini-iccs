library(dplyr)
library(readr)
library(tibble)

resolve_project_output_dir <- function(output_dir) {
  is_abs <- grepl("^(?:[A-Za-z]:|/|\\\\)\\\\?", output_dir)
  if (is_abs) {
    return(output_dir)
  }

  cmd <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  hit <- grep(file_arg, cmd, value = TRUE)

  if (length(hit) > 0) {
    script_path <- sub(file_arg, "", hit[1])
    script_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE))
  } else {
    script_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  }

  project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
  file.path(project_root, output_dir)
}

build_codebook <- function(output_dir = "documents") {
  output_dir <- resolve_project_output_dir(output_dir)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  final_schema <- tibble(
    variable = c(
      "idstudent", "idschool", "idcountry", "label_country",
      "contact", "peaceful", "sign", "spray", "trafic", "occupy",
      "gini", "parents_hisced", "parents_univ", "gender", "civic_test", "weights",
      "jkzone", "jkrep", "study"
    ),
    type = c(
      "numeric", "numeric", "numeric", "character",
      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric"
    ),
    description = c(
      "Student ID",
      "School ID",
      "Country numeric ID",
      "Country label used for SWIID matching",
      "Expected participation: contact elected representative; 1999 proxy = write letters about social/political concerns (1=certainly not ... 4=certainly do)",
      "Expected participation: peaceful protest/march (1-4, higher = more willing)",
      "Expected participation: sign/collect petition signatures (1-4, higher = more willing)",
      "Expected participation: spray protest slogans (1-4, higher = more willing)",
      "Expected participation: block traffic (1-4, higher = more willing)",
      "Expected participation: occupy buildings (1-4, higher = more willing)",
      "Gini disposable income from SWIID (0-100)",
      "Highest parental education (wave-specific scale; see mapping)",
      "At least one university-educated parent (1=yes, 0=no)",
      "Gender (harmonized: 1=female, 0=male)",
      "Civic knowledge score (1999 KNOWLMLE mean 100/SD 20; ICCS PV1CIV mean 500/SD 100)",
      "Student total weight",
      "Jackknife zone (JKZONE/JKZONES)",
      "Jackknife replicate indicator (JKREP/JKREPS)",
      "Study wave (1999, 2009, 2016, 2022)"
    )
  )

  mapping <- tribble(
    ~study, ~final_variable, ~raw_variable, ~raw_file_pattern, ~raw_label, ~coding_rule,

    "1999", "idstudent", "IDSTUD", "bs_*.sav", "Student ID", "as numeric",
    "1999", "idschool", "IDSCHOOL", "bs_*.sav", "School ID", "as numeric",
    "1999", "idcountry", "IDCNTRY/CNTRYID", "bs_*.sav", "Country ID", "as numeric",
    "1999", "label_country", "filename code + fallback", "bs_*.sav", "country from file code", "fallback map_cived_country_code",
    "1999", "contact", "BS5M4", "bs_*.sav", "Letters about social/political concerns", "keep 1..4 (1=certainly not ... 4=certainly do); 0/8 to NA",
    "1999", "peaceful", "BS5M9", "bs_*.sav", "Participate in a non-violent protest march", "keep 1..4 (1=certainly not ... 4=certainly do); 0/8 to NA",
    "1999", "sign", "BS5M8", "bs_*.sav", "Collect signatures for a petition", "keep 1..4 (1=certainly not ... 4=certainly do); 0/8 to NA",
    "1999", "spray", "BS5M10", "bs_*.sav", "Spray-paint protest slogans", "keep 1..4 (1=certainly not ... 4=certainly do); 0/8 to NA",
    "1999", "trafic", "BS5M11", "bs_*.sav", "Block traffic as protest", "keep 1..4 (1=certainly not ... 4=certainly do); 0/8 to NA",
    "1999", "occupy", "BS5M12", "bs_*.sav", "Occupy public buildings", "keep 1..4 (1=certainly not ... 4=certainly do); 0/8 to NA",
    "1999", "parents_hisced", "BSGEDUM + BSGEDUF", "bs_*.sav", "Mother/Father education", "map each to 1..5 then max(mother,father)",
    "1999", "parents_univ", "BSGEDUM + BSGEDUF", "bs_*.sav", "Mother/Father education", "1 if either parent raw 7 (bachelor), 0 if max in 1..6; 0/8 to NA",
    "1999", "gender", "BSGGEND/ITSEX", "bs_*.sav", "Girl or boy", "1=female,2=male; others NA",
    "1999", "civic_test", "KNOWLMLE/KNOCG_ML", "bs_*.sav", "Civic knowledge score", "as numeric; 998 to NA",
    "1999", "weights", "TOTWGT/WGTFAC1", "bs_*.sav", "Total/analysis weight", "as numeric",

    "2009", "idstudent", "IDSTUD", "ISG***C2.sav", "Student ID", "as numeric",
    "2009", "idschool", "IDSCHOOL", "ISG***C2.sav", "School ID", "as numeric",
    "2009", "idcountry", "IDCNTRY", "ISG***C2.sav", "Country ID", "as numeric",
    "2009", "label_country", "filename code + fallback", "ISG***C2.sav", "country from file code", "fallback map_iccs2009_country_code",
    "2009", "contact", "IS2P31C", "ISG***C2.sav", "PROTEST-CONTACT <ELECTED REPRESENTATIVE>", "reversed 5-x (raw 1=certainly do) so 4=certainly do; 7/8/9 to NA",
    "2009", "peaceful", "IS2P31D", "ISG***C2.sav", "Peaceful march", "reversed 5-x (raw 1=certainly do) so 4=certainly do; 7/8/9 to NA",
    "2009", "sign", "IS2P31E", "ISG***C2.sav", "PROTEST-COLLECT SIGNATURES", "reversed 5-x (raw 1=certainly do) so 4=certainly do; 7/8/9 to NA",
    "2009", "spray", "IS2P31G", "ISG***C2.sav", "Spray-paint slogans", "reversed 5-x (raw 1=certainly do) so 4=certainly do; 7/8/9 to NA",
    "2009", "trafic", "IS2P31H", "ISG***C2.sav", "Block traffic", "reversed 5-x (raw 1=certainly do) so 4=certainly do; 7/8/9 to NA",
    "2009", "occupy", "IS2P31I", "ISG***C2.sav", "Occupy buildings", "reversed 5-x (raw 1=certainly do) so 4=certainly do; 7/8/9 to NA",
    "2009", "parents_hisced", "HISCED", "ISG***C2.sav", "Highest parental education", "0..5 keep (ISCED); 7/8/9 to NA",
    "2009", "parents_univ", "HISCED", "ISG***C2.sav", "Highest parental education", "1 if HISCED=5 (ISCED 5A/6), 0 if 0..4",
    "2009", "gender", "SGENDER", "ISG***C2.sav", "Gender", "keep 1=female,0=male; others NA",
    "2009", "civic_test", "PV1CIV", "ISG***C2.sav", "Plausible value civic knowledge", "as numeric",
    "2009", "weights", "TOTWGTS/TOTWGT", "ISG***C2.sav", "Student total weight", "as numeric",

    "2016", "idstudent", "IDSTUD", "ISG***C3.sav", "Student ID", "as numeric",
    "2016", "idschool", "IDSCHOOL", "ISG***C3.sav", "School ID", "as numeric",
    "2016", "idcountry", "IDCNTRY", "ISG***C3.sav", "Country ID", "as numeric",
    "2016", "label_country", "filename code + fallback", "ISG***C3.sav", "country from file code", "fallback map_iccs2016_country_code",
    "2016", "contact", "IS3G30B", "ISG***C3.sav", "Contact an <elected representative>", "reversed 5-x (raw 1=certainly do) so 4=certainly do; 7/8/9 to NA",
    "2016", "peaceful", "IS3G30C", "ISG***C3.sav", "Peaceful march", "reversed 5-x (raw 1=certainly do) so 4=certainly do; 7/8/9 to NA",
    "2016", "sign", "IS3G30D", "ISG***C3.sav", "Collect signatures", "reversed 5-x (raw 1=certainly do) so 4=certainly do; 7/8/9 to NA",
    "2016", "spray", "IS3G30I", "ISG***C3.sav", "Spray-paint slogans", "reversed 5-x (raw 1=certainly do) so 4=certainly do; 7/8/9 to NA",
    "2016", "trafic", "IS3G30J", "ISG***C3.sav", "Block traffic", "reversed 5-x (raw 1=certainly do) so 4=certainly do; 7/8/9 to NA",
    "2016", "occupy", "IS3G30K", "ISG***C3.sav", "Occupy buildings", "reversed 5-x (raw 1=certainly do) so 4=certainly do; 7/8/9 to NA",
    "2016", "parents_hisced", "S_HISCED", "ISG***C3.sav", "Highest parental education", "0..4 keep (ISCED 2011); 7/8/9 to NA",
    "2016", "parents_univ", "S_HISCED", "ISG***C3.sav", "Highest parental education", "1 if S_HISCED=4 (ISCED 6-8), 0 if 0..3",
    "2016", "gender", "S_GENDER", "ISG***C3.sav", "Gender", "raw 1=girl,0=boy -> 1=female,0=male; 7/8/9 NA",
    "2016", "civic_test", "PV1CIV", "ISG***C3.sav", "Plausible value civic knowledge", "as numeric",
    "2016", "weights", "TOTWGTS/TOTWGT", "ISG***C3.sav", "Student total weight", "as numeric",

    "2022", "idstudent", "IDSTUD", "ISG***[BC]4.Rdata", "Student ID", "as numeric",
    "2022", "idschool", "IDSCHOOL", "ISG***[BC]4.Rdata", "School ID", "as numeric",
    "2022", "idcountry", "IDCNTRY", "ISG***[BC]4.Rdata", "Country ID", "as numeric",
    "2022", "label_country", "filename code + fallback", "ISG***[BC]4.Rdata", "country from file code", "C4 preferred, then B4",
    "2022", "contact", "IS4G31B", "ISG***[BC]4.Rdata", "Contact an <elected representative>", "reversed 5-x (raw 1=certainly do) so 4=certainly do; 7/8/9 to NA",
    "2022", "peaceful", "IS4G31C", "ISG***[BC]4.Rdata", "Peaceful march", "reversed 5-x (raw 1=certainly do) so 4=certainly do; 7/8/9 to NA",
    "2022", "sign", "IS4G31D", "ISG***[BC]4.Rdata", "Collect signatures for a petition", "reversed 5-x (raw 1=certainly do) so 4=certainly do; 7/8/9 to NA",
    "2022", "spray", "IS4G31K", "ISG***[BC]4.Rdata", "Spray-paint slogans", "reversed 5-x (raw 1=certainly do) so 4=certainly do; 7/8/9 to NA",
    "2022", "trafic", "IS4G31L", "ISG***[BC]4.Rdata", "Block traffic", "reversed 5-x (raw 1=certainly do) so 4=certainly do; 7/8/9 to NA",
    "2022", "occupy", "IS4G31M", "ISG***[BC]4.Rdata", "Occupy buildings", "reversed 5-x (raw 1=certainly do) so 4=certainly do; 7/8/9 to NA",
    "2022", "parents_hisced", "S_HISCED", "ISG***[BC]4.Rdata", "Highest parental education", "0..4 keep (ISCED 2011); 7/8/9 to NA",
    "2022", "parents_univ", "S_HISCED", "ISG***[BC]4.Rdata", "Highest parental education", "1 if S_HISCED=4 (ISCED 6-8), 0 if 0..3",
    "2022", "gender", "SD_GENDER", "ISG***[BC]4.Rdata", "Gender", "raw 1=girl,0=boy -> 1=female,0=male; 7/8/9 NA",
    "2022", "civic_test", "PV1CIV", "ISG***[BC]4.Rdata", "Plausible value civic knowledge", "as numeric",
    "2022", "weights", "TOTWGTS/TOTWGT", "ISG***[BC]4.Rdata", "Student total weight", "as numeric",

    "all", "gini", "gini_disp", "swiid9_92_summary.csv", "SWIID disposable-income gini", "join by normalized country label + study year"
  )

  write_csv(final_schema, file.path(output_dir, "codebook_final_schema.csv"))
  write_csv(mapping, file.path(output_dir, "codebook_mapping_table.csv"))

  writeLines(
    c(
      "# Pooled Data Codebook",
      "",
      "Generated by proc/06-codebook.R.",
      "",
      "## Files",
      "- codebook_final_schema.csv: final output schema",
      "- codebook_mapping_table.csv: study-by-study raw to harmonized mapping",
      "",
      "## Notes",
      "- CIVED 1999 cohort: Grade 8 only (bs_*.sav).",
      "- contact: CivED 1999 has no contact-representative item; BS5M4 (letters about social/political concerns) is used as the closest proxy.",
      "- Participation variables are harmonized to 1=certainly not, 2=probably not, 3=probably, 4=certainly would do.",
      "  CivED 1999 already uses this direction; ICCS 2009/2016/2022 raw items (1=certainly do) are reversed as 5 - x.",
      "- Non-substantive codes (CivED 0/8; ICCS 7/8/9) are recoded to NA.",
      "- Gender: raw ICCS 2009/2016/2022 codes are 1=girl, 0=boy; CivED 1999 1=girl, 2=boy. Harmonized 1=female, 0=male.",
      "- parents_hisced keeps the wave-specific scale (code 0 is valid in ICCS). parents_univ is the comparable binary:",
      "  1999 bachelor's degree (raw 7) for either parent; 2009 HISCED=5 (ISCED 5A/6); 2016/2022 S_HISCED=4 (ISCED 6-8).",
      "- parents_hisced in 1999 is computed as max(mother, father) after harmonization.",
      "- gini comes from SWIID via country-name normalization and study-year join."
    ),
    con = file.path(output_dir, "codebook.md")
  )

  message("Codebook files written to: ", output_dir)

  invisible(
    list(
      final_schema = final_schema,
      mapping = mapping
    )
  )
}

if (sys.nframe() == 0) {
  build_codebook("documents")
}
