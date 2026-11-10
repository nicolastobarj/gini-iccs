library(dplyr)
library(haven)

build_iccs_2009 <- function(
  data_dir = "raw_data/ICCS2009_IDB_SPSS/Data_G8"
) {
  files <- list.files(data_dir, pattern = "^ISG[A-Z]{3}C2\\.sav$", full.names = TRUE)
  if (length(files) == 0) {
    stop("No ICCS 2009 ISG student files found in: ", data_dir)
  }

  pick_var <- function(df, candidates, required = TRUE) {
    nm <- names(df)
    idx <- match(tolower(candidates), tolower(nm))
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0) {
      if (required) {
        stop("Missing required variable. Tried: ", paste(candidates, collapse = ", "))
      }
      return(rep(NA_real_, nrow(df)))
    }
    df[[nm[idx[1]]]]
  }

  map_iccs2009_country_code <- function(code) {
    dplyr::recode(
      toupper(code),
      "AUT" = "Austria",
      "BFL" = "Belgium",
      "BGR" = "Bulgaria",
      "CHE" = "Switzerland",
      "CHL" = "Chile",
      "COL" = "Colombia",
      "CYP" = "Cyprus",
      "CZE" = "Czech Republic",
      "DNK" = "Denmark",
      "DOM" = "Dominican Republic",
      "ENG" = "United Kingdom",
      "ESP" = "Spain",
      "EST" = "Estonia",
      "FIN" = "Finland",
      "GRC" = "Greece",
      "GTM" = "Guatemala",
      "HKG" = "Hong Kong",
      "IDN" = "Indonesia",
      "IRL" = "Ireland",
      "ITA" = "Italy",
      "KOR" = "Korea",
      "LIE" = "Liechtenstein",
      "LTU" = "Lithuania",
      "LUX" = "Luxembourg",
      "LVA" = "Latvia",
      "MEX" = "Mexico",
      "MLT" = "Malta",
      "NLD" = "Netherlands",
      "NOR" = "Norway",
      "NZL" = "New Zealand",
      "POL" = "Poland",
      "PRY" = "Paraguay",
      "RUS" = "Russia",
      "SVK" = "Slovakia",
      "SVN" = "Slovenia",
      "SWE" = "Sweden",
      "THA" = "Thailand",
      "TWN" = "Taiwan",
      .default = NA_character_
    )
  }

  frames <- lapply(files, function(path) {
    df <- read_sav(path)
    file_code <- substr(basename(path), 4, 6)

    idcountry_raw <- pick_var(df, c("IDCNTRY", "idcntry"))
    label_country <- suppressWarnings(as.character(haven::as_factor(idcountry_raw, levels = "labels")))
    label_country <- ifelse(grepl("^[0-9.]+$", label_country), NA_character_, label_country)
    label_country <- dplyr::coalesce(label_country, map_iccs2009_country_code(file_code))

    gender_raw <- as.numeric(pick_var(df, c("SGENDER", "sgender"), required = FALSE))
    gender <- case_when(
      gender_raw == 1 ~ 1,
      gender_raw == 0 ~ 0,
      TRUE ~ NA_real_ # 7 invalid, 8 not administered, 9 omitted
    )

    hisced_raw <- as.numeric(pick_var(df, c("HISCED", "hisced"), required = FALSE))
    parents_hisced <- case_when(
      hisced_raw %in% 0:5 ~ hisced_raw, # 0 = did not complete ISCED 1 (valid)
      TRUE ~ NA_real_ # 7 invalid, 8 not administered, 9 omitted
    )
    parents_univ <- ifelse(is.na(parents_hisced), NA_real_, as.numeric(parents_hisced == 5)) # ISCED 5A/6

    tibble(
      idstudent = as.numeric(pick_var(df, c("IDSTUD", "idstud"), required = FALSE)),
      idschool = as.numeric(pick_var(df, c("IDSCHOOL", "idschool"))),
      idcountry = as.numeric(idcountry_raw),
      label_country = label_country,
      contact = as.numeric(pick_var(df, c("IS2P31C", "is2p31c"))), # PROTEST-CONTACT <ELECTED REPRESENTATIVE>
      peaceful = as.numeric(pick_var(df, c("IS2P31D", "is2p31d"))),
      sign = as.numeric(pick_var(df, c("IS2P31E", "is2p31e"))), # PROTEST-COLLECT SIGNATURES
      spray = as.numeric(pick_var(df, c("IS2P31G", "is2p31g"))),
      trafic = as.numeric(pick_var(df, c("IS2P31H", "is2p31h"))),
      occupy = as.numeric(pick_var(df, c("IS2P31I", "is2p31i"))),
      parents_hisced = parents_hisced,
      parents_univ = parents_univ,
      gender = gender,
      civic_test = as.numeric(pick_var(df, c("PV1CIV", "pv1civ"), required = FALSE)),
      weights = as.numeric(pick_var(df, c("TOTWGTS", "totwgts", "TOTWGT", "totwgt"), required = FALSE)),
      jkzone = as.numeric(pick_var(df, c("JKZONES", "jkzones"), required = FALSE)),
      jkrep = as.numeric(pick_var(df, c("JKREPS", "jkreps"), required = FALSE)),
      study = 2009
    )
  })

  bind_rows(frames)
}
