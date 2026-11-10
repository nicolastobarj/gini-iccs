library(dplyr)
library(haven)

build_iccs_2016 <- function(
  data_dir = "raw_data/ICCS2016_IDB_SPSS/Data"
) {
  files <- list.files(data_dir, pattern = "^ISG[A-Z]{3}C3\\.sav$", full.names = TRUE)
  if (length(files) == 0) {
    stop("No ICCS 2016 ISG student files found in: ", data_dir)
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

  map_iccs2016_country_code <- function(code) {
    dplyr::recode(
      toupper(code),
      "BFL" = "Belgium",
      "BGR" = "Bulgaria",
      "CHL" = "Chile",
      "COL" = "Colombia",
      "DNK" = "Denmark",
      "DNW" = "Germany",
      "DOM" = "Dominican Republic",
      "EST" = "Estonia",
      "FIN" = "Finland",
      "HKG" = "Hong Kong",
      "HRV" = "Croatia",
      "ITA" = "Italy",
      "KOR" = "Korea",
      "LTU" = "Lithuania",
      "LVA" = "Latvia",
      "MEX" = "Mexico",
      "MLT" = "Malta",
      "NLD" = "Netherlands",
      "NOR" = "Norway",
      "PER" = "Peru",
      "RUS" = "Russia",
      "SVN" = "Slovenia",
      "SWE" = "Sweden",
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
    label_country <- dplyr::coalesce(label_country, map_iccs2016_country_code(file_code))

    gender_raw <- as.numeric(pick_var(df, c("S_GENDER", "s_gender"), required = FALSE))
    gender <- case_when(
      # S_GENDER value labels (verified in all 2016 country files): 1 = Girl, 0 = Boy
      gender_raw == 1 ~ 1, # female
      gender_raw == 0 ~ 0, # male
      TRUE ~ NA_real_
    )

    hisced_raw <- as.numeric(pick_var(df, c("S_HISCED", "s_hisced"), required = FALSE))
    parents_hisced <- case_when(
      hisced_raw %in% 0:4 ~ hisced_raw, # 0 = did not complete ISCED 2 (valid)
      TRUE ~ NA_real_ # 7 invalid, 8 not administered, 9 omitted
    )
    parents_univ <- ifelse(is.na(parents_hisced), NA_real_, as.numeric(parents_hisced == 4)) # ISCED 6-8

    tibble(
      idstudent = as.numeric(pick_var(df, c("IDSTUD", "idstud"), required = FALSE)),
      idschool = as.numeric(pick_var(df, c("IDSCHOOL", "idschool"))),
      idcountry = as.numeric(idcountry_raw),
      label_country = label_country,
      contact = as.numeric(pick_var(df, c("IS3G30B", "is3g30b"))), # Contact an <elected representative>
      peaceful = as.numeric(pick_var(df, c("IS3G30C", "is3g30c"))),
      sign = as.numeric(pick_var(df, c("IS3G30D", "is3g30d"))),
      spray = as.numeric(pick_var(df, c("IS3G30I", "is3g30i"))),
      trafic = as.numeric(pick_var(df, c("IS3G30J", "is3g30j"))),
      occupy = as.numeric(pick_var(df, c("IS3G30K", "is3g30k"))),
      parents_hisced = parents_hisced,
      parents_univ = parents_univ,
      gender = gender,
      civic_test = as.numeric(pick_var(df, c("PV1CIV", "pv1civ"), required = FALSE)),
      weights = as.numeric(pick_var(df, c("TOTWGTS", "totwgts", "TOTWGT", "totwgt"), required = FALSE)),
      jkzone = as.numeric(pick_var(df, c("JKZONES", "jkzones"), required = FALSE)),
      jkrep = as.numeric(pick_var(df, c("JKREPS", "jkreps"), required = FALSE)),
      study = 2016
    )
  })

  bind_rows(frames)
}
