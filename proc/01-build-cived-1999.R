library(dplyr)
library(haven)

build_cived_1999 <- function(
  data_dir = "raw_data/CivED1999_IDB_SPSS_G8/Data"
) {
  files <- list.files(data_dir, pattern = "^bs_.*\\.sav$", full.names = TRUE)
  if (length(files) == 0) {
    stop("No CIVED 1999 Grade 8 student files found in: ", data_dir)
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

  map_1999_hisced <- function(x) {
    # CIVED codes 1..7 are collapsed to a 1..5 harmonized scale.
    case_when(
      x %in% c(0, 8, 9, 98, 99) ~ NA_real_,
      x == 1 ~ 1,
      x %in% c(2, 3) ~ 2,
      x == 4 ~ 3,
      x %in% c(5, 6) ~ 4,
      x == 7 ~ 5,
      TRUE ~ NA_real_
    )
  }

  map_cived_country_code <- function(code) {
    dplyr::recode(
      tolower(code),
      "aus" = "Australia",
      "bfr" = "Belgium",
      "bgr" = "Bulgaria",
      "che" = "Switzerland",
      "chl" = "Chile",
      "col" = "Colombia",
      "cyp" = "Cyprus",
      "cze" = "Czech Republic",
      "deu" = "Germany",
      "dnk" = "Denmark",
      "eng" = "United Kingdom",
      "est" = "Estonia",
      "fin" = "Finland",
      "grc" = "Greece",
      "hkg" = "Hong Kong",
      "hun" = "Hungary",
      "ita" = "Italy",
      "ltu" = "Lithuania",
      "lva" = "Latvia",
      "nor" = "Norway",
      "pol" = "Poland",
      "prt" = "Portugal",
      "rom" = "Romania",
      "rus" = "Russia",
      "svk" = "Slovakia",
      "svn" = "Slovenia",
      "swe" = "Sweden",
      "usa" = "United States",
      .default = NA_character_
    )
  }

  frames <- lapply(files, function(path) {
    df <- read_sav(path)
    file_code <- substr(basename(path), 4, 6)

    idcountry_raw <- pick_var(df, c("IDCNTRY", "CNTRYID"))
    idschool <- as.numeric(pick_var(df, c("IDSCHOOL")))
    idstudent <- as.numeric(pick_var(df, c("IDSTUD", "IDSTUDENT")))

    label_country <- suppressWarnings(as.character(haven::as_factor(idcountry_raw, levels = "labels")))
    label_country <- ifelse(grepl("^[0-9.]+$", label_country), NA_character_, label_country)
    label_country <- dplyr::coalesce(label_country, map_cived_country_code(file_code))

    mother <- as.numeric(pick_var(df, c("BSGEDUM"), required = FALSE))
    father <- as.numeric(pick_var(df, c("BSGEDUF"), required = FALSE))
    mother_h <- map_1999_hisced(mother)
    father_h <- map_1999_hisced(father)
    parents_hisced <- pmax(mother_h, father_h, na.rm = TRUE)
    parents_hisced[is.infinite(parents_hisced)] <- NA_real_
    # University: either parent completed a bachelor's degree (raw code 7); raw 1-6 = no.
    mother_v <- ifelse(mother %in% 1:7, mother, NA_real_)
    father_v <- ifelse(father %in% 1:7, father, NA_real_)
    parents_max <- suppressWarnings(pmax(mother_v, father_v, na.rm = TRUE))
    parents_max[is.infinite(parents_max)] <- NA_real_
    parents_univ <- ifelse(is.na(parents_max), NA_real_, as.numeric(parents_max == 7))

    civic_raw <- as.numeric(pick_var(df, c("KNOWLMLE", "KNOCG_ML"), required = FALSE))
    civic_raw[civic_raw >= 998] <- NA_real_ # 998 = not administered

    gender_raw <- as.numeric(pick_var(df, c("BSGGEND", "ITSEX"), required = FALSE))
    gender <- case_when(
      gender_raw == 1 ~ 1, # female
      gender_raw == 2 ~ 0, # male
      gender_raw %in% c(8, 9, 98, 99) ~ NA_real_,
      TRUE ~ NA_real_
    )

    tibble(
      idstudent = idstudent,
      idschool = idschool,
      idcountry = as.numeric(idcountry_raw),
      label_country = label_country,
      contact = as.numeric(pick_var(df, c("BS5M4"))), # letters about social/political concerns (no contact-representative item in CivED)
      peaceful = as.numeric(pick_var(df, c("BS5M9"))),
      sign = as.numeric(pick_var(df, c("BS5M8"))),
      spray = as.numeric(pick_var(df, c("BS5M10"))),
      trafic = as.numeric(pick_var(df, c("BS5M11"))),
      occupy = as.numeric(pick_var(df, c("BS5M12"))),
      parents_hisced = parents_hisced,
      parents_univ = parents_univ,
      gender = gender,
      civic_test = civic_raw,
      weights = as.numeric(pick_var(df, c("TOTWGT", "WGTFAC1"), required = FALSE)),
      jkzone = as.numeric(pick_var(df, c("JKZONE", "JKZONES"), required = FALSE)),
      jkrep = as.numeric(pick_var(df, c("JKREP", "JKREPS"), required = FALSE)),
      study = 1999
    )
  })

  bind_rows(frames)
}
