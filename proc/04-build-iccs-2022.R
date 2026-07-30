library(dplyr)

select_iccs2022_isg_files <- function(data_dir = "raw_data/ICCS2022_IDB_R/Data") {
  all_files <- list.files(data_dir, pattern = "^ISG[A-Z]{3}[BC]4\\.Rdata$", full.names = TRUE)
  if (length(all_files) == 0) {
    stop("No ICCS 2022 ISG files found in: ", data_dir)
  }

  meta <- tibble::tibble(
    path = all_files,
    file = basename(all_files),
    country_code = substr(file, 4, 6),
    form = substr(file, 7, 8),
    form_priority = dplyr::case_when(
      form == "C4" ~ 1,
      form == "B4" ~ 2,
      TRUE ~ 9
    )
  )

  meta |>
    arrange(country_code, form_priority) |>
    group_by(country_code) |>
    slice(1) |>
    ungroup() |>
    pull(path)
}

load_rdata_object <- function(path) {
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  obj_names <- ls(env)
  if (length(obj_names) == 0) {
    stop("No object found in file: ", path)
  }
  obj <- env[[obj_names[[1]]]]
  if (!inherits(obj, "data.frame")) {
    stop("Object in file is not a data.frame: ", path)
  }
  tibble::as_tibble(obj)
}

map_iccs2022_country_code <- function(code) {
  dplyr::recode(
    code,
    "BGR" = "Bulgaria",
    "BRA" = "Brazil",
    "COL" = "Colombia",
    "CYP" = "Cyprus",
    "DNK" = "Denmark",
    "DNW" = "Germany",
    "DSH" = "Germany",
    "ESP" = "Spain",
    "EST" = "Estonia",
    "FRA" = "France",
    "HRV" = "Croatia",
    "ITA" = "Italy",
    "LTU" = "Lithuania",
    "LVA" = "Latvia",
    "MLT" = "Malta",
    "NLD" = "Netherlands",
    "NOR" = "Norway",
    "POL" = "Poland",
    "ROU" = "Romania",
    "SRB" = "Serbia",
    "SVK" = "Slovakia",
    "SVN" = "Slovenia",
    "SWE" = "Sweden",
    "TWN" = "Taiwan",
    .default = NA_character_
  )
}

build_iccs_2022 <- function(
  data_dir = "raw_data/ICCS2022_IDB_R/Data"
) {
  files <- select_iccs2022_isg_files(data_dir)

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

  frames <- lapply(files, function(path) {
    df <- load_rdata_object(path)
    file <- basename(path)
    country_code <- substr(file, 4, 6)

    idcountry_raw <- pick_var(df, c("IDCNTRY", "idcntry"))
    label_country <- suppressWarnings(as.character(haven::as_factor(idcountry_raw, levels = "labels")))
    label_country <- ifelse(grepl("^[0-9.]+$", label_country), NA_character_, label_country)
    label_country <- dplyr::coalesce(label_country, map_iccs2022_country_code(country_code))

    gender_raw <- as.numeric(pick_var(df, c("SD_GENDER", "sd_gender"), required = FALSE))
    gender <- case_when(
      gender_raw == 0 ~ 1, # female
      gender_raw == 1 ~ 0, # male
      gender_raw %in% c(8, 9, 98, 99) ~ NA_real_,
      TRUE ~ NA_real_
    )

    hisced_raw <- as.numeric(pick_var(df, c("S_HISCED", "s_hisced"), required = FALSE))
    parents_hisced <- case_when(
      hisced_raw %in% c(0, 8, 9, 98, 99) ~ NA_real_,
      hisced_raw %in% 1:5 ~ hisced_raw,
      TRUE ~ NA_real_
    )

    tibble(
      idstudent = as.numeric(pick_var(df, c("IDSTUD", "idstud"), required = FALSE)),
      idschool = as.numeric(pick_var(df, c("IDSCHOOL", "idschool"))),
      idcountry = as.numeric(idcountry_raw),
      label_country = label_country,
      contact = as.numeric(pick_var(df, c("IS4G30C", "is4g30c"))),
      peaceful = as.numeric(pick_var(df, c("IS4G31C", "is4g31c"))),
      sign = as.numeric(pick_var(df, c("IS4G30D", "is4g30d"))),
      spray = as.numeric(pick_var(df, c("IS4G31K", "is4g31k"))),
      trafic = as.numeric(pick_var(df, c("IS4G31L", "is4g31l"))),
      occupy = as.numeric(pick_var(df, c("IS4G31M", "is4g31m"))),
      parents_hisced = parents_hisced,
      gender = gender,
      civic_test = as.numeric(pick_var(df, c("PV1CIV", "pv1civ"), required = FALSE)),
      weights = as.numeric(pick_var(df, c("TOTWGTS", "totwgts", "TOTWGT", "totwgt"), required = FALSE)),
      study = 2022
    )
  })

  bind_rows(frames)
}
