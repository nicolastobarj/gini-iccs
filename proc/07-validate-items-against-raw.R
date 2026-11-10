# Independent validation of participation items against raw IEA files
# ---------------------------------------------------------------------------
# For every wave x country file x item, the share of students answering
# "would probably / certainly do this" is computed from the raw VALUE-LABEL
# TEXT (independent of numeric codes) and compared with the share of 3/4 in
# proc/output/pooled_data.rds. It also checks that each raw variable's label
# matches the intended item. Any mismatch indicates a mapping or direction
# error. Output: documents/item_validation.csv
# Run from project root: Rscript proc/07-validate-items-against-raw.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
})

items <- tibble::tribble(
  ~item, ~y1999, ~y2009, ~y2016, ~y2022, ~pattern,
  "contact", "BS5M4", "IS2P31C", "IS3G30B", "IS4G31B", "letters|contact",
  "sign", "BS5M8", "IS2P31E", "IS3G30D", "IS4G31D", "signature",
  "peaceful", "BS5M9", "IS2P31D", "IS3G30C", "IS4G31C", "march",
  "spray", "BS5M10", "IS2P31G", "IS3G30I", "IS4G31K", "spray",
  "trafic", "BS5M11", "IS2P31H", "IS3G30J", "IS4G31L", "traffic",
  "occupy", "BS5M12", "IS2P31I", "IS3G30K", "IS4G31M", "occupy"
)

read_rdata <- function(f) { e <- new.env(); load(f, envir = e); get(ls(e)[1], e) }

waves <- list(
  `1999` = list(files = list.files("raw_data/CivED1999_IDB_SPSS_G8/Data", "^bs_.*[.]sav$", full.names = TRUE), read = read_sav, col = "y1999"),
  `2009` = list(files = list.files("raw_data/ICCS2009_IDB_SPSS/Data_G8", "^ISG...C2[.]sav$", full.names = TRUE), read = read_sav, col = "y2009"),
  `2016` = list(files = list.files("raw_data/ICCS2016_IDB_SPSS/Data", "^ISG...C3[.]sav$", full.names = TRUE), read = read_sav, col = "y2016"),
  `2022` = list(files = list.files("raw_data/ICCS2022_IDB_R/Data", "^ISG...[BC]4[.]Rdata$", full.names = TRUE), read = read_rdata, col = "y2022")
)

# 2022: the pipeline keeps one form per country (C4 preferred over B4)
w22 <- waves$`2022`$files
code22 <- substr(basename(w22), 4, 6)
form22 <- substr(basename(w22), 7, 8)
keep22 <- tapply(seq_along(w22), code22, function(i) i[order(form22[i] != "C4")][1])
waves$`2022`$files <- w22[unlist(keep22)]

raw_rows <- list()
for (w in names(waves)) {
  for (f in waves[[w]]$files) {
    df <- waves[[w]]$read(f)
    names(df) <- toupper(names(df))
    idc <- as.numeric(df$IDCNTRY)[1]
    for (k in seq_len(nrow(items))) {
      v <- items[[waves[[w]]$col]][k]
      x <- df[[v]]
      lb <- attr(x, "labels")
      txt <- tolower(names(lb))
      do_codes <- lb[grepl("do this", txt) & !grepl("not", txt)]
      sub_codes <- lb[grepl("do this", txt)]
      xv <- as.numeric(x)
      raw_rows[[length(raw_rows) + 1]] <- tibble(
        study = as.integer(w), file = basename(f), idcountry = idc, item = items$item[k],
        raw_var = v, raw_label = attr(x, "label"),
        label_ok = grepl(items$pattern[k], tolower(attr(x, "label"))),
        raw_pct_do = mean(xv %in% do_codes[] & !is.na(xv)) / mean(xv %in% sub_codes)
      )
    }
  }
}
raw_tab <- bind_rows(raw_rows)

pooled <- readRDS("proc/output/pooled_data.rds")
pooled_tab <- pooled |>
  select(study, idcountry, all_of(items$item)) |>
  tidyr::pivot_longer(all_of(items$item), names_to = "item", values_to = "value") |>
  filter(!is.na(value)) |>
  summarise(pooled_pct_do = mean(value %in% c(3, 4)), .by = c(study, idcountry, item))

check <- raw_tab |>
  left_join(pooled_tab, by = c("study", "idcountry", "item")) |>
  mutate(diff = round(pooled_pct_do - raw_pct_do, 4),
         ok = label_ok & !is.na(diff) & abs(diff) < 0.001)

dir.create("documents", showWarnings = FALSE)
readr::write_csv(check, "documents/item_validation.csv")

cat("Checks:", nrow(check), " | label mismatches:", sum(!check$label_ok),
    " | value mismatches:", sum(!check$ok & check$label_ok), "\n")
if (any(!check$ok)) print(check |> filter(!ok), n = 50, width = 200)
