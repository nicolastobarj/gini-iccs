# Reconciliation with the INVALSI 2022 (Stata) analyses
# ---------------------------------------------------------------------------
# Moves step by step from the INVALSI pooled specification
#   radical ~ i.year + gini + University + pct_know + S_GENDER
#             || country_year: || idschool:      (unweighted, 1999-2016)
# to the preregistered specification, to locate which choice changes the
# Gini coefficient. Output: analyses/output/reconciliation.rds
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(lme4)
})

db <- readRDS("proc/output/pooled_data.rds")

dat <- db |>
  mutate(
    study = as.integer(study),
    country = country_key,
    cw = paste(country_key, study, sep = "_"),
    school = paste(cw, idschool, sep = "_"),
    female = gender,
    radical = rowMeans(cbind(spray, trafic, occupy), na.rm = TRUE),
    pacific = rowMeans(cbind(contact, sign, peaceful), na.rm = TRUE)
  ) |>
  mutate(radical = ifelse(is.nan(radical), NA, radical),
         pacific = ifelse(is.nan(pacific), NA, pacific)) |>
  mutate(civic_z = as.numeric(scale(civic_test)), .by = study) |>
  mutate(
    pct_know = ceiling(100 * rank(civic_test, na.last = "keep", ties.method = "average") /
                         sum(!is.na(civic_test))),
    .by = cw
  )

fit <- function(y, rhs, re, waves, label) {
  s <- dat |>
    filter(study %in% waves) |>
    filter(if_all(all_of(c(y, "gini", "female", "parents_univ", "civic_z", "pct_know")), ~ !is.na(.x)))
  f <- as.formula(paste(y, "~", rhs, "+ factor(study) +", re))
  m <- lmer(f, data = s, REML = FALSE,
            control = lmerControl(calc.derivs = FALSE))
  co <- summary(m)$coefficients
  tibble(
    outcome = y, model = label, waves = paste(range(waves), collapse = "-"),
    term = rownames(co), estimate = co[, 1], se = co[, 2], n = nobs(m)
  )
}

w3 <- c(1999, 2009, 2016)
w4 <- c(1999, 2009, 2016, 2022)
base <- "gini + parents_univ + pct_know + female"
pre  <- "gini + female + civic_z + parents_univ"

specs <- list(
  list("A. INVALSI replica: country-year + school RE, pct_know", base, "(1|cw) + (1|school)", w3),
  list("B. A with 2022 added", base, "(1|cw) + (1|school)", w4),
  list("C. B without school level", base, "(1|cw)", w4),
  list("D. C with civic_z (within wave) instead of pct_know", pre, "(1|cw)", w4),
  list("E. Country RE + country-year RE, civic_z", pre, "(1|country) + (1|cw)", w4),
  list("F. Preregistered: country RE only, civic_z", pre, "(1|country)", w4),
  list("G. Country RE only, pct_know", base, "(1|country)", w4)
)

res <- list()
for (y in c("radical", "trafic", "peaceful", "pacific")) {
  for (sp in specs) {
    message(y, " | ", sp[[1]])
    res[[length(res) + 1]] <- fit(y, sp[[2]], sp[[3]], sp[[4]], sp[[1]])
  }
}
res <- bind_rows(res)

# Between-country vs within-country Gini variation (explains A-C vs F)
cwg <- dat |> distinct(country, study, gini) |> filter(!is.na(gini)) |>
  mutate(gini_mean = mean(gini), gini_dev = gini - gini_mean, .by = country)
var_decomp <- tibble(
  sd_between = sd(cwg$gini_mean), sd_within = sd(cwg$gini_dev)
)

saveRDS(list(coefs = res, var_decomp = var_decomp), "analyses/output/reconciliation.rds")
print(res |> filter(term == "gini") |> select(outcome, model, estimate, se), n = 50)
print(var_decomp)
