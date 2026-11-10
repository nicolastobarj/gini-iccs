# Confirmatory models (preregistered primary specification)
# ---------------------------------------------------------------------------
# For each outcome (peaceful, trafic):
#   Y_ict = b0 + b1*Gini_ct + b2*female + b3*civic_z + b4*parents_univ
#           + wave FE + u_country + e
# Primary: WeMix weighted two-level model (level-1 = student total weight
#          rescaled within country-wave to the analytic sample size;
#          level-2 = 1), cluster-robust SEs at country level.
# Comparison: unweighted lme4::lmer (REML), same formula.
#
# Run from project root:  Rscript analyses/01-confirmatory-models.R
# Output: analyses/output/confirmatory_models.rds
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(lme4)
  library(WeMix)
})

dir.create("analyses/output", showWarnings = FALSE, recursive = TRUE)

db <- readRDS("proc/output/pooled_data.rds")

# ---- Analysis variables ----------------------------------------------------
dat <- db |>
  mutate(
    study = as.integer(study),
    country = country_key, # national unit (sub-national samples -> nation)
    female = gender,
    civic_z = (civic_test - mean(civic_test, na.rm = TRUE)) / sd(civic_test, na.rm = TRUE),
    .by = study
  ) |>
  mutate(
    wave2009 = as.numeric(study == 2009),
    wave2016 = as.numeric(study == 2016),
    wave2022 = as.numeric(study == 2022)
  )

outcomes <- c("peaceful", "trafic")
covars <- c("gini", "female", "civic_z", "parents_univ")

prep_sample <- function(y) {
  dat |>
    filter(if_all(all_of(c(y, covars, "weights", "country")), ~ !is.na(.x))) |>
    filter(weights > 0) |>
    mutate(
      w_stud = weights * n() / sum(weights),
      .by = c(country, study)
    ) |>
    mutate(w_cntry = 1, y = .data[[y]]) |>
    arrange(country)
}

rhs <- "gini + female + civic_z + parents_univ + wave2009 + wave2016 + wave2022"

results <- list()

for (y in outcomes) {
  message("== Outcome: ", y)
  s <- prep_sample(y)

  sample_info <- tibble(
    outcome = y,
    n_students = nrow(s),
    n_countries = n_distinct(s$country),
    n_country_waves = n_distinct(paste(s$country, s$study))
  )
  print(sample_info)

  # Comparison: unweighted lmer
  t0 <- Sys.time()
  m_lmer <- lmer(as.formula(paste("y ~", rhs, "+ (1 | country)")), data = s, REML = TRUE)
  m_null <- lmer(y ~ 1 + (1 | country), data = s, REML = TRUE)
  message("lmer done in ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

  fe <- summary(m_lmer)$coefficients
  tab_lmer <- tibble(
    term = rownames(fe),
    estimate = fe[, "Estimate"],
    se = fe[, "Std. Error"]
  )
  vc_lmer <- as.data.frame(VarCorr(m_lmer))
  vc_null <- as.data.frame(VarCorr(m_null))

  # Primary: WeMix weighted
  t0 <- Sys.time()
  m_wemix <- mix(
    as.formula(paste("y ~", rhs, "+ (1 | country)")),
    data = as.data.frame(s),
    weights = c("w_stud", "w_cntry")
  )
  message("WeMix done in ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

  sw <- summary(m_wemix)
  co <- sw$coef
  tab_wemix <- tibble(
    term = rownames(co),
    estimate = co[, "Estimate"],
    se = co[, "Std. Error"]
  )

  # ---- Prespecified robustness F: add country-wave random intercept --------
  # Gini varies at the country-wave level. With only a country intercept and
  # ~3,000+ students per country, the Gini slope is identified almost entirely
  # by within-country change over time (see analyses/02-reconciliation-invalsi.R).
  s$cw <- paste(s$country, s$study, sep = "_")
  m_lmer_cw <- lmer(as.formula(paste("y ~", rhs, "+ (1 | country) + (1 | cw)")), data = s, REML = TRUE)
  fe_cw <- summary(m_lmer_cw)$coefficients
  tab_lmer_cw <- tibble(term = rownames(fe_cw), estimate = fe_cw[, 1], se = fe_cw[, 2])

  t0 <- Sys.time()
  s$w_cw <- 1
  tab_wemix_cw <- tryCatch({
    m3 <- mix(
      as.formula(paste("y ~", rhs, "+ (1 | country) + (1 | cw)")),
      data = as.data.frame(s |> arrange(country, cw)),
      weights = c("w_stud", "w_cw", "w_cntry")
    )
    co3 <- summary(m3)$coef
    tibble(term = rownames(co3), estimate = co3[, "Estimate"], se = co3[, "Std. Error"])
  }, error = function(e) {
    message("WeMix 3-level failed: ", conditionMessage(e))
    NULL
  })
  message("WeMix 3-level step done in ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

  # ---- Wave-by-wave models (descriptive, comparable to INVALSI tables) ----
  by_wave <- bind_rows(lapply(sort(unique(s$study)), function(w) {
    sw_ <- s |> filter(study == w)
    m <- lmer(y ~ gini + female + civic_z + parents_univ + (1 | country), data = sw_, REML = TRUE)
    co_ <- summary(m)$coefficients
    tibble(study = w, term = rownames(co_), estimate = co_[, 1], se = co_[, 2],
           n_countries = n_distinct(sw_$country))
  }))

  results[[y]] <- list(
    lmer_cw = tab_lmer_cw,
    wemix_cw = tab_wemix_cw,
    by_wave = by_wave,
    sample = sample_info,
    lmer = tab_lmer,
    wemix = tab_wemix,
    wemix_vars = sw$vars,
    lmer_varcorr = vc_lmer,
    null_varcorr = vc_null,
    lmer_n = nobs(m_lmer),
    gini_cw = s |> distinct(country, study, gini),
    cw_means = s |>
      summarise(
        y_mean = weighted.mean(y, w_stud),
        n = n(),
        gini = first(gini),
        .by = c(country, study, label_country)
      ),
    covar_means = s |> summarise(across(c(female, civic_z, parents_univ), mean))
  )
  print(tab_wemix)
}

saveRDS(results, "analyses/output/confirmatory_models.rds")
message("Saved analyses/output/confirmatory_models.rds")
