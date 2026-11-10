# Analyses

This folder centralizes project outputs for:

- Visualizations
- Statistical analyses
- Models

## Available Reports

1. Items descriptions
- Source: [items_descriptions.qmd](items_descriptions.qmd)
- HTML: [items_descriptions.html](items_descriptions.html)
- GitHub Pages: `https://nicolastobarj.github.io/gini-iccs/analyses/items_descriptions.html`

2. Confirmatory models (preregistered)
- Estimation: [01-confirmatory-models.R](01-confirmatory-models.R) → `output/confirmatory_models.rds`
- Reconciliation with INVALSI 2022: [02-reconciliation-invalsi.R](02-reconciliation-invalsi.R) → `output/reconciliation.rds`
- Report: [01-confirmatory-models.qmd](01-confirmatory-models.qmd) / [01-confirmatory-models.html](01-confirmatory-models.html)
- Figures: [figures/](figures/)
- GitHub Pages: `https://nicolastobarj.github.io/gini-iccs/analyses/01-confirmatory-models.html`

Run from the project root:

```
Rscript analyses/01-confirmatory-models.R
Rscript analyses/02-reconciliation-invalsi.R
quarto render analyses/01-confirmatory-models.qmd
```
