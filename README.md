# ntlgini

**Global Subnational Nighttime-Light Gini Evolution (1992–2024)**

[![R-CMD-check](https://github.com/MedDhia/NTLGini/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MedDhia/NTLGini/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![GitHub release](https://img.shields.io/github/v/release/MedDhia/NTLGini)](https://github.com/MedDhia/NTLGini/releases/tag/v0.1.0)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

`ntlgini` provides harmonized annual nighttime-light Gini coefficients and **Pyatt (1976) / Dagum (1997)** within-, between-, and overlap-component decompositions across administrative levels (`ADM_1` through `ADM_4`) for **228 countries and territories** from **1992 to 2024**.

The dataset is derived from **Cheng et al. (2026)** 500 m-resolution VIIRS-like nighttime lights ([*Earth System Science Data*](https://doi.org/10.5194/essd-18-3449-2026)) intersected with **GADM 4.1** administrative boundaries (and **HDX COD-AB** `ADM_1`–`ADM_4` boundaries for Tunisia).

---

## Installation

```r
# From GitHub
# install.packages("remotes")
remotes::install_github("MedDhia/NTLGini")

# Optional: for subunit-level Parquet tables (ntl_gini_units, ntl_gini_top_units)
install.packages("arrow")

# Optional: for built-in ggplot2 visualizations
install.packages("ggplot2")
```

---

## Data Coverage

Country-level decompositions (`ntl_gini_global`) and coverage metadata (`ntl_gini_countries`) are bundled directly in the package (`< 1 MB` total, zero external dependencies). Subunit-level Parquet tables are hosted on [GitHub Releases (`v0.1.0`)](https://github.com/MedDhia/NTLGini/releases/tag/v0.1.0) and downloaded on demand into `tools::R_user_dir("ntlgini", "cache")` using `arrow` predicate pushdown:

| Object / Function | Scope | Rows | Countries | Size |
|---|---|---:|---:|---:|
| `ntl_gini_global` / `ntl_gini_decomp()` | Country × year × level (`adm1`–`adm4`) | 16,203 | 228 | 0.74 MB (bundled) |
| `ntl_gini_countries` | Country metadata & max admin level | 228 | 228 | < 0.01 MB (bundled) |
| `ntl_gini_units("adm1")` | `ADM_1` subunits × year | 120,021 | 228 | 3.49 MB (cached) |
| `ntl_gini_units("adm2")` | `ADM_2` subunits × year | 1,549,614 | 171 | 44.83 MB (cached) |
| `ntl_gini_units("adm3")` | `ADM_3` subunits × year | 4,680,753 | 70 | 102.06 MB (cached) |
| `ntl_gini_units("adm4")` | `ADM_4` subunits × year | 5,114,736 | 22 | 101.16 MB (cached) |

---

## Function Reference

| Function | Purpose |
|---|---|
| `ntl_gini_decomp()` | Filter bundled country-year Gini decompositions by `iso3`, `level`, and `years` |
| `ntl_gini_trends()` | Fit log-linear `%/yr` trends & $R^2$ over `full` (1992–2024), `dmsp` (1992–2011), or `viirs` (2012–2024) |
| `ntl_gini_units()` | Query subunit-level Parquet tables (`adm1`–`adm4`) with `arrow` pushdown filtering |
| `ntl_gini_top_units()` | Rank top or bottom `n` subunits by `within_contribution`, `gini`, `mean_radiance`, or `value_share` |
| `ntl_gini_plot_series()` | Plot `total_gini`, `between_gini`, `within_gini`, and `overlap_gini` over time |
| `ntl_gini_plot_decomp()` | Plot stacked area shares (`between_share`, `within_share`, `overlap_share`) |
| `ntl_gini_plot_levels()` | Compare `between_gini` across all available `adm1`–`adm4` levels |
| `ntl_gini_plot_top_units()` | Horizontal bar chart of top/bottom subunits from `ntl_gini_top_units()` |
| `ntl_gini_cache_info()` / `ntl_gini_clear_cache()` | Inspect or clear locally cached `.parquet` files |

---

## Quick Start

### 1. Country Coverage & Multi-Scale Decompositions

```r
library(ntlgini)

# Check which countries have ADM_4 boundaries (22 countries)
subset(ntl_gini_countries, max_level == 4)

# Compare Tunisia and Morocco at ADM_1 and ADM_2 (2010-2024)
tun_mar <- ntl_gini_decomp(
  iso3 = c("TUN", "MAR"),
  level = c("adm1", "adm2"),
  years = 2010:2024
)

# Fit VIIRS-era (2012-2024) log-linear trends in between-unit Gini
ntl_gini_trends(iso3 = c("TUN", "MAR"), level = "adm1", window = "viirs")
```

### 2. Subunit Queries & Rankings (Requires `arrow`)

```r
# Downloads and caches gini_by_unit_adm4.parquet on first call;
# filters to Tunisia in 2024 via Arrow Dataset Scanner pushdown (~0.06s)
tun_imadas <- ntl_gini_units(level = "adm4", iso3 = "TUN", years = 2024)

# Top 10 delegations contributing to national within-unit inequality in 2024
ntl_gini_top_units(level = "adm3", iso3 = "TUN", year = 2024, n = 10)
```

### 3. Built-In Visualizations (Requires `ggplot2`)

```r
# Decomposition series over 1992-2024 (marks the 2012 sensor transition)
ntl_gini_plot_series(c("TUN", "MAR"), level = "adm1")

# Stacked area chart of between / within / overlap shares
ntl_gini_plot_decomp("TUN", level = "adm2")

# Multi-scale between-unit Gini across ADM_1 to ADM_4
ntl_gini_plot_levels(c("TUN", "MAR"))

# Top 10 ADM_2 subunits by within-unit contribution in 2024
ntl_gini_plot_top_units("adm2", iso3 = "TUN", year = 2024, n = 10)
```

---

## Methodology & Data Dictionary

For each country $c$, year $t$, and administrative level $k \in \{\text{adm1}, \dots, \text{adm4}\}$, pixel-level radiance across all land pixels is decomposed additively following Pyatt (1976) / Dagum (1997):

$$G_{\text{total}} = G_{\text{within}} + G_{\text{between}} + G_{\text{overlap}}$$

* **`within_gini`**: $\sum_g p_g s_g G_g$, where $p_g$ is subunit $g$'s pixel share, $s_g$ is its light share, and $G_g$ is its internal pixel Gini.
* **`between_gini`**: Pixel-weighted Gini across subunit mean radiances $\mu_g$.
* **`overlap_gini`**: Transvariation residual measuring distributional overlap across subunits.

### Methodological Notes
* **2011 → 2012 sensor transition:** Cheng et al. (2026) reconstruct 1992–2011 from DMSP-OLS (*NightNet*) and use native NPP-VIIRS from 2012 onward. While `sum_of_lights` and urban-core radiances are continuous, dim rural background pixels (`0 < radiance < 0.5`) drop sharply in 2012. Use `ntl_gini_trends(..., window = "dmsp")` and `window = "viirs"` to verify robustness across eras.
* **Completely unlit subunits:** When every pixel in a subunit is `0`, `gini` is `NA` (inequality is undefined) and its `within_contribution` is `0`.

---

## Citation & License

* **Package code:** MIT License (`LICENSE`). Run `citation("ntlgini")` in R for full BibTeX entries.
* **Underlying NTL dataset:** Cheng, H., Geng, M., Li, X., Li, S., Zhao, M., Lin, C., Wang, J., Gong, P., & Zhou, Y. (2026). A temporally consistent global 500 m-resolution monthly VIIRS-like nighttime light dataset (1992–2024). *Earth System Science Data*, 18, 3449–3479. https://doi.org/10.5194/essd-18-3449-2026 (CC BY 4.0).
