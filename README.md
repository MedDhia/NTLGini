# ntlgini

**Global Subnational Nighttime-Light Gini Evolution (1992–2024)**

`ntlgini` provides harmonized annual nighttime-light Gini coefficients and **Pyatt (1976) / Dagum (1997)** within-, between-, and overlap-component decompositions across administrative levels (`ADM_1` through `ADM_4`) for **228 countries and territories** from **1992 to 2024**.

The dataset is derived from **Cheng et al. (2026)** 500 m-resolution VIIRS-like nighttime lights ([ESSD](https://doi.org/10.5194/essd-18-3449-2026)) intersected with **GADM 4.1** administrative boundaries (and **MapsTN** `ADM_1`–`ADM_4` boundaries for Tunisia).

---

## Installation

```r
# From GitHub
# install.packages("remotes")
remotes::install_github("MedDhia/NTLGini")

# Optional: required only for subunit-level Parquet tables (ntl_gini_units)
install.packages("arrow")
```

---

## Included & On-Demand Data

Country-level decompositions (`ntl_gini_global`) are bundled directly in the package (`739 KB`). Larger subunit-level Parquet tables are hosted on GitHub Releases (`v0.1.0`) and automatically downloaded and cached in `tools::R_user_dir("ntlgini", "cache")` on first use:

| Table / Function | Scope | Rows | Countries | Size |
|---|---|---:|---:|---:|
| `ntl_gini_global` / `ntl_gini_decomp()` | Country × year × level (`adm1`–`adm4`) | 16,203 | 228 | 0.74 MB (bundled) |
| `ntl_gini_units("adm1")` | `ADM_1` subunits × year | 120,021 | 228 | 3.49 MB (cached) |
| `ntl_gini_units("adm2")` | `ADM_2` subunits × year | 1,549,614 | 171 | 44.83 MB (cached) |
| `ntl_gini_units("adm3")` | `ADM_3` subunits × year | 4,680,753 | 70 | 102.06 MB (cached) |
| `ntl_gini_units("adm4")` | `ADM_4` subunits × year | 5,114,736 | 22 | 101.16 MB (cached) |

---

## Usage

### 1. Country-Level Gini Decomposition (No Extra Dependencies)

```r
library(ntlgini)

# Filter bundled decompositions for Tunisia and Morocco (2010-2024)
tun_mar <- ntl_gini_decomp(
  iso3 = c("TUN", "MAR"),
  level = c("adm1", "adm2"),
  years = 2010:2024
)

head(tun_mar)
```

### 2. Subunit-Level Gini Series (Requires `arrow`)

```r
# Downloads and caches gini_by_unit_adm4.parquet on first call
tun_imadas <- ntl_gini_units(
  level = "adm4",
  iso3 = "TUN",
  years = 2010:2024
)
```

---

## Methodology

For each country $c$, year $t$, and administrative level $k \in \{\text{adm1}, \dots, \text{adm4}\}$, pixel-level radiance across all land pixels is decomposed additively following Pyatt (1976) / Dagum (1997):

$$G_{\text{total}} = G_{\text{within}} + G_{\text{between}} + G_{\text{overlap}}$$

* **`within_gini`**: $\sum_g p_g s_g G_g$, where $p_g$ is the pixel share, $s_g$ is the light share, and $G_g$ is the internal pixel Gini of subunit $g$.
* **`between_gini`**: Pixel-weighted Gini across subunit mean radiances $\mu_g$.
* **`overlap_gini`**: Transvariation residual measuring distributional overlap across subunits.

### Methodological Notes
* **2011 → 2012 sensor transition:** Cheng et al. (2026) reconstruct 1992–2011 from DMSP-OLS (*NightNet*) and use native NPP-VIIRS from 2012 onward. While `sum_of_lights` and urban-core radiances are continuous, dim rural background pixels (`0 < radiance < 0.5`) drop sharply in 2012.
* **Completely unlit subunits:** When every pixel in a subunit is `0`, `gini` is `NA` (inequality is undefined) and its `within_contribution` is `0`.

---

## Citation & License

* **Package code:** MIT License (`LICENSE`).
* **Underlying NTL dataset:** Cheng, H., Geng, M., Li, X., Li, S., Zhao, M., Lin, C., Wang, J., Gong, P., & Zhou, Y. (2026). A temporally consistent global 500 m-resolution monthly VIIRS-like nighttime light dataset (1992–2024). *Earth System Science Data*, 18, 3449–3479. https://doi.org/10.5194/essd-18-3449-2026 (CC BY 4.0).
