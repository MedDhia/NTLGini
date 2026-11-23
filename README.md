# ntlgini

Global subnational nighttime-light Gini evolution (`1992–2024`) across `ADM_1`–`ADM_4` administrative levels, derived from **Cheng et al. (2026, *ESSD*)** 500 m VIIRS-like nighttime lights and GADM 4.1 (with MapsTN `ADM_1`–`ADM_4` for Tunisia).

## Usage

```r
library(ntlgini)

# Country-level within/between/overlap Gini decomposition (1992-2024)
decomp <- ntl_gini_decomp(iso3 = c("TUN", "MAR"))

# Subunit-level Gini series at ADM_4
adm4 <- ntl_gini_units(level = "adm4", iso3 = "TUN", years = 2010:2024)
```
