# ntlgini (development version)

## Research helpers

* New `ntl_countries` dataset: country name, continent, World Bank region and
  UN subregion for all 228 codes (including GADM-specific `XAD`, `XKO`, `ZNC`).
* `ntl_gini_decomp()` gains `region` and `continent` filters.
* New `ntl_add_country_info()` joins country names and regions onto any table.
* New `ntl_flag_quality()` adds a sensor column (DMSP-reconstructed vs VIIRS)
  and flags for the 2012 sensor break, few subunits, saturated Gini (> 0.99),
  large year-on-year jumps, and missing values.
* New `ntl_gini_trend()` summarises start/end values, change and OLS slope per
  country and level, optionally split at the 2012 sensor switch.
* New vignette, "Getting started with ntlgini".
* `citation("ntlgini")` now cites both the package and Cheng et al. (2026).
* `?ntl_gini_global` documents comparability caveats.
* GitHub Actions runs `R CMD check` on Linux, macOS and Windows.

## Infrastructure

* `ntl_gini_units()` now pushes `iso3` / `years` filters down to 'arrow' when
  'dplyr' is installed, so only matching rows are read into memory.
* Downloads use a timeout of at least 600 seconds, verify MD5 checksums when
  known, and never leave partial files behind.
* The cache is now versioned by data release (`ntl_cache_dir()`), so files from
  an older release are never reused.
* New cache helpers: `ntl_cache_dir()`, `ntl_cache_list()`, `ntl_cache_clear()`.
* `ntl_gini_decomp()` and `ntl_gini_units()` validate their inputs: ISO3 codes
  are case-insensitive and unknown codes warn; invalid levels and non-integer
  years error; years outside 1992-2024 warn.
* New `plot_ntl_gini()` draws stacked-area decomposition charts (requires
  'ggplot2').
* Fixed the GitHub URL casing (`MedDhia/NTLGini`).

# ntlgini 0.1.0

* Initial release.
