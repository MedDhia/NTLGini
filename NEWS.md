# ntlgini (development version)

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
