# ntlgini 0.1.0

* Initial release to CRAN.
* Provides harmonized 1992–2024 nighttime-light Gini coefficients and Pyatt (1976) / Dagum (1997) within-, between-, and overlap-group decompositions across administrative levels (`ADM_1` through `ADM_4`) for 228 countries and territories.
* Bundled nationwide decomposition series in `ntl_gini_global` and country metadata in `ntl_gini_countries`.
* On-demand download and local caching of subunit-level Parquet datasets via `ntl_gini_units()`, `ntl_gini_top_units()`, `ntl_gini_cache_info()`, and `ntl_gini_clear_cache()`.
* Added monthly inequality series query `ntl_gini_monthly()` with national and `ADM_1` Gini, Theil, sum of lights, and lit-pixel proportions.
* Added BlackMarble-style cloud raster streaming and zonal extraction functions `ntl_raster()` and `ntl_extract()` using Cheng et al. (2026) harmonized monthly/annual 500 m rasters (1992–2024).
* Multi-scale time-series, scatter, and spatial mapping visualizations using `ggplot2` and `sf` with UN-adopted Equal Earth projection and plasma color palette (`ntl_gini_plot_series()`, `ntl_gini_plot_decomp()`, `ntl_gini_plot_levels()`, `ntl_gini_plot_scatter()`, `ntl_gini_map_countries()`, `ntl_gini_map_units()`).
