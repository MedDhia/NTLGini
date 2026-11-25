## Test environments
* macOS 15.6 (local aarch64), R 4.4.1
* Ubuntu Linux (GitHub Actions), R-release, R-devel
* Windows Server (GitHub Actions), R-release

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Data storage and downloads
* Country-level Gini decompositions (17,553 rows across 228 countries and territories, 1992-2024) are bundled in `data/ntl_gini_global.rda` (< 1 MB).
* Monthly inequality series (984 rows across national and ADM_1 levels) are bundled in `inst/extdata/gini_monthly.parquet` (33 KB).
* Subunit-level Parquet tables are downloaded only on demand when functions like `ntl_gini_units()` are called with `download = TRUE`, and are stored in standard user cache directory `tools::R_user_dir("ntlgini", "cache")`.
* Cloud raster extraction functions (`ntl_raster()`, `ntl_extract()`) use GDAL HTTP range requests (/vsizip/ and /vsicurl/) to stream small cropped regions on demand from the Cheng et al. (2026) open repository.
* Extensive unit test suite comprising 223 automated test assertions with full testthat coverage.
