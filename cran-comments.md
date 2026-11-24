## Test environments
* macOS 15.6 (local aarch64), R 4.4.1
* Ubuntu Linux (GitHub Actions), R-release, R-devel
* Windows Server (GitHub Actions), R-release

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Data storage and downloads
* Country-level Gini decompositions (16,203 rows across 228 countries and territories, 1992-2024) are bundled in `data/ntl_gini_global.rda` (< 1 MB).
* Subunit-level Parquet tables are downloaded only on demand when functions like `ntl_gini_units()` are called with `download = TRUE`, and are stored in standard user cache directory `tools::R_user_dir("ntlgini", "cache")`.
