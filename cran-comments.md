## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.
* Country-level Gini decompositions (16,203 rows across 228 countries/territories,
  1992-2024) are bundled in `data/ntl_gini_global.rda` (< 1 MB).
* Larger subunit-level Parquet tables (3.5 MB to 102 MB) are hosted on GitHub
  Releases and downloaded on demand into `tools::R_user_dir("ntlgini", "cache")`.
