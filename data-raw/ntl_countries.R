# Build the `ntl_countries` lookup table (run from the package root).
# Requires the 'countrycode' package; it is not a package dependency.
# Run in a UTF-8 locale (e.g. LC_ALL=C.UTF-8) so accents transliterate cleanly.
library(countrycode)
load("data/ntl_gini_global.rda")

iso3 <- sort(unique(ntl_gini_global$iso3))
ntl_countries <- data.frame(
  iso3 = iso3,
  country = countrycode(iso3, "iso3c", "country.name", warn = FALSE),
  continent = countrycode(iso3, "iso3c", "continent", warn = FALSE),
  region = countrycode(iso3, "iso3c", "region", warn = FALSE),
  un_subregion = countrycode(iso3, "iso3c", "un.regionsub.name", warn = FALSE),
  stringsAsFactors = FALSE
)

# GADM-specific codes and territories that countrycode does not fully cover.
manual <- data.frame(
  iso3 = c("XAD", "XKO", "ZNC", "ESH", "MYT", "REU", "SHN", "UMI", "WLF", "TWN"),
  country = c("Akrotiri and Dhekelia", "Kosovo", "Northern Cyprus", NA, NA, NA, NA,
              "United States Minor Outlying Islands", NA, NA),
  continent = c("Asia", "Europe", "Asia", NA, NA, NA, NA, "Oceania", NA, NA),
  region = c("Europe & Central Asia", "Europe & Central Asia", "Europe & Central Asia",
             "Middle East, North Africa, Afghanistan & Pakistan", "Sub-Saharan Africa",
             "Sub-Saharan Africa", "Sub-Saharan Africa", "East Asia & Pacific",
             "East Asia & Pacific", NA),
  un_subregion = c("Western Asia", "Southern Europe", "Western Asia", NA, NA, NA, NA, NA,
                   NA, "Eastern Asia"),
  stringsAsFactors = FALSE
)
for (k in seq_len(nrow(manual))) {
  row <- match(manual$iso3[k], ntl_countries$iso3)
  if (is.na(row)) next
  for (col in c("country", "continent", "region", "un_subregion")) {
    if (!is.na(manual[[col]][k])) ntl_countries[[col]][row] <- manual[[col]][k]
  }
}

# Keep the bundled data ASCII-only (avoids a CRAN note), e.g. "Cote d'Ivoire".
ntl_countries$country <- iconv(ntl_countries$country, "UTF-8", "ASCII//TRANSLIT")
stopifnot(!anyNA(ntl_countries$country))

ntl_countries <- tibble::as_tibble(ntl_countries)
save(ntl_countries, file = "data/ntl_countries.rda", compress = "xz")
