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

# Merge keys for political-science datasets.
ntl_countries$cown <- countrycode(iso3, "iso3c", "cown", warn = FALSE)
ntl_countries$vdem_id <- countrycode(iso3, "iso3c", "vdem", warn = FALSE)
ntl_countries$cown[ntl_countries$iso3 == "XKO"] <- 347L
ntl_countries$vdem_id[ntl_countries$iso3 == "XKO"] <- 43L
ntl_countries$cown <- as.integer(ntl_countries$cown)
ntl_countries$vdem_id <- as.integer(ntl_countries$vdem_id)

# World Bank income group (current classification) from the World Bank API.
wb <- jsonlite::fromJSON("https://api.worldbank.org/v2/country?format=json&per_page=400")[[2]]
wb <- data.frame(iso3 = wb$id, income_group = wb$incomeLevel$value, stringsAsFactors = FALSE)
wb$iso3[wb$iso3 == "XKX"] <- "XKO"  # World Bank code for Kosovo
wb <- wb[wb$income_group != "Aggregates" & wb$income_group != "", ]
ntl_countries$income_group <- wb$income_group[match(ntl_countries$iso3, wb$iso3)]
ntl_countries$income_group[ntl_countries$income_group == "Not classified"] <- NA

# Keep the bundled data ASCII-only (avoids a CRAN note), e.g. "Cote d'Ivoire".
ntl_countries$country <- iconv(ntl_countries$country, "UTF-8", "ASCII//TRANSLIT")
stopifnot(!anyNA(ntl_countries$country))

ntl_countries <- tibble::as_tibble(ntl_countries)
save(ntl_countries, file = "data/ntl_countries.rda", compress = "xz")
