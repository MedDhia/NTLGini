# Download / cache the external political-science data used in this analysis.
# All sources are distributed as R packages; for the two GitHub-only packages
# (vdemdata, democracyData) we fetch only their bundled .rda data files.
# peacesciencer (CRAN) and WDI (CRAN, World Bank API) are used directly.

cache <- file.path("analysis", "spatial-inequality-politics", "cache")
dir.create(cache, showWarnings = FALSE, recursive = TRUE)

get_raw <- function(repo, ref, path) {
  dest <- file.path(cache, basename(path))
  if (!file.exists(dest)) {
    url <- sprintf("https://raw.githubusercontent.com/%s/%s/%s", repo, ref, path)
    utils::download.file(url, dest, mode = "wb", quiet = TRUE)
  }
  dest
}

# V-Dem v16 (vdemdata, commit f4dd269)
get_raw("vdeminstitute/vdemdata", "f4dd269", "data/vdem.RData")

# democracyData (commit 84290a7): Polity5, Freedom House, Extended UDS, LIED
for (f in c("polity5", "fh", "extended_uds", "LIED")) {
  get_raw("xmarquez/democracyData", "84290a7", sprintf("data/%s.rda", f))
}

# World Development Indicators (controls)
wdi_file <- file.path(cache, "wdi.rds")
if (!file.exists(wdi_file)) {
  w <- WDI::WDI(
    indicator = c(gdppc = "NY.GDP.PCAP.PP.KD", pop = "SP.POP.TOTL",
                  urb = "SP.URB.TOTL.IN.ZS", rents = "NY.GDP.TOTL.RT.ZS"),
    start = 1990, end = 2024, extra = TRUE
  )
  saveRDS(w, wdi_file)
}
elec_file <- file.path(cache, "wdi_elec.rds")
if (!file.exists(elec_file)) {
  saveRDS(WDI::WDI(indicator = c(elec = "EG.ELC.ACCS.ZS", elec_rural = "EG.ELC.ACCS.RU.ZS",
                                 elec_urban = "EG.ELC.ACCS.UR.ZS"),
                   start = 1990, end = 2024, extra = TRUE), elec_file)
}
message("Data cached in ", cache)
