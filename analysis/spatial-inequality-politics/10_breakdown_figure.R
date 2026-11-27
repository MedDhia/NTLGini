# Separate event-time caterpillar plots after democratic breakdowns (Sun & Abraham,
# country + region-by-year FE; bars = 95% CI clustered by country).
# One series per file, written to output/figures/.
suppressPackageStartupMessages(library(dplyr))
here <- file.path("analysis", "spatial-inequality-politics"); out <- file.path(here, "output")
figs <- file.path(out, "figures"); dir.create(figs, showWarnings = FALSE)
source(file.path(here, "plot_event_path.R"))
paths <- read.csv(file.path(out, "breakdown_paths.csv")) |> filter(fe == "iso3 + region^year")
hetp  <- read.csv(file.path(out, "breakdown_heterogeneity_paths.csv"))
sub_row <- "32 RoW-dated breakdowns vs 37 always-democracies; country + region-by-year FE; 95% CI"
lp <- "Log points x 100 (approx. %)"
specs <- list(
  list(d = filter(paths, outcome == "log_lights"),    t = "Total light after democratic breakdown (RoW dating)", s = sub_row, f = "breakdown_total_light"),
  list(d = filter(paths, outcome == "log_core"),      t = "Light in the core region after democratic breakdown", s = paste(sub_row, "| core = brightest ADM1 in 1992-94"), f = "breakdown_core_light"),
  list(d = filter(paths, outcome == "log_periphery"), t = "Light outside the core region after democratic breakdown", s = sub_row, f = "breakdown_periphery_light"),
  list(d = filter(paths, outcome == "log_gdppc100"),  t = "GDP per capita after democratic breakdown", s = sub_row, f = "breakdown_gdp"),
  list(d = filter(hetp, outcome == "log_lights", sample == "LIED: incumbent-led (regression / self-coup)"),
       t = "Total light after incumbent-led breakdown (LIED dating)", s = "18 LIED incumbent-led breakdowns vs always-democracies; country + region-by-year FE; 95% CI", f = "breakdown_lied_incumbent_light"),
  list(d = filter(hetp, outcome == "log_lights", sample == "LIED: coups & civil conflict"),
       t = "Total light after coups and civil conflict (LIED dating)", s = "15 LIED coup / civil-conflict breakdowns vs always-democracies; country + region-by-year FE; 95% CI", f = "breakdown_lied_coup_light")
)
for (sp in specs) plot_event_path(sp$d, sp$t, sp$s, lp, file.path(figs, paste0(sp$f, ".png")))
message("saved ", length(specs), " breakdown figures to ", figs)
