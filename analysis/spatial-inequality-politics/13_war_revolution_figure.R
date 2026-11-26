# Separate event-time figures after civil-war onset, conflict termination and
# popular-uprising revolutions (Sun & Abraham, country + region-by-year FE;
# shaded band = 95% CI clustered by country). One series per file.
suppressPackageStartupMessages(library(dplyr))
here <- file.path("analysis", "spatial-inequality-politics"); out <- file.path(here, "output")
figs <- file.path(out, "figures"); dir.create(figs, showWarnings = FALSE)
source(file.path(here, "plot_event_path.R"))
R <- read.csv(file.path(out, "war_revolution_event_study.csv")) |> filter(fe == "iso3 + region^year")
ev <- list(
  war = list(name = "Civil-war onset (1,000+ deaths)", lab = "civil-war onset", sub = "18 civil-war onsets vs 84 never-conflict countries"),
  termination = list(name = "Conflict termination", lab = "conflict termination", sub = "32 conflict terminations vs 84 never-conflict countries"),
  revolution = list(name = "Revolution: popular uprising (V-Dem)", lab = "popular-uprising revolution", sub = "11 popular-uprising revolutions vs 59 regime-stable countries")
)
oc <- list(
  total_light = list(var = "log_lights", t = "Total light", y = "Log points x 100 (approx. %)"),
  between_gini = list(var = "between_gini_adm1", t = "Between-region Gini (ADM1)", y = "Gini points x 100"),
  core_share = list(var = "core_share", t = "Core region's share of national light", y = "Percentage points")
)
n <- 0
for (e in names(ev)) for (o in names(oc)) {
  df <- R |> filter(event == ev[[e]]$name, outcome == oc[[o]]$var)
  plot_event_path(df, paste(oc[[o]]$t, "after", ev[[e]]$lab),
                  paste0(ev[[e]]$sub, "; country + region-by-year FE; 95% CI"),
                  oc[[o]]$y, file.path(figs, paste0(e, "_", o, ".png")))
  n <- n + 1
}
message("saved ", n, " war/revolution figures to ", figs)
