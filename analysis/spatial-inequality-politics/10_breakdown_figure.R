# Event-time paths of light after democratic breakdowns (Sun & Abraham,
# country + region-by-year FE; 95% CIs clustered by country).
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(patchwork) })
here <- file.path("analysis", "spatial-inequality-politics"); out <- file.path(here, "output")
paths <- read.csv(file.path(out, "breakdown_paths.csv")) |> filter(fe == "iso3 + region^year")
hetp  <- read.csv(file.path(out, "breakdown_heterogeneity_paths.csv"))
add_ref <- function(df) bind_rows(df, df |> distinct(series) |> mutate(rel = -1, est = 0, se = 0))

a <- paths |> filter(outcome %in% c("log_lights", "log_core", "log_periphery", "log_gdppc100")) |>
  mutate(series = recode(outcome, log_lights = "Total light", log_core = "Light in core region",
                         log_periphery = "Light outside core", log_gdppc100 = "GDP per capita")) |>
  select(series, rel, est, se) |> add_ref() |>
  mutate(series = factor(series, c("Total light", "Light in core region", "Light outside core", "GDP per capita")))
b <- bind_rows(
  paths |> filter(outcome == "log_lights") |> mutate(series = "RoW threshold crossing (32)"),
  hetp |> filter(outcome == "log_lights", sample == "LIED: incumbent-led (regression / self-coup)") |>
    mutate(series = "LIED incumbent-led (18)"),
  hetp |> filter(outcome == "log_lights", sample == "LIED: coups & civil conflict") |>
    mutate(series = "LIED coups & conflict (15)")) |>
  select(series, rel, est, se) |> add_ref() |>
  mutate(series = factor(series, c("RoW threshold crossing (32)", "LIED incumbent-led (18)", "LIED coups & conflict (15)")))

pal <- c("#257de0", "#f47230", "#00b894", "#f2a600")
base <- function(df, title, sub) {
  ggplot(df, aes(rel, est, colour = series, shape = series)) +
    geom_hline(yintercept = 0, colour = "#9c9c9c", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, colour = "#9c9c9c", linewidth = 0.4, linetype = "dashed") +
    geom_linerange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), linewidth = 0.5,
                   position = position_dodge(width = 0.6), alpha = 0.7) +
    geom_line(position = position_dodge(width = 0.6), linewidth = 0.7) +
    geom_point(size = 2, position = position_dodge(width = 0.6)) +
    scale_colour_manual(values = pal, name = NULL) + scale_shape_manual(values = c(16, 17, 15, 18), name = NULL) +
    scale_x_continuous(breaks = seq(-6, 10, 2)) +
    labs(x = "Years relative to breakdown (t = -1 is reference)", y = "Log points x 100 (approx. %)",
         title = title, subtitle = sub) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "top", panel.grid.minor = element_blank(), plot.title.position = "plot",
          text = element_text(colour = "#191919"), axis.text = element_text(colour = "#666666"),
          plot.background = element_rect(fill = "white", colour = NULL))
}
pa <- base(a, "A. Incumbent-led erosions (RoW): light rises, mostly in the core region",
           "Core = each country's brightest ADM1 in 1992-94. GDP barely moves.")
pb <- base(b, "B. Total light depends on how breakdowns are defined and dated",
           "Coups and civil conflict dim the lights; LIED incumbent-led breakdowns show no post-event rise")
ggsave(file.path(out, "breakdown_paths.png"), pa / pb, width = 10, height = 10, dpi = 150)
message("saved breakdown_paths.png")
