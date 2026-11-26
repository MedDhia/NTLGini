# Event-time paths after civil-war onset, conflict termination and popular-uprising
# revolutions (Sun & Abraham, country + region-by-year FE, 95% CIs by country).
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(patchwork) })
here <- file.path("analysis", "spatial-inequality-politics"); out <- file.path(here, "output")
R <- read.csv(file.path(out, "war_revolution_event_study.csv")) |> filter(fe == "iso3 + region^year")
lab <- c("Civil-war onset (1,000+ deaths)" = "Civil-war onset (18)",
         "Conflict termination" = "Conflict termination (32)",
         "Revolution: popular uprising (V-Dem)" = "Popular-uprising revolution (11)")
pd <- R |> filter(event %in% names(lab)) |> mutate(series = factor(lab[event], lab)) |>
  select(series, outcome, rel, est, se)
pd <- bind_rows(pd, pd |> distinct(series, outcome) |> mutate(rel = -1, est = 0, se = 0))
pal <- c("#257de0", "#f47230", "#00b894")
panel <- function(y, title, ylab) {
  ggplot(pd |> filter(outcome == y), aes(rel, est, colour = series, shape = series)) +
    geom_hline(yintercept = 0, colour = "#9c9c9c", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, colour = "#9c9c9c", linewidth = 0.4, linetype = "dashed") +
    geom_linerange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), linewidth = 0.5,
                   position = position_dodge(width = 0.6), alpha = 0.7) +
    geom_line(position = position_dodge(width = 0.6), linewidth = 0.7) +
    geom_point(size = 2, position = position_dodge(width = 0.6)) +
    scale_colour_manual(values = pal, name = NULL) + scale_shape_manual(values = c(16, 17, 15), name = NULL) +
    scale_x_continuous(breaks = seq(-6, 10, 2)) +
    labs(x = "Years relative to event (t = -1 is reference)", y = ylab, title = title) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "top", panel.grid.minor = element_blank(), plot.title.position = "plot",
          text = element_text(colour = "#191919"), axis.text = element_text(colour = "#666666"),
          plot.background = element_rect(fill = "white", colour = NA))
}
p <- panel("log_lights", "A. Total light", "Log points x 100 (approx. %)") /
  panel("between_gini_adm1", "B. Between-region Gini (ADM1)", "Gini points x 100") /
  panel("core_share", "C. Core region's share of national light", "Percentage points") +
  plot_layout(guides = "collect") & theme(legend.position = "top")
ggsave(file.path(out, "war_revolution_paths.png"), p, width = 10, height = 12, dpi = 150)
message("saved war_revolution_paths.png")
