# Caterpillar plots for the event screen (14_event_screen.R). One series per file.
#  (a) Overview: for each spatial measure and window, the estimate for every one of
#      the 16 event types (point + 95% CI), sorted. Filled = passes the suggestive
#      rule (BH q < .10, pre-trend p > .10, >= 80% of drop-one-country estimates p < .05);
#      hollow = does not; "+" after the label marks a pre-trend (p <= .10).
#  (b) Event-time caterpillars for every event x measure that passes the rule.
suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })
here <- file.path("analysis", "spatial-inequality-politics"); out <- file.path(here, "output")
figs <- file.path(out, "figures"); dir.create(figs, showWarnings = FALSE)
source(file.path(here, "plot_event_path.R"))
tests <- read.csv(file.path(out, "screen_tests.csv"))
paths <- read.csv(file.path(out, "screen_paths.csv"))
summ  <- read.csv(file.path(out, "screen_summary.csv"))
outs <- c(total_gini = "Total Gini", between_gini_adm1 = "Between-region Gini (ADM1)",
          within_gini_adm1 = "Within-region Gini (ADM1)", between_gini_adm2 = "Between-region Gini (ADM2)",
          core_share = "Core region's share of national light", dark_share = "Darkest-third regions' share of light")
units <- c(total_gini = "Gini points x 100", between_gini_adm1 = "Gini points x 100",
           within_gini_adm1 = "Gini points x 100", between_gini_adm2 = "Gini points x 100",
           core_share = "Percentage points", dark_share = "Percentage points")
slug <- function(x) gsub("[^a-z0-9]+", "_", tolower(x)) |> sub(pattern = "_$", replacement = "")
blue <- "#257de0"

n_tr <- summ |> distinct(event, outcome, n_treated)
for (y in names(outs)) for (st in c("ATT (years 0-10)", "Years 5-9")) {
  df <- tests |> filter(outcome == y, stat == st) |> left_join(n_tr, by = c("event", "outcome")) |>
    mutate(label = paste0(event, " (", n_treated, ")", ifelse(pre_p <= .10, " +", "")),
           pass = ifelse(suggestive %in% TRUE, "passes", "does not pass")) |>
    arrange(est) |> mutate(label = factor(label, label))
  p <- ggplot(df, aes(est, label)) +
    geom_vline(xintercept = 0, colour = "#9c9c9c", linewidth = 0.4) +
    geom_errorbarh(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se), colour = blue, height = 0.25, linewidth = 0.6) +
    geom_point(aes(shape = pass), colour = blue, fill = blue, size = 2.6, stroke = 0.9) +
    scale_shape_manual(values = c("passes" = 21, "does not pass" = 1), drop = FALSE, name = NULL) +
    labs(x = paste0(units[[y]], ", ", if (st == "Years 5-9") "average of years 5-9 after the event" else "average of years 0-10 after the event"),
         y = NULL, title = paste0(outs[[y]], ": effect of 16 political events (", st, ")"),
         subtitle = paste("Sun & Abraham event studies, country + region-by-year FE, 95% CI. (n) = treated countries;",
                          "+ = pre-trend p <= .10. Filled = BH q < .10, no pre-trend, robust to dropping any one country.") |>
           strwrap(width = 110) |> paste(collapse = "\n")) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "top", panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          plot.title.position = "plot", text = element_text(colour = "#191919"),
          axis.text = element_text(colour = "#666666"), plot.subtitle = element_text(colour = "#666666"),
          plot.background = element_rect(fill = "white", colour = NA))
  ggsave(file.path(figs, paste0("screen_", y, "_", if (st == "Years 5-9") "years5_9" else "att", ".png")),
         p, width = 9, height = 6, dpi = 150)
}

sel <- tests |> filter(suggestive %in% TRUE) |> distinct(event, outcome) |> left_join(n_tr, by = c("event", "outcome"))
for (i in seq_len(nrow(sel))) {
  df <- paths |> filter(event == sel$event[i], outcome == sel$outcome[i])
  q  <- tests |> filter(event == sel$event[i], outcome == sel$outcome[i], suggestive %in% TRUE)
  plot_event_path(df, paste0(outs[[sel$outcome[i]]], " after: ", sel$event[i]),
                  paste0(sel$n_treated[i], " events; country + region-by-year FE; 95% CI. ",
                         paste0(q$stat, ": ", sprintf("%+.2f", q$est), " (BH q = ", sprintf("%.2f", q$q), ")"), collapse = "; "),
                  units[[sel$outcome[i]]],
                  file.path(figs, paste0("screen_path_", slug(sel$event[i]), "_", sel$outcome[i], ".png")))
}
message("saved ", 12 + nrow(sel), " screen figures")
