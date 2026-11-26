# Shared helper: one event-time caterpillar plot (single series) per figure file:
# a point estimate per event year with its 95% CI as a vertical bar.
suppressPackageStartupMessages(library(ggplot2))
plot_event_path <- function(df, title, subtitle, ylab, file, colour = "#257de0") {
  df <- rbind(df[, c("rel", "est", "se")], data.frame(rel = -1, est = 0, se = 0))
  p <- ggplot(df, aes(rel, est)) +
    geom_hline(yintercept = 0, colour = "#9c9c9c", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, colour = "#9c9c9c", linewidth = 0.4, linetype = "dashed") +
    geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), colour = colour,
                  width = 0.25, linewidth = 0.6) +
    geom_point(colour = colour, size = 2.6) +
    scale_x_continuous(breaks = seq(-6, 10, 2)) +
    labs(x = "Years relative to event (t = -1 is the reference year)", y = ylab,
         title = title, subtitle = paste(strwrap(subtitle, width = 85), collapse = "\n")) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), plot.title.position = "plot",
          text = element_text(colour = "#191919"), axis.text = element_text(colour = "#666666"),
          plot.subtitle = element_text(colour = "#666666"),
          plot.background = element_rect(fill = "white", colour = NA))
  ggsave(file, p, width = 8, height = 4.5, dpi = 150)
  invisible(file)
}
