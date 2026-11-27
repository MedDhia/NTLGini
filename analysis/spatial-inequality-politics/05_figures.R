# Coefficient plot: bivariate vs. controlled vs. within-country estimates.
suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })
here <- file.path("analysis", "spatial-inequality-politics")
out  <- file.path(here, "output")
res  <- readRDS(file.path(out, "spec_grid.rds"))
meta <- readRDS(file.path(out, "meta.rds"))

spec_lab <- c(S01b = "Bivariate (+ year FE)",
              S02  = "Controls + year & region FE",
              S07  = "Country + year FE (within)")
pred_lab <- c(total_gini = "Total spatial Gini",
              between_gini_adm1 = "Between-region Gini, ADM1",
              between_gini_adm2 = "Between-region Gini, ADM2")
# ADM2 rows come from S12/S13 (ADM2 twins); relabel them as their own predictor
adm2 <- readRDS(file.path(out, "spec_grid.rds")) |>
  filter(spec %in% c("S12", "S13"), predictor == "between_gini_adm1") |>
  mutate(predictor = "between_gini_adm2", spec = recode(spec, S12 = "S02", S13 = "S07"))
# bivariate ADM2 estimate on the S12 sample is not in the grid; omit rather than mix samples
pd <- bind_rows(res |> filter(spec %in% names(spec_lab), predictor %in% c("total_gini", "between_gini_adm1")),
                adm2) |>
  mutate(spec = factor(spec_lab[spec], levels = spec_lab),
         predictor = factor(pred_lab[predictor], levels = pred_lab),
         label = factor(label, levels = rev(meta$outcomes$label)),
         family = factor(family, levels = unique(meta$outcomes$family)))

pal <- c("#257de0", "#f47230", "#00b894")
p <- ggplot(pd, aes(est, label, colour = spec, shape = spec)) +
  geom_vline(xintercept = 0, colour = "#9c9c9c", linewidth = 0.4) +
  geom_linerange(aes(xmin = ci_lo, xmax = ci_hi), linewidth = 0.6,
                 position = position_dodge(width = 0.65)) +
  geom_point(size = 2.2, position = position_dodge(width = 0.65)) +
  facet_grid(family ~ predictor, scales = "free_y", space = "free_y") +
  scale_colour_manual(values = pal, name = NULL) +
  scale_shape_manual(values = c(16, 17, 15), name = NULL) +
  labs(x = "Coefficient (outcome in SD units, or probability for binary outcomes) per 1 SD of inequality, 95% CI clustered by country",
       y = NULL,
       title = "Nighttime-light spatial inequality and political outcomes, 1992-2024",
       subtitle = "Strong bivariate associations largely vanish once development is controlled; only ADM2 between-region Gini keeps a (cross-sectional) democracy association") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(), strip.text.y = element_text(angle = 0, hjust = 0),
        plot.title.position = "plot", text = element_text(colour = "#191919"),
        axis.text = element_text(colour = "#666666"),
        plot.background = element_rect(fill = "white", colour = NA))
ggsave(file.path(out, "coefplot.png"), p, width = 12, height = 9, dpi = 150)
message("saved coefplot.png")
