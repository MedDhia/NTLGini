# Generate ntlgini hex sticker (Minimalist Multi-Scale Inequality Design)
library(ggplot2)
library(hexSticker)

# Points along cumulative population / area axis
x <- seq(0, 1, length.out = 300)

# Multi-scale Lorenz curves across administrative tiers (ADM1 to Total pixel)
y1 <- x^1.65  # ADM1 (inter-regional Gini)
y2 <- x^2.30  # ADM2
y3 <- x^3.20  # ADM3
y4 <- x^4.50  # Total spatial Gini

# Shaded decomposition ribbons between inequality tiers (Pyatt / Dagum shares)
poly_adm1 <- data.frame(x = c(x, rev(x)), y = c(x, rev(y1)))
poly_adm2 <- data.frame(x = c(x, rev(x)), y = c(y1, rev(y2)))
poly_adm3 <- data.frame(x = c(x, rev(x)), y = c(y2, rev(y3)))
poly_total <- data.frame(x = c(x, rev(x)), y = c(y3, rev(y4)))

p <- ggplot() +
  # Shaded inequality decomposition layers
  geom_polygon(data = poly_adm1, aes(x, y), fill = "#4b0c6b", alpha = 0.45) +
  geom_polygon(data = poly_adm2, aes(x, y), fill = "#781c6d", alpha = 0.40) +
  geom_polygon(data = poly_adm3, aes(x, y), fill = "#b5367a", alpha = 0.35) +
  geom_polygon(data = poly_total, aes(x, y), fill = "#ed6925", alpha = 0.30) +
  # 45-degree Line of Perfect Equality
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
               linetype = "dashed", color = "#cbd5e1", linewidth = 0.85) +
  # Nested Lorenz curves in plasma palette
  geom_line(aes(x = x, y = y1), color = "#781c6d", linewidth = 1.1) +
  geom_line(aes(x = x, y = y2), color = "#b5367a", linewidth = 1.2) +
  geom_line(aes(x = x, y = y3), color = "#ed6925", linewidth = 1.3) +
  geom_line(aes(x = x, y = y4), color = "#fcffa4", linewidth = 1.5) +
  # Origin and unity nodes
  geom_point(aes(x = 0, y = 0), color = "#cbd5e1", size = 2.2) +
  geom_point(aes(x = 1, y = 1), color = "#fcffa4", size = 3.0) +
  # Minimalist coordinate axes
  geom_segment(aes(x = 0, y = 0, xend = 1.05, yend = 0), color = "#334155", linewidth = 0.5) +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1.05), color = "#334155", linewidth = 0.5) +
  coord_fixed(xlim = c(-0.06, 1.08), ylim = c(-0.06, 1.08)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

sticker(
  p,
  package = "ntlgini",
  p_size = 34,
  p_y = 1.46,
  p_color = "#ffffff",
  p_fontface = "bold",
  s_x = 1.0,
  s_y = 0.82,
  s_width = 1.28,
  s_height = 1.28,
  h_fill = "#0a0f1d",
  h_color = "#f43f5e",
  h_size = 2.0,
  filename = "man/figures/logo.png",
  dpi = 300
)
