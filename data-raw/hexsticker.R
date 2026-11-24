# Generate ntlgini hex sticker
library(ggplot2)
library(sf)
library(hexSticker)
library(ntlgini)

# Load country shapes
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world <- world[!world$iso_a3 %in% c("ATA"), ]

# Merge with 2024 ADM1 between_gini
df_2024 <- ntl_gini_decomp(level = "adm1", years = 2024L)
world_merged <- merge(world, df_2024, by.x = "iso_a3", by.y = "iso3", all.x = TRUE)

# Equal Earth globe bounding box and graticules
lons <- c(seq(-180, 180, length.out = 180), rep(180, 90), seq(180, -180, length.out = 180), rep(-180, 90))
lats <- c(rep(-89.99, 180), seq(-89.99, 89.99, length.out = 90), rep(89.99, 180), seq(89.99, -89.99, length.out = 90))
bb_globe <- sf::st_sfc(sf::st_polygon(list(cbind(c(lons, lons[1]), c(lats, lats[1])))), crs = 4326)
grat <- sf::st_graticule(lat = seq(-80, 80, 30), lon = seq(-180, 180, 45))

p_globe <- ggplot() +
  geom_sf(data = bb_globe, fill = "#070c18", color = "#334155", linewidth = 0.35) +
  geom_sf(data = grat, color = "#1e293b", linewidth = 0.15) +
  geom_sf(
    data = world_merged,
    aes(fill = between_gini),
    color = "#0b0f19",
    linewidth = 0.08
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "#161b2e",
    begin = 0.12,
    end = 0.96,
    guide = "none"
  ) +
  coord_sf(crs = "+proj=eqearth") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

sticker(
  p_globe,
  package = "ntlgini",
  p_size = 30,
  p_y = 1.48,
  p_color = "#ffffff",
  p_fontface = "bold",
  s_x = 1.0,
  s_y = 0.86,
  s_width = 1.45,
  s_height = 0.95,
  h_fill = "#0a0f1d",
  h_color = "#f43f5e",
  h_size = 2.0,
  url = "1992 - 2024",
  u_color = "#94a3b8",
  u_size = 6.2,
  u_y = 0.08,
  filename = "man/figures/logo.png",
  dpi = 300
)
