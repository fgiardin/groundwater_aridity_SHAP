# Script to plot maps of the main variables used in this study

# load libraries
library(tidyverse)
library(sf)
library(maps)
library(viridis)
library(ggpubr)

# 0 - data preparation --------------------------------------
sf_use_s2(FALSE)

# read in data
df_raw <- readRDS("data/main.rds")

# Create USA map and transform its CRS
usa_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) # for map without state borders: "usa" instead of "state"
usa_map <- st_transform(usa_map, 4326)

# focus on US
df_us <- df_raw %>% dplyr::filter(lon > -125, # focus on USA
                                  lon < -65,
                                  lat < 50,
                                  lat > 24) %>%
  mutate(SIF_over_PAR = SIF_over_PAR * 10^6) %>%
  dplyr::select(lon, lat, SIF, SIF_over_PAR, P_over_Rn, WTD_Fan, WTD_globgm,
         elevation, GDE_frac)

data_sf <- st_as_sf(df_us, coords = c("lon", "lat"), crs = 4326)

# Filter points that fall inside the USA
data_sf_filtered <- st_intersection(data_sf, usa_map)

# Set the desired Albers projection string for the contiguous US
albers_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Transform the filtered data to Albers projection
data_sf_albers <- st_transform(data_sf_filtered, crs = albers_crs)
usa_map_albers <- st_transform(usa_map, crs = albers_crs)

# 1 - Plot SIF/PAR --------------------------------------
p_SIFoverPAR <- ggplot() +
  geom_sf(data = data_sf_albers %>%
            mutate(SIF_over_PAR = ifelse(SIF_over_PAR > 4, 4, SIF_over_PAR)),
          aes(color = SIF_over_PAR), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  scale_color_viridis_c(
    name = expression(paste("SIF/PAR x 10"^6 * " (sr"^-1 * "nm"^-1 * ")")),
    option = "turbo",
    breaks = c(0, 1, 2, 3, 4),   # Add breaks
    labels = c("0", "1", "2", "3", "≥4"),  # Corresponding labels for breaks
    direction = -1
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text = element_blank(),         # Remove axis text (lat/lon numbers)
        axis.ticks = element_blank(),        # Remove axis ticks
        panel.background = element_blank(),  # Remove background panel
        panel.border = element_blank()       # Remove panel border (if any)
        ) +
  guides(color = guide_colourbar(frame.linewidth = 0.5,
                                 ticks.linewidth = 0.5,
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 barwidth = 15,
                                 barheight = 1))

# 1 - Plot Aridity --------------------------------------
p_Aridity <- ggplot() +
  geom_sf(data = data_sf_albers %>%
            mutate(P_over_Rn = ifelse(P_over_Rn > 2, 2, P_over_Rn)), # assign same color to outliers
          aes(color = P_over_Rn), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  scale_color_viridis_c(
    name = expression(paste("λP/R"[n], " (", "-", ")")),
    option = "turbo",
    breaks = c(0.07, 0.5, 1, 1.5, 2),   # Add breaks as per data range
    labels = c("0", "0.5", "1", "1.5","≥2"),  # Corresponding labels for breaks
    direction = -1
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text = element_blank(),         # Remove axis text (lat/lon numbers)
        axis.ticks = element_blank(),        # Remove axis ticks
        panel.background = element_blank(),  # Remove background panel
        panel.border = element_blank()       # Remove panel border (if any)
        ) +
  guides(color = guide_colourbar(frame.linewidth = 0.5,
                                 ticks.linewidth = 0.5,
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 barwidth = 15,
                                 barheight = 1))

# 1 - Plot WTD from Fan et al. --------------------------------------
p_FanWTD <- ggplot() +
  geom_sf(data = data_sf_albers %>%
            mutate(WTD = ifelse(WTD_Fan < -100, -100, WTD_Fan)), # same color for outliers / block the colorbar to values within the IQ
          aes(color = WTD), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  scale_color_viridis_c(
    name = "WTD (m)  ",
    option = "turbo",
    breaks = c(-100, -75, -50, -25, -5.684342e-14),   # Add breaks as per data range
    labels = c("≤-100", "-75", "-50", "-25","0"),  # Corresponding labels for breaks
    direction = -1
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text = element_blank(),         # Remove axis text (lat/lon numbers)
        axis.ticks = element_blank(),        # Remove axis ticks
        panel.background = element_blank(),  # Remove background panel
        panel.border = element_blank()       # Remove panel border (if any)
        ) +
  guides(color = guide_colourbar(frame.linewidth = 0.5,
                                 ticks.linewidth = 0.5,
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 barwidth = 15,
                                 barheight = 1))


# 1 - Plot elevation --------------------------------------
p_Elevation <- ggplot() +
  geom_sf(data = data_sf_albers, # same color for outliers / block the colorbar to values within the IQ
          aes(color = elevation), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  scale_color_viridis_c(
    name = "Elevation (m)  ",
    option = "turbo",
    breaks = c(0, 1000, 2000, 3000, 4000),   # Add breaks as per data range
    labels = c("0", "1000", "2000", "3000","4000"),  # Corresponding labels for breaks
    direction = -1
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text = element_blank(),         # Remove axis text (lat/lon numbers)
        axis.ticks = element_blank(),        # Remove axis ticks
        panel.background = element_blank(),  # Remove background panel
        panel.border = element_blank()       # Remove panel border (if any)
        ) +
  guides(color = guide_colourbar(frame.linewidth = 0.5,
                                 ticks.linewidth = 0.5,
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 barwidth = 15,
                                 barheight = 1))

# 1 - plot PFT groups --------------------------------------
df_SHAP <- readRDS("data/main.rds") # reload data (different processing)
source("data-raw/0_land_cover_mapping.R")

df <- df_SHAP %>%
  drop_na(SIF, WTD_Fan, P_over_Rn, elevation, PAR) %>%
  mutate(major_land_cover = recode(land_cover_num, !!!major_land_cover_mapping)) %>%
  dplyr::filter(P_over_Rn < 3,
                P_over_Rn > 0,
                major_land_cover != "other",
                land_cover_change == 0) %>%
  dplyr::filter(lon > -125, # focus on USA
                lon < -65,
                lat < 50,
                lat > 24) %>%
  dplyr::select(lon, lat, GDE_frac, major_land_cover)

df_land_cover <- df_SHAP %>%
  mutate(major_land_cover = recode(land_cover_num, !!!major_land_cover_mapping)) %>%
  filter(lon > -125, # focus on USA
         lon < -65,
         lat < 50,
         lat > 24) %>%
  select(lon, lat, major_land_cover)

# plot
data_lc_sf <- st_as_sf(df_land_cover, coords = c("lon", "lat"), crs = 4326)
data_lc_sf_filtered <- st_intersection(data_lc_sf, usa_map) # Filter points that fall inside the USA
data_lc_sf_albers <- st_transform(data_lc_sf_filtered, crs = albers_crs) # Transform the filtered data to Albers projection

# Colorblind-friendly colors
savanna_shrub_color <- "#8C510A"
cropland_color <- "#D9BF77"
forest_color <- "#006837"
grassland_color <- "#76C476"
other_color <- "grey"

# Plotting
p_landcover <- ggplot() +
  geom_sf(data = data_lc_sf_albers, aes(color = major_land_cover), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  scale_color_manual(
    name = "Land Cover",
    values = c("forests" = forest_color,
               "savannas_and_shrublands" = savanna_shrub_color,
               "grasslands" = grassland_color,
               "croplands" = cropland_color,
               "other" = other_color),
    labels = c("forests" = "Forests",            # Capitalize and add spaces
               "savannas_and_shrublands" = "Savannas and Shrublands",
               "grasslands" = "Grasslands",
               "croplands" = "Croplands",
               "other" = "Other"),
    guide = guide_legend(
      title = "",
      override.aes = list(size = 4),
      nrow = 2)) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text = element_blank(),         # Remove axis text (lat/lon numbers)
        axis.ticks = element_blank(),        # Remove axis ticks
        panel.background = element_blank(),  # Remove background panel
        panel.border = element_blank()       # Remove panel border (if any)
  )

p_landcover


# put everything together -------------------------------------------------
all <- ggarrange(p_SIFoverPAR,
                 p_Aridity,
                 p_Elevation,
                 p_FanWTD,
                 NULL,
                 p_landcover,
                 labels = "auto",
                 ncol = 2, nrow = 3)


library(cowplot)

# Create an empty plot to serve as a spacer (to center the 5th plot)
spacer <- ggplot() +
  theme_void() +          # Remove all elements (axes, grid, etc.)
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))  # No margins

# Arrange the plots using cowplot::plot_grid
all <- plot_grid(
  plot_grid(p_SIFoverPAR, p_Aridity, ncol = 2, labels = c("a", "b")),
  plot_grid(p_Elevation, p_FanWTD, ncol = 2, labels = c("c", "d")),
  plot_grid(spacer, p_landcover, spacer, ncol = 3, rel_widths = c(0.5, 1, 0.5), labels = c("", "e", "")),
  ncol = 1
)

ggsave("Fig_2.png", plot = all,
       path = "./",
       dpi = 300, width = 10.5, height = 13.5)


### Maps for supplementary information
# SIF, WTD, GDE --------------------------------------
p_SIF <- ggplot() +
  geom_sf(data = data_sf_albers %>%
            mutate(SIF = ifelse(SIF > 1.5, 1.5, SIF)),
          aes(color = SIF), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  scale_color_viridis_c(
    name = expression(paste('SIF (mW'~'m'^-2~'sr'^-1~'nm'^-1*')')),
    option = "turbo",
    breaks = c(0, 0.5, 1, 1.5),   # Add breaks as per your data range.
    labels = c("0", "0.5", "1", "≥1.5"),  # Corresponding labels for breaks
    direction = -1
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text = element_blank(),         # Remove axis text (lat/lon numbers)
        axis.ticks = element_blank(),        # Remove axis ticks
        panel.background = element_blank(),  # Remove background panel
        panel.border = element_blank()       # Remove panel border (if any)
        ) +
  guides(color = guide_colourbar(frame.linewidth = 0.5,
                                 ticks.linewidth = 0.5,
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 barwidth = 15,
                                 barheight = 1))

# WTD from GLOBGM
p_GLOBGMWTD <- ggplot() +
  geom_sf(data = data_sf_albers %>%
            mutate(WTD = ifelse(WTD_globgm < -300, -300, WTD_globgm)), # same color for outliers / block the colorbar to values within the IQ
          aes(color = WTD), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  scale_color_viridis_c(
    name = "GLOBGM WTD (m)  ",
    option = "turbo",
    breaks = c(-300, -200, -100, 0),   # Add breaks as per data range
    labels = c("≤-300", "-200", "-100", "0"),  # Corresponding labels for breaks
    direction = -1
  ) +
  theme_minimal() +
  # labs(title = "WTD") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text = element_blank(),         # Remove axis text (lat/lon numbers)
        axis.ticks = element_blank(),        # Remove axis ticks
        panel.background = element_blank(),  # Remove background panel
        panel.border = element_blank()       # Remove panel border (if any)
        ) +
  guides(color = guide_colourbar(frame.linewidth = 0.5,
                                 ticks.linewidth = 0.5,
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 barwidth = 15,
                                 barheight = 1))

# Plot GDE frac
p_GDE <- ggplot() +
  geom_sf(data = data_sf_albers,
          aes(color = GDE_frac), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  scale_color_viridis_c(
    name = "GDE fraction  ",
    option = "turbo",
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),   # Add breaks as per data range
    labels = c("0", "0.2", "0.4", "0.6", "0.8", "1"),  # Corresponding labels for breaks
    direction = 1,
    na.value = "white"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text = element_blank(),         # Remove axis text (lat/lon numbers)
        axis.ticks = element_blank(),        # Remove axis ticks
        panel.background = element_blank(),  # Remove background panel
        panel.border = element_blank()       # Remove panel border (if any)
        ) +
  guides(color = guide_colourbar(frame.linewidth = 0.5,
                                 ticks.linewidth = 0.5,
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 barwidth = 15,
                                 barheight = 1))+
  expand_limits(color = c(0, 1))



Sall <- ggarrange(p_SIF, p_GLOBGMWTD, p_GDE,
                 labels = "auto",
                 ncol = 1, nrow = 3)

ggsave("FigS_Smap.png", plot = Sall,
       path = "./",
       dpi = 300, width = 6, height = 12)



# Detailed land cover map --------------------------------------------
data_sf_albers$groupings <- ifelse(data_sf_albers$land_cover_chr %in%
                                     c("water", "urban", "snow_and_ice", "bare_areas"), "other", data_sf_albers$land_cover_chr)
library(RColorBrewer)
land_cover_colors <- colorRampPalette(brewer.pal(12, "Set3"))(19)  # "Set3" palette

A <- ggplot() +
  geom_sf(data = data_sf_albers, aes(color = groupings), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  scale_color_manual(
    name = "Land Cover",
    values = setNames(land_cover_colors, unique(data_sf_albers$groupings)),
    guide = guide_legend(
      title = "Land cover",
      ncol = 1,  # Number of columns
      nrow = 19,  # Number of rows
      byrow = TRUE,  # Arrange items by row
      override.aes = list(size = 4)  # Adjusting point size in the legend here
    )
  ) +
  theme_minimal() +
  # labs(title = "PFT") +
  theme(legend.position = "right",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.5, "lines"),  # Decrease the size of the legend keys
        legend.box = "vertical",
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text = element_blank(),         # Remove axis text (lat/lon numbers)
        axis.ticks = element_blank(),        # Remove axis ticks
        panel.background = element_blank(),  # Remove background panel
        panel.border = element_blank()       # Remove panel border (if any)
        )

png(filename = paste0("./",
                      "Fig_us_land_cover.png"),
    width = 11, height = 5, units = "in", res = 300)
print(A)
dev.off()


