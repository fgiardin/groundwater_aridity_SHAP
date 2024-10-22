# script to plot maps of the main variables used in this study (SIF/PAR, P/Rn, WTD, Elevation)
library(tidyverse)
library(sf)
library(maps)
library(viridis)
library(ggpubr)

# 0 - data preparation --------------------------------------
sf_use_s2(FALSE)
input_folder <- "data/jiangong/"
df_raw <- readRDS(paste0(input_folder, "main.rds"))

# Create USA map and transform its CRS
usa_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) # for map without state borders: "usa" instead of "state"
usa_map <- st_transform(usa_map, 4326)

df_us <- df_raw %>% dplyr::filter(lon > -125, # focus on USA
                                  lon < -65,
                                  lat < 50,
                                  lat > 24) %>%
  mutate(SIF_over_PAR = SIF_over_PAR * 10^6) %>% # the unit here is X10^3 m sr-1 nm-1 or n sr-1 nm-1
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
    breaks = c(0, 1, 2, 3, 4),   # Add breaks as per your data range.
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
    breaks = c(-100, -75, -50, -25, -5.684342e-14),   # Add breaks as per your data range
    labels = c("≤-100", "-75", "-50", "-25","0"),  # Corresponding labels for breaks
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


# 1 - Plot elevation --------------------------------------
p_Elevation <- ggplot() +
  geom_sf(data = data_sf_albers, # same color for outliers / block the colorbar to values within the IQ
          aes(color = elevation), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  scale_color_viridis_c(
    name = "Elevation (m)  ",
    option = "turbo",
    breaks = c(0, 1000, 2000, 3000, 4000),   # Add breaks as per your data range
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

all <- ggarrange(p_SIFoverPAR, p_Aridity, p_Elevation,p_FanWTD,
                 labels = "auto",
                 ncol = 2, nrow = 2)

ggsave("Fig_2.png", plot = all,
       path = "./",
       dpi = 300, width = 10.5, height = 9)


# maps for supplementary information
# script to plot maps of the main variables used in this study (SIF, WTD, GDE)
# 2 - SIF --------------------------------------
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

# 2 - Plot WTD from GLOBGM --------------------------------------
p_GLOBGMWTD <- ggplot() +
  geom_sf(data = data_sf_albers %>%
            mutate(WTD = ifelse(WTD_globgm < -300, -300, WTD_globgm)), # same color for outliers / block the colorbar to values within the IQ
          aes(color = WTD), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  scale_color_viridis_c(
    name = "GLOBGM WTD (m)  ",
    option = "turbo",
    breaks = c(-300, -200, -100, 0),   # Add breaks as per your data range
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

# 2 - Plot GDE frac --------------------------------------
p_GDE <- ggplot() +
  geom_sf(data = data_sf_albers,
          aes(color = GDE_frac), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  scale_color_viridis_c(
    name = "GDE fraction  ",
    option = "turbo",
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),   # Add breaks as per your data range
    labels = c("0", "0.2", "0.4", "0.6", "0.8", "1"),  # Corresponding labels for breaks
    direction = 1,
    na.value = "white"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        # panel.background = element_rect(fill = "white", color = NA),
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




# maps for supplementary information
# script to plot detailed land cover
data_sf_albers$groupings <- ifelse(data_sf_albers$land_cover_chr %in%
                                     c("water", "urban", "snow_and_ice", "bare_areas"), "other", data_sf_albers$land_cover_chr)
library(RColorBrewer)
land_cover_colors <- colorRampPalette(brewer.pal(12, "Set3"))(19)  # Using "Set3" palette

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


