# script to plot maps of the main variables used in this study (SIF/PAR, P/Rn, WTD, PFT)

devtools::load_all(".")
library(tidyverse)
library(sf)
library(maps)
library(viridis)
sf_use_s2(FALSE)

df_raw <- readRDS("data/reprocessed_intmeans/dataframes/SIF_no_neg/df_SHAP.rds") # SIF means calculated with negative values

df_raw <- readRDS("data/reprocessed_intmeans/dataframes/df_SHAP.rds") # SIF means calculated with negative values

# Create USA map and transform its CRS
usa_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) # for map without state borders: "usa" instead of "state"
usa_map <- st_transform(usa_map, 4326)

# Convert data to an sf object
data_sf <- st_as_sf(df_raw %>%
                      dplyr::filter(lon > -125, # focus on USA
                                    lon < -65,
                                    lat < 50,
                                    lat > 24),
                    coords = c("lon", "lat"), crs = 4326)

# Filter points that fall inside the USA
data_sf_filtered <- st_intersection(data_sf, usa_map)

# Set the desired Albers projection string for the contiguous US
albers_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Transform the filtered data to Albers projection
data_sf_albers <- st_transform(data_sf_filtered, crs = albers_crs) %>%
  dplyr::filter(PFT != "WET") # remove wetlands
usa_map_albers <- st_transform(usa_map, crs = albers_crs)


# plots -------------------------------------------------------------------

# plot SIF/PAR
a <- ggplot() +
  geom_sf(data = data_sf_albers,
          aes(color = SIF_over_PAR), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  scale_color_viridis_c(
    name = expression(paste("SIF/PAR x 10"^5 * " (sr"^-1 * "nm"^-1 * ")")),
    option = "turbo",
    breaks = c(-1.082395e-06, 1e-05, 2e-05, 3e-05),   # Add breaks as per your data range.
    labels = c("0", "1", "2", "3"),  # Corresponding labels for breaks
    direction = -1
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12), legend.text = element_text(size = 10)) +
  guides(color = guide_colourbar(frame.linewidth = 0.5,
                                 ticks.linewidth = 0.5,
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 barwidth = 15,
                                 barheight = 1))

# plot P/Rn
b <- ggplot() +
  geom_sf(data = data_sf_albers %>% mutate(P_over_Rn = ifelse(P_over_Rn > 2, 2, P_over_Rn)), # assign same color to outliers
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
        legend.title = element_text(size = 12), legend.text = element_text(size = 10)) +
  guides(color = guide_colourbar(frame.linewidth = 0.5,
                                 ticks.linewidth = 0.5,
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 barwidth = 15,
                                 barheight = 1))

# plot WTD
c <- ggplot() +
  geom_sf(data = data_sf_albers %>% mutate(WTD = ifelse(WTD < -100, -100, WTD)), # same color for outliers / block the colorbar to values within the IQ
          aes(color = WTD), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  scale_color_viridis_c(
    name = "WTD   ",
    option = "turbo",
    breaks = c(-100, -75, -50, -25, -5.684342e-14),   # Add breaks as per your data range
    labels = c("≤-100", "-75", "-50", "-25","0"),  # Corresponding labels for breaks
    direction = -1
  ) +
  theme_minimal() +
  # labs(title = "WTD") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12), legend.text = element_text(size = 10)) +
  guides(color = guide_colourbar(frame.linewidth = 0.5,
                                 ticks.linewidth = 0.5,
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 barwidth = 15,
                                 barheight = 1))

# PFTs
# Create a new variable for groupings in the data_sf_albers data frame
data_sf_albers$groupings <- ifelse(data_sf_albers$PFT %in% c("DBF", "EBF", "ENF", "MF"), "Forests",
                                   ifelse(data_sf_albers$PFT %in% c("SAV", "WSA", "CSH", "OSH"), "Savannas and shrublands",
                                          ifelse(data_sf_albers$PFT == "GRA", "Grasslands",
                                                 ifelse(data_sf_albers$PFT == "CRO", "Croplands", NA))))

# Colorblind-friendly colors
savanna_shrub_color <- "#8C510A" # Brownish color
cropland_color <- "#D9BF77" # Golden yellow
forest_color <- "#006837"      # A deeper green
grassland_color <- "#76C476"   # A more pastel-like green

# Plotting
d <- ggplot() +
  geom_sf(data = data_sf_albers, aes(color = groupings), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  scale_color_manual(
    name = "Land Cover",
    values = c("Forests" = forest_color,
               "Savannas and shrublands" = savanna_shrub_color,
               "Grasslands" = grassland_color,
               "Croplands" = cropland_color),
    guide = guide_legend(
      title = "PFT",
      override.aes = list(size = 4)  # Adjusting point size in the legend here
    )
  ) +
  theme_minimal() +
  # labs(title = "PFT") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12), legend.text = element_text(size = 10))

# group plots
all <- ggarrange(a, b, c, d,
                 labels = "auto",
                 ncol = 2, nrow = 2
                 )

ggsave("map_features.png", path = "./", width = 12, height = 11, dpi = 600)
