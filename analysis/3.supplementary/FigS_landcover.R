# script to plot map of raw land cover and calculate % of irrigated croplands

# load libraries
library(tidyverse)
library(sf)
library(maps)
library(viridis)
library(ggpubr)
library(ggplot2)
library(reshape2)
library(ggtext)

# load data
df_raw <- readRDS("data/main.rds")
source("data-raw/0_land_cover_mapping.R")

# process data
df_us <- df_raw %>% dplyr::filter(lon > -125, # focus on USA
                                  lon < -65,
                                  lat < 50,
                                  lat > 24) %>%
  dplyr::select(lon, lat, land_cover_num) %>%
  mutate(land_cover = recode(land_cover_num, !!!land_cover_mapping),
         vegetation = recode(land_cover_num, !!!vegetation_flag)) %>%
  dplyr::filter(
    vegetation == 1) # only keep vegetated land area


# 0 - data preparation for plotting --------------------------------------
sf_use_s2(FALSE)

# Create USA map and transform its CRS
usa_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) # for map without state borders: "usa" instead of "state"
usa_map <- st_transform(usa_map, 4326)

data_sf <- st_as_sf(df_us, coords = c("lon", "lat"), crs = 4326)

# Filter points that fall inside the USA
data_sf_filtered <- st_intersection(data_sf, usa_map)

# Set the desired Albers projection string for the contiguous US
albers_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Transform the filtered data to Albers projection
data_sf_albers <- st_transform(data_sf_filtered, crs = albers_crs)
usa_map_albers <- st_transform(usa_map, crs = albers_crs)


# plot --------------------------------------------------------------------

p_landcover <- ggplot() +
  geom_sf(data = data_sf_albers, aes(color = land_cover), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
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
