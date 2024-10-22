rm(list=ls())
library(tidyverse)
library(LSD)
library(sf)
library(maps)
library(viridis)
library(ggpubr)

sf_use_s2(FALSE)
# 0 - prepare data --------------------------------------
input_folder <- "/Users/jiangongliu/Desktop/5_WTD_Aridity/new_dataframe/"
model_training_folder <- '/Users/jiangongliu/Desktop/5_WTD_Aridity/new_dataframe/model_training_elevation_US_Fan/'
source("/Users/jiangongliu/Desktop/5_WTD_Aridity/code/0_land_cover_mapping.R")

df_SHAP <- readRDS(paste0(input_folder, "main.rds"))
df <- df_SHAP %>% 
  drop_na(SIF, WTD_Fan, P_over_Rn, elevation, PAR) %>% 
  mutate(major_land_cover = recode(land_cover_num, !!!major_land_cover_mapping)) %>% 
  filter(P_over_Rn < 3,
         P_over_Rn > 0,
         major_land_cover != "other",
         land_cover_change == 0) %>%  #irrigated crop
  filter(lon > -125, # focus on USA
         lon < -65,
         lat < 50,
         lat > 24) %>%
  select(lon, lat, GDE_frac, major_land_cover)

# forest
df_forest <- readRDS(paste0(model_training_folder, "cshap_long_forests.rds"))
ori_forest <- as.data.frame(df_forest$x_test)
df_SHAP_forest <- df %>% filter(major_land_cover == "forests")

shap_forest <- df_forest$dt %>% 
  mutate(WTD_rfvalue = ori_forest$WTD,
         Aridity_rfvalue = ori_forest$Aridity, 
         Elevation_rfvalue = ori_forest$Elevation,
         lon = df_SHAP_forest$lon,
         lat = df_SHAP_forest$lat,
         Land_cover = df_SHAP_forest$major_land_cover,
         GDE_frac = df_SHAP_forest$GDE_frac)
rm(df_forest)
rm(ori_forest)
rm(df_SHAP_forest)

# savannas_and_scrublands
df_savannas_and_scrublands <- readRDS(paste0(model_training_folder, "cshap_long_savannas_and_scrublands.rds"))
ori_savannas_and_scrublands <- as.data.frame(df_savannas_and_scrublands$x_test)
df_SHAP_savannas_and_scrublands <- df %>% filter(major_land_cover == "savannas_and_scrublands")

shap_savannas_and_scrublands <- df_savannas_and_scrublands$dt %>% 
  mutate(WTD_rfvalue = ori_savannas_and_scrublands$WTD,
         Aridity_rfvalue = ori_savannas_and_scrublands$Aridity, 
         Elevation_rfvalue = ori_savannas_and_scrublands$Elevation,
         lon = df_SHAP_savannas_and_scrublands$lon,
         lat = df_SHAP_savannas_and_scrublands$lat,
         Land_cover = df_SHAP_savannas_and_scrublands$major_land_cover,
         GDE_frac = df_SHAP_savannas_and_scrublands$GDE_frac)
rm(df_savannas_and_scrublands)
rm(ori_savannas_and_scrublands)
rm(df_SHAP_savannas_and_scrublands)

# croplands
df_croplands <- readRDS(paste0(model_training_folder, "cshap_long_croplands.rds"))
ori_croplands <- as.data.frame(df_croplands$x_test)
df_SHAP_croplands <- df %>% filter(major_land_cover == "croplands")

shap_croplands <- df_croplands$dt %>% 
  mutate(WTD_rfvalue = ori_croplands$WTD,
         Aridity_rfvalue = ori_croplands$Aridity, 
         Elevation_rfvalue = ori_croplands$Elevation,
         lon = df_SHAP_croplands$lon,
         lat = df_SHAP_croplands$lat,
         Land_cover = df_SHAP_croplands$major_land_cover,
         GDE_frac = df_SHAP_croplands$GDE_frac)
rm(df_croplands)
rm(ori_croplands)
rm(df_SHAP_croplands)

# grasslands
df_grasslands <- readRDS(paste0(model_training_folder, "cshap_long_grasslands.rds"))
ori_grasslands <- as.data.frame(df_grasslands$x_test)
df_SHAP_grasslands <- df %>% filter(major_land_cover == "grasslands")

shap_grasslands <- df_grasslands$dt %>% 
  mutate(WTD_rfvalue = ori_grasslands$WTD,
         Aridity_rfvalue = ori_grasslands$Aridity, 
         Elevation_rfvalue = ori_grasslands$Elevation,
         lon = df_SHAP_grasslands$lon,
         lat = df_SHAP_grasslands$lat,
         Land_cover = df_SHAP_grasslands$major_land_cover,
         GDE_frac = df_SHAP_grasslands$GDE_frac)
rm(df_grasslands)
rm(ori_grasslands)
rm(df_SHAP_grasslands)

df_total <- rbind(shap_forest, shap_savannas_and_scrublands,
                  shap_croplands, shap_grasslands)

# 1 - map Shapley values --------------------------------------
# Create USA map and transform its CRS
usa_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) # for map without state borders: "usa" instead of "state"
usa_map <- st_transform(usa_map, 4326)

data_sf <- st_as_sf(df_total, coords = c("lon", "lat"), crs = 4326)

# Filter points that fall inside the USA
data_sf_filtered <- st_intersection(data_sf, usa_map)

# Set the desired Albers projection string for the contiguous US
albers_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Transform the filtered data to Albers projection
data_sf_albers <- st_transform(data_sf_filtered, crs = albers_crs)
usa_map_albers <- st_transform(usa_map, crs = albers_crs)

# Get state centroids and abbreviations
usa_states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
state_centroids <- st_centroid(usa_states)
state_centroids <- state_centroids %>%
  mutate(state_name = tolower(ID),  # Lowercase to match with state.name
         state_abb = state.abb[match(state_name, tolower(state.name))])

# Transform centroids to Albers projection
state_centroids_albers <- st_transform(state_centroids, crs = albers_crs)

# Extract coordinates for labeling
state_centroids_albers <- state_centroids_albers %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])

# Create the plot with state abbreviations
a <- ggplot() +
  geom_sf(data = data_sf_albers %>% 
            mutate(Aridity = ifelse(Aridity > 1.8, 1.8, Aridity),
                   Aridity = ifelse(Aridity < -1.2, -1.2, Aridity)),
          aes(color = Aridity), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  # Add state abbreviations
  geom_text(data = state_centroids_albers, 
            aes(x = lon, y = lat, label = state_abb), 
            size = 3, color = "black") +
  scale_color_viridis_c(
    name = expression(paste('Shapley values for Aridity (sr'^-1~'nm'^-1*')')),
    option = "turbo",
    breaks = c(-1.2, -0.6, 0, 0.6, 1.2, 1.8),   # Add breaks as per your data range.
    labels = c("≤-1.2", "-0.6", "0", "0.6", "1.2", "≥1.8"),  # Corresponding labels for breaks
    direction = -1
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10)) +
  guides(color = guide_colourbar(frame.linewidth = 0.5,
                                 ticks.linewidth = 0.5,
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 barwidth = 12,
                                 barheight = 1))

# Save the plot
png(filename = paste0("/Users/jiangongliu/Desktop/5_WTD_Aridity/figure/", 
                      "FigS_USmap_Aridity.png"), 
    width = 7, height = 5, units = "in", res = 600)
print(a)
dev.off()



