rm(list=ls())
library(tidyverse)
library(LSD)
library(sf)
library(maps)
library(viridis)
library(ggpubr)
library(cowplot)

sf_use_s2(FALSE)
# 0 - prepare data --------------------------------------
input_folder <- "data/jiangong/"
model_training_folder <- 'data/jiangong/model_training_elevation_US_Fan/'
source("data/jiangong/0_land_cover_mapping.R")

df_SHAP <- readRDS(paste0(input_folder, "main.rds"))
df <- df_SHAP %>%
  drop_na(SIF, WTD_Fan, P_over_Rn, elevation, PAR) %>%
  mutate(major_land_cover = recode(land_cover_num, !!!major_land_cover_mapping)) %>%
  dplyr::filter(P_over_Rn < 3,
         P_over_Rn > 0,
         major_land_cover != "other",
         land_cover_change == 0) %>%  #irrigated crop
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

# major_types <- c("savannas_and_shrublands", "forests", "grasslands", "croplands")

# forest
df_forest <- readRDS(paste0(model_training_folder, "cshap_long_forests.rds"))
ori_forest <- as.data.frame(df_forest$x_test)
df_SHAP_forest <- df %>% dplyr::filter(major_land_cover == "forests")

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
unique(df$major_land_cover)

# savannas_and_shrublands
df_savannas_and_shrublands <- readRDS(paste0(model_training_folder, "cshap_long_savannas_and_scrublands.rds"))
ori_savannas_and_shrublands <- as.data.frame(df_savannas_and_shrublands$x_test)
df_SHAP_savannas_and_shrublands <- df %>% filter(major_land_cover == "savannas_and_shrublands")

shap_savannas_and_shrublands <- df_savannas_and_shrublands$dt %>%
  mutate(WTD_rfvalue = ori_savannas_and_shrublands$WTD,
         Aridity_rfvalue = ori_savannas_and_shrublands$Aridity,
         Elevation_rfvalue = ori_savannas_and_shrublands$Elevation,
         lon = df_SHAP_savannas_and_shrublands$lon,
         lat = df_SHAP_savannas_and_shrublands$lat,
         Land_cover = df_SHAP_savannas_and_shrublands$major_land_cover,
         GDE_frac = df_SHAP_savannas_and_shrublands$GDE_frac)
rm(df_savannas_and_shrublands)
rm(ori_savannas_and_shrublands)
rm(df_SHAP_savannas_and_shrublands)

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

df_total <- rbind(shap_forest, shap_savannas_and_shrublands,
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


a <- ggplot() +
  geom_sf(data = data_sf_albers %>%
            mutate(WTD = ifelse(WTD > 0.6, 0.6, WTD),
                   WTD = ifelse(WTD < -0.8, -0.8, WTD)),
          aes(color = WTD), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  geom_text(data = state_centroids_albers,
            aes(x = lon, y = lat, label = state_abb),
            size = 3, color = "black") +
  labs(x = "Lon",y = "Lat") +
  scale_color_viridis_c(
    name = expression(paste('Shapley values for WTD (sr'^-1~'nm'^-1*')   ')),
    option = "turbo",
    breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6),
    labels = c("≤-0.8", "-0.6", "-0.4", "-0.2", "0", "0.2","0.4", "≥0.6"),
    direction = -1) +
  coord_sf(expand = TRUE) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 12), legend.text = element_text(size = 10),
        panel.spacing = unit(0, "lines")) +
  guides(color = guide_colourbar(frame.linewidth = 0.5,
                                 ticks.linewidth = 0.5,
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 barwidth = 15,
                                 barheight = 1))


# updated Shapley plot ----------------------------------------------------

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(sf)
library(scales)
library(RColorBrewer)

# Data preparation as before
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

# Define colorbar parameters
n_colors <- 11  # Adjust if needed
col_vector <- brewer.pal(n_colors, "RdBu")

# Calculate boxplot statistics and outliers
bp_stats <- boxplot.stats(data_sf_albers$WTD)  # Use 'WTD' instead of 'value'
lower_whisker <- bp_stats$stats[1]
upper_whisker <- bp_stats$stats[5]

# Assign same color to outliers
data_sf_albers_cut <- data_sf_albers %>%
  mutate(WTD = ifelse(WTD < lower_whisker, lower_whisker, WTD),
         WTD = ifelse(WTD > upper_whisker, upper_whisker, WTD))

# Define breaks and labels for the color scale
breaks <- c(lower_whisker, lower_whisker/2, 0, upper_whisker/2, upper_whisker)
labels <- lapply(breaks, function(x) {
  if (x < 0 && x != lower_whisker/2) {
    paste("≤", scales::number_format(accuracy = 0.1)(x))
  } else if (x > 0 && x != upper_whisker/2) {
    paste("≥", scales::number_format(accuracy = 0.1)(x))
  } else {
    scales::number_format(accuracy = 0.1)(x)
  }
})

# Plot the data with the custom colorbar
a <- ggplot() +
  geom_sf(data = data_sf_albers_cut, aes(color = WTD), pch = 15, size = 0.1) +  # Now plotting WTD with updated colors
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.5) +
  # geom_text(data = state_centroids_albers,
  #           aes(x = lon, y = lat, label = state_abb),
  #           size = 3.5, color = "black",
  #           fontface = "bold") +
  labs(x = "Lon", y = "Lat") +
  scale_color_gradientn(
    name = expression(paste('Shapley values for WTD (sr'^-1~'nm'^-1*')   ')),
    colors = col_vector,
    breaks = breaks,
    labels = labels
  ) +
  coord_sf(expand = TRUE) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 10.5), legend.text = element_text(size = 10),
        panel.spacing = unit(0, "lines")) +
  guides(color = guide_colourbar(frame.linewidth = 0.5,
                                 ticks.linewidth = 0.5,
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 barwidth = 14,
                                 barheight = 1))

ggsave("map_cSHAP_WTD.png", path = "./", width = 6, height = 5.5, dpi = 600)

# 2 - map land cover --------------------------------------
data_lc_sf <- st_as_sf(df_land_cover, coords = c("lon", "lat"), crs = 4326)
# Filter points that fall inside the USA
data_lc_sf_filtered <- st_intersection(data_lc_sf, usa_map)
# Transform the filtered data to Albers projection
data_lc_sf_albers <- st_transform(data_lc_sf_filtered, crs = albers_crs)

# Colorblind-friendly colors
savanna_shrub_color <- "#8C510A" # Brownish color
cropland_color <- "#D9BF77" # Golden yellow
forest_color <- "#006837"      # A deeper green
grassland_color <- "#76C476"   # A more pastel-like green
other_color <- "grey"

# Plotting
b <- ggplot() +
  geom_sf(data = data_lc_sf_albers, aes(color = major_land_cover), pch = 15, size = 0.1) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  scale_color_manual(
    name = "Land Cover",
    values = c("forests" = forest_color,
               "savannas_and_shrublands" = savanna_shrub_color,
               "grasslands" = grassland_color,
               "croplands" = cropland_color,
               "other" = other_color),
    guide = guide_legend(
      title = "",
      override.aes = list(size = 4),
      nrow = 2)) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12), legend.text = element_text(size = 10),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        aspect.ratio = 0.55)

# 3 - compare GDE and non-GDE --------------------------------------
df_GDE <- df_total %>%
  mutate(GDE = case_when(
    is.na(GDE_frac) ~ "non-GDE",
    GDE_frac >= 0.6 ~ "GDE",
    TRUE ~ "NA"),
    WTD = abs(WTD)) %>%
  dplyr::filter(GDE != "NA",
         Land_cover == "savannas_and_shrublands") %>%
  dplyr::select(GDE, WTD)


# df_total %>% mutate(GDE = case_when(
#   is.na(GDE_frac) ~ "non-GDE",
#   GDE_frac >= 0.6 ~ "GDE",
#   TRUE ~ "NA")) %>%
#   group_by(Land_cover, GDE) %>%
#   summarise(n = n())

# Cropland GDE propotion: 32 * 100 /(32+2178+24715) = 0.11%
# Forest GDE propotion: 150 * 100 /(150+1131+51604) = 0.28%
# savannas_and_shrublands GDE propotion: 3134 * 100 /(3134+4867+21421) = 10.7%
# grasslands GDE propotion: 668 * 100 /(668+4563+20217) = 2.6%

# data does not follow normal distribution
# `summarise()` has grouped output by 'GDE'. You can override using the `.groups` argument.
# # A tibble: 2 × 3
# # Groups:   GDE [1]
# GDE   Land_cover               p_value
# <chr> <chr>                      <dbl>
#   1 GDE   grasslands              7.34e-13
# 2 GDE   savannas_and_shrublands 1.24e-33

# Perform Mann-Whitney U test (Wilcoxon rank-sum test)
test_result <- wilcox.test(WTD ~ GDE, data = df_GDE)
# Calculate sample sizes
sample_sizes <- df_GDE %>%
  group_by(GDE) %>%
  summarise(n = n())

p_value <- wilcox.test(WTD ~ GDE, data = df_GDE)$p.value

# Create the plot
c <- ggplot(df_GDE, aes(x = GDE, y = WTD, fill = GDE)) +
  geom_violin(trim = FALSE, alpha = 0.5, color = "darkgrey") +
  geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
  annotate("text", x = 1.5, y = 0.29, label = paste("italic(p) ==", signif(p_value, 3)), parse = TRUE, size = 5) +
  scale_x_discrete(labels = paste0(sample_sizes$GDE, "\n(n = ", sample_sizes$n, ")")) +
  scale_y_continuous(limits = c(0, 0.3)) +
  labs(y = expression(atop('Absolute Shapley values', paste('for WTD (sr'^-1~'nm'^-1*')')))) +
  scale_fill_manual(values = c("#DCE6F0", "#A7C6EA")) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 13),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        aspect.ratio = 0.5)


# group plots
right_col <- plot_grid(b, c, ncol = 1, nrow = 2,align = "v",
                       rel_heights = c(1, 0.9), labels = c("b", "c"))

all <- plot_grid(a, right_col, NULL, ncol = 3, nrow = 1,
                 rel_widths = c(1, 0.5, 0.05), labels = c("a", "",NULL))

ggsave("Fig_4.pdf", plot = all,
       path = "./",
       dpi=600, width = 13.5, height = 6.5)



# png(filename = paste0("/Users/jiangongliu/Desktop/5_WTD_Aridity/figure/",
#                       "Fig_2_1.png"),
#     width = 13.5, height = 7.5, units = "in", res = 600)
# print(all)
# dev.off()





