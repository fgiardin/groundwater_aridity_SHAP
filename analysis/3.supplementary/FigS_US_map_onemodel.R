# plot map of Causal Shapley values of the Moisture Index

library(tidyverse)
library(LSD)
library(sf)
library(maps)
library(viridis)
library(ggpubr)
library(cowplot)
library(scales)
library(RColorBrewer)

sf_use_s2(FALSE)

# load data --------------------------------------
input_folder <- "data/jiangong/"
model_training_folder <- 'data/jiangong/model_training_elevation_US_Fan_oneModel/'
source("data/jiangong/0_land_cover_mapping.R")

df_SHAP <- readRDS(paste0(input_folder, "main.rds"))

df <- df_SHAP %>%
  drop_na(SIF, WTD_Fan, P_over_Rn, elevation, PAR) %>%
  mutate(major_land_cover = recode(land_cover_num, !!!major_land_cover_mapping),
         SIF_over_PAR = SIF * 10^3 / PAR) %>%
  filter(P_over_Rn < 3,
         P_over_Rn > 0,
         major_land_cover != "other",
         land_cover_change == 0) %>%
  filter(lon > -125, # focus on USA
         lon < -65,
         lat < 50,
         lat > 24) %>%
  select(lon, lat, SIF_over_PAR, WTD_Fan, P_over_Rn, elevation, major_land_cover, GDE_frac) %>%
  rename(WTD = WTD_Fan,
         Aridity = P_over_Rn,
         Elevation = elevation)

df_land_cover <- df_SHAP %>%
  mutate(major_land_cover = recode(land_cover_num, !!!major_land_cover_mapping)) %>%
  filter(lon > -125, # focus on USA
         lon < -65,
         lat < 50,
         lat > 24) %>%
  select(lon, lat, major_land_cover)

# Determine the size of each chunk

df.main <- data.frame()

for (i in 1:3) {
  df_SHAP <- readRDS(paste0(model_training_folder, "cshap_long_", i, ".rds"))$dt
  df_sub <- readRDS(paste0(model_training_folder, "df_sub_", i, ".rds"))
  df_SHAP <- df_SHAP %>%
    mutate(lon = df_sub$lon, lat = df_sub$lat,
           Land_cover = df_sub$major_land_cover,
           GDE_frac = df_sub$GDE_frac)
  df.main <- rbind(df.main, df_SHAP)
}

# prepare data for plotting --------------------------------------
# Create USA map and transform its CRS
usa_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) # for map without state borders: "usa" instead of "state"
usa_map <- st_transform(usa_map, 4326)

data_sf <- st_as_sf(df.main, coords = c("lon", "lat"), crs = 4326)

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

## NEW COLORBAR
# Define the number of colors
n_colors <- 11
col_vector <- brewer.pal(n_colors, "RdBu")

## Outliers
# hist(data_sf_albers$WTD) -> skewed distribution, use quantiles to calculate outliers
# quantiles don't assume a symmetrical distribution (the traditional boxplot method does)
# Calculate the empirical percentiles for outlier detection (remove 1% of data in both directions)
lower_threshold <- quantile(data_sf_albers$Aridity, probs = 0.005)
upper_threshold <- quantile(data_sf_albers$Aridity, probs = 0.995)

# Removed outliers based on the calculated thresholds
data_sf_albers_cut <- data_sf_albers %>%
  mutate(Aridity = ifelse(Aridity < lower_threshold, lower_threshold, Aridity),
         Aridity = ifelse(Aridity > upper_threshold, upper_threshold, Aridity))

# Define breaks, making sure zero is included
breaks <- seq(floor(lower_threshold * 10) / 10, ceiling(upper_threshold * 10) / 10, by = 0.1)
breaks <- round(breaks, digits = 10) # Round very small values to zero
breaks <- ifelse(breaks == 0, 0, breaks) # remove decimal point

# define labels
labels <- lapply(breaks, function(x) {
  formatted_value <- if (x == 0) {
    "0"  # Display zero as "0" instead of "0.0"
  } else {
    scales::number_format(accuracy = 0.1)(x)
  }

  if (x == breaks[1]) {
    paste0("≤",formatted_value)
  } else if (x == breaks[length(breaks)]) {
    paste0("≥", formatted_value)
  } else {
    formatted_value
  }
})

# Split colors into two gradients: one for negative and one for positive values
n_negative_colors <- sum(breaks < 0)
n_positive_colors <- sum(breaks > 0)
col_vector <- col_vector[col_vector != "#F7F7F7"] # exclude central color ("white", which must correspond to zero)
col_vector_negative <- colorRampPalette(col_vector[1:(n_colors / 2)])(n_negative_colors)
col_vector_positive <- colorRampPalette(col_vector[(n_colors / 2 + 1):n_colors])(n_positive_colors)

# Combine the two color scales
final_col_vector <- c(col_vector_negative, "#F7F7F7", col_vector_positive)

length(col_vector)
length(final_col_vector)
length(breaks)

# new plot --------------------------------------
a <- ggplot() +
  geom_sf(data = data_sf_albers_cut, aes(color = Aridity), pch = 15, size = 0.1) +  # Now plotting Aridity with updated colors
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.5) +
  scale_color_gradientn(
    name = expression(paste('Shapley values for λP/R'[n], ' (sr'^-1~'nm'^-1*')  ')),
    colors = final_col_vector,
    breaks = breaks,
    labels = ifelse(seq_along(labels) %% 2 == 0, "", labels),  # Only display every other label (replace with space)
    limits = range(breaks)
  ) +
  coord_sf(expand = TRUE) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 10), legend.text = element_text(size = 8.8),
        legend.spacing = unit(0.5, "cm"),  # spacing between legend elements
        panel.spacing = unit(0, "lines"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text = element_blank(),         # Remove axis text (lat/lon numbers)
        axis.ticks = element_blank(),        # Remove axis ticks
        panel.background = element_blank(),  # Remove background panel
        panel.border = element_blank()      # Remove panel border (if any)
  ) +
  guides(color = guide_colourbar(frame.linewidth = 0.5,
                                 ticks.linewidth = 0.5,
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 barwidth = 15,
                                 barheight = 1))

ggsave("FigS_US_map_onemodel.png", path = "./", width = 6, height = 5.5, dpi = 300)
