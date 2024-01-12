# plot map of SHAP values of each feature/predictor

# load packages
devtools::load_all(".")
library(tidyverse)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(maps)
library(viridis) # For a better color palette
library(patchwork)
library(RColorBrewer)
library(cowplot)
library(ingestr)
library(terra)
library(rgdal)  # for readOGR
library(ggnewscale) # to add multiple scales in same ggplot
sf_use_s2(FALSE)

plot_variable <- "WTD"  # either "WTD" or "P_over_Rn"

# USA: load SHAP results
shap_forests <- readRDS("data/results_SHAP/USA_PFTgroups/WTD/SHAP_maps/Forests/shap_long.rds")
shap_savannas <- readRDS("data/results_SHAP/USA_PFTgroups/WTD/SHAP_maps/Savannas/shap_long.rds")
shap_croplands <- readRDS("data/results_SHAP/USA_PFTgroups/WTD/SHAP_maps/CRO/shap_long.rds")
shap_grasslands <- readRDS("data/results_SHAP/USA_PFTgroups/WTD/SHAP_maps/GRA/shap_long.rds")

# load train df
train_forests <- readRDS("data/results_SHAP/USA_PFTgroups/WTD/SHAP_maps/Forests/df_train_test.rds")
train_savannas <- readRDS("data/results_SHAP/USA_PFTgroups/WTD/SHAP_maps/Savannas/df_train_test.rds")
train_croplands <- readRDS("data/results_SHAP/USA_PFTgroups/WTD/SHAP_maps/CRO/df_train_test.rds")
train_grasslands <- readRDS("data/results_SHAP/USA_PFTgroups/WTD/SHAP_maps/GRA/df_train_test.rds")

# merge SHAP dataframe with original dataframe with training data
# forests
merged_forests <- shap_forests %>% # IDs are repeated for the two variables in shap df
  left_join(train_forests %>%
              dplyr::select(ID, lon, lat),
            by = "ID") %>%
  dplyr::select( # Reorder columns if necessary
         ID, lon, lat,
         variable, value, rfvalue, stdfvalue, mean_value
         )

# savannas
merged_savannas <- shap_savannas %>%
  left_join(train_savannas %>%
              dplyr::select(ID, lon, lat),
            by = "ID") %>%
  dplyr::select(
    ID, lon, lat,
    variable, value, rfvalue, stdfvalue, mean_value
  )

# For croplands
merged_croplands <- shap_croplands %>%
  left_join(train_croplands %>%
              dplyr::select(ID, lon, lat),
            by = "ID") %>%
  dplyr::select(
    ID, lon, lat,
    variable, value, rfvalue, stdfvalue, mean_value
  )

# For grasslands
merged_grasslands <- shap_grasslands %>%
  left_join(train_grasslands %>%
              dplyr::select(ID, lon, lat),
            by = "ID") %>%
  dplyr::select(
    ID, lon, lat,
    variable, value, rfvalue, stdfvalue, mean_value
  )


# Combine the four dataframes into one, adding a new column 'PFT'
df_merged <- dplyr::bind_rows(
  merged_forests %>% dplyr::mutate(PFT = "forests"),
  merged_savannas %>% dplyr::mutate(PFT = "savannas"),
  merged_croplands %>% dplyr::mutate(PFT = "croplands"),
  merged_grasslands %>% dplyr::mutate(PFT = "grasslands")
)

# saveRDS(df_merged, "df_merged.rds", compress = "xz")
# write.csv(df_merged, "df_merged.csv", row.names = FALSE)


# USA map ggplot ---------------------------------------------------------------------
# Create the USA map and transform its CRS
usa_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) # without state borders: "usa" instead of "state"
usa_map <- st_transform(usa_map, 4326)

# Convert data to an sf object
data_to_plot <- df_merged %>%
  dplyr::filter(variable == plot_variable) # filter variable to print (WTD or P_over_Rn)

data_sf <- st_as_sf(data_to_plot, coords = c("lon", "lat"), crs = 4326)

# Filter points that fall inside the USA
data_sf_filtered <- st_intersection(data_sf, usa_map)

# Set the desired Albers projection string for the continental US
albers_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Transform the filtered data to Albers projection
data_sf_albers <- st_transform(data_sf_filtered, crs = albers_crs)
usa_map_albers <- st_transform(usa_map, crs = albers_crs)

# define figure text and save name
if (plot_variable == "WTD") {
  title_text <- "SHAP values for WTD"
  legend_name <- "SHAP value for WTD  "
  save_name <- "map_SHAP_WTD.png"
} else if (plot_variable == "P_over_Rn") {
  title_text <- expression(paste("SHAP values for ", lambda, "P/R"[n]))
  legend_name <- expression(paste(lambda, "P/R"[n], " (", "-", ")"))
  save_name <- "map_SHAP_P_over_Rn.png"
}

# define colorbar
n_colors <- 11  # or another odd number to ensure white is in the middle
col_vector <- brewer.pal(n_colors, "RdBu")  # reverse to have red for negative and blue for positive

# calculate outliers
bp_stats <- boxplot.stats(data_sf_albers$value)
lower_whisker <- bp_stats$stats[1]
upper_whisker <- bp_stats$stats[5]

# assign same color to outliers (to higlight the distribution of SHAP values in the map)
data_sf_albers_cut <- data_sf_albers %>%
  mutate(value = ifelse(value < lower_whisker, lower_whisker, value),
         value = ifelse(value > upper_whisker, upper_whisker, value)
  )

# define breaks based on distribution of the variable
breaks <- c(lower_whisker, lower_whisker/2, 0, upper_whisker/2, upper_whisker)

# Create labels with ≤ and ≥ symbols
labels <- lapply(breaks, function(x) {
  if (x < 0 && x != lower_whisker/2) {
    paste("≤", scales::number_format(accuracy = 0.1)(x))
  } else if (x > 0 && x != upper_whisker/2) {
    paste("≥", scales::number_format(accuracy = 0.1)(x))
  } else {
    scales::number_format(accuracy = 0.1)(x)
  }
})


if (plot_variable == "WTD") {
  title_text <- "SHAP values for WTD"
  legend_name <- "SHAP value for WTD  "
  save_name <- "map_SHAP_WTD.png"
} else if (plot_variable == "P_over_Rn") {
  title_text <- expression(paste("SHAP values for ", lambda, "P/R"[n]))
  legend_name <- expression(paste(lambda, "P/R"[n], " (", "-", ")"))
  save_name <- "map_SHAP_P_over_Rn.png"
}

plot <- ggplot() +
  geom_sf(data = data_sf_albers_cut, aes(color = value), pch = 15, size = 0.01) +
  geom_sf(data = usa_map_albers, fill = "NA", color = "black", linewidth = 0.4) +
  scale_color_gradientn(
    name = legend_name,
    colors = col_vector,
    breaks = breaks,
    labels = labels
  ) +
  theme_minimal() +
  labs(title = title_text,
       subtitle = "(impact on SIF/PAR)") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12), legend.text = element_text(size = 10)) +
  guides(color = guide_colourbar(frame.linewidth = 0.5,
                                 ticks.linewidth = 0.5,
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 barwidth = 15,
                                 barheight = 1))

ggsave(save_name, path = "./", width = 6, height = 5.5, dpi = 600) # changing the width/height here will affect the size of the points!! aka keep this ratio approx








