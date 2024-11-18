# Script to verify spatial alignment of the two global WTD datasets used in this study (GLOBGM and Fan et al.)
# The final merged product is saved as .rds and .csv files for the rest of the analysis

# Load packages
rm(list=ls())
pacman::p_load(tidyverse, xgboost, SHAPforxgboost, caret, rsample, LSD, beepr, ncdf4,
               doMC, sf, maps, data.table, rnaturalearth, cowplot, terra, FNN)

# Specify the path to your TIFF file from GLOBGM v1.0 https://doi.org/10.5194/gmd-17-275-2024
raster_data <- rast("data-raw/globgm-wtd-ss.tif")
# print(raster_data)

# Define the new resolution (0.083 degrees)
new_res <- 0.08333333

# Create a template raster with the new resolution and the same extent
template <- rast(nrows = nrow(raster_data) * res(raster_data)[1] / new_res,
                 ncols = ncol(raster_data) * res(raster_data)[2] / new_res,
                 xmin = xmin(raster_data), xmax = xmax(raster_data),
                 ymin = ymin(raster_data), ymax = ymax(raster_data),
                 crs = crs(raster_data))

# Resample the original raster to the new resolution
resampled_raster <- resample(raster_data, template, method = "bilinear")
df_tif <- as.data.frame(resampled_raster, xy = TRUE, na.rm = FALSE)

input_folder <- "data/"
saveRDS(df_tif, paste0(input_folder,"globgm-wtd_resample_0083.rds"), compress = "xz") # save shap dataframe

df <- readRDS(paste0(input_folder, "main.rds"))
df_reor <- df_tif %>% arrange(y)
df$WTD_globgm <- df_reor$`globgm-wtd-ss`
df_reor$lon_adj <- ifelse(df_reor$x > 180, df_reor$x - 360, df_reor$x)

## plot to double check results
# color_palette <- rev(RColorBrewer::brewer.pal(999, "RdYlBu"))
# ggplot(data = df %>% filter(WTD_globgm < 500)) +
#   geom_tile(aes(x = lon_adj, y = lat, fill = WTD_globgm, color = NA), colour = NA) +
#   scale_fill_gradientn(colors = color_palette, na.value = "transparent") +
#   coord_fixed(ratio = 1.5) +
#   borders("world", colour = "grey50", size = 0.4) +
#   theme_minimal()

# load global WTD map from Fan et al.
nc_wtd <- nc_open("data/processed_WTD/world_WTD.nc")

data_wtd <- ncvar_get(nc_wtd, "world_WTD")
lon <- ncvar_get(nc_wtd, "longitude")
lat <- ncvar_get(nc_wtd, "latitude")
lonlat <- expand.grid(lon=lon, lat=lat)
lonlat$WTD_Fan <- as.vector(data_wtd)
nc_close(nc_wtd)

# Find the nearest neighbors
nearest <- get.knnx(data = as.matrix(df[, c("lon", "lat")]),
                    query = as.matrix(lonlat[, c("lon", "lat")]),
                    k = 1)

# Create a new data frame with the nearest neighbors
lonlat$nearest_index <- nearest$nn.index[,1]

# Add an index column to both dataframes
df <- df %>% mutate(index = row_number())
lonlat <- lonlat %>% mutate(nearest_index = nearest$nn.index[,1])

merged_data <- left_join(df,
                         lonlat,
                         by = c("index" = "nearest_index"))

## plot to double check results
# ggplot(data = merged_data) +
#   geom_tile(aes(x = lon_adj, y = lat.x, fill = WTD_Fan, color = NA), colour = NA) +
#   scale_fill_gradientn(colors = color_palette, na.value = "transparent") +
#   coord_fixed(ratio = 1.5) +
#   borders("world", colour = "grey50", size = 0.4) +
#   theme_minimal()

df_final <- merged_data %>% select(-lon.y, -lat.y, -index) %>%
  rename(lon = lon.x, lat = lat.x, WTD_obs_year_mean = WTD_year_mean,
         WTD_obs_year_std = WTD_year_std,
         WTD_obs_mean = WTD_mean,
         WTD_obs_std = WTD_std,
         WTD_obs_cv = WTD_cv,
         WTD_obs_n_sample = WTD_n_sample) %>%
  mutate(WTD_globgm = -WTD_globgm)

saveRDS(df_final, paste0(input_folder,"raw.rds"), compress = "xz")
write.csv(df_final, file = paste0(input_folder, "raw.csv"), row.names = F)

# double check everything
plot(df_us$WTD_Fan, df_us$WTD_globgm)
plot(df_us$WTD_obs_mean, df_us$WTD_Fan)
plot(df_us$WTD_obs_mean, df_us$WTD_globgm)








