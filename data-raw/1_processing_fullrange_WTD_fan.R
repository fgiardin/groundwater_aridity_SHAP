# Script to to perform some data checks and integrate WTD from Fan et al. to the final dataset

rm(list=ls())
pacman::p_load(tidyverse, xgboost, SHAPforxgboost, caret, rsample, ncdf4,
               doMC, terra, FNN)

input_folder <- "data-raw/"
df <- readRDS(paste0(input_folder, "raw.rds"))

nc_wtd <- nc_open("data-raw/WTD_Fan_full_0083deg.nc")
data_wtd <- ncvar_get(nc_wtd, "world_WTD")
lon <- ncvar_get(nc_wtd, "lon")
lat <- ncvar_get(nc_wtd, "lat")
lonlat <- expand.grid(lon=lon, lat=lat)
lonlat$WTD_Fan <- as.vector(data_wtd)
nc_close(nc_wtd)

df$WTD_Fan2 <- lonlat$WTD_Fan

# df_final <- merged_data %>% select(-lon.y, -lat.y, -index) %>%
#   rename(lon = lon.x, lat = lat.x, WTD_obs_year_mean = WTD_year_mean,
#          WTD_obs_year_std = WTD_year_std,
#          WTD_obs_mean = WTD_mean,
#          WTD_obs_std = WTD_std,
#          WTD_obs_cv = WTD_cv,
#          WTD_obs_n_sample = WTD_n_sample) %>%
#   mutate(WTD_globgm = -WTD_globgm)

color_palette <- rev(RColorBrewer::brewer.pal(999, "RdYlBu"))
ggplot(data = df) +
  geom_tile(aes(x = lon_adj, y = lat, fill = WTD_Fan2, color = NA), colour = NA) +
  scale_fill_gradientn(colors = color_palette, na.value = "transparent") +
  coord_fixed(ratio = 1.5) +
  borders("world", colour = "grey50", size = 0.4) +
  theme_minimal()

df_final <- df %>% select(-WTD_Fan) %>% rename(WTD_Fan = WTD_Fan2)
saveRDS(df_final, paste0(input_folder,"Main.rds"), compress = "xz")
write.csv(df_final, file = paste0(input_folder, "Main.csv"), row.names = F)

ggplot(data = df_final) +
  geom_point(aes(x = WTD_Fan, y = WTD_globgm))


# plots to double check
plot(df_us$WTD_Fan, df_us$WTD_globgm)
plot(df_us$WTD_obs_mean, df_us$WTD_Fan)
plot(df_us$WTD_obs_mean, df_us$WTD_globgm)








