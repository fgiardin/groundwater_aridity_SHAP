# script to put together pre-processed dataframes of all variables and
# create a single reference dataframe

rm(list=ls())
pacman::p_load(tidyverse, ncdf4, terra, bigleaf)
# RColorBrewer,rnaturalearth,scales,
# maps,sf,lubridate, cowplot, reshape2,viridis
input_folder <- "data/"
output_folder <- "data/"

# SIF ---------------------------------------------------------------------
df_SIF <- readRDS(paste0(output_folder, "df_SIF_summary.rds")) %>% arrange(lat)

# PAR ---------------------------------------------------------------------
nc_par <- nc_open(paste0(input_folder, "PAR_2018_2021_0083.nc"))
data_par <- ncvar_get(nc_par, "PAR")
lon <- ncvar_get(nc_par, "lon")
lat <- ncvar_get(nc_par, "lat")
land_mask <- !is.na(data_par) # we use
ocean_mask <- is.na(data_par)
lonlat <- expand.grid(lon=lon, lat=lat)
nc_close(nc_par)

# TEMP ---------------------------------------------------------------------
nc_temp <- nc_open(paste0(input_folder, "t2m_2018_2021_0083.nc"))
data_temp <- ncvar_get(nc_temp, "t2m")
data_temp[!land_mask] <- NA
nc_close(nc_temp)

# SNSR ------------------------------------------------------------------
# surface_net_solar_radiation
nc_ssr <- nc_open(paste0(input_folder, "ssr_2018_2021_0083.nc"))
data_ssr <- ncvar_get(nc_ssr, "ssr")
data_ssr[!land_mask] <- NA
nc_close(nc_ssr)

# SNTR ------------------------------------------------------------------
# surface_net_thermal_radiation
nc_str <- nc_open(paste0(input_folder, "str_2018_2021_0083.nc"))
data_str <- ncvar_get(nc_str, "str")
data_str[!land_mask] <- NA
nc_close(nc_str)

# PRECIP ------------------------------------------------------------------
# Read the NetCDF file and extract CRS and geotransform
dname = "//precipitation"
nc <- nc_open(paste0(input_folder, "processing_original_file/precipitation_soilT_pressure_wind.nc"))
crs_wkt <- ncatt_get(nc, "spatial_ref", "crs_wkt")$value
lon <- ncvar_get(nc, "x")
lat <- ncvar_get(nc, "y")
time <- ncvar_get(nc, "time")

# Calculate extent based on lon/lat values
xmin <- min(lon)
xmax <- max(lon)
ymin <- min(lat)
ymax <- max(lat)
nc_close(nc)

# Create the extent object from the lon/lat values
ext <- ext(xmin, xmax, ymin, ymax)
precip_rast <- rast(paste0(input_folder, "processing_original_file/precipitation_soilT_pressure_wind.nc"), dname)
crs(precip_rast) <- crs_wkt
ext(precip_rast) <- ext

# Define the indices corresponding to the period from 2018-03-01 to 2021-03-31
start_index <- 4
end_index <- 145

# Subset the raster to these indices
precip_subset <- precip_rast[[start_index:end_index]]
inter_precip <- mean(precip_rast, na.rm = TRUE)*365/8
writeCDF(inter_precip, paste0(input_folder, "inter_precip.nc"), overwrite=TRUE)

# convert to dataframe for plotting
df_precip <- terra::as.data.frame(inter_precip, xy = TRUE)
names(df_precip) <- c("lon", "lat", "PRECIP")
saveRDS(df_precip, paste0("/Users/jiangongliu/Desktop/Projects/Francesco/revision/new_dataframe/",
                       "df_PRECIP.rds"), compress = "xz")

# Elevation ---------------------------------------------------------------------
nc_elevation <- nc_open(paste0(input_folder, "GMTED2010_15n015_0083deg.nc"))
data_elevation <- ncvar_get(nc_elevation, "elevation")
data_elevation[!land_mask] <- NA
nc_close(nc_elevation)

# land cover ------------------------------------------------------------------
nc_lc <- nc_open(paste0(input_folder, "C3S-LC-L4-LCCS-Map-0083-degree-P1Y-2020-v2.1.1.nc"))
print(nc_lc)
data_lc <- ncvar_get(nc_lc, "lccs_class")
lon1 <- ncvar_get(nc_lc, "lon")
lat1 <- ncvar_get(nc_lc, "lat")
lonlat1 <- expand.grid(lon=lon1, lat=lat1)
lonlat1$Land_cover <- as.vector(data_lc)
lonlat1 <- lonlat1 %>% arrange(lat)
nc_close(nc_lc)

nc_lc_1992 <- nc_open(paste0(input_folder, "C3S-LC-L4-LCCS-Map-0083-degree-P1Y-1992-v2.0.7.nc"))
data_lc_1992 <- ncvar_get(nc_lc_1992, "lccs_class")
lon1 <- ncvar_get(nc_lc_1992, "lon")
lat1 <- ncvar_get(nc_lc_1992, "lat")
lonlat1 <- expand.grid(lon=lon1, lat=lat1)
lonlat1$Land_cover_1992 <- as.vector(data_lc_1992)
lonlat1 <- lonlat1 %>% arrange(lat)
nc_close(nc_lc_1992)

nc_lc_2000 <- nc_open(paste0(input_folder, "C3S-LC-L4-LCCS-Map-0083-degree-P1Y-2000-v2.0.7.nc"))
data_lc_2000 <- ncvar_get(nc_lc_2000, "lccs_class")
lon1 <- ncvar_get(nc_lc_2000, "lon")
lat1 <- ncvar_get(nc_lc_2000, "lat")
lonlat2 <- expand.grid(lon=lon1, lat=lat1)
lonlat2$Land_cover_2000 <- as.vector(data_lc_2000)
lonlat2 <- lonlat2 %>% arrange(lat)
nc_close(nc_lc_2000)

nc_lc_2010 <- nc_open(paste0(input_folder, "C3S-LC-L4-LCCS-Map-0083-degree-P1Y-2010-v2.0.7.nc"))
data_lc_2010 <- ncvar_get(nc_lc_2010, "lccs_class")
lon1 <- ncvar_get(nc_lc_2010, "lon")
lat1 <- ncvar_get(nc_lc_2010, "lat")
lonlat3 <- expand.grid(lon=lon1, lat=lat1)
lonlat3$Land_cover_2010 <- as.vector(data_lc_2010)
lonlat3 <- lonlat3 %>% arrange(lat)
nc_close(nc_lc_2010)

lonlat1$Land_cover_2000 <- lonlat2$Land_cover_2000
lonlat1$Land_cover_2010 <- lonlat3$Land_cover_2010
lonlat1$Land_cover_2020 <- lonlat$land_cover_num

lonlat1 <- lonlat1 %>%
  mutate(land_cover_change = ifelse(Land_cover_1992 == Land_cover_2000 &
                                    Land_cover_2000 == Land_cover_2010 &
                                    Land_cover_2010 == Land_cover_2020, 0, 1))

lonlat <- readRDS(paste0(output_folder, "data.rds")) %>% arrange(lat)
lonlat$land_cover_1992 <- lonlat1$Land_cover_1992
lonlat$land_cover_2000 <- lonlat1$Land_cover_2000
lonlat$land_cover_2010 <- lonlat1$Land_cover_2010
lonlat$land_cover_change <- lonlat1$land_cover_change

# Merge all ------------------------------------------------------------------
# df_precip <- readRDS(paste0(output_folder, "df_precip.rds")) %>% arrange(lat)
# adjust longitude for plotting
lonlat$lon_adj <- ifelse(lonlat$lon > 180, lonlat$lon - 360, lonlat$lon)

# Add SIF
# lonlat$SIF <- df_SIF$SIF
lonlat$PAR <- as.vector(data_par)
lonlat$TEMP <- as.vector(data_temp) - 273.15
lonlat$NETRAD <- (as.vector(data_ssr) + as.vector(data_str)) / 86400 # convert from J m-2 to W m-2
lonlat$PRECIP <- df_precip$PRECIP

# Import land cover mapping
source("data-raw/0_land_cover_mapping.R")
lonlat1 <- lonlat1 %>%
  mutate(land_cover_code = as.character(Land_cover)) %>%
  mutate(land_cover = recode(land_cover_code, !!!land_cover_mapping),
         vegetation_flag = recode(land_cover_code, !!!vegetation_flag))

lonlat$land_cover_num <- lonlat1$Land_cover
lonlat$land_cover_chr <- lonlat1$land_cover
lonlat$veg_flag <- lonlat1$vegetation_flag

lonlat_ <- lonlat %>%
  mutate(lon = df_SIF$lon,
         lat = df_SIF$lat,
         lon_adj = ifelse(lon > 180, lon - 360, lon),
         SIF_2018 = df_SIF$SIF_2018,
         SIF_2019 = df_SIF$SIF_2019,
         SIF_2020 = df_SIF$SIF_2020,
         SIF = df_SIF$SIF_mean,
         SIF_std = df_SIF$SIF_sd,
         SIF_cv = df_SIF$SIF_cv)

wtd_obs <- read.csv(paste0(output_folder, "DepthToGroundwater_Grouped.csv"))
lonlat_$WTD_year_mean <- wtd_obs$mean_year
lonlat_$WTD_year_std <- wtd_obs$std_year
lonlat_$WTD_mean <- wtd_obs$mean_DepthToWater_m
lonlat_$WTD_std <- wtd_obs$std_DepthToWater_m
lonlat_$WTD_cv <- wtd_obs$std_DepthToWater_m/wtd_obs$mean_DepthToWater_m
lonlat_$WTD_n_sample <- wtd_obs$n_samples

lonlat2 <- lonlat_ %>%
  mutate(NETRAD = LE.to.ET(NETRAD, TEMP)) %>% # convert Rn from W/m2 to mm/s
  mutate(NETRAD = NETRAD*86400*365) %>% # convert Rn from mm/s to mm/year
  mutate(SIF_W = SIF/1000) %>% # convert SIF from mW to W
  mutate(SIF_over_PAR = SIF_W/PAR,    # calculate ratios
         P_over_Rn = PRECIP/NETRAD)

lonlat2$elevation <- as.vector(data_elevation)

saveRDS(lonlat2, paste0(output_folder, "main.rds"), compress = "xz")
write.csv(lonlat2, paste0(output_folder, "main.csv"), row.names = FALSE)

# color_palette <- rev(RColorBrewer::brewer.pal(999, "RdYlBu"))
# ggplot(data = lonlat2) +
#   geom_tile(aes(x = lon_adj, y = lat, fill = land_cover_change, color = NA), colour = NA) +
#   scale_fill_gradientn(colors = color_palette, na.value = "transparent") +
#   coord_fixed(ratio = 1.5) +
#   borders("world", colour = "grey50", size = 0.4) +
#   theme_minimal()
#
#
# lonlat_test <- lonlat2 %>% drop_na(SIF) %>% filter(veg_flag == 1)
# ggplot(data = lonlat_test) +
#   geom_tile(aes(x = lon_adj, y = lat, fill = elevation, color = NA), colour = NA) +
#   scale_fill_gradientn(colors = color_palette, na.value = "transparent") +
#   coord_fixed(ratio = 1.5) +
#   borders("world", colour = "grey50", size = 0.4) +
#   theme_minimal()


