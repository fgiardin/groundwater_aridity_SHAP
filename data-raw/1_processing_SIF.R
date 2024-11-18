# script to load and process the raw SIF dataset

rm(list=ls())
pacman::p_load(tidyverse, ncdf4, terra)
input_folder <- "/data-raw"
output_folder <- "/"

# SIF ---------------------------------------------------------------------
dname = "//sif"

# Read the NetCDF file and extract CRS and geotransform
nc <- nc_open(paste0(input_folder, "processing_original_file/SIF_yield_v4.nc"))
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
SIF_rast <- rast(paste0(input_folder, "processing_original_file/SIF_yield_v4.nc"), dname) # terra::rast is faster than functions in 'raster' package

# Date: 2018-02-02, 2021-10-24, 1360 days
# Set the CRS and extent of the raster
crs(SIF_rast) <- crs_wkt
ext(SIF_rast) <- ext

# Define the indices corresponding to the period from 2018-03-01 to 2021-03-31
# start_index <- 4
# end_index <- 145
# Subset the raster to these indices
SIF_subset1 <- SIF_rast[[4:51]]
SIF_subset2 <- SIF_rast[[52:99]]
SIF_subset3 <- SIF_rast[[100:147]]

# if there are too many NAs, remove entire cell-pixel
SIF_na1 <- sum(is.na(SIF_subset1))
SIF_na2 <- sum(is.na(SIF_subset2))
SIF_na3 <- sum(is.na(SIF_subset3))
SIF_masked1 <- terra::mask(SIF_subset1, SIF_na1 > 35, maskvalue=TRUE) # at least one good observation per month
SIF_masked2 <- terra::mask(SIF_subset2, SIF_na2 > 35, maskvalue=TRUE)
SIF_masked3 <- terra::mask(SIF_subset3, SIF_na3 > 35, maskvalue=TRUE)

# do interannual mean of growing season only
inter_SIF1 <- mean(SIF_masked1, na.rm = TRUE)
inter_SIF2 <- mean(SIF_masked2, na.rm = TRUE)
inter_SIF3 <- mean(SIF_masked3, na.rm = TRUE)

# points that have negative interannual means here are most likely deserts and will be filtered with the PFTs below
writeCDF(inter_SIF1, paste0(input_folder, "inter_SIF_2018.nc"), overwrite=TRUE)
writeCDF(inter_SIF2, paste0(input_folder, "inter_SIF_2019.nc"), overwrite=TRUE)
writeCDF(inter_SIF3, paste0(input_folder, "inter_SIF_2020.nc"), overwrite=TRUE)

# convert to dataframe for plotting
df_SIF1 <- terra::as.data.frame(inter_SIF1, xy = TRUE, na.rm = F)
df_SIF2 <- terra::as.data.frame(inter_SIF2, xy = TRUE, na.rm = F)
df_SIF3 <- terra::as.data.frame(inter_SIF3, xy = TRUE, na.rm = F)

names(df_SIF1) <- c("lon", "lat", "SIF_2018")
names(df_SIF2) <- c("lon", "lat", "SIF_2019")
names(df_SIF3) <- c("lon", "lat", "SIF_2020")

df_SIF1$SIF_2019 <- df_SIF2$SIF_2019
df_SIF1$SIF_2020 <- df_SIF3$SIF_2020
df_SIF1 <- df_SIF1 %>%
  rowwise() %>%
  mutate(
    SIF_mean = mean(c(SIF_2018, SIF_2019, SIF_2020), na.rm = TRUE),
    SIF_sd = ifelse(sum(!is.na(c(SIF_2018, SIF_2019, SIF_2020))) <= 3,
                    sd(c(SIF_2018, SIF_2019, SIF_2020), na.rm = TRUE),
                    NA_real_),
    SIF_cv = SIF_sd / SIF_mean)%>%
  ungroup()

saveRDS(df_SIF1, paste0(output_folder, "df_SIF_summary.rds"), compress = "xz")

