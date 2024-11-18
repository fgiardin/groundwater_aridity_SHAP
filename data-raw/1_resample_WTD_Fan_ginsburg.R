# Script to resample WTD data from Fan et al.
# Originally executed on a HPC

library(tidyverse)
library(ncdf4)
library(doMC)
library(terra)
library(FNN)

rm(list=ls())
input_folder <- "/burg/glab/users/jl6314/WTD_SIF/file/"

# df <- readRDS(paste0(input_folder,"raw.rds"))

raster_data_Fan <- rast(paste0(input_folder,"world_WTD.nc"))
new_res <- 0.08333333
template1 <- rast(nrows = nrow(raster_data_Fan) * 0.0174532925199433 / new_res,
                  ncols = ncol(raster_data_Fan) * 0.0174532925199433 / new_res,
                  xmin = xmin(raster_data_Fan), xmax = xmax(raster_data_Fan),
                  ymin = ymin(raster_data_Fan), ymax = ymax(raster_data_Fan),
                  crs = crs(raster_data_Fan))

# Resample the original raster to the new resolution
resampled_raster_Fan <- resample(raster_data_Fan, template1, method = "bilinear")

df_Fan <- as.data.frame(resampled_raster_Fan, xy = TRUE, na.rm = FALSE)

saveRDS(df_Fan, paste0(input_folder,"Fan-wtd_resample_0083.rds"), compress = "xz")



