# script to resample the ESA landcover map

rm(list=ls())
library(ncdf4)
library(raster)
library(tidyverse)

input_folder <- "/data-raw"
nc_file <- paste0(input_folder, "C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc") # name of the raw ESA file
nc_data <- nc_open(nc_file)

# Load the variable `lccs_class` using the raster package
lccs_raster <- raster(nc_file, varname = "lccs_class")

original_res <- res(lccs_raster)
print(original_res)

target_res <- 0.083
agg_factor <- round(target_res / original_res)

# Aggregate using majority sampling (modal function)
lccs_resampled <- aggregate(lccs_raster, fact = agg_factor, fun = modal, na.rm = TRUE)

# Check the new resolution
res(lccs_resampled)  # Should be approximately 0.083

# Write the resampled raster to a new NetCDF file
output_file <- paste0(input_folder, "C3S-LC-L4-LCCS-Map-0083-degree-P1Y-2020-v2.1.1.nc")
writeRaster(lccs_resampled, output_file, format = "CDF", overwrite = TRUE, varname = "lccs_class",
            varunit = "NA", longname = "Land Cover Class", xname = "lon", yname = "lat")


# vis
land_cover_colors <- c(
  "#ffff64", "#ffff00", "#aaf0f0", "#dcf064", "#c8c864", "#006400", "#00a000", "#00a000",
  "#aac800", "#003c00", "#003c00", "#005000", "#285000", "#285000", "#286400", "#788200",
  "#8ca000", "#be9600", "#966400", "#966400", "#966400", "#ffb432", "#ffdcd2", "#ffebaf",
  "#ffc864", "#ffd278", "#ffebaf", "#00785a", "#009678", "#00dc82", "#c31400", "#fff5d7",
  "#dcdcdc", "#fff5d7", "#0046c8", "#ffffff"
)

# Plot the resampled raster with custom colors to double check
plot(lccs_resampled, main = "Resampled Land Cover (LCCS) at 0.083 Degrees", col = land_cover_colors, legend = FALSE)

