# script to load and process the raw GDE dataset

# load libraries
library(terra)
library(tidyverse)

# load data
file <- "data-raw/GDE_30arcsec.tif"
raster_data <- rast(file)

# Select the 'GDE_frac_GA' layer (third layer)
gde_frac_ga <- raster_data[[3]]  # GDE_frac_GA is the third layer

# Adjust units  (dividing the 'GDE_frac_GA' layer by 10^8)
gde_frac_ga_divided <- gde_frac_ga / 10^8
new_extent <- ext(-180, 180, -90, 90)

# Extend the raster to the new extent, filling with NA
raster_extended <- extend(gde_frac_ga_divided, new_extent)

# Define the new resolution (0.083 degrees)
new_res <- 0.08333333

# Create a template raster with the new resolution and the same extent
template <- rast(nrows = nrow(raster_extended) * res(raster_extended)[1] / new_res,
                 ncols = ncol(raster_extended) * res(raster_extended)[2] / new_res,
                 xmin = xmin(raster_extended), xmax = xmax(raster_extended),
                 ymin = ymin(raster_extended), ymax = ymax(raster_extended),
                 crs = crs(raster_extended))

# Resample the original raster to the new resolution
resampled_raster <- resample(raster_extended, template, method = "bilinear")
df_tif <- as.data.frame(resampled_raster, xy = TRUE, na.rm = FALSE)

# save extracted dataframe
input_folder <- "data/"
saveRDS(df_tif, paste0(input_folder,"gde_frac_ga_resample_0083.rds"), compress = "xz")

# add it to the main.rds dataframe and save it
df <- readRDS(paste0(input_folder, "main.rds"))
df_reor <- df_tif %>% arrange(y)
df$GDE_frac <- df_reor$GDE_frac_GA

saveRDS(df, paste0(input_folder,"main.rds"), compress = "xz") # save shap dataframe
# write.csv(df, paste0(input_folder,"main.csv"),row.names = F)







