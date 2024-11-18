# script to load and process the raw fPAR dataset

library(terra)

# Indicate your path to the directory containing the downloaded .tif files
file_path <- "data-raw/WGS84_0.05degree_bimonth/"

files <- list.files(file_path, pattern = "SI_LAI_FPAR_CDR_WGS84_0.05degree_bimonth_2018.*.tif|
                    SI_LAI_FPAR_CDR_WGS84_0.05degree_bimonth_2019.*.tif|
                    SI_LAI_FPAR_CDR_WGS84_0.05degree_bimonth_2020.*.tif", full.names = TRUE)

# Initialize an empty list to store each fPAR layer
fpar_layers <- list()

# Loop through each file to extract the second layer and apply the scaling factor
for (file in files) {
  # Load the raster file
  raster_data <- rast(file)

  # Extract the second layer (fPAR)
  fpar_layer <- raster_data[[2]]

  # Apply the scaling factor
  fpar_layer <- fpar_layer * 0.01

  # Store the scaled fPAR layer in the list
  fpar_layers[[file]] <- fpar_layer
}

# Stack all fPAR layers
fpar_stack <- rast(fpar_layers)

# Calculate the mean of the stack
fpar_mean <- mean(fpar_stack)

# Define the new resolution (0.08333333 degrees)
new_res <- 0.08333333

# Create a template raster with the new resolution and the same extent
template <- rast(nrows = nrow(fpar_mean) * res(fpar_mean)[1] / new_res,
                 ncols = ncol(fpar_mean) * res(fpar_mean)[2] / new_res,
                 xmin = xmin(fpar_mean), xmax = xmax(fpar_mean),
                 ymin = ymin(fpar_mean), ymax = ymax(fpar_mean),
                 crs = crs(fpar_mean))

# Resample the mean raster to the new resolution
resampled_fpar_mean <- resample(fpar_mean, template, method = "bilinear")

# Convert the resampled raster to a data frame
df_tif <- as.data.frame(resampled_fpar_mean, xy = TRUE, na.rm = FALSE)

# Save the result if needed
writeRaster(resampled_fpar_mean, "data/fPAR_2018_2021_0083.nc.tif",
            overwrite = TRUE)



