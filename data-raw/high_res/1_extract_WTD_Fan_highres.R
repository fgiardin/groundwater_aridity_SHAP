# Script to extract Water table depth (WTD) data from Fan et al. and transform it to a dataframe with the same resolution of fpar

# load packages
library(tidyverse)
library(terra)
library(ncdf4)
library(ncdf4.helpers)
library(sf)
library(data.table)

# load fpar (to match extent/grid)
LT_dt <- readRDS("data/high_res/fpar_longterm_mean_1km_201803-202103_24filter.rds") # using filter of 24 days over total number of days per pixel

# extract WTD data (Fat et al.)
dname = "WTD"
# north america
WTD_NAmerica_raw <- rast("data/WTD_global/NAMERICA_WTD_annualmean.nc", dname)
NAmerica_mask <- rast("data/WTD_global/NAMERICA_WTD_annualmean.nc", "mask")

# process raw map ---------------------------------------------------------
WTD_NAmerica <- terra::mask(WTD_NAmerica_raw, NAmerica_mask, maskvalue=0) # masks ocean
WTD_NAmerica <- ifel(WTD_NAmerica == 0, NA, WTD_NAmerica) # removes fillvalue (0)

# Align WTD with FPAR grid ------------------------------------------------------
# building FPAR template from LT_dt grid (already at 0.0083 degrees)
res0  <- 1/120  # 0.0083
ext_ll <- ext(min(LT_dt$lon) - res0/2, max(LT_dt$lon) + res0/2,
              min(LT_dt$lat) - res0/2, max(LT_dt$lat) + res0/2)
tmpl <- rast(ext_ll, resolution = res0, crs = "EPSG:4326")

# Resample WTD on same grid
WTD_on_fpar <- resample(WTD_NAmerica, tmpl, method = "bilinear")

# Convert to data.table
wtd_full <- as.data.table(
  as.data.frame(WTD_on_fpar, xy = TRUE, cells = TRUE, na.rm = TRUE)
  )

# Check names(wtd_full)
print(names(wtd_full)) # Should get: "cell", "x", "y", "<layername>"

# Renaming using existing names
setnames(
  wtd_full,
  old = c("cell", "x", "y", names(WTD_on_fpar)),
  new = c("cell", "lon", "lat", "WTD")
)

# Add matching cell index to LT_dt using same raster
LT_dt[, cell := cellFromXY(WTD_on_fpar, cbind(lon, lat))]

LT_core <- LT_dt[, .(cell, fpar_mean, n_obs)]

setkey(wtd_full, cell)
setkey(LT_core,   cell)

# Right join on WTD grid: keep all WTD cells (always more since it's a continuous grid from a model), attach FPAR where available
grid_dt <- LT_core[wtd_full, on = "cell"]

summary(grid_dt)
saveRDS(grid_dt, "data/high_res/fpar_wtd_1km_CONUS.rds", compress = "xz")

# plot to double check ---------------------------------------------------------
plot <- ggplot(grid_dt) +
  geom_tile(aes(lon, lat, fill = WTD), width = 1/120, height = 1/120) +
  coord_equal(xlim = c(-130,-60), ylim = c(20,60)) +
  scale_color_viridis_c() +
  theme_minimal()

ggsave("map_check_WTD.png", path = "./",
       width = 6, height = 4, dpi = 600)

