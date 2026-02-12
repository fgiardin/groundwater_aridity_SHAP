# extract and process elevation at highres

# load packages
library(terra)
library(data.table)

# load data
grid_dt <- readRDS("data/high_res/fpar_wtd_1km_CONUS.rds") # Grid with FPAR and WTD on the right 1/120° grid (generated with scripts 0 and 1 in same directory)
LT_dt <- readRDS("data/high_res/fpar_longterm_mean_1km_201803-202103_24filter.rds") # FPAR DT (used only to rebuild the exact extent/origin/res)

# Read DEM (arcgrid format)
grid_dir <- "data-raw/high_res/elv/mn30_grd/mn30_grd"  # folder with all .adf files
dem <- rast(grid_dir)                                  # terra reads arcgrid format automatically from the directory

# Checking NAflag + make sure it's NA
if (!is.na(NAflag(dem))) {
  dem[dem == NAflag(dem)] <- NA
}

# build 1/120° template from LT_dt (same grid as the other variables)
res0  <- 1/120  # 0.0083
ext_ll <- ext(min(LT_dt$lon) - res0/2, max(LT_dt$lon) + res0/2,
              min(LT_dt$lat) - res0/2, max(LT_dt$lat) + res0/2)
tmpl <- rast(ext_ll, resolution = res0, crs = "EPSG:4326")

# aligning DEM to template
dem_crop     <- crop(dem, ext(tmpl), snap = "out")
dem_on_grid  <- resample(dem_crop, tmpl, method = "bilinear") # using bilinear for continuous variable (elevation)

# Check that DEM still has data after resampling
print(summary(values(dem_on_grid)))

# convert DEM to data.table and join by coordinates
dem_dt <- as.data.table(as.data.frame(dem_on_grid, xy = TRUE, na.rm = TRUE))
setnames(dem_dt, c("lon", "lat", "elev_m"))

# Round coordinates so floating-point issues don't break the join
round_to <- 6L  # for 1/120° (approx 0.008333)
dem_dt[,  `:=`(lon_r = round(lon, round_to),  lat_r = round(lat, round_to))]
grid_dt[, `:=`(lon_r = round(lon, round_to),  lat_r = round(lat, round_to))]

setkey(dem_dt,  lon_r, lat_r)
setkey(grid_dt, lon_r, lat_r)

# keep all rows from existing grid + add elevation where the variable is not NA
grid_dt <- dem_dt[grid_dt]

# drop helper columns
grid_dt[, c("lon_r","lat_r") := NULL]

# sanity check (optional)
print(summary(grid_dt$elev_m))
cat("Share NA in elev_m:", mean(is.na(grid_dt$elev_m)), "\n")

# save combined table
saveRDS(grid_dt, "data/high_res/fpar_wtd_elev_1km_CONUS.rds", compress = "xz")



# plot to double check ----------------------------------------------------
plot <- ggplot(grid_dt) +
  geom_tile(aes(lon, lat, fill = elev_m), width = 1/120, height = 1/120) +
  coord_equal(xlim = c(-130,-60), ylim = c(20,60)) +
  scale_color_viridis_c() +
  theme_minimal()

ggsave("map_check_elev_m.png", path = "./",
       width = 6, height = 4, dpi = 600)


