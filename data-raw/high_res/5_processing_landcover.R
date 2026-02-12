# processing land cover data at high resolution
# using parallel computing on local unix environment

# load packages
library(terra)
library(data.table)

# settings for parallel computing
terraOptions(progress = 1)
Sys.setenv(GDAL_NUM_THREADS = "ALL_CPUS")

# load data table with all other variables (lon/lat, elev, WTD, P, etc.)
grid_path <- "data/high_res/fpar_wtd_elev_pr_rn_1km_CONUS.rds"
grid_dt   <- readRDS(grid_path)

# load high-res template
LT_dt   <- readRDS("data/high_res/fpar_longterm_mean_1km_201803-202103_24filter.rds")

res0   <- 1/120
ext_ll <- ext(min(LT_dt$lon) - res0/2, max(LT_dt$lon) + res0/2,
              min(LT_dt$lat) - res0/2, max(LT_dt$lat) + res0/2)
tmpl   <- rast(ext_ll, resolution = res0, crs = "EPSG:4326")

# read CCI/C3S land cover rasters  (each file has several layers: we only need lccs_class)
lc1992_all <- rast("data-raw/high_res/landcover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992-v2.0.7cds.area-subset.50.-66.24.-125.nc")
lc2000_all <- rast("data-raw/high_res/landcover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7cds.area-subset.50.-66.24.-125.nc")
lc2010_all <- rast("data-raw/high_res/landcover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2010-v2.0.7cds.area-subset.50.-66.24.-125.nc")
lc2020_all <- rast("data-raw/high_res/landcover/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.area-subset.50.-66.24.-125.nc")

lc1992 <- lc1992_all$"lccs_class"
lc2000 <- lc2000_all$"lccs_class"
lc2010 <- lc2010_all$"lccs_class"
lc2020 <- lc2020_all$"lccs_class"

# helper function to convert from 300m to 1km (using majority principle)
agg_to_tmpl <- function(r300, tmpl) {
  r300c <- crop(r300, ext(tmpl), snap = "out")
  r1km  <- aggregate(r300c, fact = 3, fun = "modal", na.rm = TRUE)
  r1km_aligned <- resample(r1km, tmpl, method = "near")
  r1km_aligned
}

lc_1992_1km <- agg_to_tmpl(lc1992, tmpl)
lc_2000_1km <- agg_to_tmpl(lc2000, tmpl)
lc_2010_1km <- agg_to_tmpl(lc2010, tmpl)
lc_2020_1km <- agg_to_tmpl(lc2020, tmpl)

# double check
stopifnot(all.equal(res(lc_1992_1km), res(tmpl)))
stopifnot(all.equal(ext(lc_1992_1km), ext(tmpl)))
stopifnot(same.crs(lc_1992_1km, tmpl))

# convert to indexed table
lc_stack <- c(lc_1992_1km, lc_2000_1km, lc_2010_1km, lc_2020_1km)
names(lc_stack) <- c("lc_1992","lc_2000","lc_2010","lc_2020")

lc_dt <- as.data.table(as.data.frame(lc_stack, cells = TRUE, na.rm = FALSE))
setnames(lc_dt, c("cell","land_cover_1992","land_cover_2000","land_cover_2010","land_cover_2020"))
setkey(lc_dt, cell)

# calculate land_cover_change
# mapping table (from 0_land_cover_mapping.R)
land_cover_mapping <- c(
  "0"="no_data","10"="cropland_rainfed","11"="cropland_rainfed_herbaceous_cover",
  "12"="cropland_rainfed_tree_or_shrub_cover","20"="cropland_irrigated","30"="mosaic_cropland",
  "40"="mosaic_natural_vegetation","50"="tree_broadleaved_evergreen_closed_to_open",
  "60"="tree_broadleaved_deciduous_closed_to_open","61"="tree_broadleaved_deciduous_closed",
  "62"="tree_broadleaved_deciduous_open","70"="tree_needleleaved_evergreen_closed_to_open",
  "71"="tree_needleleaved_evergreen_closed","72"="tree_needleleaved_evergreen_open",
  "80"="tree_needleleaved_deciduous_closed_to_open","81"="tree_needleleaved_deciduous_closed",
  "82"="tree_needleleaved_deciduous_open","90"="tree_mixed","100"="mosaic_tree_and_shrub",
  "110"="mosaic_herbaceous","120"="shrubland","121"="shrubland_evergreen",
  "122"="shrubland_deciduous","130"="grassland","140"="lichens_and_mosses",
  "150"="sparse_vegetation","151"="sparse_tree","152"="sparse_shrub","153"="sparse_herbaceous",
  "160"="tree_cover_flooded_fresh_or_brakish_water","170"="tree_cover_flooded_saline_water",
  "180"="shrub_or_herbaceous_cover_flooded","190"="urban","200"="bare_areas",
  "201"="bare_areas_consolidated","202"="bare_areas_unconsolidated","210"="water","220"="snow_and_ice"
)

major_land_cover_mapping <- c(
  "0"="other","10"="croplands","11"="croplands","12"="croplands","20"="croplands",
  "30"="other","40"="other","50"="forests","60"="forests","61"="forests","62"="forests",
  "70"="forests","71"="forests","72"="forests","80"="forests","81"="forests","82"="forests",
  "90"="forests","100"="other","110"="other","120"="savannas_and_shrublands",
  "121"="savannas_and_shrublands","122"="savannas_and_shrublands","130"="grasslands",
  "140"="other","150"="other","151"="forests","152"="savannas_and_shrublands","153"="grasslands",
  "160"="forests","170"="forests","180"="other","190"="other","200"="other","201"="other",
  "202"="other","210"="other","220"="other"
)

vegetation_flag <- c(
  "0"=0,"10"=1,"11"=1,"12"=1,"20"=1,"30"=1,"40"=1,"50"=1,"60"=1,"61"=1,"62"=1,"70"=1,"71"=1,
  "72"=1,"80"=1,"81"=1,"82"=1,"90"=1,"100"=1,"110"=1,"120"=1,"121"=1,"122"=1,"130"=1,"140"=1,
  "150"=1,"151"=1,"152"=1,"153"=1,"160"=1,"170"=1,"180"=1,"190"=0,"200"=0,"201"=0,"202"=0,"210"=0,"220"=0
)

# change flag (0 = unchanged across all four epochs, 1 = changed)
lc_dt[, land_cover_change :=
        as.integer(!(land_cover_1992 == land_cover_2000 &
                       land_cover_2000 == land_cover_2010 &
                       land_cover_2010 == land_cover_2020))]

lc_dt[, land_cover_num := as.integer(land_cover_2020)]

lc_dt[, land_cover_chr        := land_cover_mapping[as.character(land_cover_num)]]
lc_dt[, major_land_cover      := major_land_cover_mapping[as.character(land_cover_num)]]
lc_dt[, veg_flag              := as.integer(vegetation_flag[as.character(land_cover_num)])]
lc_dt[is.na(veg_flag), veg_flag := 0L]

# merge into master grid by cell
if (!"cell" %in% names(grid_dt)) {
  grid_dt[, cell := cellFromXY(tmpl, cbind(lon, lat))]
}

setkey(grid_dt, cell)
grid_dt <- lc_dt[grid_dt, on = "cell"]

# delete leftovers from old joins
for (nm in grep("^i\\.", names(grid_dt), value = TRUE)) grid_dt[, (nm) := NULL]

# double check
cat("Unique LC codes (2020) in grid:", paste(sort(na.omit(unique(grid_dt$land_cover_num))), collapse=", "), "\n")
cat("Share NA in land_cover_2020:", mean(is.na(grid_dt$land_cover_num)), "\n")
cat("Share 'changed' pixels:", mean(grid_dt$land_cover_change == 1, na.rm = TRUE), "\n")

# drop duplicates
setkey(grid_dt, lon, lat)
grid_dt <- unique(grid_dt, by = key(grid_dt))

saveRDS(grid_dt, "data/high_res/fpar_wtd_elev_pr_rn_lc_1km_CONUS.rds", compress = "xz")



# plot to double check ----------------------------------------------------
plot <- ggplot(grid_dt) +
  geom_tile(aes(lon, lat, fill = major_land_cover), width = 1/120, height = 1/120) +
  annotate("rect",
           xmin = -125, xmax = -65, ymin = 24, ymax = 50,
           fill = NA, colour = "red", linewidth = 0.6, linetype = "dashed") +
  coord_equal(xlim = c(-130, -60), ylim = c(20, 60), expand = FALSE) +
  scale_fill_viridis_d(name = "Major land cover",
                       na.value = "grey85",
                       drop = FALSE)    +
  theme_minimal()

ggsave("map_check_landcover.png", path = "./",
       width = 6, height = 4, dpi = 600)


