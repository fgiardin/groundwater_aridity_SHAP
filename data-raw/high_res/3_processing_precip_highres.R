# script to process precipitation from monthly data
# using parallel computing on local unix environment

# load packages
library(terra)
library(data.table)

# settings for parallel computing
terraOptions(progress = 1)
Sys.setenv(GDAL_NUM_THREADS = "ALL_CPUS")


# Template from FPAR grid (as in previous scripts)
LT_dt <- readRDS("data/high_res/fpar_longterm_mean_1km_201803-202103_24filter.rds")
res0  <- 1/120
ext_ll <- ext(min(LT_dt$lon) - res0/2, max(LT_dt$lon) + res0/2,
              min(LT_dt$lat) - res0/2, max(LT_dt$lat) + res0/2)
tmpl <- rast(ext_ll, resolution = res0, crs = "EPSG:4326")


# read daymet precip (mm/month) across 4 years
mfiles <- file.path("data-raw/high_res/precip/monthly",
                    c("daymet_v4_prcp_monttl_na_2018.nc",
                      "daymet_v4_prcp_monttl_na_2019.nc",
                      "daymet_v4_prcp_monttl_na_2020.nc",
                      "daymet_v4_prcp_monttl_na_2021.nc"))

r <- rast(mfiles, subds = "prcp")      # select precip variable
roi_ll  <- as.polygons(ext_ll, crs = "EPSG:4326")
roi_lcc <- project(roi_ll, crs(r))
r  <- crop(r, ext(roi_lcc), snap = "out")

# selecting consistent time window
start_date <- as.Date("2018-03-01")
end_date   <- as.Date("2021-02-28")
d <- time(r)
keep <- d >= start_date & d <= end_date
r <- r[[keep]]; d <- d[keep]
r[r < 0] <- 0

m  <- as.integer(format(d, "%m"))
y  <- as.integer(format(d, "%Y"))
cy <- ifelse(m >= 3, y, y - 1)

# calculate yearly mean for each year, and then average across the 4 available years
ann_tot       <- tapp(r, cy, fun = sum, na.rm = TRUE)
pr_lt_native  <- app(ann_tot, mean, na.rm = TRUE)
pr_lt_ll      <- project(pr_lt_native, tmpl, method = "near") # using high res template defined above

# convert to data.table
pr_dt <- as.data.table(as.data.frame(pr_lt_ll, xy = TRUE, na.rm = TRUE))
setnames(pr_dt, c("lon","lat","prcp_mean_mm_yr"))

# double check grid keys and remove duplicates
pr_dt[, cell := cellFromXY(tmpl, cbind(lon, lat))]
setkey(pr_dt, lon, lat)
pr_dt <- unique(pr_dt, by = c("lon","lat"))
setkey(pr_dt, cell)

# merge into master grid (keeping all existing rows, just adding or updating precip)
grid_path_in  <- "data/high_res/fpar_wtd_elev_1km_CONUS.rds"
grid_path_out <- "data/high_res/fpar_wtd_elev_pr_1km_CONUS.rds"

if (file.exists(grid_path_in)) {
  grid_dt <- readRDS(grid_path_in)

  grid_dt[, cell := cellFromXY(tmpl, cbind(lon, lat))]

  setkey(grid_dt, cell)

  grid_dt[pr_dt, prcp_mean_mm_yr := i.prcp_mean_mm_yr, on = "cell"]

  setkey(grid_dt, lon, lat)
  grid_dt <- unique(grid_dt, by = c("lon","lat"))

  saveRDS(grid_dt, grid_path_out, compress = "xz")
  cat("Merged precip into grid_dt →", grid_path_out, "\n")

  cat("\nNon-NA P rows:", sum(!is.na(grid_dt$prcp_mean_mm_yr)), "/", nrow(grid_dt), "\n")
  print(summary(grid_dt$prcp_mean_mm_yr))

} else {

  saveRDS(pr_dt[, .(lon, lat, prcp_mean_mm_yr)],
          "data/high_res/precip_LT_Mar2018-Feb2021_1km.rds", compress = "xz")
  cat("Saved precip table → data/high_res/precip_LT_Mar2018-Feb2021_1km.rds\n")
  print(summary(pr_dt$prcp_mean_mm_yr))
}


# plot to double check ---------------------------------------------------------
plot <- ggplot(grid_dt) +
  geom_tile(aes(lon, lat, fill = prcp_mean_mm_yr), width = 1/120, height = 1/120) +
  annotate("rect",
           xmin = -125, xmax = -65,
           ymin = 24,   ymax = 50,
           fill = NA, colour = "red",
           linewidth = 0.6, linetype = "dashed") +
  coord_equal(xlim = c(-130, -60), ylim = c(20, 60), expand = FALSE) +
  scale_fill_viridis_c(name = "P (mm/yr)") +
  theme_minimal()

ggsave("map_check_precip.png", path = "./",
       width = 6, height = 4, dpi = 600)


