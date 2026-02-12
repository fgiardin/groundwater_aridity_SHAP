# script to read and process MODIS fpar
# using parallel computing on local unix environment

# load libraries
library(tidyverse)
library(terra)
library(data.table)
library(sf)
library(rnaturalearth)
library(viridisLite)
library(future.apply)

# settings ----------------------------------------------------------------
in_dir    <- "data-raw/high_res/fpar/all_files"
start_day <- as.Date("2018-03-01")
end_day   <- as.Date("2021-03-31")
min_pts   <- 24L                  # keep cells with 24 or more than 24 eight-day-observations
grid_inv  <- 120L                 # 1 / 0.0083 deg
workers   <- max(1L, parallel::detectCores() - 2L)
plan(multisession, workers = workers)

# Quality check -----------------------------------------------------------
mask_fun <- function(qc, extra) {
  modland_good  <- bitwAnd(qc, 1L) == 0L
  scf           <- bitwShiftR(bitwAnd(qc, 224L), 5L)      # bits 5-7 (consistent with legend of the dataset)
  scf_main      <- scf %in% c(0L, 1L)
  cloudstate    <- bitwShiftR(bitwAnd(qc, 24L), 3L)       # bits 3-4
  cloud_clear   <- cloudstate == 0L
  land          <- bitwAnd(extra, 3L) == 0L               # bits 0-1
  snow_free     <- bitwAnd(extra, bitwShiftL(1L, 2L)) == 0L
  aerosol_low   <- bitwAnd(extra, bitwShiftL(1L, 3L)) == 0L
  cirrus_free   <- bitwAnd(extra, bitwShiftL(1L, 4L)) == 0L
  cloudmask_free<- bitwAnd(extra, bitwShiftL(1L, 5L)) == 0L
  shadow_free   <- bitwAnd(extra, bitwShiftL(1L, 6L)) == 0L
  as.integer(modland_good & scf_main & cloud_clear &
               land & snow_free & aerosol_low & cirrus_free &
               cloudmask_free & shadow_free)
}



# helper functions --------------------------------------------------------
f_date <- function(f) {
  ydoy <- sub(".*\\.A(\\d{7})\\..*", "\\1", basename(f))
  as.Date(paste0(substr(ydoy,1,4), "-01-01")) + as.integer(substr(ydoy,5,7)) - 1L
}

# processing one HDF: per-date per-1km bins (simple average) for that tile
proc_one_file <- function(f) {
  r <- rast(f)
  fpar  <- r$Fpar_500m
  qc    <- r$FparLai_QC
  extra <- r$FparExtra_QC

  # QC
  msk <- lapp(c(qc, extra), mask_fun)
  fpar[fpar > 100] <- NA
  fpar <- mask(fpar, msk, maskvalues = 0) * 0.01

  vals <- values(fpar, mat = FALSE)
  keep <- which(!is.na(vals))
  if (!length(keep)) return(NULL)

  # 500m cell centers, lon/lat (no resampling), then bin to 1km
  xy_sin <- xyFromCell(fpar, keep)
  ll     <- crds(project(vect(xy_sin, crs = crs(fpar)), "EPSG:4326"), df = TRUE)
  lon <- pmin(ll[,1], 180 - 1e-9)                 # avoid edge case exactly at 180°
  lat <- ll[,2]

  # 1km bin indices + per-bin average within this tile
  lon_idx <- floor((lon + 180) * grid_inv)
  lat_idx <- floor((lat +  90) * grid_inv)

  dt <- data.table(lon_idx, lat_idx, fpar = vals[keep])
  dt[, .(sum = sum(fpar), n = .N), by = .(lon_idx, lat_idx)]
}


# list files, keep window, group by date ----------------------------------
files_all <- list.files(in_dir, pattern = "MOD15A2H.*\\.hdf$", full.names = TRUE)
dates_all <- f_date(files_all)
sel       <- which(dates_all >= start_day & dates_all <= end_day)
files     <- files_all[sel]
dates     <- dates_all[sel]

u_dates <- sort(unique(dates))


# streaming accumulation over dates ---------------------------------------
# Cumulating: per 1km cell, sum of per-date means, and count of dates with data
acc <- data.table(lon_idx = integer(), lat_idx = integer(),
                  sum_val = numeric(), n_dates = integer())
setkey(acc, lon_idx, lat_idx)


for (d in u_dates) { # looping over dates (i.e. each unique 8-day composite date d)
  f_d <- files[dates == d] # f_d = set of all tiles (HDF files) on that date that intersect our study area

  parts <- future_lapply(f_d, proc_one_file, future.seed = TRUE) # looping over all tiles with the same date (reconstructing a complete map)
  parts <- parts[!vapply(parts, is.null, FALSE)]
  if (!length(parts)) next

  # combine overlapping tiles for this date by weighted mean
  day_bins <- rbindlist(parts)
  day_bins <- day_bins[, .(sum = sum(sum), n = sum(n)), by = .(lon_idx, lat_idx)]
  day_bins[, val := sum / n][, c("sum","n") := NULL]

  acc <- merge(acc, day_bins, by = c("lon_idx","lat_idx"), all = TRUE)

  # replace NAs with 0s and update running totals
  a <- acc$sum_val; a[is.na(a)] <- 0
  v <- acc$val;     v[is.na(v)] <- 0
  acc$sum_val <- a + v

  n <- acc$n_dates; n[is.na(n)] <- 0L
  acc$n_dates <- n + as.integer(!is.na(acc$val))

  acc[, val := NULL]
  setkey(acc, lon_idx, lat_idx)
}

# save an "unfiltered" version first (all data from all cells)
if (nrow(acc)) {
  acc_all <- copy(acc)

  # add lon/lat + mean using ALL available dates for each cell
  acc_all[, `:=`(
    lon = (lon_idx + 0.5) / grid_inv - 180,
    lat = (lat_idx + 0.5) / grid_inv -  90,
    fpar_mean = ifelse(n_dates > 0, sum_val / n_dates, NA_real_)
  )]

  LT_dt_all <- acc_all[, .(lon, lat, fpar_mean, n_obs = n_dates)]

  saveRDS(LT_dt_all,
          "fpar_longterm_mean_1km_201803-202103_UNFILTERED.rds",
          compress = "xz")
}


# finalize: filter by temporal coverage (min_pts defined before) a --------
acc <- acc[n_dates >= min_pts]
if (nrow(acc)) {
  acc[, `:=`(
    lon = (lon_idx + 0.5)/grid_inv - 180,
    lat = (lat_idx + 0.5)/grid_inv -  90,
    fpar_mean = sum_val / n_dates
  )]
  LT_dt <- acc[, .(lon, lat, fpar_mean, n_obs = n_dates)]
} else {
  LT_dt <- data.table(lon = numeric(), lat = numeric(),
                      fpar_mean = numeric(), n_obs = integer())
}

# re-scale values
if (max(LT_dt$fpar_mean, na.rm = TRUE) < 0.02) {
  LT_dt[, fpar_mean := fpar_mean * 100]
}

summary(LT_dt$fpar_mean)  # should look like ~0.1–0.8 in vegetated regions

# save final DT
LT_dt[]
saveRDS(LT_dt,
        "./fpar_longterm_mean_1km_201803-202103.rds",
        compress = "xz")



# PLOT --------------------------------------------------------------------
world <- ne_countries(scale = "small", returnclass = "sf")
plot <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey80", linewidth = 0.1) +
  geom_tile(data = LT_dt, aes(lon, lat, fill = fpar_mean),
            width = 1/120, height = 1/120) +
  scale_fill_gradientn(colours = viridis(256), limits = c(0, 1), oob = scales::squish,
                       name = "FPAR") +
  coord_sf(expand = FALSE) +
  coord_equal(xlim = c(-130,-60), ylim = c(20,60)) +
  labs(title = "MOD15A2H FPAR — long-term mean (2018-03 to 2021-03)",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12)


plot <- ggplot(LT_dt[!is.na(fpar_mean)]) +
  geom_tile(aes(lon, lat, fill = fpar_mean), width = 1/120, height = 1/120) +
  coord_equal(xlim = c(-130,-60), ylim = c(20,60)) +
  scale_color_viridis_c() +
  theme_minimal()

ggsave("world_map_check_24filter.png", path = "./",
       width = 6, height = 4, dpi = 600)






