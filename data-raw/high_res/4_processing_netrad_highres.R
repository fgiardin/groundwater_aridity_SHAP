# Script to merge net radiation (0.083°) into fPAR grid at 1/120°

# load packages
library(terra)
library(data.table)
library(tidyverse)

# load high-res template
LT_dt <- readRDS("data/high_res/fpar_longterm_mean_1km_201803-202103_24filter.rds")
res0  <- 1/120
ext_ll <- ext(min(LT_dt$lon) - res0/2, max(LT_dt$lon) + res0/2,
              min(LT_dt$lat) - res0/2, max(LT_dt$lat) + res0/2)
tmpl <- rast(ext_ll, resolution = res0, crs = "EPSG:4326")

# load data table with all other variables (lon/lat, elev, WTD, P, etc.)
grid_dt <- readRDS("data/high_res/fpar_wtd_elev_pr_1km_CONUS.rds")

# load NETRAD from df_SHAP (analysis at lower resolution) and keep non-NA values
df_SHAP <- readRDS("data/main.rds")
net_lo  <- df_SHAP[, c("lon","lat","NETRAD")]
net_lo  <- net_lo[!is.na(net_lo$NETRAD) & !is.na(net_lo$lon) & !is.na(net_lo$lat), ]

# build coarse grid from the net rad data (0.083°)
res_x <- median(diff(sort(unique(round(net_lo$lon, 4)))))
res_y <- median(diff(sort(unique(round(net_lo$lat, 4)))))

# adjust for edge points
ext_coarse <- ext(min(net_lo$lon) - res_x/2, max(net_lo$lon) + res_x/2,
                  min(net_lo$lat) - res_y/2, max(net_lo$lat) + res_y/2)
coarse <- rast(ext_coarse, resolution = c(res_x, res_y), crs = "EPSG:4326")

# rasterize points to the coarse grid
pts <- vect(net_lo, geom = c("lon","lat"), crs = "EPSG:4326")
r_net_lo <- rasterize(pts, coarse, field = "NETRAD", fun = mean, background = NA)

# check if we have values
g1 <- global(!is.na(r_net_lo), "sum", na.rm = TRUE)
cat("Non-NAs in NETRAD:", g1$sum, "\n")
print(global(r_net_lo, c("min","mean","max"), na.rm = TRUE))

# upscale to 1/120° template
r_net_hi <- resample(r_net_lo, tmpl, method = "bilinear")

g2 <- global(!is.na(r_net_hi), "sum", na.rm = TRUE)
cat("Non-NAs after resampling:", g2$sum, "\n")
print(global(r_net_hi, c("min","mean","max"), na.rm = TRUE))

# write values directly into grid_dt via cell indexing on the same tmpl
grid_dt[, cell := cellFromXY(tmpl, cbind(lon, lat))]
vals <- values(r_net_hi, mat = FALSE)
grid_dt[, NETRAD := vals[cell]]

# drop duplicate lon/lat
setkey(grid_dt, lon, lat)
grid_dt <- unique(grid_dt, by = c("lon","lat"))

print(summary(grid_dt$NETRAD))
saveRDS(grid_dt, "data/high_res/fpar_wtd_elev_pr_rn_1km_CONUS.rds", compress = "xz")



# plot to double check ----------------------------------------------------
plot <- ggplot(grid_dt) +
  geom_tile(aes(lon, lat, fill = NETRAD), width = 1/120, height = 1/120) +
  annotate("rect",
           xmin = -125, xmax = -65,
           ymin = 24,   ymax = 50,
           fill = NA, colour = "red",
           linewidth = 0.6, linetype = "dashed") +
  coord_equal(xlim = c(-130, -60), ylim = c(20, 60), expand = FALSE) +
  scale_fill_viridis_c(name = "Rn") +
  theme_minimal()

ggsave("map_check_NETRAD.png", path = "./",
       width = 6, height = 4, dpi = 600)



