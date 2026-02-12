# script to extract apparent rooting depth from Stocker et al. 2023
# (Supplementary Fig. 13)

library(terra)
library(tidyverse)
library(ncdf4)


# load apparent rooting depth from stocker et al 2023 ---------------------
f <- "~/data/stocker_natgeo2023/zroot_cwd80.nc"

r0 <- rast(f)

# convert mm to m
r0 <- r0 / 1000
units(r0) <- "m"

# drop non physical values
r0 <- clamp(r0, lower = 0, upper = 300, values = TRUE)


# load Shapley values (same processing of Fig. 4) -------------------------
df_SHAP <- readRDS("data/main.rds")
model_training_folder <- 'data/model_output/model_training_elevation_US_Fan_oneModel/'
source("data-raw/0_land_cover_mapping.R")

df <- df_SHAP %>%
  drop_na(SIF, WTD_Fan, P_over_Rn, elevation, PAR) %>%
  mutate(major_land_cover = recode(land_cover_num, !!!major_land_cover_mapping),
         SIF_over_PAR = SIF * 10^3 / PAR) %>%
  filter(P_over_Rn < 3,
         P_over_Rn > 0,
         major_land_cover != "other",
         land_cover_change == 0) %>%
  filter(lon > -125,
         lon < -65,
         lat < 50,
         lat > 24) %>%
  dplyr::select(lon, lat, SIF_over_PAR, WTD_Fan, P_over_Rn, elevation, major_land_cover, GDE_frac) %>%
  rename(WTD = WTD_Fan, Aridity = P_over_Rn, Elevation = elevation)

df_land_cover <- df_SHAP %>%
  mutate(major_land_cover = recode(land_cover_num, !!!major_land_cover_mapping)) %>%
  filter(lon > -125,
         lon < -65,
         lat < 50,
         lat > 24) %>%
  dplyr::select(lon, lat, major_land_cover)


df.main <- data.frame()

for (i in 1:3) {
  df_SHAP <- readRDS(paste0(model_training_folder, "cshap_long_", i, ".rds"))$dt
  df_sub <- readRDS(paste0(model_training_folder, "df_sub_", i, ".rds"))
  df_SHAP <- df_SHAP %>%
    mutate(lon = df_sub$lon, lat = df_sub$lat,
           Land_cover = df_sub$major_land_cover,
           GDE_frac = df_sub$GDE_frac)
  df.main <- rbind(df.main, df_SHAP)
}


# match grids of both datasets --------------------------------------------
dx <- median(diff(sort(unique(df.main$lon))), na.rm=TRUE)
dy <- median(diff(sort(unique(df.main$lat))), na.rm=TRUE)
dx; dy

x0 <- -180
y0 <- -90

xmin <- x0 + floor((min(df.main$lon, na.rm=TRUE) - x0)/dx) * dx
xmax <- x0 + ceiling((max(df.main$lon, na.rm=TRUE) - x0)/dx) * dx
ymin <- y0 + floor((min(df.main$lat, na.rm=TRUE) - y0)/dy) * dy
ymax <- y0 + ceiling((max(df.main$lat, na.rm=TRUE) - y0)/dy) * dy

tmpl <- rast(ext(xmin, xmax, ymin, ymax), resolution=c(dx, dy), crs="EPSG:4326")

r_dfgrid <- resample(r0, tmpl, method="bilinear")

pts <- vect(df.main, geom=c("lon","lat"), crs="EPSG:4326")

root_vals <- terra::extract(r_dfgrid, pts)[,2]
df.main$root_depth <- root_vals


# correlation per PFT -----------------------------------------------------
cor_safe <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) return(NA_real_)
  cor(x[ok], y[ok], method = method)
}

corr_by_pft_stocker <- df.main %>%
  as_tibble() %>%
  group_by(Land_cover) %>%
  summarise(
    n = sum(is.finite(WTD) & is.finite(root_depth)),
    r_signed = cor_safe(WTD, root_depth, "pearson"),
    r_abs    = cor_safe(abs(WTD), root_depth, "pearson"),
    r_spear  = cor_safe(abs(WTD), root_depth, "spearman"),
    r_log    = cor_safe(abs(WTD), log1p(root_depth), "pearson"),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

print(corr_by_pft_stocker)



# boxplots rooting depth --------------------------------------------------

# WTD predictor used in Fig. 5
df_wtd <- df %>%
  distinct(lon, lat, .keep_all = TRUE) %>%   # avoid many-to-many
  dplyr::select(lon, lat, WTD) %>%
  rename(WTD_m = WTD)

# join to df.main (contains SHAP values in column "WTD" + Land_cover + root_depth already)
df5 <- df.main %>%
  as_tibble() %>%
  rename(SHAP_WTD = WTD) %>%
  left_join(df_wtd, by = c("lon", "lat")) %>%
  mutate(root_m = root_depth) %>%
  filter(is.finite(WTD_m), is.finite(root_m), is.finite(SHAP_WTD)) %>%
  filter(!is.na(Land_cover))

# 25m-bins (to match Fig. 5)
breaks_25 <- seq(
  floor(min(df5$WTD_m, na.rm = TRUE) / 25) * 25,
  ceiling(max(df5$WTD_m, na.rm = TRUE) / 25) * 25,
  by = 25
)

df5 <- df5 %>%
  mutate(WTD_bin = cut(WTD_m, breaks = breaks_25, include.lowest = TRUE, right = FALSE))

# option A
# rename + order Land_cover for facets
df5_plot <- df5 %>%
  mutate(
    Land_cover = recode(Land_cover,
                        "forests"    = "Forests",
                        "savannas_and_scrublands"   = "Savannahs and shrublands",
                        "grasslands" = "Grasslands",
                        "croplands"  = "Croplands"),
    Land_cover = factor(Land_cover,
                        levels = c("Forests",
                                   "Savannahs and shrublands",
                                   "Grasslands",
                                   "Croplands"))
  ) %>%
  filter(!is.na(Land_cover))

# keep only bins from -150 m and shallower (to match Fig. 5)
df5_plot <- df5_plot %>%
  filter(WTD_m >= -150) %>%
  mutate(WTD_bin = forcats::fct_drop(WTD_bin))

ggplot(df5_plot, aes(x = WTD_bin, y = root_m)) +
  geom_boxplot(outlier.alpha = 0.15) +
  facet_wrap(~ Land_cover, scales = "fixed") +
  scale_y_continuous(
    breaks = seq(0, 120, by = 10),
    expand = expansion(mult = 0, add = 0)
  ) +
  labs(x = "WTD (m), 25 m bins", y = "Apparent rooting depth (m)") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.spacing = unit(1.0, "lines"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),

    # remove background grid
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),

    # add axis lines + ticks
    axis.line = element_line(),
    axis.ticks = element_line(),
    axis.ticks.length = unit(2.5, "mm")
  ) +
  coord_cartesian(ylim = c(0, 55))

ggsave("boxplot_rootingdepth_Stocker.png", path = "./",
       width = 9, height = 7, dpi = 600)



