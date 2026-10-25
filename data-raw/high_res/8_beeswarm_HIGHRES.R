# Plot beeswarm plots for the moisture index and water table depth (dataset from Fan et al)

rm(list=ls())
library(tidyverse)
library(cowplot)
library(ggforce)
library(ggpubr)
library(data.table)


# load and prepare data ---------------------------------------------------

std1 <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

model_training_folder <- "data-raw/high_res/model_output_highres/"


# Forests -----------------------------------------------------------------
df_forest <- readRDS(paste0(model_training_folder, "cshap_long_forests.rds"))
ori_forest <- as.data.frame(df_forest$x_test)
shap_forest <- df_forest$dt %>%
  mutate(WTD_rfvalue = ori_forest$WTD,
         Aridity_rfvalue = ori_forest$Aridity) %>%
  dplyr::select(-Elevation)

# standardization
shap_forest$WTD_stdfvalue <- std1(shap_forest$WTD_rfvalue)
shap_forest$Aridity_stdfvalue <- std1(shap_forest$Aridity_rfvalue)

# contribution calculation
shap_forest$WTD_mean_value <- mean(abs(shap_forest$WTD), na.rm = TRUE)
shap_forest$Aridity_mean_value <- mean(abs(shap_forest$Aridity), na.rm = TRUE)
shap_long_forest <- melt(shap_forest,
                         measure.vars = list(
                           c("WTD", "Aridity"),        # SHAP values
                           c("WTD_rfvalue", "Aridity_rfvalue"),  # rfvalue
                           c("WTD_stdfvalue", "Aridity_stdfvalue"),  # stdfvalue
                           c("WTD_mean_value", "Aridity_mean_value")  # mean_value
                         ),
                         variable.name = "variable",
                         value.name = c("value", "rfvalue", "stdfvalue", "mean_value"))
shap_long_forest[, variable := factor(variable, labels = c("WTD", "Aridity"))]


# Savannahs and shrublands ------------------------------------------------
df_savannas_and_scrublands <- readRDS(paste0(model_training_folder, "cshap_long_savannas_and_shrublands.rds"))
ori_savannas_and_scrublands <- as.data.frame(df_savannas_and_scrublands$x_test)
shap_savannas_and_scrublands <- df_savannas_and_scrublands$dt %>%
  mutate(WTD_rfvalue = ori_savannas_and_scrublands$WTD,
         Aridity_rfvalue = ori_savannas_and_scrublands$Aridity) %>%
  dplyr::select(-Elevation)

# standardization
shap_savannas_and_scrublands$WTD_stdfvalue <- std1(shap_savannas_and_scrublands$WTD_rfvalue)
shap_savannas_and_scrublands$Aridity_stdfvalue <- std1(shap_savannas_and_scrublands$Aridity_rfvalue)

# contribution calculation
shap_savannas_and_scrublands$WTD_mean_value <- mean(abs(shap_savannas_and_scrublands$WTD), na.rm = TRUE)
shap_savannas_and_scrublands$Aridity_mean_value <- mean(abs(shap_savannas_and_scrublands$Aridity), na.rm = TRUE)
shap_long_dry <- melt(shap_savannas_and_scrublands,
                      measure.vars = list(
                        c("WTD", "Aridity"),        # SHAP values
                        c("WTD_rfvalue", "Aridity_rfvalue"),  # rfvalue
                        c("WTD_stdfvalue", "Aridity_stdfvalue"),  # stdfvalue
                        c("WTD_mean_value", "Aridity_mean_value")  # mean_value
                      ),
                      variable.name = "variable",
                      value.name = c("value", "rfvalue", "stdfvalue", "mean_value"))
shap_long_dry[, variable := factor(variable, labels = c("WTD", "Aridity"))]

# Croplands ---------------------------------------------------------------
df_cropland <- readRDS(paste0(model_training_folder, "cshap_long_croplands.rds"))
ori_cropland <- as.data.frame(df_cropland$x_test)
shap_cropland <- df_cropland$dt %>%
  mutate(WTD_rfvalue = ori_cropland$WTD,
         Aridity_rfvalue = ori_cropland$Aridity,
         Elevation_rfvalue = ori_cropland$Elevation) %>%
  rename(Elevation = Elevation)

# standardization
shap_cropland$WTD_stdfvalue <- std1(shap_cropland$WTD_rfvalue)
shap_cropland$Aridity_stdfvalue <- std1(shap_cropland$Aridity_rfvalue)
shap_cropland$Elevation_stdfvalue <- std1(shap_cropland$Elevation_rfvalue)

# contribution calculation
shap_cropland$WTD_mean_value <- mean(abs(shap_cropland$WTD), na.rm = TRUE)
shap_cropland$Aridity_mean_value <- mean(abs(shap_cropland$Aridity), na.rm = TRUE)
shap_cropland$Elevation_mean_value <- mean(abs(shap_cropland$Elevation), na.rm = TRUE)
shap_long_crop <- melt(shap_cropland,
                       measure.vars = list(
                         c("WTD", "Aridity"),        # SHAP values
                         c("WTD_rfvalue", "Aridity_rfvalue"),  # rfvalue
                         c("WTD_stdfvalue", "Aridity_stdfvalue"),  # stdfvalue
                         c("WTD_mean_value", "Aridity_mean_value")  # mean_value
                       ),
                       variable.name = "variable",
                       value.name = c("value", "rfvalue", "stdfvalue", "mean_value"))
shap_long_crop[, variable := factor(variable, labels = c("WTD", "Aridity"))]


# Grasslands --------------------------------------------------------------
df_grassland <- readRDS(paste0(model_training_folder, "cshap_long_grasslands.rds"))
ori_grassland <- as.data.frame(df_grassland$x_test)
shap_grassland <- df_grassland$dt %>%
  mutate(WTD_rfvalue = ori_grassland$WTD,
         Aridity_rfvalue = ori_grassland$Aridity,
         Elevation_rfvalue = ori_grassland$Elevation) %>%
  rename(Elevation = Elevation)

# standardization
shap_grassland$WTD_stdfvalue <- std1(shap_grassland$WTD_rfvalue)
shap_grassland$Aridity_stdfvalue <- std1(shap_grassland$Aridity_rfvalue)
shap_grassland$Elevation_stdfvalue <- std1(shap_grassland$Elevation_rfvalue)

# contribution calculation
shap_grassland$WTD_mean_value <- mean(abs(shap_grassland$WTD), na.rm = TRUE)
shap_grassland$Aridity_mean_value <- mean(abs(shap_grassland$Aridity), na.rm = TRUE)
shap_grassland$Elevation_mean_value <- mean(abs(shap_grassland$Elevation), na.rm = TRUE)
shap_long_grass <- melt(shap_grassland,
                        measure.vars = list(
                          c("WTD", "Aridity"),        # SHAP values
                          c("WTD_rfvalue", "Aridity_rfvalue"),  # rfvalue
                          c("WTD_stdfvalue", "Aridity_stdfvalue"),  # stdfvalue
                          c("WTD_mean_value", "Aridity_mean_value")  # mean_value
                        ),
                        variable.name = "variable",
                        value.name = c("value", "rfvalue", "stdfvalue", "mean_value"))
shap_long_grass[, variable := factor(variable, labels = c("WTD", "Aridity"))]


# define common theme -----------------------------------------------------
common_theme <-  theme(legend.position = "bottom",
                       plot.title = element_text(hjust = 0.5, size = 14),
                       legend.key.width = unit(1.62, "cm"),
                       legend.key.height = unit(0.2, "cm"),
                       legend.margin = margin(t = 15, unit = "pt"),
                       panel.grid.major.x = element_blank(),
                       panel.grid.minor.x = element_blank(),
                       legend.title = element_text(size = 14),
                       legend.text = element_text(size = 12),
                       axis.title.y = element_text(size = 13), # vertical axis
                       axis.text.y = element_text(size = 13),
                       axis.title.x = element_text(size = 13, vjust = -1), # horizontal axis
                       axis.text.x = element_text(size = 12),
                       plot.margin = unit(c(3, 5, 8, 3), "points")
)

custom_labels <- c(expression(lambda * P/R[n]), "WTD")



# plots -------------------------------------------------------------------

a <- ggplot(data = shap_long_forest) +
  coord_flip() +
  geom_sina(aes(x = variable, y = value, color = stdfvalue), alpha = 0.1, size = 0.35) +
  geom_text(data = unique(shap_long_forest[, c("variable", "mean_value")]),
            aes(x = variable, y = -Inf, label = sprintf("%.3f", mean_value)),
            size = 4, alpha = 0.8,
            hjust = -0.2,
            fontface = "bold") +
  scale_color_viridis_c(option = "inferno", begin = 0.15,
                        breaks = c(0, 1), labels = c("Low", "High"),
                        guide = guide_colourbar(display = "raster", nbin = 512)) +
  ggtitle("Forests") +
  theme_bw() +
  common_theme +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-0.50, 0.50),
                     breaks = seq(-0.50, 0.50, by = 0.25)) +
  scale_x_discrete(limits = rev(levels(shap_long_forest$variable)),
                   labels = custom_labels
  ) +
  labs(y = "", x = "", color = "Feature value   ")

b <- ggplot(data = shap_long_dry) +
  coord_flip() +
  geom_sina(aes(x = variable, y = value, color = stdfvalue), alpha = 0.1, size = 0.55) +
  geom_text(data = unique(shap_long_dry[, c("variable", "mean_value")]),
            aes(x = variable, y = -Inf, label = sprintf("%.3f", mean_value)),
            size = 4, alpha = 0.8,
            hjust = -0.2,
            fontface = "bold") +
  scale_color_viridis_c(option = "inferno", begin = 0.15,
                        breaks = c(0, 1), labels = c("Low", "High"),
                        guide = guide_colourbar(display = "raster", nbin = 512)) +
  ggtitle("Savannahs and shrublands") +
  theme_bw() +
  common_theme +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-0.50, 0.50),
                     breaks = seq(-0.50, 0.50, by = 0.25)) +
  scale_x_discrete(limits = rev(levels(shap_long_dry$variable)),
                   labels = custom_labels
  ) +
  labs(y = "", x = "", color = "Feature value   ")

c <- ggplot(data = shap_long_crop) +
  coord_flip() +
  geom_sina(aes(x = variable, y = value, color = stdfvalue), alpha = 0.1, size = 0.55) +
  geom_text(data = unique(shap_long_crop[, c("variable", "mean_value")]),
            aes(x = variable, y = -Inf, label = sprintf("%.3f", mean_value)),
            size = 4, alpha = 0.8,
            hjust = -0.2,
            fontface = "bold") +
  scale_color_viridis_c(option = "inferno", begin = 0.15,
                        breaks = c(0, 1), labels = c("Low", "High"),
                        guide = guide_colourbar(display = "raster", nbin = 512)) +
  ggtitle("Croplands") +
  theme_bw() +
  common_theme +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-0.50, 0.50),
                     breaks = seq(-0.50, 0.50, by = 0.25)) +
  scale_x_discrete(limits = rev(levels(shap_long_crop$variable)),
                   labels = custom_labels
  ) +
  labs(y = "Shapley value (impact on fPAR)", x = "", color = "Feature value   ")

d <- ggplot(data = shap_long_grass) +
  coord_flip() +
  geom_sina(aes(x = variable, y = value, color = stdfvalue), alpha = 0.1, size = 0.55) +
  geom_text(data = unique(shap_long_grass[, c("variable", "mean_value")]),
            aes(x = variable, y = -Inf, label = sprintf("%.3f", mean_value)),
            size = 4, alpha = 0.8,
            hjust = -0.2,
            fontface = "bold") +
  scale_color_viridis_c(option = "inferno", begin = 0.15,
                        breaks = c(0, 1), labels = c("Low", "High"),
                        guide = guide_colourbar(display = "raster", nbin = 512)) +
  ggtitle("Grasslands") +
  theme_bw() +
  common_theme +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-0.50, 0.50),
                     breaks = seq(-0.50, 0.50, by = 0.25)) +
  scale_x_discrete(limits = rev(levels(shap_long_grass$variable)),
                   labels = custom_labels
  ) +
  labs(y = "Shapley value (impact on fPAR)", x = "", color = "Feature value   ")

# combine graphs
fig <- ggarrange(a, b, c, d,
                 labels = "auto",
                 ncol = 2, nrow = 2,
                 align = "hv",
                 common.legend = TRUE,
                 legend="bottom"
)



png(filename = paste0("./",
                      "FigS_highres_beeswarm_without_elevation.png"),
    width = 9.82, height = 5, units = "in", res = 300)
print(fig)
dev.off()


# Plot including elevation ------------------------------------------------

prepare_elevation_beeswarm <- function(shap_object) {
  original_values <- as.data.frame(shap_object$x_test)
  shap_values <- data.table::as.data.table(data.table::copy(shap_object$dt))

  stopifnot(
    nrow(shap_values) == nrow(original_values),
    all(c("WTD", "Aridity", "Elevation") %in% names(shap_values)),
    all(c("WTD", "Aridity", "Elevation") %in% names(original_values))
  )

  shap_values[, `:=`(
    WTD_rfvalue = original_values$WTD,
    Aridity_rfvalue = original_values$Aridity,
    Elevation_rfvalue = original_values$Elevation,
    WTD_stdfvalue = std1(original_values$WTD),
    Aridity_stdfvalue = std1(original_values$Aridity),
    Elevation_stdfvalue = std1(original_values$Elevation),
    WTD_mean_value = mean(abs(WTD), na.rm = TRUE),
    Aridity_mean_value = mean(abs(Aridity), na.rm = TRUE),
    Elevation_mean_value = mean(abs(Elevation), na.rm = TRUE)
  )]

  shap_long <- data.table::melt(
    shap_values,
    measure.vars = list(
      c("WTD", "Aridity", "Elevation"),
      c("WTD_rfvalue", "Aridity_rfvalue", "Elevation_rfvalue"),
      c("WTD_stdfvalue", "Aridity_stdfvalue", "Elevation_stdfvalue"),
      c("WTD_mean_value", "Aridity_mean_value", "Elevation_mean_value")
    ),
    variable.name = "variable",
    value.name = c("value", "rfvalue", "stdfvalue", "mean_value")
  )
  shap_long[, variable := factor(
    variable,
    levels = seq_len(3),
    labels = c("WTD", "Aridity", "Elevation")
  )]
  shap_long
}

plot_elevation_beeswarm <- function(data, title, point_size, show_axis_title,
                                    shap_limit) {
  feature_labels <- c("Elevation", expression(lambda * P/R[n]), "WTD")

  ggplot(data) +
    coord_flip() +
    geom_sina(
      aes(x = variable, y = value, color = stdfvalue),
      alpha = 0.1,
      size = point_size
    ) +
    geom_text(
      data = unique(data[, .(variable, mean_value)]),
      aes(x = variable, y = -Inf, label = sprintf("%.3f", mean_value)),
      size = 4,
      alpha = 0.8,
      hjust = -0.2,
      fontface = "bold"
    ) +
    scale_color_viridis_c(
      option = "inferno",
      begin = 0.15,
      breaks = c(0, 1),
      labels = c("Low", "High"),
      guide = guide_colourbar(display = "raster", nbin = 512)
    ) +
    scale_y_continuous(
      limits = c(-shap_limit, shap_limit),
      breaks = seq(-0.50, 0.50, by = 0.25)
    ) +
    scale_x_discrete(
      limits = rev(levels(data$variable)),
      labels = feature_labels
    ) +
    ggtitle(title) +
    theme_bw() +
    common_theme +
    geom_hline(yintercept = 0) +
    labs(
      y = if (show_axis_title) "Shapley value (impact on fPAR)" else "",
      x = "",
      color = "Feature value   "
    )
}

shap_long_forest_elevation <- prepare_elevation_beeswarm(df_forest)
shap_long_savannah_elevation <- prepare_elevation_beeswarm(df_savannas_and_scrublands)
shap_long_crop_elevation <- prepare_elevation_beeswarm(df_cropland)
shap_long_grass_elevation <- prepare_elevation_beeswarm(df_grassland)

elevation_shap_limit <- max(
  abs(shap_long_forest_elevation$value),
  abs(shap_long_savannah_elevation$value),
  abs(shap_long_crop_elevation$value),
  abs(shap_long_grass_elevation$value),
  na.rm = TRUE
)
elevation_shap_limit <- max(0.50, ceiling(elevation_shap_limit * 20) / 20)

a_elevation <- plot_elevation_beeswarm(
  shap_long_forest_elevation, "Forests", 0.35, FALSE, elevation_shap_limit
)
b_elevation <- plot_elevation_beeswarm(
  shap_long_savannah_elevation, "Savannahs and shrublands", 0.55, FALSE,
  elevation_shap_limit
)
c_elevation <- plot_elevation_beeswarm(
  shap_long_crop_elevation, "Croplands", 0.55, TRUE, elevation_shap_limit
)
d_elevation <- plot_elevation_beeswarm(
  shap_long_grass_elevation, "Grasslands", 0.55, TRUE, elevation_shap_limit
)

fig_elevation <- ggarrange(
  a_elevation, b_elevation, c_elevation, d_elevation,
  labels = "auto",
  ncol = 2,
  nrow = 2,
  align = "hv",
  common.legend = TRUE,
  legend = "bottom"
)

png(
  filename = "Supplementary Figure 10.png",
  width = 9.82,
  height = 5.8,
  units = "in",
  res = 300
)
print(fig_elevation)
dev.off()
