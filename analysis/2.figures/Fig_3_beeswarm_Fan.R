# Plot beeswarm plots for the moisture index, water table depth and elevation (dataset from Fan et al)

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

model_training_folder <- 'data/model_output/model_training_elevation_US_Fan/'


# Forests -----------------------------------------------------------------
df_forest <- readRDS(paste0(model_training_folder, "cshap_long_forests.rds"))
ori_forest <- as.data.frame(df_forest$x_test)
shap_forest <- df_forest$dt %>%
  mutate(WTD_rfvalue = ori_forest$WTD,
         Aridity_rfvalue = ori_forest$Aridity,
         Elevation_rfvalue = ori_forest$Elevation)

shap_forest$WTD_stdfvalue <- std1(shap_forest$WTD_rfvalue)
shap_forest$Aridity_stdfvalue <- std1(shap_forest$Aridity_rfvalue)
shap_forest$Elevation_stdfvalue <- std1(shap_forest$Elevation_rfvalue)

shap_forest$WTD_mean_value <- mean(abs(shap_forest$WTD), na.rm = TRUE)
shap_forest$Aridity_mean_value <- mean(abs(shap_forest$Aridity), na.rm = TRUE)
shap_forest$Elevation_mean_value <- mean(abs(shap_forest$Elevation), na.rm = TRUE)
shap_long_forest <- melt(shap_forest,
                         measure.vars = list(
                           c("WTD", "Aridity", "Elevation"),
                           c("WTD_rfvalue", "Aridity_rfvalue", "Elevation_rfvalue"),
                           c("WTD_stdfvalue", "Aridity_stdfvalue", "Elevation_stdfvalue"),
                           c("WTD_mean_value", "Aridity_mean_value", "Elevation_mean_value")
                         ),
                         variable.name = "variable",
                         value.name = c("value", "rfvalue", "stdfvalue", "mean_value"))
shap_long_forest[, variable := factor(variable, labels = c("WTD", "Aridity", "Elevation"))]


# Savannahs and shrublands ------------------------------------------------
df_savannas_and_scrublands <- readRDS(paste0(model_training_folder, "cshap_long_savannas_and_scrublands.rds"))
ori_savannas_and_scrublands <- as.data.frame(df_savannas_and_scrublands$x_test)
shap_savannas_and_scrublands <- df_savannas_and_scrublands$dt %>%
  mutate(WTD_rfvalue = ori_savannas_and_scrublands$WTD,
         Aridity_rfvalue = ori_savannas_and_scrublands$Aridity,
         Elevation_rfvalue = ori_savannas_and_scrublands$Elevation)

shap_savannas_and_scrublands$WTD_stdfvalue <- std1(shap_savannas_and_scrublands$WTD_rfvalue)
shap_savannas_and_scrublands$Aridity_stdfvalue <- std1(shap_savannas_and_scrublands$Aridity_rfvalue)
shap_savannas_and_scrublands$Elevation_stdfvalue <- std1(shap_savannas_and_scrublands$Elevation_rfvalue)

shap_savannas_and_scrublands$WTD_mean_value <- mean(abs(shap_savannas_and_scrublands$WTD), na.rm = TRUE)
shap_savannas_and_scrublands$Aridity_mean_value <- mean(abs(shap_savannas_and_scrublands$Aridity), na.rm = TRUE)
shap_savannas_and_scrublands$Elevation_mean_value <- mean(abs(shap_savannas_and_scrublands$Elevation), na.rm = TRUE)
shap_long_dry <- melt(shap_savannas_and_scrublands,
                      measure.vars = list(
                        c("WTD", "Aridity", "Elevation"),
                        c("WTD_rfvalue", "Aridity_rfvalue", "Elevation_rfvalue"),
                        c("WTD_stdfvalue", "Aridity_stdfvalue", "Elevation_stdfvalue"),
                        c("WTD_mean_value", "Aridity_mean_value", "Elevation_mean_value")
                      ),
                      variable.name = "variable",
                      value.name = c("value", "rfvalue", "stdfvalue", "mean_value"))
shap_long_dry[, variable := factor(variable, labels = c("WTD", "Aridity", "Elevation"))]

# Croplands ---------------------------------------------------------------
df_cropland <- readRDS(paste0(model_training_folder, "cshap_long_croplands.rds"))
ori_cropland <- as.data.frame(df_cropland$x_test)
shap_cropland <- df_cropland$dt %>%
  mutate(WTD_rfvalue = ori_cropland$WTD,
         Aridity_rfvalue = ori_cropland$Aridity,
         Elevation_rfvalue = ori_cropland$Elevation)

shap_cropland$WTD_stdfvalue <- std1(shap_cropland$WTD_rfvalue)
shap_cropland$Aridity_stdfvalue <- std1(shap_cropland$Aridity_rfvalue)
shap_cropland$Elevation_stdfvalue <- std1(shap_cropland$Elevation_rfvalue)

shap_cropland$WTD_mean_value <- mean(abs(shap_cropland$WTD), na.rm = TRUE)
shap_cropland$Aridity_mean_value <- mean(abs(shap_cropland$Aridity), na.rm = TRUE)
shap_cropland$Elevation_mean_value <- mean(abs(shap_cropland$Elevation), na.rm = TRUE)
shap_long_crop <- melt(shap_cropland,
                       measure.vars = list(
                         c("WTD", "Aridity", "Elevation"),
                         c("WTD_rfvalue", "Aridity_rfvalue", "Elevation_rfvalue"),
                         c("WTD_stdfvalue", "Aridity_stdfvalue", "Elevation_stdfvalue"),
                         c("WTD_mean_value", "Aridity_mean_value", "Elevation_mean_value")
                       ),
                       variable.name = "variable",
                       value.name = c("value", "rfvalue", "stdfvalue", "mean_value"))
shap_long_crop[, variable := factor(variable, labels = c("WTD", "Aridity", "Elevation"))]


# Grasslands --------------------------------------------------------------
df_grassland <- readRDS(paste0(model_training_folder, "cshap_long_grasslands.rds"))
ori_grassland <- as.data.frame(df_grassland$x_test)
shap_grassland <- df_grassland$dt %>%
  mutate(WTD_rfvalue = ori_grassland$WTD,
         Aridity_rfvalue = ori_grassland$Aridity,
         Elevation_rfvalue = ori_grassland$Elevation)

shap_grassland$WTD_stdfvalue <- std1(shap_grassland$WTD_rfvalue)
shap_grassland$Aridity_stdfvalue <- std1(shap_grassland$Aridity_rfvalue)
shap_grassland$Elevation_stdfvalue <- std1(shap_grassland$Elevation_rfvalue)

shap_grassland$WTD_mean_value <- mean(abs(shap_grassland$WTD), na.rm = TRUE)
shap_grassland$Aridity_mean_value <- mean(abs(shap_grassland$Aridity), na.rm = TRUE)
shap_grassland$Elevation_mean_value <- mean(abs(shap_grassland$Elevation), na.rm = TRUE)
shap_long_grass <- melt(shap_grassland,
                        measure.vars = list(
                          c("WTD", "Aridity", "Elevation"),
                          c("WTD_rfvalue", "Aridity_rfvalue", "Elevation_rfvalue"),
                          c("WTD_stdfvalue", "Aridity_stdfvalue", "Elevation_stdfvalue"),
                          c("WTD_mean_value", "Aridity_mean_value", "Elevation_mean_value")
                        ),
                        variable.name = "variable",
                        value.name = c("value", "rfvalue", "stdfvalue", "mean_value"))
shap_long_grass[, variable := factor(variable, labels = c("WTD", "Aridity", "Elevation"))]


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
                       axis.title.y = element_text(size = 13),
                       axis.text.y = element_text(size = 13),
                       axis.title.x = element_text(size = 13, vjust = -1),
                       axis.text.x = element_text(size = 12),
                       plot.margin = unit(c(3, 5, 8, 3), "points")
                       )

custom_labels <- c("Elevation", expression(lambda * P/R[n]), "WTD")



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
                        guide = guide_colourbar(display = "gradient")) +
  ggtitle("Forests") +
  theme_bw() +
  common_theme +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-2, 2),
                     breaks = seq(-2, 2, by = 0.5)) +
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
                        guide = guide_colourbar(display = "gradient")) +
  ggtitle("Savannahs and shrublands") +
  theme_bw() +
  common_theme +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-2, 2),
                     breaks = seq(-2, 2, by = 0.5)) +
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
                        guide = guide_colourbar(display = "gradient")) +
  ggtitle("Croplands") +
  theme_bw() +
  common_theme +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-2, 2),
                     breaks = seq(-2, 2, by = 0.5)) +
  scale_x_discrete(limits = rev(levels(shap_long_crop$variable)),
                   labels = custom_labels
                   ) +
  labs(y = "Shapley value (impact on SIF)", x = "", color = "Feature value   ")

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
                        guide = guide_colourbar(display = "gradient")) +
  ggtitle("Grasslands") +
  theme_bw() +
  common_theme +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-2, 2),
                     breaks = seq(-2, 2, by = 0.5)) +
  scale_x_discrete(limits = rev(levels(shap_long_grass$variable)),
                   labels = custom_labels
                   ) +
  labs(y = "Shapley value (impact on SIF)", x = "", color = "Feature value   ")

# combine graphs
fig <- ggarrange(a, b, c, d,
                 labels = "auto",
                 ncol = 2, nrow = 2,
                 align = "hv",
                 common.legend = TRUE,
                 legend="bottom"
)



grDevices::cairo_pdf(filename = "Figure 3.pdf",
                     width = 9.82, height = 5.5, bg = "white")
print(fig)
dev.off()

png_guide <- guide_colourbar(display = "raster", nbin = 512)
fig_png <- ggarrange(a + guides(color = png_guide),
                     b + guides(color = png_guide),
                     c + guides(color = png_guide),
                     d + guides(color = png_guide),
                     labels = "auto",
                     ncol = 2, nrow = 2,
                     align = "hv",
                     common.legend = TRUE,
                     legend = "bottom")

ggsave("Figure 3.png", plot = fig_png, path = "./",
       width = 9.82, height = 5.5, units = "in", dpi = 600, bg = "white")
