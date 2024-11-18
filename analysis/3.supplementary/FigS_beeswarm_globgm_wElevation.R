rm(list=ls())
library(tidyverse)
library(cowplot)
library(ggforce)
library(ggpubr)
library(data.table)

std1 <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

model_training_folder <- 'data/jiangong/model_training_elevation_US_globgm/'

# forest
df_forest <- readRDS(paste0(model_training_folder, "cshap_long_forests.rds"))
ori_forest <- as.data.frame(df_forest$x_test)
shap_forest <- df_forest$dt %>%
  mutate(WTD_rfvalue = ori_forest$WTD,
         Aridity_rfvalue = ori_forest$Aridity,
         Elevation_rfvalue = ori_forest$Elevation)
# Step 1: standardization
shap_forest$WTD_stdfvalue <- std1(shap_forest$WTD_rfvalue)
shap_forest$Aridity_stdfvalue <- std1(shap_forest$Aridity_rfvalue)
shap_forest$Elevation_stdfvalue <- std1(shap_forest$Elevation_rfvalue)
# Step 2: contribution calculation
shap_forest$WTD_mean_value <- mean(abs(shap_forest$WTD), na.rm = TRUE)
shap_forest$Aridity_mean_value <- mean(abs(shap_forest$Aridity), na.rm = TRUE)
shap_forest$Elevation_mean_value <- mean(abs(shap_forest$Elevation), na.rm = TRUE)
shap_long_forest <- melt(shap_forest,
                         measure.vars = list(
                           c("WTD", "Aridity", "Elevation"),        # SHAP values
                           c("WTD_rfvalue", "Aridity_rfvalue", "Elevation_rfvalue"),  # rfvalue
                           c("WTD_stdfvalue", "Aridity_stdfvalue", "Elevation_stdfvalue"),  # stdfvalue
                           c("WTD_mean_value", "Aridity_mean_value", "Elevation_mean_value")  # mean_value
                         ),
                         variable.name = "variable",
                         value.name = c("value", "rfvalue", "stdfvalue", "mean_value"))
shap_long_forest[, variable := factor(variable, labels = c("WTD", "Aridity", "Elevation"))]

# savannas_and_scrublands
df_savannas_and_scrublands <- readRDS(paste0(model_training_folder, "cshap_long_savannas_and_scrublands.rds"))
ori_savannas_and_scrublands <- as.data.frame(df_savannas_and_scrublands$x_test)
shap_savannas_and_scrublands <- df_savannas_and_scrublands$dt %>%
  mutate(WTD_rfvalue = ori_savannas_and_scrublands$WTD,
         Aridity_rfvalue = ori_savannas_and_scrublands$Aridity,
         Elevation_rfvalue = ori_savannas_and_scrublands$Elevation)
# Step 1: standardization
shap_savannas_and_scrublands$WTD_stdfvalue <- std1(shap_savannas_and_scrublands$WTD_rfvalue)
shap_savannas_and_scrublands$Aridity_stdfvalue <- std1(shap_savannas_and_scrublands$Aridity_rfvalue)
shap_savannas_and_scrublands$Elevation_stdfvalue <- std1(shap_savannas_and_scrublands$Elevation_rfvalue)
# Step 2: contribution calculation
shap_savannas_and_scrublands$WTD_mean_value <- mean(abs(shap_savannas_and_scrublands$WTD), na.rm = TRUE)
shap_savannas_and_scrublands$Aridity_mean_value <- mean(abs(shap_savannas_and_scrublands$Aridity), na.rm = TRUE)
shap_savannas_and_scrublands$Elevation_mean_value <- mean(abs(shap_savannas_and_scrublands$Elevation), na.rm = TRUE)
shap_long_dry <- melt(shap_savannas_and_scrublands,
                         measure.vars = list(
                           c("WTD", "Aridity", "Elevation"),        # SHAP values
                           c("WTD_rfvalue", "Aridity_rfvalue", "Elevation_rfvalue"),  # rfvalue
                           c("WTD_stdfvalue", "Aridity_stdfvalue", "Elevation_stdfvalue"),  # stdfvalue
                           c("WTD_mean_value", "Aridity_mean_value", "Elevation_mean_value")  # mean_value
                         ),
                         variable.name = "variable",
                         value.name = c("value", "rfvalue", "stdfvalue", "mean_value"))
shap_long_dry[, variable := factor(variable, labels = c("WTD", "Aridity", "Elevation"))]

# cropland
df_cropland <- readRDS(paste0(model_training_folder, "cshap_long_croplands.rds"))
ori_cropland <- as.data.frame(df_cropland$x_test)
shap_cropland <- df_cropland$dt %>%
  mutate(WTD_rfvalue = ori_cropland$WTD,
         Aridity_rfvalue = ori_cropland$Aridity,
         Elevation_rfvalue = ori_cropland$Elevation)
# Step 1: standardization
shap_cropland$WTD_stdfvalue <- std1(shap_cropland$WTD_rfvalue)
shap_cropland$Aridity_stdfvalue <- std1(shap_cropland$Aridity_rfvalue)
shap_cropland$Elevation_stdfvalue <- std1(shap_cropland$Elevation_rfvalue)
# Step 2: contribution calculation
shap_cropland$WTD_mean_value <- mean(abs(shap_cropland$WTD), na.rm = TRUE)
shap_cropland$Aridity_mean_value <- mean(abs(shap_cropland$Aridity), na.rm = TRUE)
shap_cropland$Elevation_mean_value <- mean(abs(shap_cropland$Elevation), na.rm = TRUE)
shap_long_crop <- melt(shap_cropland,
                         measure.vars = list(
                           c("WTD", "Aridity", "Elevation"),        # SHAP values
                           c("WTD_rfvalue", "Aridity_rfvalue", "Elevation_rfvalue"),  # rfvalue
                           c("WTD_stdfvalue", "Aridity_stdfvalue", "Elevation_stdfvalue"),  # stdfvalue
                           c("WTD_mean_value", "Aridity_mean_value", "Elevation_mean_value")  # mean_value
                         ),
                         variable.name = "variable",
                         value.name = c("value", "rfvalue", "stdfvalue", "mean_value"))
shap_long_crop[, variable := factor(variable, labels = c("WTD", "Aridity", "Elevation"))]

# grassland
df_grassland <- readRDS(paste0(model_training_folder, "cshap_long_grasslands.rds"))
ori_grassland <- as.data.frame(df_grassland$x_test)
shap_grassland <- df_grassland$dt %>%
  mutate(WTD_rfvalue = ori_grassland$WTD,
         Aridity_rfvalue = ori_grassland$Aridity,
         Elevation_rfvalue = ori_grassland$Elevation)
# Step 1: standardization
shap_grassland$WTD_stdfvalue <- std1(shap_grassland$WTD_rfvalue)
shap_grassland$Aridity_stdfvalue <- std1(shap_grassland$Aridity_rfvalue)
shap_grassland$Elevation_stdfvalue <- std1(shap_grassland$Elevation_rfvalue)
# Step 2: contribution calculation
shap_grassland$WTD_mean_value <- mean(abs(shap_grassland$WTD), na.rm = TRUE)
shap_grassland$Aridity_mean_value <- mean(abs(shap_grassland$Aridity), na.rm = TRUE)
shap_grassland$Elevation_mean_value <- mean(abs(shap_grassland$Elevation), na.rm = TRUE)
shap_long_grass <- melt(shap_grassland,
                         measure.vars = list(
                           c("WTD", "Aridity", "Elevation"),        # SHAP values
                           c("WTD_rfvalue", "Aridity_rfvalue", "Elevation_rfvalue"),  # rfvalue
                           c("WTD_stdfvalue", "Aridity_stdfvalue", "Elevation_stdfvalue"),  # stdfvalue
                           c("WTD_mean_value", "Aridity_mean_value", "Elevation_mean_value")  # mean_value
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
                       axis.title.y = element_text(size = 13), # vertical axis
                       axis.text.y = element_text(size = 13),
                       axis.title.x = element_text(size = 13, vjust = -1), # horizontal axis
                       axis.text.x = element_text(size = 12),
                       plot.margin = unit(c(3, 5, 8, 3), "points")
)

custom_labels <- c(expression("Elevation", "λP/R"[n]), "WTD") # if change data: check that they correspond to actual showed variables

x_bound <- round(max(abs(shap_long_forest$value))) + 0.5
a <- ggplot(data = shap_long_forest) +
  coord_flip() +
  geom_sina(aes(x = variable, y = value, color = stdfvalue), alpha = 0.1, size = 0.35) +
  geom_text(data = unique(shap_long_forest[, c("variable", "mean_value")]),
            aes(x = variable, y = -Inf, label = sprintf("%.3f", mean_value)),
            size = 4, alpha = 0.8,
            hjust = -0.2,
            fontface = "bold") + # bold
  scale_color_gradient(low = "#1518FD", high = "#FE0104", breaks = c(0, 1), labels = c("Low", "High")) +
  ggtitle("Forests") +
  theme_bw() +
  common_theme +
  geom_hline(yintercept = 0) + # the vertical line
  scale_y_continuous(limits = c(-2, 2),
                     breaks = seq(-2, 2, by = 0.5)) +
  scale_x_discrete(limits = rev(levels(shap_long_forest$variable)),
                   labels = custom_labels
                   ) +
  labs(y = "", x = "", color = "Feature value   ")

x_bound <- round(max(abs(shap_long_dry$value))) + 0.5
b <- ggplot(data = shap_long_dry) +
  coord_flip() +
  geom_sina(aes(x = variable, y = value, color = stdfvalue), alpha = 0.1, size = 0.55) +
  geom_text(data = unique(shap_long_dry[, c("variable", "mean_value")]),
            aes(x = variable, y = -Inf, label = sprintf("%.3f", mean_value)),
            size = 4, alpha = 0.8,
            hjust = -0.2,
            fontface = "bold") + # bold
  scale_color_gradient(low = "#1518FD", high = "#FE0104", breaks = c(0, 1), labels = c("Low", "High")) +
  ggtitle("Savannas and shrublands") +
  theme_bw() +
  common_theme +
  geom_hline(yintercept = 0) + # the vertical line
  scale_y_continuous(limits = c(-2, 2),
                     breaks = seq(-2, 2, by = 0.5)) +
  scale_x_discrete(limits = rev(levels(shap_long_dry$variable)),
                   labels = custom_labels) +
  labs(y = "", x = "", color = "Feature value   ")

x_bound <- round(max(abs(shap_long_crop$value))) + 0.5
c <- ggplot(data = shap_long_crop) +
  coord_flip() +
  geom_sina(aes(x = variable, y = value, color = stdfvalue), alpha = 0.1, size = 0.55) +
  geom_text(data = unique(shap_long_crop[, c("variable", "mean_value")]),
            aes(x = variable, y = -Inf, label = sprintf("%.3f", mean_value)),
            size = 4, alpha = 0.8,
            hjust = -0.2,
            fontface = "bold") + # bold
  scale_color_gradient(low = "#1518FD", high = "#FE0104", breaks = c(0, 1), labels = c("Low", "High")) +
  ggtitle("Croplands") +
  theme_bw() +
  common_theme +
  geom_hline(yintercept = 0) + # the vertical line
  scale_y_continuous(limits = c(-2, 2),
                     breaks = seq(-2, 2, by = 0.5)) +
  scale_x_discrete(limits = rev(levels(shap_long_crop$variable)),
                   labels = custom_labels) +
  labs(y = "Shapely value (impact on SIF)", x = "", color = "Feature value   ")

x_bound <- round(max(abs(shap_long_grass$value))) + 1
d <- ggplot(data = shap_long_grass) +
  coord_flip() +
  geom_sina(aes(x = variable, y = value, color = stdfvalue), alpha = 0.1, size = 0.55) +
  geom_text(data = unique(shap_long_grass[, c("variable", "mean_value")]),
            aes(x = variable, y = -Inf, label = sprintf("%.3f", mean_value)),
            size = 4, alpha = 0.8,
            hjust = -0.2,
            fontface = "bold") + # bold
  scale_color_gradient(low = "#1518FD", high = "#FE0104", breaks = c(0, 1), labels = c("Low", "High")) +
  ggtitle("Grasslands") +
  theme_bw() +
  common_theme +
  geom_hline(yintercept = 0) + # the vertical line
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
                 common.legend = TRUE, # have just one common legend
                 legend="bottom"
)

png(filename = paste0("./",
                      "Fig_3_1_globgm.png"),
    width = 10, height = 5.5, units = "in", res = 300)
print(fig)
dev.off()











