# this is a version for figure 3 that elevation is not exported
rm(list=ls())
library(tidyverse)
library(cowplot)
library(ggforce)
library(ggpubr)
library(data.table)

std1 <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

model_training_folder <-"/Users/jiangongliu/Desktop/5_WTD_Aridity/new_dataframe/model_training_elevation_US_globgm/"

# forest
df_forest <- readRDS(paste0(model_training_folder, "cshap_long_forests.rds"))
ori_forest <- as.data.frame(df_forest$x_test)
shap_forest <- df_forest$dt %>%
  mutate(WTD_rfvalue = ori_forest$WTD,
         Aridity_rfvalue = ori_forest$Aridity) %>%
  select(-Elevation)
# Step 1: standardization
shap_forest$WTD_stdfvalue <- std1(shap_forest$WTD_rfvalue)
shap_forest$Aridity_stdfvalue <- std1(shap_forest$Aridity_rfvalue)
# Step 2: contribution calculation
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

# savannas_and_scrublands
df_savannas_and_scrublands <- readRDS(paste0(model_training_folder, "cshap_long_savannas_and_scrublands.rds"))
ori_savannas_and_scrublands <- as.data.frame(df_savannas_and_scrublands$x_test)
shap_savannas_and_scrublands <- df_savannas_and_scrublands$dt %>%
  mutate(WTD_rfvalue = ori_savannas_and_scrublands$WTD,
         Aridity_rfvalue = ori_savannas_and_scrublands$Aridity) %>%
  select(-Elevation)
# Step 1: standardization
shap_savannas_and_scrublands$WTD_stdfvalue <- std1(shap_savannas_and_scrublands$WTD_rfvalue)
shap_savannas_and_scrublands$Aridity_stdfvalue <- std1(shap_savannas_and_scrublands$Aridity_rfvalue)
# Step 2: contribution calculation
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

# cropland
df_cropland <- readRDS(paste0(model_training_folder, "cshap_long_croplands.rds"))
ori_cropland <- as.data.frame(df_cropland$x_test)
shap_cropland <- df_cropland$dt %>%
  mutate(WTD_rfvalue = ori_cropland$WTD,
         Aridity_rfvalue = ori_cropland$Aridity,
         Elevation_rfvalue = ori_cropland$Elevation) %>%
  rename(Elevation = Elevation)
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
                         c("WTD", "Aridity"),        # SHAP values
                         c("WTD_rfvalue", "Aridity_rfvalue"),  # rfvalue
                         c("WTD_stdfvalue", "Aridity_stdfvalue"),  # stdfvalue
                         c("WTD_mean_value", "Aridity_mean_value")  # mean_value
                       ),
                       variable.name = "variable",
                       value.name = c("value", "rfvalue", "stdfvalue", "mean_value"))
shap_long_crop[, variable := factor(variable, labels = c("WTD", "Aridity"))]

# grassland
df_grassland <- readRDS(paste0(model_training_folder, "cshap_long_grasslands.rds"))
ori_grassland <- as.data.frame(df_grassland$x_test)
shap_grassland <- df_grassland$dt %>%
  mutate(WTD_rfvalue = ori_grassland$WTD,
         Aridity_rfvalue = ori_grassland$Aridity,
         Elevation_rfvalue = ori_grassland$Elevation) %>%
  rename(Elevation = Elevation)
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
                          c("WTD", "Aridity"),        # SHAP values
                          c("WTD_rfvalue", "Aridity_rfvalue"),  # rfvalue
                          c("WTD_stdfvalue", "Aridity_stdfvalue"),  # stdfvalue
                          c("WTD_mean_value", "Aridity_mean_value")  # mean_value
                        ),
                        variable.name = "variable",
                        value.name = c("value", "rfvalue", "stdfvalue", "mean_value"))
shap_long_grass[, variable := factor(variable, labels = c("WTD", "Aridity"))]

# define common theme
common_theme <-  theme(legend.position = "bottom",
                       plot.title = element_text(hjust = 0.5, size = 14),
                       panel.grid.major.x = element_blank(),
                       panel.grid.minor.x = element_blank(),
                       legend.title = element_text(size = 14), legend.text = element_text(size = 12),
                       axis.title.y = element_text(size = 14), axis.text.y = element_text(size = 12),
                       axis.title.x = element_text(size = 14, vjust = -1), axis.text.x = element_text(size = 12),
                       plot.margin = unit(c(3, 5, 8, 3), "points")
)

x_bound <- round(max(abs(shap_long_forest$value))) + 0.5
a <- ggplot(data = shap_long_forest) +
  coord_flip() +
  geom_sina(aes(x = variable, y = value, color = stdfvalue), alpha = 0.1, size = 0.35) +
  geom_text(data = unique(shap_long_forest[, c("variable", "mean_value")]),
            aes(x = variable, y = -Inf, label = sprintf("%.3f", mean_value)),
            size = 4, alpha = 0.8,
            hjust = -0.2,
            fontface = "bold") + # bold
  scale_color_gradient(low = "#FFCC33", high = "#3F708A", breaks = c(0, 1), labels = c("Low", "High")) +
  ggtitle("Forests") +
  theme_bw() +
  common_theme +
  geom_hline(yintercept = 0) + # the vertical line
  scale_y_continuous(limits = c(-x_bound, x_bound),breaks = seq(-x_bound, x_bound, by = 0.5)) +
  scale_x_discrete(limits = rev(levels(shap_long_forest$variable))) +
  labs(y = "", x = "", color = "Feature value")

x_bound <- round(max(abs(shap_long_dry$value))) + 0.5
b <- ggplot(data = shap_long_dry) +
  coord_flip() +
  geom_sina(aes(x = variable, y = value, color = stdfvalue), alpha = 0.1, size = 0.55) +
  geom_text(data = unique(shap_long_dry[, c("variable", "mean_value")]),
            aes(x = variable, y = -Inf, label = sprintf("%.3f", mean_value)),
            size = 4, alpha = 0.8,
            hjust = -0.2,
            fontface = "bold") + # bold
  scale_color_gradient(low = "#FFCC33", high = "#3F708A", breaks = c(0, 1), labels = c("Low", "High")) +
  ggtitle("Savannas and shrublands") +
  theme_bw() +
  common_theme +
  geom_hline(yintercept = 0) + # the vertical line
  scale_y_continuous(limits = c(-x_bound, x_bound),,breaks = seq(-x_bound, x_bound, by = 0.5)) +
  scale_x_discrete(limits = rev(levels(shap_long_dry$variable))) +
  labs(y = "", x = "", color = "Feature value")

x_bound <- round(max(abs(shap_long_crop$value))) + 0.5
c <- ggplot(data = shap_long_crop) +
  coord_flip() +
  geom_sina(aes(x = variable, y = value, color = stdfvalue), alpha = 0.1, size = 0.55) +
  geom_text(data = unique(shap_long_crop[, c("variable", "mean_value")]),
            aes(x = variable, y = -Inf, label = sprintf("%.3f", mean_value)),
            size = 4, alpha = 0.8,
            hjust = -0.2,
            fontface = "bold") + # bold
  scale_color_gradient(low = "#FFCC33", high = "#3F708A", breaks = c(0, 1), labels = c("Low", "High")) +
  ggtitle("Croplands") +
  theme_bw() +
  common_theme +
  geom_hline(yintercept = 0) + # the vertical line
  scale_y_continuous(limits = c(-x_bound, x_bound),breaks = seq(-x_bound, x_bound, by = 0.5)) +
  scale_x_discrete(limits = rev(levels(shap_long_crop$variable))) +
  labs(y = "Shapley value (impact on SIF prediction)", x = "", color = "Feature value")

x_bound <- round(max(abs(shap_long_grass$value))) + 1
d <- ggplot(data = shap_long_grass) +
  coord_flip() +
  geom_sina(aes(x = variable, y = value, color = stdfvalue), alpha = 0.1, size = 0.55) +
  geom_text(data = unique(shap_long_grass[, c("variable", "mean_value")]),
            aes(x = variable, y = -Inf, label = sprintf("%.3f", mean_value)),
            size = 4, alpha = 0.8,
            hjust = -0.2,
            fontface = "bold") + # bold
  scale_color_gradient(low = "#FFCC33", high = "#3F708A", breaks = c(0, 1), labels = c("Low", "High")) +
  ggtitle("Grasslands") +
  theme_bw() +
  common_theme +
  geom_hline(yintercept = 0) + # the vertical line
  scale_y_continuous(limits = c(-x_bound, x_bound),
                     breaks = seq(-x_bound, x_bound, by = 1)) +
  scale_x_discrete(limits = rev(levels(shap_long_grass$variable))) +
  labs(y = "Shapley value (impact on SIF prediction)", x = "", color = "Feature value")

a <- a + theme(legend.key.width = unit(2, "cm"),
               legend.margin = margin(t = 15, unit = "pt"))  # Adjust the width as needed
b <- b + theme(legend.key.width = unit(2, "cm"),
               legend.margin = margin(t = 15, unit = "pt"))  # Adjust the width as needed
c <- c + theme(legend.key.width = unit(2, "cm"),
               legend.margin = margin(t = 100, unit = "pt"))  # Adjust the width as needed
d <- d + theme(legend.key.width = unit(2, "cm"),
               legend.margin = margin(t = 100, unit = "pt"))  # Adjust the width as needed

# combine graphs
fig <- ggarrange(a, b, c, d,
                 labels = "auto",
                 ncol = 2, nrow = 2,
                 align = "hv",
                 common.legend = TRUE, # have just one common legend
                 legend="bottom"
)

png(filename = paste0("/Users/jiangongliu/Desktop/5_WTD_Aridity/figure/",
                      "Fig_3_2_globgm.png"),
    width = 10, height = 4.5, units = "in", res = 600)
print(fig)
dev.off()

