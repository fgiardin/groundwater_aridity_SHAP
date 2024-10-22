rm(list=ls())
library(tidyverse)
library(cowplot)
library(ggforce)
library(ggpubr)
library(data.table)
library(gridExtra)

std1 <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

model_training_folder <- '/Users/jiangongliu/Desktop/5_WTD_Aridity/new_dataframe/model_training_elevation_US_Fan/'

/Users/fgiardina/groundwater_aridity_SHAP/data/jiangong/model_training_elevation_US_Fan/all_causalSHAP_grasslands.csv

# forest
df_forest <- readRDS(paste0(model_training_folder, "cshap_long_forests.rds"))
ori_forest <- as.data.frame(df_forest$x_test)
shap_forest <- df_forest$dt %>%
  mutate(WTD_rfvalue = ori_forest$WTD,
         Aridity_rfvalue = ori_forest$Aridity,
         Elevation_rfvalue = ori_forest$Elevation) %>%
  rename(Elevation = Elevation)
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
         Elevation_rfvalue = ori_savannas_and_scrublands$Elevation) %>%
  rename(Elevation = Elevation)
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
                          c("WTD", "Aridity", "Elevation"),        # SHAP values
                          c("WTD_rfvalue", "Aridity_rfvalue", "Elevation_rfvalue"),  # rfvalue
                          c("WTD_stdfvalue", "Aridity_stdfvalue", "Elevation_stdfvalue"),  # stdfvalue
                          c("WTD_mean_value", "Aridity_mean_value", "Elevation_mean_value")  # mean_value
                        ),
                        variable.name = "variable",
                        value.name = c("value", "rfvalue", "stdfvalue", "mean_value"))
shap_long_grass[, variable := factor(variable, labels = c("WTD", "Aridity", "Elevation"))]

# define common theme
common_theme <-  theme(legend.position = "bottom",
                       plot.title = element_text(hjust = 0.5, size = 14),
                       panel.grid.major.x = element_blank(),
                       panel.grid.minor.x = element_blank(),
                       legend.title = element_text(size = 14), legend.text = element_text(size = 12),
                       axis.title.y = element_text(size = 14), axis.text.y = element_text(size = 12),
                       axis.title.x = element_text(size = 14, vjust = -1), axis.text.x = element_text(size = 12),
                       plot.margin = unit(c(5, 5, 5, 5), "points")
)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

a1 <- ggplot(shap_forest, aes(WTD_rfvalue, WTD)) +
  geom_point(aes(color = Aridity_rfvalue), alpha = 0.35, size = 0.3) +
  scale_color_gradient(low = "#FFCC33", high = "#0C2C84",
                       limits = c(0, 1.8),  # Set the range for the legend
                       breaks = seq(0, 1.8, by = 0.3)) +
  labs(x = "",
       y = expression(atop('Shapley values for', 'WTD (sr'^-1 * 'nm'^-1 * ")")),
       color = "Aridity") +
  scale_x_continuous(limits = c(-150, 0)) +
  theme_bw() +
  common_theme +
  # theme(legend.key.width = unit(2.5, "cm"))
  theme(legend.position="none")

mylegend<-arrangeGrob(g_legend(a1))


a2 <- ggplot(shap_forest, aes(x = WTD_rfvalue)) +
  geom_density(fill = "grey", alpha = 0.7, color = NA) +
  ggtitle("Forests") +
  theme_void() +  # Minimal theme to remove axes, etc.
  theme(plot.margin = margin(t = 0, r = 5, b = 0, l = 5, unit = "pt"),
        plot.title = element_text(hjust = 0.5, size = 14))

a <- combined_plot <- plot_grid(
  a2,  # The density plot on top
  a1,  # The scatter plot on the bottom
  ncol = 1,      # Arrange vertically
  align = "v",   # Align vertically
  rel_heights = c(0.2, 1)  # Adjust the relative height; 0.3 for density and 1 for scatter
)

#shap_savannas_and_scrublands
b1 <- ggplot(shap_savannas_and_scrublands, aes(WTD_rfvalue, WTD)) +
  geom_point(aes(color = Aridity_rfvalue), alpha = 0.35, size = 0.3) +
  scale_color_gradient(low = "#FFCC33", high = "#0C2C84",
                       limits = c(0, 1.8),  # Set the range for the legend
                       breaks = seq(0, 1.8, by = 0.3)) +
  scale_x_continuous(limits = c(-150, 0)) +
  labs(x = "",
       y = "",
       color = "Aridity") +
  theme_bw() +
  common_theme+
  theme(legend.position="none")

b2 <- ggplot(shap_savannas_and_scrublands, aes(x = WTD_rfvalue)) +
  geom_density(fill = "grey", alpha = 0.7, color = NA) +
  ggtitle("Savannas and shrublands") +
  theme_void() +  # Minimal theme to remove axes, etc.
  theme(plot.margin = margin(t = 0, r = 5, b = 0, l = 5, unit = "pt"),
        plot.title = element_text(hjust = 0.5, size = 14))

b <- combined_plot <- plot_grid(
  b2,  # The density plot on top
  b1,  # The scatter plot on the bottom
  ncol = 1,      # Arrange vertically
  align = "v",   # Align vertically
  rel_heights = c(0.2, 1)  # Adjust the relative height; 0.3 for density and 1 for scatter
)

c1 <- ggplot(shap_cropland, aes(WTD_rfvalue, WTD)) +
  geom_point(aes(color = Aridity_rfvalue), alpha = 0.35, size = 0.3) +
  scale_color_gradient(low = "#FFCC33", high = "#0C2C84",
                       limits = c(0, 1.8),  # Set the range for the legend
                       breaks = seq(0, 1.8, by = 0.3)) +
  labs(x = expression('WTD (m)'),
       y = expression(atop('Shapley values for', 'WTD (sr'^-1 * 'nm'^-1 * ")")),
       color = "Aridity") +
  scale_x_continuous(limits = c(-100, 0)) +
  theme_bw() +
  common_theme+
  theme(legend.position="none")

c2 <- ggplot(shap_cropland, aes(x = WTD_rfvalue)) +
  geom_density(fill = "grey", alpha = 0.7, color = NA) +
  ggtitle("Croplands") +
  theme_void() +  # Minimal theme to remove axes, etc.
  theme(plot.margin = margin(t = 0, r = 5, b = 0, l = 5, unit = "pt"),
        plot.title = element_text(hjust = 0.5, size = 14))

c <- combined_plot <- plot_grid(
  c2,  # The density plot on top
  c1,  # The scatter plot on the bottom
  ncol = 1,      # Arrange vertically
  align = "v",   # Align vertically
  rel_heights = c(0.2, 1)  # Adjust the relative height; 0.3 for density and 1 for scatter
)


d1 <- ggplot(shap_grassland, aes(WTD_rfvalue, WTD)) +
  geom_point(aes(color = Aridity_rfvalue), alpha = 0.35, size = 0.3) +
  scale_color_gradient(low = "#FFCC33", high = "#0C2C84",
                       limits = c(0, 1.8),  # Set the range for the legend
                       breaks = seq(0, 1.8, by = 0.3)) +
  scale_x_continuous(limits = c(-120, 0), breaks = c(-120, -80, -40, 0)) +
  labs(x = expression('WTD (m)'),
       y = "",
       color = "Aridity") +
  theme_bw() +
  common_theme+
  theme(legend.position="none")

d2 <- ggplot(shap_grassland, aes(x = WTD_rfvalue)) +
  geom_density(fill = "grey", alpha = 0.7, color = NA) +
  ggtitle("Grasslands") +
  theme_void() +  # Minimal theme to remove axes, etc.
  theme(plot.margin = margin(t = 0, r = 5, b = 0, l = 5, unit = "pt"),
        plot.title = element_text(hjust = 0.5, size = 14))

d <- combined_plot <- plot_grid(
  d2,  # The density plot on top
  d1,  # The scatter plot on the bottom
  ncol = 1,      # Arrange vertically
  align = "v",   # Align vertically
  rel_heights = c(0.2, 1)  # Adjust the relative height; 0.3 for density and 1 for scatter
)


# a <- a + theme(legend.key.width = unit(2, "cm"),
#                legend.margin = margin(t = 15, unit = "pt"))  # Adjust the width as needed
# b <- b + theme(legend.key.width = unit(2, "cm"),
#                legend.margin = margin(t = 15, unit = "pt"))  # Adjust the width as needed
# c <- c + theme(legend.key.width = unit(2, "cm"),
#                legend.margin = margin(t = 100, unit = "pt"))  # Adjust the width as needed
# d <- d + theme(legend.key.width = unit(2, "cm"),
#                legend.margin = margin(t = 100, unit = "pt"))  # Adjust the width as needed

# combine graphs
fig <- ggarrange(a, b, c, d,
                 labels = "auto",
                 ncol = 2, nrow = 2,
                 align = "hv"
)

fig_ <- ggarrange(fig, mylegend,
                 ncol = 1, nrow = 2,
                 align = "hv",
                 heights = c(8,1)
)

png(filename = paste0("/Users/jiangongliu/Desktop/5_WTD_Aridity/figure/",
                      "Fig_5.png"),
    width = 9, height = 7, units = "in", res = 600)
print(fig_)
dev.off()







