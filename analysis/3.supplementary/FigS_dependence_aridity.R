# Plot dependence plots for aridity per each vegetation group

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

model_training_folder <- 'data/model_output/model_training_elevation_US_Fan/'

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

# savannas and shrublands
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

# Croplands
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

# Grasslands
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

# define common theme
common_theme <-  theme(legend.position = "bottom",
                       plot.title = element_text(hjust = 0.5, size = 16),
                       panel.grid.major = element_blank(),  # Remove major gridlines
                       panel.grid.minor = element_blank(),  # Remove minor gridlines
                       legend.title = element_text(size = 14), legend.text = element_text(size = 12),
                       axis.title.y = element_text(size = 14), axis.text.y = element_text(size = 12),
                       axis.title.x = element_text(size = 14, vjust = -1), axis.text.x = element_text(size = 12),
                       plot.margin = unit(c(5, 5, 5, 5), "points")
)

# function to extract legend from ggplot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

a1 <- ggplot(shap_forest, aes(Aridity_rfvalue, Aridity)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(aes(color = WTD_rfvalue), alpha = 0.35, size = 0.3) +
  scale_color_gradient(low = "#1518FD", high = "#FE0104",
                       limits = c(-150,0),  # Set the range for the legend
                       breaks = seq(-150,0,by = 30)) +
  labs(x = "",
       y = expression(paste("Shapley values for ", "λP/R"[n], " (", "-", ")   ")),
       color = "WTD (m)   ") +
  scale_x_continuous(limits = c(0, 2.7),
                     breaks = c(0, 0.5, 1, 1.5, 2, 2.5),
                     expand = c(0, 0) # remove space below zero
                     ) +
  scale_y_continuous(limits = c(-1.4, 1.9),
                     breaks = c(-1, -0.5, 0, 0.5, 1, 1.5)
  ) +
  theme_bw() +
  common_theme +
  theme(legend.key.width = unit(1.62, "cm"),
        legend.key.height = unit(0.2, "cm")
  )

# extract legend for final graph and remove from this panel
mylegend <- arrangeGrob(g_legend(a1))
a1 <- a1 + theme(legend.position="none")

a2 <- ggplot(shap_forest, aes(x = Aridity_rfvalue)) +
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
b1 <- ggplot(shap_savannas_and_scrublands, aes(Aridity_rfvalue, Aridity)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(aes(color = WTD_rfvalue), alpha = 0.35, size = 0.3) +
  scale_color_gradient(low = "#1518FD", high = "#FE0104",
                       limits = c(-150, 0),  # Set the range for the legend
                       breaks = seq(-150, 0, by = 30)) +
  scale_y_continuous(limits = c(-1.4, 1.9),
                     breaks = c(-1, -0.5, 0, 0.5, 1, 1.5)
  ) +
  scale_x_continuous(limits = c(0, 2.7),
                     breaks = c(0, 0.5, 1, 1.5, 2, 2.5),
                     expand = c(0, 0) # remove space below zero
  ) +
  labs(x = "",
       y = "\n", # spaceholder to have same x-axes lengths in all panels
       color = "WTD (m)") +
  theme_bw() +
  common_theme+
  theme(legend.position="none")

b2 <- ggplot(shap_savannas_and_scrublands, aes(x = Aridity_rfvalue)) +
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

c1 <- ggplot(shap_cropland, aes(Aridity_rfvalue, Aridity)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(aes(color = WTD_rfvalue), alpha = 0.35, size = 0.3) +
  scale_color_gradient(low = "#1518FD", high = "#FE0104",
                       limits = c(-150, 0),  # Set the range for the legend
                       breaks = seq(-150, 0, by = 30)) +
  labs(x = expression(paste("λP/R"[n], " (", "-", ")   ")),
       y = "\n", # spaceholder to have same x-axes lengths in all panels
       color = "WTD (m)") +
  scale_y_continuous(limits = c(-1.4, 1.9),
                     breaks = c(-1, -0.5, 0, 0.5, 1, 1.5)
  ) +
  scale_x_continuous(limits = c(0, 2.7),
                     breaks = c(0, 0.5, 1, 1.5, 2, 2.5),
                     expand = c(0, 0) # remove space below zero
  ) +
  theme_bw() +
  common_theme+
  theme(legend.position="none")

c2 <- ggplot(shap_cropland, aes(x = Aridity_rfvalue)) +
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

d1 <- ggplot(shap_grassland, aes(Aridity_rfvalue, Aridity)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(aes(color = WTD_rfvalue), alpha = 0.35, size = 0.3) +
  scale_color_gradient(low = "#1518FD", high = "#FE0104",
                       limits = c(-150, 0),  # Set the range for the legend
                       breaks = seq(-150, 0, by = 30)) +
  scale_y_continuous(limits = c(-1.4, 1.9),
                     breaks = c(-1, -0.5, 0, 0.5, 1, 1.5)
  ) +
  scale_x_continuous(limits = c(0, 2.7),
                     breaks = c(0, 0.5, 1, 1.5, 2, 2.5),
                     expand = c(0, 0) # remove space below zero
  ) +
  labs(x = expression(paste("λP/R"[n], " (", "-", ")   ")),
       y = expression(paste("Shapley values for ", "λP/R"[n], " (", "-", ")   ")),
       color = "WTD (m)") +
  theme_bw() +
  common_theme+
  theme(legend.position="none")

d2 <- ggplot(shap_grassland, aes(x = Aridity_rfvalue)) +
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



# combine graphs
fig <- ggarrange(a, b, d, c,
                 labels = "auto",
                 ncol = 2, nrow = 2,
                 align = "hv"
)

fig_ <- ggarrange(fig, mylegend,
                 ncol = 1, nrow = 2,
                 align = "hv",
                 heights = c(8,1)
)


png(filename = paste0("./",
                      "FigS_dependence_moisture.png"),
    width = 9, height = 7, units = "in", res = 300)
print(fig_)
dev.off()







