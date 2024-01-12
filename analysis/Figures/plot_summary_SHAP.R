# script to load SHAP results and plot summary plots SHAP (bee-swarm and dependence plots)

# load relevant packages
library(tidyverse)
library(SHAPforxgboost)
library(ggpubr)
library(ggforce)
library(ggExtra) # to plot marginal histograms
library(gridExtra) # to use grid.arrange
library(grid) # to use grid.arrange

USA = 1 # if 0, plots results for the entire world
percentile = 0 # plot 95th percentile of WTD in dependence plots

if (USA) {
  # load results USA (put results to be plotted in directory: data/results_SHAP/USA_PFTgroups/WTD)
  shap_forests <- readRDS("data/results_SHAP/USA_PFTgroups/WTD/Forests/shap_long.rds")
  shap_savannas <- readRDS("data/results_SHAP/USA_PFTgroups/WTD/Savannas/shap_long.rds")
  shap_croplands <- readRDS("data/results_SHAP/USA_PFTgroups/WTD/CRO/shap_long.rds")
  shap_grasslands <- readRDS("data/results_SHAP/USA_PFTgroups/WTD/GRA/shap_long.rds")
} else {
  # load results World (put results to be plotted in directory: data/results_SHAP/USA_PFTgroups/WTD)
  shap_forests <- readRDS("data/results_SHAP/World/Forests/shap_long.rds")
  shap_savannas <- readRDS("data/results_SHAP/World/Savannas/shap_long.rds")
  shap_croplands <- readRDS("data/results_SHAP/World/CRO/shap_long.rds")
  shap_grasslands <- readRDS("data/results_SHAP/World/GRA/shap_long.rds")
}

set.seed(23) # set seed for reproducibility

# bee-swarm plots ---------------------------------------------------------

# define common theme
common_theme <-  theme(legend.position = "bottom",
                       plot.title = element_text(hjust = 0.5, size = 16),
                       panel.grid.major.x = element_blank(),
                       panel.grid.minor.x = element_blank(),
                       legend.title = element_text(size = 16), legend.text = element_text(size = 14),
                       axis.title.y = element_text(size = 16), axis.text.y = element_text(size = 14),
                       axis.title.x = element_text(size = 16, vjust = -1), axis.text.x = element_text(size = 12)
                       )


# Define custom labels
custom_labels <- c("WTD", expression("λP/R"[n]))

p_forests <- shap_forests %>% mutate(variable = ifelse(variable == "P_over_Rn", "P/Rn", "WTD"))
a <- ggplot(data = p_forests) +
  coord_flip(ylim = c(-1, 1)) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ggforce::geom_sina(aes(x = variable, y = value, color = stdfvalue),
  method = "counts", maxwidth = 0.7, alpha = 0.7) +
  geom_text(data = unique(p_forests[, c("variable", "mean_value")]),
            aes(x = variable, y = -Inf, label = sprintf("%.3f", mean_value)),
            size = 4, alpha = 0.7, hjust = -0.2, fontface = "bold") +
  ggtitle("Forests") +
  theme_bw() +
  common_theme +
  scale_x_discrete(labels = setNames(custom_labels, c("WTD", "P/Rn"))) + # important! Same here order as "custom_labels"
  scale_color_gradient(
    low = "#008BFB", high = "#ff0051",
    breaks = c(0, 1), labels = c(" Low", "High "),
    guide = guide_colorbar(barwidth = 12, barheight = 0.3)
  ) +
  labs(y = "", x = "", color = "Feature value  ")
a


p_savannas <- shap_savannas %>% mutate(variable = ifelse(variable == "P_over_Rn", "P/Rn", "WTD"))
b <- ggplot(data = p_savannas) +
  coord_flip(ylim = c(-1, 1)) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ggforce::geom_sina(aes(x = variable, y = value, color = stdfvalue),
                     method = "counts", maxwidth = 0.7, alpha = 0.7) +
  geom_text(data = unique(p_savannas[, c("variable", "mean_value")]),
            aes(x = variable, y = -Inf, label = sprintf("%.3f", mean_value)),
            size = 4, alpha = 0.7, hjust = -0.2, fontface = "bold") +
  ggtitle("Savannas and shrublands") +
  theme_bw() +
  common_theme +
  scale_x_discrete(labels = setNames(custom_labels, c("WTD", "P/Rn"))) + # important! Same here order as "custom_labels"
  scale_color_gradient(
    low = "#008BFB", high = "#ff0051",
    breaks = c(0, 1), labels = c(" Low", "High "),
    guide = guide_colorbar(barwidth = 12, barheight = 0.3)
  ) +
  labs(y = "", x = "", color = "Feature value  ")


p_croplands <- shap_croplands %>% mutate(variable = ifelse(variable == "P_over_Rn", "P/Rn", "WTD"))
c <- ggplot(data = p_croplands) +
  coord_flip(ylim = c(-1, 1)) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ggforce::geom_sina(aes(x = variable, y = value, color = stdfvalue),
                     method = "counts", maxwidth = 0.7, alpha = 0.7) +
  geom_text(data = unique(p_croplands[, c("variable", "mean_value")]),
            aes(x = variable, y = -Inf, label = sprintf("%.3f", mean_value)),
            size = 4, alpha = 0.7, hjust = -0.2, fontface = "bold") +
  ggtitle("Croplands") +
  theme_bw() +
  common_theme +
  scale_x_discrete(labels = setNames(custom_labels, c("WTD", "P/Rn"))) + # important! Same here order as "custom_labels"
  scale_color_gradient(
    low = "#008BFB", high = "#ff0051",
    breaks = c(0, 1), labels = c(" Low", "High "),
    guide = guide_colorbar(barwidth = 12, barheight = 0.3)
  ) +
  labs(x = "",
       y = "SHAP value (impact on SIF)",
       color = "Feature value  ")

p_grasslands <- shap_grasslands %>% mutate(variable = ifelse(variable == "P_over_Rn", "P/Rn", "WTD"))
d <- ggplot(data = p_grasslands) +
  coord_flip(ylim = c(-1, 1)) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ggforce::geom_sina(aes(x = variable, y = value, color = stdfvalue),
                     method = "counts", maxwidth = 0.7, alpha = 0.7) +
  geom_text(data = unique(p_grasslands[, c("variable", "mean_value")]),
            aes(x = variable, y = -Inf, label = sprintf("%.3f", mean_value)),
            size = 4, alpha = 0.7, hjust = -0.2, fontface = "bold") +
  ggtitle("Grasslands") +
  theme_bw() +
  common_theme +
  scale_x_discrete(labels = setNames(custom_labels, c("WTD", "P/Rn"))) + # important! Same here order as "custom_labels"
  scale_color_gradient(
    low = "#008BFB", high = "#ff0051",
    breaks = c(0, 1), labels = c(" Low", "High "),
    guide = guide_colorbar(barwidth = 12, barheight = 0.3)
  ) +
  labs(x = "",
       y = "SHAP value (impact on SIF)",
       color = "Feature value  ")


# combine graphs
fig <- ggarrange(a, b, c, d,
          labels = "auto",
          ncol = 2, nrow = 2,
          common.legend = TRUE, # have just one common legend
          legend="bottom"
          )
ggsave("bee_swarm.png", plot = fig, path = "./", width = 10.8, height = 5.5, dpi = 600)

# 7.2

# save plot
# ggsave("bee_swarm.png", path = "./", width = 7, height = 6)



# dependence plot WTD ---------------------------------------------------------

#define labels
new_labels <- list(
  SIF_over_PAR = "Vegetation activity (-)",
  WTD = "WTD (m)",
  P_over_Rn = "P/Rn (-)"
)


# calculate 95th percentiles of SHAP values
percentile_forests <- quantile(shap_forests %>% dplyr::filter(variable == "WTD") %>% pull(rfvalue), probs = 0.05) # 0.05 instead of 0.95 because the distribution is negative
percentile_savannas <- quantile(shap_savannas %>% dplyr::filter(variable == "WTD") %>% pull(rfvalue), probs = 0.05) # equivalent of the 95th percentile of abs(WTD)
percentile_croplands <- quantile(shap_croplands %>% dplyr::filter(variable == "WTD") %>% pull(rfvalue), probs = 0.05)
percentile_grasslands <- quantile(shap_grasslands %>% dplyr::filter(variable == "WTD") %>% pull(rfvalue), probs = 0.05)

common_theme <- theme(
  axis.line = element_line(color='black'), # remove grid lines
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  plot.title = element_text(hjust = 0.5, size = 16), # center title
  axis.text=element_text(size = 14),
  axis.title=element_text(size = 14),
  legend.position="none",
  plot.tag = element_text(face = "bold", size = 14) # control the figure label (a,b,c,d)
  )

body(shap.plot.dependence)

a <- shap.plot.dependence(data_long = shap_forests,
                            smooth = F,
                            x = 'WTD', # real values to display
                            y = 'WTD', # shap values to display (in this case: dependence plot of WTD, so both x and y are WTD)
                            color_feature = 'P_over_Rn') +
  geom_hline(aes(yintercept=0), alpha = 0.3) +
  ggtitle("Forests") +
  common_theme +
  scale_x_continuous(name = element_blank(),
                     limits = c(-150, 0),
                     breaks = c(-200, -175, -150, -125, -100, -75, -50, -25, 0)
  ) +
  scale_y_continuous(
                     limits = c(-0.8, 0.4),
                     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4)
  ) +
  labs(color = expression(paste("λP/R"[n], " (", "-", ")"))) +
  scale_color_gradient(low = "#008BFB", high = "#ff0051",
                       guide = guide_colorbar(barwidth = 10, barheight = 0.3)) +
  labs(tag = 'a')
  # labs(color = expression(atop("P/R"[n]~"(-)", "(Feature value)"))) +
  # labs(color = paste0("Hello", "\n", "(Feature value)"))

b <- shap.plot.dependence(data_long = shap_savannas,
                          smooth = F,
                          x = 'WTD',
                          y = 'WTD',
                          color_feature = 'P_over_Rn') +
  geom_hline(aes(yintercept=0), alpha = 0.3) +
  ggtitle("Savannas and shrublands") +
  common_theme +
  scale_x_continuous(name = element_blank(),
                     limits = c(-150, 0),
                     breaks = c(-200, -175, -150, -125, -100, -75, -50, -25, 0)
  ) +
  scale_y_continuous(name = element_blank(),
                     limits = c(-0.8, 0.4),
                     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4)
  ) +
  labs(color = expression(paste("λP/R"[n], " (", "-", ")"))) +
  scale_color_gradient(low = "#008BFB", high = "#ff0051",
                       guide = guide_colorbar(barwidth = 10, barheight = 0.3)) +
  labs(tag = 'b')

c <- shap.plot.dependence(data_long = shap_grasslands,
                          smooth = F,
                          x = 'WTD',
                          y = 'WTD',
                          color_feature = 'P_over_Rn') +
  geom_hline(aes(yintercept=0), alpha = 0.3) +
  ggtitle("Grasslands") +
  common_theme +
  scale_x_continuous(name = "WTD (m)",
                     limits = c(-150, 0),
                     breaks = c(-200, -175, -150, -125, -100, -75, -50, -25, 0)
  ) +
  scale_y_continuous(
                     limits = c(-0.8, 0.4),
                     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4)
  ) +
  labs(color = expression(paste("λP/R"[n], " (", "-", ")"))) +
  scale_color_gradient(low = "#008BFB", high = "#ff0051",
                       guide = guide_colorbar(barwidth = 10, barheight = 0.3)) +
  labs(tag = 'c')

d <- shap.plot.dependence(data_long = shap_croplands,
                          smooth = F,
                          x = 'WTD',
                          y = 'WTD',
                          color_feature = 'P_over_Rn') +
  geom_hline(aes(yintercept=0), alpha = 0.3) +
  ggtitle("Croplands") +
  common_theme +
  scale_x_continuous(name = "WTD (m)",
                     limits = c(-150, 0),
                     breaks = c(-200, -175, -150, -125, -100, -75, -50, -25, 0)
  ) +
  scale_y_continuous(name = element_blank(),
                     limits = c(-0.8, 0.4),
                     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4)
  ) +
  labs(color = expression(paste("λP/R"[n], " (", "-", ")"))) +
  scale_color_gradient(low = "#008BFB", high = "#ff0051",
                       guide = guide_colorbar(barwidth = 10, barheight = 0.3)) +
  labs(tag = 'd')


if (percentile) {
  a <- a + geom_vline(xintercept = percentile_forests[[1]], linetype = "dashed", color = "black")
  b <- b + geom_vline(xintercept = percentile_savannas[[1]], linetype = "dashed", color = "black")
  c <- c + geom_vline(xintercept = percentile_croplands[[1]], linetype = "dashed", color = "black")
  d <- d + geom_vline(xintercept = percentile_grasslands[[1]], linetype = "dashed", color = "black")
}


# # add marginal distributions (histograms) for the x axis
a1 <- ggExtra::ggMarginal(a, type = "histogram", margins = "x", size = 15, col = "grey86", fill = "grey86")
b1 <- ggExtra::ggMarginal(b, type = "histogram", margins = "x", size = 15, col = "grey86", fill = "grey86")
c1 <- ggExtra::ggMarginal(c, type = "histogram", margins = "x", size = 15, col = "grey86", fill = "grey86")
d1 <- ggExtra::ggMarginal(d, type = "histogram", margins = "x", size = 15, col = "grey86", fill = "grey86")

# re-plot just for legend
ciaone <- shap.plot.dependence(data_long = shap_forests,
                          smooth = F,
                          x = 'WTD',
                          y = 'WTD',
                          color_feature = 'P_over_Rn') +
  geom_hline(aes(yintercept=0), alpha = 0.3) +
  ggtitle("Forests") +
  scale_x_continuous(name = element_blank(),
                     limits = c(-200, 0),
                     breaks = c(-200, -175, -150, -125, -100, -75, -50, -25, 0)
  ) +
  scale_y_continuous(
    limits = c(-0.8, 0.4),
    breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4)
  ) +
  labs(color = expression(paste("λP/R"[n], " (", "-", ")"))) +
  scale_color_gradient(low = "#008BFB", high = "#ff0051",
                       guide = guide_colorbar(barwidth = 10, barheight = 0.3)) +
  theme(legend.title = element_text(size = 14),   # Adjust title size
        legend.text = element_text(size = 14)) + # Adjust text size
  guides(colour = guide_colourbar(barwidth = 12, barheight = 0.3)) # Adjust colorbar size


# Extract the legend from one of the plots
source("R/get_legend.R")
legend <- get_legend(ciaone)

# common legend
arranged_plot <- grid.arrange(arrangeGrob(a1, b1, c1, d1, ncol = 2),
                              legend, ncol = 1, heights = c(10, 1))


# save
png(filename = "./dependence_WTD.png", width = 10, height = 8, units = "in", res = 600) # Open a PNG device
grid.draw(arranged_plot)  # plot
dev.off() # Turn device


# dependence plot P/Rn ---------------------------------------------------------

#define labels
new_labels <- list(
  SIF_over_PAR = "Vegetation activity (-)",
  WTD = "WTD (m)",
  P_over_Rn = "P/Rn (-)"
)

# calculate 95th percentiles of SHAP values
percentile_forests <- quantile(shap_forests %>% dplyr::filter(variable == "WTD") %>% pull(rfvalue), probs = 0.05) # 0.05 instead of 0.95 because the distribution is negative
percentile_savannas <- quantile(shap_savannas %>% dplyr::filter(variable == "WTD") %>% pull(rfvalue), probs = 0.05) # equivalent of the 95th percentile of abs(WTD)
percentile_croplands <- quantile(shap_croplands %>% dplyr::filter(variable == "WTD") %>% pull(rfvalue), probs = 0.05)
percentile_grasslands <- quantile(shap_grasslands %>% dplyr::filter(variable == "WTD") %>% pull(rfvalue), probs = 0.05)

common_theme <- theme(
  axis.line = element_line(color='black'), # remove grid lines
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  plot.title = element_text(hjust = 0.5, size = 16), # center title
  axis.text=element_text(size = 14),
  axis.title=element_text(size = 14),
  legend.position="none",
  plot.tag = element_text(face = "bold", size = 14) # control the figure label (a,b,c,d)
)

body(shap.plot.dependence)

a <- shap.plot.dependence(data_long = shap_forests,
                          smooth = F,
                          x = 'P_over_Rn', # real values to display
                          y = 'P_over_Rn', # shap values to display
                          color_feature = 'WTD') +
  geom_hline(aes(yintercept=0), alpha = 0.3) +
  ggtitle("Forests") +
  common_theme +
  scale_x_continuous(name = element_blank(),
                     limits = c(0, 3),
                     breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3)
  ) +
  scale_y_continuous(
    name = expression(paste("SHAP value for λP/R"[n], " (", "-", ")")),
    limits = c(-0.8, 0.8),
    breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8)
  ) +
  labs(color = "WTD (m)") +
  scale_color_gradient(low = "#008BFB", high = "#ff0051",
                       guide = guide_colorbar(barwidth = 10, barheight = 0.3)) +
  labs(tag = 'a') +
  guides(colour = guide_colourbar(barwidth = 12, barheight = 0.3)) # Adjust colorbar size
a

b <- shap.plot.dependence(data_long = shap_savannas,
                          smooth = F,
                          x = 'P_over_Rn', # real values to display
                          y = 'P_over_Rn', # shap values to display
                          color_feature = 'WTD') +
  geom_hline(aes(yintercept=0), alpha = 0.3) +
  ggtitle("Savannahs and Shurblands") +
  common_theme +
  scale_x_continuous(name = element_blank(),
                     limits = c(0, 3),
                     breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3)
  ) +
  scale_y_continuous(
    name = element_blank(),
    limits = c(-0.8, 0.8),
    breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8)
  ) +
  labs(color = "WTD (m)") +
  scale_color_gradient(low = "#008BFB", high = "#ff0051",
                       guide = guide_colorbar(barwidth = 10, barheight = 0.3)) +
  labs(tag = 'b')

c <- shap.plot.dependence(data_long = shap_grasslands,
                          smooth = F,
                          x = 'P_over_Rn', # real values to display
                          y = 'P_over_Rn', # shap values to display
                          color_feature = 'WTD') +
  geom_hline(aes(yintercept=0), alpha = 0.3) +
  ggtitle("Grasslands") +
  common_theme +
  scale_x_continuous(name = expression(paste("λP/R"[n], " (", "-", ")")),
                     limits = c(0, 3),
                     breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3)
  ) +
  scale_y_continuous(
    name = expression(paste("SHAP value for λP/R"[n], " (", "-", ")")),
    limits = c(-0.8, 0.8),
    breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8)
  ) +
  labs(color = "WTD (m)") +
  scale_color_gradient(low = "#008BFB", high = "#ff0051",
                       guide = guide_colorbar(barwidth = 10, barheight = 0.3)) +
  labs(tag = 'c')

d <- shap.plot.dependence(data_long = shap_croplands,
                          smooth = F,
                          x = 'P_over_Rn', # real values to display
                          y = 'P_over_Rn', # shap values to display
                          color_feature = 'WTD') +
  geom_hline(aes(yintercept=0), alpha = 0.3) +
  ggtitle("Croplands") +
  common_theme +
  scale_x_continuous(name = expression(paste("λP/R"[n], " (", "-", ")")),
                     limits = c(0, 3),
                     breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3)
  ) +
  scale_y_continuous(
    name = element_blank(),
    limits = c(-0.8, 0.8),
    breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8)
  ) +
  labs(color = "WTD (m)") +
  scale_color_gradient(low = "#008BFB", high = "#ff0051",
                       guide = guide_colorbar(barwidth = 10, barheight = 0.3)) +
  labs(tag = 'd')


if (percentile) {
  a <- a + geom_vline(xintercept = percentile_forests[[1]], linetype = "dashed", color = "black")
  b <- b + geom_vline(xintercept = percentile_savannas[[1]], linetype = "dashed", color = "black")
  c <- c + geom_vline(xintercept = percentile_croplands[[1]], linetype = "dashed", color = "black")
  d <- d + geom_vline(xintercept = percentile_grasslands[[1]], linetype = "dashed", color = "black")
}


# add marginal distributions for the x axis
a1 <- ggExtra::ggMarginal(a, type = "histogram", margins = "x", size = 15, col = "grey86", fill = "grey86")
b1 <- ggExtra::ggMarginal(b, type = "histogram", margins = "x", size = 15, col = "grey86", fill = "grey86")
c1 <- ggExtra::ggMarginal(c, type = "histogram", margins = "x", size = 15, col = "grey86", fill = "grey86")
d1 <- ggExtra::ggMarginal(d, type = "histogram", margins = "x", size = 15, col = "grey86", fill = "grey86")

# re-plot just for legend (without common_theme, so that legend gets plotted)
ciaone <- shap.plot.dependence(data_long = shap_forests,
                          smooth = F,
                          x = 'P_over_Rn', # real values to display
                          y = 'P_over_Rn', # shap values to display
                          color_feature = 'WTD') +
  geom_hline(aes(yintercept=0), alpha = 0.3) +
  ggtitle("Forests") +
  scale_x_continuous(name = element_blank(),
                     limits = c(0, 3),
                     breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3)
  ) +
  scale_y_continuous(
    name = expression(paste("SHAP value for λP/R"[n], " (", "-", ")")),
    limits = c(-0.8, 0.8),
    breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8)
  ) +
  labs(color = "WTD (m)") +
  scale_color_gradient(low = "#008BFB", high = "#ff0051",
                       guide = guide_colorbar(barwidth = 10, barheight = 0.3)) +
  labs(tag = 'a') +
  guides(colour = guide_colourbar(barwidth = 12, barheight = 0.3)) # Adjust colorbar size


# Extract the legend from one of the plots
source("R/get_legend.R")
legend <- get_legend(ciaone)

# common legend
arranged_plot <- grid.arrange(arrangeGrob(a1, b1, c1, d1, ncol = 2),
                              legend, ncol = 1, heights = c(10, 1))


# save
png(filename = "./dependence_P_Rn.png", width = 10, height = 8, units = "in", res = 600) # Open a PNG device
grid.draw(arranged_plot)  # plot
dev.off() # Turn off device and save





