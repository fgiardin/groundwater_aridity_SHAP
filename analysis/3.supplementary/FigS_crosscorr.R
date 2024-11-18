# SCRIPT to plot the cross-correlation between variables used in the XGB models

library(tidyverse)
library(sf)
library(maps)
library(viridis)
library(ggpubr)
library(ggplot2)
library(reshape2)
library(ggcorrplot)

# load data
input_folder <- "data/jiangong/"
source("data/jiangong/0_land_cover_mapping.R")
df_raw <- readRDS(paste0(input_folder, "main.rds"))

# process data
df_us <- df_raw %>% dplyr::filter(lon > -125, # focus on USA
                                  lon < -65,
                                  lat < 50,
                                  lat > 24) %>%
  mutate(SIF_over_PAR = SIF_over_PAR * 10^6, # the unit here is X10^3 m sr-1 nm-1 or n sr-1 nm-1
         major_land_cover = recode(land_cover_num, !!!major_land_cover_mapping)) %>%
  dplyr::filter(P_over_Rn < 3,
                P_over_Rn > 0,
                major_land_cover != "other",
                land_cover_change == 0) %>%  #irrigated crop
  dplyr::select(lon, lat, SIF, SIF_over_PAR, P_over_Rn, WTD_Fan, WTD_globgm,
                elevation, major_land_cover)


## PLOT
# Select relevant columns for correlation analysis
cor_data <- df_us[, c("SIF_over_PAR", "P_over_Rn", "WTD_Fan", "elevation")]

# Calculate the correlation matrix
cor_matrix <- cor(cor_data, use = "complete.obs")

# Create a heatmap of the correlation matrix
plotto <- ggcorrplot(cor_matrix,
           method = "square",           # square tiles for a cleaner look
           type = "upper",              # Only display the lower triangle
           lab = TRUE,                  # Show the correlation coefficients
           lab_size = 3.5,              # Adjust label size for readability
           colors = c("#B2182B", "white", "#2166AC"), # Use colorblind-friendly colors
           outline.color = "black",     # Add a border around tiles
           # title = "Correlation Matrix",
           ggtheme = theme_classic() +  # Use a classic theme for a clean look
             theme(
               plot.title = element_blank(),
               # plot.title = element_text(hjust = 0.5, face = "bold", size = 12), # Center and bold title
               axis.text = element_text(size = 10),   # Adjust axis text size
               axis.title = element_blank(),          # Remove axis titles
               legend.position = "right",             # Place the legend on the right
               legend.title = element_text(size = 10, face = "bold"), # Bold legend title
               legend.text = element_text(size = 9)   # Adjust legend text size
             )) +
  scale_x_discrete(labels = c("SIF_over_PAR" = "SIF/PAR",
                              "P_over_Rn" = expression(lambda * "P/R"[n]),
                              "WTD_Fan" = "WTD",
                              "elevation" = "Elevation")) +
  scale_y_discrete(labels = c("SIF_over_PAR" = "SIF/PAR",
                              "P_over_Rn" = expression(lambda * "P/R"[n]),
                              "WTD_Fan" = "WTD",
                              "elevation" = "Elevation"))

# save final plot
ggsave("FigS_crosscorr.png", plotto, width = 5, height = 4, dpi = 600)
