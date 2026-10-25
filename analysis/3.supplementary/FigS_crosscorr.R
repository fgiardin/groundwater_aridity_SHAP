# Plot correlations among the variables used in the XGBoost models.

library(tidyverse)
library(sf)
library(maps)
library(viridis)
library(ggpubr)
library(ggplot2)
library(reshape2)
library(ggcorrplot)

df_raw <- readRDS("data/main.rds")
source("data-raw/0_land_cover_mapping.R")

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


cor_data <- df_us[, c("SIF_over_PAR", "P_over_Rn", "WTD_Fan", "elevation")]

cor_matrix <- cor(cor_data, use = "complete.obs")

plotto <- ggcorrplot(cor_matrix,
           method = "square",
           type = "upper",
           lab = TRUE,
           lab_size = 3.5,
           colors = scico::scico(3, palette = "vik"),
           outline.color = "black",
           ggtheme = theme_classic() +
             theme(
               plot.title = element_blank(),
               axis.text = element_text(size = 10),
               axis.title = element_blank(),
               legend.position = "right",
               legend.title = element_text(size = 10, face = "bold"),
               legend.text = element_text(size = 9)
             )) +
  scale_x_discrete(labels = c("SIF_over_PAR" = "SIF/PAR",
                              "P_over_Rn" = expression(lambda * "P/R"[n]),
                              "WTD_Fan" = "WTD",
                              "elevation" = "Elevation")) +
  scale_y_discrete(labels = c("SIF_over_PAR" = "SIF/PAR",
                              "P_over_Rn" = expression(lambda * "P/R"[n]),
                              "WTD_Fan" = "WTD",
                              "elevation" = "Elevation"))

ggsave("Supplementary Figure 1.png", plotto, width = 5, height = 4, dpi = 600)
