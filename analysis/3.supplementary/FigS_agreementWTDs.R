# SCRIPT to plot the agreement between the two WTD datasets

library(tidyverse)
library(sf)
library(maps)
library(viridis)
library(ggpubr)
library(ggplot2)
library(reshape2)
library(ggtext)

# load data
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
                land_cover_change == 0) %>%
  dplyr::select(lon, lat, SIF, SIF_over_PAR, P_over_Rn, WTD_Fan, WTD_globgm,
                elevation, major_land_cover)

# Set order for the land cover groups
df_us$major_land_cover <- factor(df_us$major_land_cover,
                                 levels = c("forests", "savannas_and_shrublands", "croplands", "grasslands"),
                                 labels = gsub("_", " ", tools::toTitleCase(c("forests", # reformat for printing
                                                                              "savannas_and_shrublands",
                                                                              "croplands",
                                                                              "grasslands"))))


# Calculate R2, Bias, and RMSE by land cover group, removing NA values
df_results <- df_us %>%
  group_by(major_land_cover) %>%
  summarise(
    R2 = cor(WTD_Fan, WTD_globgm, use = "complete.obs")^2,
    Bias = abs((mean(WTD_Fan - WTD_globgm, na.rm = TRUE) / (mean((WTD_Fan + WTD_globgm) / 2, na.rm = TRUE))) * 100), # mean bias
    RMSE = sqrt(mean((WTD_Fan - WTD_globgm)^2, na.rm = TRUE)),
    # Calculate rRMSE using the mean of both datasets
    rRMSE = abs((sqrt(mean((WTD_Fan - WTD_globgm)^2, na.rm = TRUE)) / (mean((WTD_Fan + WTD_globgm) / 2, na.rm = TRUE))) * 100) # absolute value because the mean of WTD is negative
  ) %>%
  ungroup()

print(df_results)

# Merge stats with original data for annotation
df_annotated <- df_us %>%
  left_join(df_results, by = "major_land_cover")

# Create the scatter plot with 4 panels and display statistics in each panel
fig <- ggplot(df_annotated, aes(x = WTD_Fan, y = WTD_globgm)) +
  geom_point(alpha = 0.03) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 0.5) +  # 1:1 line with thinner line
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 0.5) +  # Fit line with matching thickness
  facet_wrap(~ major_land_cover) +
  labs(x = "WTD from Fan et al. (m)", y = "WTD GLOBGM (m)") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),  # Increase axis title size
    strip.text = element_text(size = 14),  # Increase the facet title (subpanel) size
    axis.line = element_line(color = "black", size = 0.5)
  ) +
  ylim(-1500, NA) +  # Limit the range of WTD_globgm to -1500 and above
  geom_textbox(data = df_results, aes(label = paste0("R² = ", round(R2, 2),
                                                     "<br>Bias = ", round(Bias, 2),
                                                     # "<br>RMSE = ", round(RMSE, 2),
                                                     "<br>rRMSE = ", round(rRMSE, 2), "%")),
               x = -Inf, y = -Inf, hjust = -0.1, vjust = -0.1, size = 4, color = "blue",
               box.size = 0,  # Remove box background
               fill = NA,  # No fill
               halign = 0)  # Align text to left


ggsave("FigS_agreementWTDs.png", fig, width = 8, height = 8, dpi = 300)



# Bland-Altman Plot by land cover
df_us %>%
  mutate(Average = (WTD_Fan + WTD_globgm) / 2,
         Difference = WTD_Fan - WTD_globgm) %>%
  ggplot(aes(x = Average, y = Difference, color = major_land_cover)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Bland-Altman Plot",
       x = "Average of WTD_Fan and WTD_globgm",
       y = "Difference (WTD_Fan - WTD_globgm)") +
  theme_minimal()

