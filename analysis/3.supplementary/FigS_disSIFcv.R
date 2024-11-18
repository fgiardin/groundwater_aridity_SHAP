# plot coefficient of variation in time of WTD
# figure used in review process

rm(list=ls())
library(tidyverse)

# load data
df_SHAP <- readRDS("data/main.rds")
source("data-raw/0_land_cover_mapping.R")

hist(df_SHAP$WTD_obs_year_std)
summary(df_SHAP$WTD_obs_year_std)
summary(df_SHAP$WTD_obs_std)

df <- df_SHAP %>%
  mutate(major_land_cover = recode(land_cover_num, !!!major_land_cover_mapping)) %>%
  filter(P_over_Rn < 3,
         P_over_Rn > 0,
         major_land_cover != "other",
         land_cover_change == 0,
         WTD_obs_year_mean > 2000, #select recent years
         WTD_obs_year_std < 5, #make sure the sampling is relatively consistent
         WTD_obs_n_sample >= 5,
         WTD_obs_n_sample < 1000,
         WTD_obs_std >= 0,
         WTD_obs_mean >= 1) %>%
  select(WTD_obs_cv)


a <- ggplot(df, aes(x = "", y = WTD_obs_cv, fill = "")) +
  geom_violin(trim = FALSE, alpha = 0.5, color = "grey") +
  geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 2.5)) +
  labs(y = "Coefficient of Variation (-)",
       x = "Observed WTD (m)") +
  scale_fill_manual(values = c("#A7C6EA")) +
  annotate("text", x = 0.5, y = 2.3, label = "italic(n) == 2436", parse = TRUE, size = 4.5, hjust = 0) +
  annotate("text", x = 0.5, y = 2.1, label = "italic('50% quantile') == 0.11", parse = TRUE, size = 4.5, hjust = 0) +
  annotate("text", x = 0.5, y = 1.9, label = "italic('90% quantile') == 0.68", parse = TRUE, size = 4.5, hjust = 0) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))


# Save the plot
png(filename = paste0("./",
                      "FigS_Dis_WTDcv.png"),
    width = 6, height = 4, units = "in", res = 600)
print(a)
dev.off()






