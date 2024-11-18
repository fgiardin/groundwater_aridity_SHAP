# script to plot Fig. 1

# load libraries and data
library(lubridate)
library(tidyverse)
library(segmented)
library(ggpubr)

# read in data
df_int <- readRDS("data/main.rds")

### calculate EFmax ###
df_PFT <- df_int %>%
  dplyr::filter(lon > -125, # filter USA
                lon < -65,
                lat < 50,
                lat > 24) %>%
  # dplyr::filter(PFT %in% namePFT) %>% # do for all PFTs within USA
  mutate(SIF_over_PAR = SIF_over_PAR * 10^6 + 0.4) #the intercept
  # drop_na() # remove NAs for model

# Fig. 1 B -----------------------------------------------------------------
common_theme <- theme(
  plot.title = element_text(hjust = 0.5, size = 14), # center title
  plot.subtitle = element_text(hjust = 0.5),
  axis.text.y=element_text(size = 10),
  axis.text.x=element_text(size = 10, color = "black"),
  axis.title=element_text(size = 12),
  legend.title = element_blank(),
  legend.position = "none" # hide legend and use user-defined
)

lwidth = 0.8 # define line width
color_triangle = "#D7191C"

EFmax <- 3.2
theta_crit <- 1

### point PLOT ###
b <- ggplot(df_PFT) +
  geom_point(aes(x = P_over_Rn, y = SIF_over_PAR),
             color =  "black",
             alpha = 14/sqrt(nrow(df_PFT)), # set transparency of points
             size = 0.1) +
  labs(
    x = expression(paste("Moisture index: λP/", R[n], " (-)")),
    y = expression(paste("SIF/PAR x 10"^6 * " (sr"^-1 * "nm"^-1 * ")"))
  ) +
  theme_classic() +
  common_theme +
  geom_segment(aes(x = theta_crit, xend = 2, y = EFmax, yend = EFmax), # upper segment
               linetype = "solid",
               color = "black",
               linewidth = lwidth) +
  geom_segment(aes(x = 0, xend = theta_crit, y = 0, yend = EFmax), # diagonal segment
               linetype = "solid",
               color = color_triangle,
               linewidth = lwidth) +
  geom_segment(aes(x = 0, xend = theta_crit, y = EFmax, yend = EFmax), # dashed upper segment
               linetype = "dashed",
               color = color_triangle,
               linewidth = lwidth) +
  geom_segment(aes(x = 0.015, xend = 0.015, y = 0, yend = EFmax), # dashed vertical segment
               linetype = "dashed",
               color = color_triangle,
               linewidth = lwidth) +
  scale_x_continuous(expand = c(0, 0), limits = c(0,2.05)) + # remove space between axis and plotted data (!!!) , limits = c(0, 3)
  scale_y_continuous(expand = c(0, 0), limits = c(0,4.05)) +
  theme(plot.margin = unit(c(5.5, 20, 5.5, 5.5), "points"))


# Fig. 1 A ----------------------------------------------------------------

# define horizontal spread of figure
Xmax = 2

# define EFmax1 and theta_crit1 for lower line
theta_crit1 = 1
EFmax1 = 1

# define EFmax1 and theta_crit1 for upper line
theta_crit2 = 1
EFmax2 = EFmax1 + 0.005
cept = 0.4 # intercept of lower line with y axis (if any)

# plot figure
a <- ggplot() +
  labs(
    x = expression(paste("Moisture index: λP/", R[n], " (-)")),
    y = expression(paste("Evaporative fraction: λE/", R[n], " (-)"))
  ) +
  theme_classic() +
  common_theme +
  # lower line
  geom_segment(aes(x = 0, xend = theta_crit1, y = 0, yend = EFmax1), # diagonal segment
               linetype = "solid",
               color = "#3A5ECC",
               linewidth = lwidth + 0.2) +
  geom_segment(aes(x = theta_crit1, xend = Xmax, y = EFmax1, yend = EFmax1), # upper segment
               linetype = "solid",
               color = "#3A5ECC",
               linewidth = lwidth + 0.2) +
  geom_segment(aes(x = 0, xend = theta_crit1, y = EFmax1, yend = EFmax1), # dashed upper segment
               linetype = "dashed",
               color = "grey23",
               linewidth = lwidth + 0.2) +
  # upper line
  geom_segment(aes(x = 0, xend = theta_crit2, y = 0 + cept, yend = EFmax2), # diagonal segment
               linetype = "solid",
               color = "#D7191C",
               linewidth = lwidth + 0.2) +
  geom_segment(aes(x = theta_crit2, xend = Xmax, y = EFmax2, yend = EFmax2), # upper segment
               linetype = "solid",
               color = "#D7191C",
               linewidth = lwidth + 0.2) +
  geom_vline(xintercept = theta_crit1, # line parallel to y-axis passing through x=1
             linetype = "dotted",
             color = "grey23",
             linewidth = lwidth) +
  # annotations for the equations
  annotate("text", x = 0.5, y = 0.54, label = "ET == P", color = "#3A5ECC",
           angle = 57, vjust = 0, size = 5, parse = TRUE) +
  annotate("text", x = 0.45, y = 0.7, label = "ET == P + G", color = "#D7191C",
           angle = 45, vjust = 0, size = 5, parse = TRUE) +
  annotate("text", x = 1.5, y = 0.99, label = "ET == R[n]", color = "black",
           vjust = -0.5, size = 5, parse = TRUE) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2.05)) + # remove space between axis and plotted data (!!!)
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.3)) +
  theme(plot.margin = unit(c(5.5, 5.5, 5.5, 5.5), "points"))


# combine and save --------------------------------------------------------

c <- ggarrange(a, b,
               labels = c("a", "b"),
               ncol = 2, nrow = 1)
ggsave("Fig_1.png", path = "./",
       width = 8, height = 4, dpi = 600)





