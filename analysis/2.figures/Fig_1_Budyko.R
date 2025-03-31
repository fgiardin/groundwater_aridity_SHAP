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
  mutate(SIF_over_PAR = SIF_over_PAR * 10^6 + 0.4) #the intercept

# Fig. 1 B -----------------------------------------------------------------
common_theme <- theme(
  plot.title = element_text(hjust = 0.5, size = 16), # center title
  plot.subtitle = element_text(hjust = 0.5),
  axis.text.y=element_text(size = 10, color = "black"), #
  axis.text.x=element_text(size = 10, color = "black"),
  axis.title=element_text(size = 14),
  legend.title = element_blank(),
  legend.position = "none" # hide legend and use user-defined
)

lwidth = 1 # define line width
color_triangle = "#D7191C"
triangle_offset <- 0.05
konst = 2 # to move the diagonal segment of the red triangle closer to the other two segments (smaller triangle)

EFmax <- 3.2
theta_crit <- 1

### point PLOT ###
b <- ggplot() +
  geom_point(data = df_PFT,
             aes(x = P_over_Rn, y = SIF_over_PAR),
             color =  "black",
             alpha = 14/sqrt(nrow(df_PFT)), # set transparency of points
             size = 0.1) +
  labs(
    x = expression(paste("Moisture index: λP/", R[n], " (-)")),
    y = expression(paste("SIF/PAR x 10"^6 * " (sr"^-1 * "nm"^-1 * ")"))
  ) +
  theme_classic() +
  common_theme +

  geom_segment( # dotted upper segment
    aes(x = 0, xend = theta_crit, y = EFmax, yend = EFmax),
    linetype = "dotted",
    color = "black",
    linewidth = lwidth) +
  geom_segment( # line parallel to y-axis passing through x=1 and stopping at y=1
    aes(x = theta_crit, xend = theta_crit, y = 0, yend = EFmax),
    linetype = "dotted",
    color = "black",
    linewidth = lwidth) +
  geom_segment( # upper segment
    aes(x = theta_crit, xend = 2, y = EFmax, yend = EFmax),
    linetype = "solid",
    color = "#3A5ECC",
    linewidth = lwidth) +
  geom_segment(
    aes(x = 0, xend = theta_crit, y = 0, yend = EFmax), # diagonal segment
    linetype = "solid",
    color = "#3A5ECC",
    linewidth = lwidth) +

  # red triangle
  geom_segment( # diagonal segment
    # x = triangle_offset*0.5,
    # xend = theta_crit - triangle_offset*0.5,
    aes(
      x = triangle_offset*0.5,
      y = triangle_offset*2*konst,

      xend = theta_crit - triangle_offset*0.5*konst,
      yend = EFmax - triangle_offset
      ),
    linetype = "dashed",
    color = "#D7191C",
    size = lwidth*0.9
    ) +
  geom_segment( # vertical segment
    # inherit.aes = FALSE,
    aes(
      x = triangle_offset*0.5,
      y = triangle_offset*2*konst,

      xend = triangle_offset*0.5,
      yend = EFmax - triangle_offset
    ),
    linetype = "dashed",
    color = "#D7191C",
    size = lwidth*0.9
  ) +
  geom_segment( # horizontal segment
    aes(
      x = triangle_offset*0.5,
      y = EFmax - triangle_offset,

      xend = theta_crit - triangle_offset*0.5*konst,
      yend = EFmax - triangle_offset
      ),
    linetype = "dashed",
    color = "#D7191C",
    size = lwidth*0.9
    ) +

  scale_x_continuous(expand = c(0, 0), # remove space between axis and plotted data
                     limits = c(0,2.05)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,4.05)) +
  theme(plot.margin = unit(c(5.5, 20, 5.5, 5.5), "points"))
b



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
               linewidth = lwidth) +
  geom_segment(aes(x = theta_crit1, xend = Xmax, y = EFmax1, yend = EFmax1), # upper segment
               linetype = "solid",
               color = "#3A5ECC",
               linewidth = lwidth) +
  geom_segment(aes(x = 0, xend = theta_crit1, y = EFmax1, yend = EFmax1), # dotted upper segment
               linetype = "dotted",
               color = "black",
               linewidth = lwidth) +

  # upper line
  geom_segment(aes(x = 0, xend = theta_crit2, y = 0 + cept, yend = EFmax2), # diagonal segment
               linetype = "solid",
               color = "#D7191C",
               linewidth = lwidth) +
  geom_segment(aes(x = theta_crit2, xend = Xmax, y = EFmax2, yend = EFmax2), # upper segment
               linetype = "solid",
               color = "#D7191C",
               linewidth = lwidth) +
  geom_segment(aes(x = theta_crit1, xend = theta_crit1, y = 0, yend = 1), # line parallel to y-axis passing through x=1 and stopping at y=1
               linetype = "dotted",
               color = "black",
               linewidth = lwidth) +

  # annotations for the equations
  annotate("text", x = 0.5, y = 0.54, label = "ET == P", color = "#3A5ECC",
           angle = 57, vjust = 0, size = 5, parse = TRUE) +
  annotate("text", x = 0.45, y = 0.7, label = "ET == P + G", color = "#D7191C",
           angle = 45, vjust = 0, size = 5, parse = TRUE) +
  annotate("text", x = 1.5, y = 0.99, label = "ET == R[n]", color = "black",
           vjust = -0.5, size = 5, parse = TRUE) +
  scale_x_continuous(expand = c(0, 0), # remove space between axis and plotted data
                     limits = c(0, 2.05)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1.27)) +
  theme(plot.margin = unit(c(5.5, 5.5, 5.5, 5.5), "points"))


# combine and save --------------------------------------------------------

c <- ggarrange(a, b,
               labels = c("a", "b"),
               ncol = 2, nrow = 1)
ggsave("Fig_1.png", path = "./",
       width = 8, height = 4, dpi = 200) # 600





