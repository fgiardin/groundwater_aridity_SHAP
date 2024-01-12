# script to plot Fig. 1

# load libraries and data
devtools::load_all(".")
library(lubridate)
library(tidyverse)
library(segmented)
library(beepr)
library(ggpubr)

# read in data
df_int <- readRDS("data/reprocessed_intmeans/dataframes/df_int_bins.rds")
# Y variable: either SIF_over_PAR / SM / CV_SM

# calculate theta_crit and EFmax ------------------------------------------

vec_PFT <- c("ENF", "EBF", "DNF", "DBF", "MF", "CSH", "OSH", "WSA", "SAV", "GRA", "WET", "CRO")

namePFT = "SAV" # focus on savannahs

### calculate EFmax ###
df_PFT <- df_int %>%
  dplyr::filter(lon > -125, # filter USA
                lon < -65,
                lat < 50,
                lat > 24) %>%
  # dplyr::filter(PFT %in% namePFT) %>% # do for all PFTs within USA
  mutate(SIF_over_PAR = SIF_over_PAR * 10^5) %>%
  drop_na() # remove NAs for model

# calculate EFmax, slope and θcrit in EF vs Soil relationships as in Zheng et al 2021
test <- df_PFT %>%
  dplyr::select(P_over_Rn, SIF_over_PAR) %>%
  rename(Y = SIF_over_PAR) %>%
  rename(X = P_over_Rn)

# fit segmented regression
out.lm <- lm(Y~1, test) # linear regression with Y = EF = 1 (flat EF)
xx <- -(test$X) # set to negative because of how the segmented function is defined
out.segmented <- try(segmented(out.lm, seg.Z=~xx), silent=T) # test segmented against linear model with Y = 1
summary(out.segmented)

# extract parameters from summary
Slope <- -out.segmented[["coefficients"]][["U1.xx"]] # setting positive again
theta_crit <- -out.segmented$psi[1,2]
EFmax <- out.segmented[["coefficients"]][["(Intercept)"]]
Intercept = EFmax-Slope*theta_crit # intercept with y axis of the non-flat line


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

lwidth = 0.5 # define line width
color_triangle = "#D7191C"

### point PLOT ###
b <- ggplot(df_PFT) +
  geom_point(aes(x = P_over_Rn, y = SIF_over_PAR),
             color =  "black",
             alpha = 14/sqrt(nrow(df_PFT)), # set transparency of points
             size = 0.05) +
  labs(
    x = expression(paste("λP/", R[n], " (-)")),
    y = expression(paste("SIF/PAR x 10"^5 * " (sr"^-1 * "nm"^-1 * ")"))
    ) +
  theme_classic() +
  common_theme +
  geom_segment(aes(x = theta_crit, xend = max(P_over_Rn), y = EFmax, yend = EFmax), # upper segment
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
  scale_x_continuous(expand = c(0, 0)) + # remove space between axis and plotted data (!!!) , limits = c(0, 3)
  scale_y_continuous(expand = c(0, 0))


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
    x = expression(paste("Moisture index (λP/", R[n], ")")),
    y = expression(paste("Evaporative fraction (λE/", R[n], ")"))
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
  scale_x_continuous(expand = c(0, 0), limits = c(0, Xmax + 0.1)) + # remove space between axis and plotted data (!!!)
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5*EFmax2))
a

# combine and save --------------------------------------------------------

c <- ggarrange(a, b,
          labels = c("a", "b"),
          ncol = 2, nrow = 1)
ggsave("Fig_1.png", path = "./", width = 8, height = 4, dpi = 600)


# calculate number of points in red triangle in Fig. 1b ---------------

# Condition for the horizontal boundary
condition1 <- df_PFT$SIF_over_PAR <= EFmax

# Condition for the vertical boundary (Actually, since every point will have a value >= 0 for x, this condition
# might not be necessary, but we'll keep it for clarity)
condition2 <- df_PFT$P_over_Rn >= 0

# Condition for the diagonal boundary: y = (EFmax/theta_crit) * x
# For a point to be above this line, its y value should be greater than or equal to (EFmax/theta_crit) * its x value
condition3 <- df_PFT$SIF_over_PAR >= (EFmax/theta_crit) * df_PFT$P_over_Rn

# Now combine all the conditions using & (AND) to check for points inside the triangle
inside_triangle <- condition1 & condition2 & condition3

# Calculate the percentage
percentage_inside <- sum(inside_triangle) / nrow(df_PFT) * 100

percentage_inside # 15.16617




