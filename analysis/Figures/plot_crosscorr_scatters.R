# SCRIPT to plot the cross-correlation between variables used in the XGB models

devtools::load_all(".")
library(tidyverse)
library(xgboost)
library(caret)
library(SHAPforxgboost)
library(rsample)
library(LSD)
library(beepr)
library(doMC) # to run models in parallel
library(sf)
library(maps)
library(data.table)
library(rnaturalearth)
library(ggpubr)

registerDoMC(cores=5) # specify number of cores to run in parallel // 200 for Euler, 5 when local

# load data
df_raw <- readRDS("data/reprocessed_intmeans/dataframes/df_SHAP.rds") # SIF means calculated with negative values


# loop calculations around PFT groups -----------------------------------

results <- foreach(namePFT = c("GRA", "CRO", "Forests", "Savannas")) %dopar% {

  # # option: run locally by specifying PFT:
  # namePFT = "Forests" # GRA / CRO / Forests / Savannas

  set.seed(23) # set seed for reproducibility

  # create directory to save results
  dir_name = sprintf("./%s", namePFT)
  dir.create(dir_name)

  if (namePFT == "Forests") {
    filter = c("DBF", "EBF", "ENF", "MF")
  } else if (namePFT == "Savannas") {
    filter = c("SAV", "WSA", "CSH", "OSH")
  } else {
    filter = namePFT
  }

  df <- df_raw %>%
    dplyr::filter(lon > -125, # focus on USA
                  lon < -65,
                  lat < 50,
                  lat > 24) %>%
    dplyr::filter(PFT %in% filter) %>%   # select PFT
    dplyr::select(
      lon, lat, # keep lon/lat to save df train later
      SIF_over_PAR, WTD, P_over_Rn) %>%   # select predictors
    drop_na() %>%
    mutate(SIF_over_PAR = SIF_over_PAR * 10^5 # scale with other predictors
    )


  # scatters ----------------------------------------------------------------

  if (namePFT == "Forests") {
    plottitle = namePFT
  } else if (namePFT == "Savannas") {
    plottitle = "Savannas and shrublands"
  } else if (namePFT == "CRO") {
    plottitle = "Croplands"
  } else if (namePFT == "GRA") {
    plottitle = "Grasslands"
  }

  a <- scatterheat(df, "WTD", "SIF_over_PAR", # variables
                   plottitle, # title of the plot
                   TRUE) # whether to use Pearson correlation coefficient or just R2/RMSE

  b <- scatterheat(df, "P_over_Rn", "SIF_over_PAR",
                   plottitle,
                   TRUE)

  c <- scatterheat(df, "P_over_Rn", "WTD",
                   plottitle,
                   TRUE)

  # Return these plots from current iteration
  return(list(a, b, c))

}

# Flatten the results list to get all plots in a single list
all_plots <- do.call("c", results)

# Arrange all the plots together after the loop
final_plot <- ggarrange(plotlist = all_plots, ncol = 3, nrow = 4, labels = "auto")

# save final plot
ggsave("crosscorr_plot.png", final_plot, width = 12, height = 16, dpi = 600)
