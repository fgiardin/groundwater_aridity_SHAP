# Script to merge WTD and SIF variable into one dataframe and prepare for SHAP analysis

# load packages
library(tidyverse)
library(terra)
library(tidyterra)

# load data
df_int_raw <- readRDS("data/reprocessed_intmeans/dataframes/df_int_bins.rds") # environmental variables
df_WTD <- readRDS("data-raw/Fan_data/processed_data/df_WTD.rds") # global map of WTD


vec_PFT <- c("ENF", "EBF", "DNF", "DBF", "MF", "CSH", "OSH", "WSA", "SAV", "GRA", "WET", "CRO") # vector of PFT to consider

# select relevant variables from df_int
df_int <- df_int_raw %>%
  dplyr::select(lon,
                lat,
                SIF,
                PAR,
                PRECIP,
                NETRAD,
                SIF_over_PAR,
                P_over_Rn,
                SM,
                CV_SM,
                # perc_SIF_50,
                # perc_SM_50,
                # perc_cv_SM_25,
                PFT) %>%
  dplyr::filter(PFT %in% vec_PFT) # exclude water bodies

# merge with dplyr
df <- df_int %>%
  left_join(df_WTD,
            by = c("lon", "lat"))

saveRDS(df, "df_SHAP.rds", compress = "xz")




