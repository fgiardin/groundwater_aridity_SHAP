# Script to merge WTD and SIF variable into one dataframe and prepare for SHAP analysis

# load packages
library(tidyverse)
library(terra)
library(tidyterra)

# load data
df_int <- readRDS("data/reprocessed_intmeans/dataframes/df_int.rds") # environmental variables (see script 1.calculate_longterm_means.R)
df_WTD <- readRDS("data/processed_WTD/df_WTD.rds") # map of WTD (see data-raw/extract_WTD.R)


vec_PFT <- c("ENF", "EBF", "DNF", "DBF", "MF", "CSH", "OSH", "WSA", "SAV", "GRA", "WET", "CRO") # vector of PFT to consider

# select relevant variables from df_int
df_int <- df_int %>%
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
df_SHAP <- df_int %>%
  left_join(df_WTD,
            by = c("lon", "lat"))

saveRDS(df_SHAP, "df_SHAP.rds", compress = "xz")



