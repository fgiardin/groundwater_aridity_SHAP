# script to calculate the long-term means for all data fast using raster formats via terra packages

# load packages
library(LSD)
devtools::load_all(".")
library(raster)
library(tidyverse)
library(ncdf4)
library(ncdf4.helpers)
library(data.table)
library(beepr)
library(bigleaf)
library(terra)
library(doMC) # to run in parallel

registerDoMC(cores=5) # specify number of cores to run code in parallel (~200 for our HPC Euler or ~5 when running locally)

#InputFolder <- "/cluster/work/climate/fgiardina/" # for Euler
InputFolder <- "data-raw/TROPOMI/" # drop NetCDF files of all variables in this folder (very big dataframes; not uploaded to Git)

# if you don't have all the raw NetCDFs files, you can directly skip to "merge all"
# and use intermediate files available in this repo to continue the analysis

# SIF ---------------------------------------------------------------------
dname = "sif"
SIF_rast <- rast(paste0(InputFolder, "SIF_yield_v4.nc"), dname) # terra::rast is faster than functions in 'raster' package
# Date       : 2018-02-02, 2021-10-24, 1360 days
SIF_rast

# # remove negative values --> don't do it anymore, as it bias the final average
# SIF <- ifel(SIF_rast < 0, NA, SIF_rast)

# if there are too many NAs, remove entire cell-pixel
SIF_na <- sum(is.na(SIF_rast)) # 1-layer raster with total number of NAs per cell
SIF_masked <- terra::mask(SIF_rast, SIF_na > 130, maskvalue=TRUE) # at least one good observation per month
plot(SIF_masked[[1]])

# calculate interannual mean on all values
SIF_season <- mean(SIF_masked, na.rm = TRUE)

# to focus on the growing season, we only take points that are greater than interannual mean on all values
SIF_good <- ifel(SIF_masked > SIF_season, SIF_masked, NA)

# do interannual mean of growing season only
inter_SIF <- mean(SIF_good, na.rm=TRUE) # point that have negative interannual means here are most likely deserts and will be filtered with the PFTs below
writeCDF(inter_SIF, "./inter_SIF.nc", overwrite=TRUE)

# convert to dataframe for plotting
df_SIF <- terra::as.data.frame(inter_SIF, xy = TRUE)
names(df_SIF) <- c("lon", "lat", "SIF")
saveRDS(df_SIF, "df_SIF.rds", compress = "xz")


# PAR ---------------------------------------------------------------------
dname = "par"
PAR_rast <- rast(paste0(InputFolder, "SIF_yield_v4.nc"), dname)
# Date       : 2018-02-02, 2021-10-24 (min, max)

# apply same filtering as SIF
PAR_good <- ifel(is.na(SIF_good), NA, PAR_rast)

# interannual mean
inter_PAR <- mean(PAR_good, na.rm=TRUE)
writeCDF(inter_PAR, "./inter_PAR.nc", overwrite=TRUE)

# convert to dataframe for plotting
df_PAR <- terra::as.data.frame(inter_PAR, xy = TRUE) # xy =TRUE keeps the spatial coordinates
names(df_PAR) <- c("lon", "lat", "PAR")
saveRDS(df_PAR, "df_PAR.rds", compress = "xz")

# free memory (very large files, several GBs)
rm(list=ls()) # delete all variables
gc() # return memory to the OS
InputFolder <- "data-raw/TROPOMI/"
beep(sound = 2) # beep when it's done

# SOIL MOISTURE -----------------------------------------------------------
dname = "soil_moisture"
SM_rast <- rast(paste0(InputFolder, "Soil_moisture_v3.nc"), dname)
# Date       : 2018-02-02, 2021-10-24 (min, max)

# interannual mean and CV
inter_SM <- mean(SM_rast, na.rm=TRUE)
std_SM <- stdev(SM_rast, na.rm = TRUE)
cv_SM <- std_SM / inter_SM
writeCDF(inter_SM, "./inter_SM.nc", overwrite=TRUE)
writeCDF(cv_SM, "./cv_SM.nc", overwrite=TRUE)

# convert to dataframes for plotting
df_SM <- terra::as.data.frame(inter_SM, xy = TRUE) # xy =TRUE keeps the spatial coordinates
names(df_SM) <- c("lon", "lat", "SM")

df_SM_sd <- terra::as.data.frame(cv_SM, xy = TRUE)
names(df_SM_sd) <- c("lon", "lat", "CV_SM")

# merge two dataframes
df_SM <- cbind(df_SM,
               df_SM_sd %>% dplyr::select(CV_SM))
saveRDS(df_SM, "df_SM_cvSM", compress = "xz")

# clean memory
rm(list=ls()) # delete all variables
gc() # return memory to the OS
InputFolder <- "data-raw/TROPOMI/"

# TEMP ------------------------------------------------------------------
# find fillvalue (runs fast)
dname = "t2m"

# load all .nc
t2m2018 <- rast("data-raw/ERA-5_Land/monthly_means/2m_temperature/era5-land_recent.t2m.01deg.1m.2018.nc", dname)
t2m2019 <- rast("data-raw/ERA-5_Land/monthly_means/2m_temperature/era5-land_recent.t2m.01deg.1m.2019.nc", dname)
t2m2020 <- rast("data-raw/ERA-5_Land/monthly_means/2m_temperature/era5-land_recent.t2m.01deg.1m.2020.nc", dname)
t2m2021 <- rast("data-raw/ERA-5_Land/monthly_means/2m_temperature/era5-land_recent.t2m.01deg.1m.2021.nc", dname)

t2m <- c(t2m2018, t2m2019, t2m2020, t2m2021) # merge years together
t2m <- ifel(t2m == -32767, NA, t2m) # remove fill value
t2m_mean <- mean(t2m, na.rm=TRUE) # do interannual mean
t2m_mean <- t2m_mean -272.15 # convert to Celsius

# resample to SIF
inter_TEMP <- resample(t2m_mean, inter_SIF)

# save map
writeCDF(inter_TEMP, "./inter_TEMP.nc", overwrite=TRUE)

# convert to dataframe
df_TEMP <- terra::as.data.frame(inter_TEMP, xy = TRUE) # xy =TRUE keeps the spatial coordinates
names(df_TEMP) <- c("lon", "lat", "TEMP")
saveRDS(df_TEMP, "df_TEMP.rds", compress = "xz")


# NETRAD ------------------------------------------------------------------

# surface_net_solar_radiation
dname = "ssr"

# load all .nc
ssr2018 <- rast("data-raw/ERA-5_Land/monthly_means/surface_net_solar_radiation/era5-land_recent.ssr.01deg.1m.2018.nc", dname)
ssr2019 <- rast("data-raw/ERA-5_Land/monthly_means/surface_net_solar_radiation/era5-land_recent.ssr.01deg.1m.2019.nc", dname)
ssr2020 <- rast("data-raw/ERA-5_Land/monthly_means/surface_net_solar_radiation/era5-land_recent.ssr.01deg.1m.2020.nc", dname)
ssr2021 <- rast("data-raw/ERA-5_Land/monthly_means/surface_net_solar_radiation/era5-land_recent.ssr.01deg.1m.2021.nc", dname)

ssr <- c(ssr2018, ssr2019, ssr2020, ssr2021) # merge years together
ssr <- ifel(ssr == -32767, NA, ssr) # remove fill value
ssr_mean <- mean(ssr, na.rm=TRUE) # do interannual mean

# surface_net_thermal_radiation
dname = "str"
str_nc <- nc_open("data-raw/ERA-5_Land/monthly_means/surface_net_thermal_radiation/era5-land_recent.str.01deg.1m.2019.nc")
fillvalue <- ncatt_get(str_nc, dname, "_FillValue")
fillvalue$value # -32767
str_units <- ncatt_get(str_nc, dname,"units")
str_units$value # "J m**-2" --> J m-2 (daily scale)
nc_close(str_nc)

# load all .nc
str2018 <- rast("data-raw/ERA-5_Land/monthly_means/surface_net_thermal_radiation/era5-land_recent.str.01deg.1m.2018.nc", dname)
str2019 <- rast("data-raw/ERA-5_Land/monthly_means/surface_net_thermal_radiation/era5-land_recent.str.01deg.1m.2019.nc", dname)
str2020 <- rast("data-raw/ERA-5_Land/monthly_means/surface_net_thermal_radiation/era5-land_recent.str.01deg.1m.2020.nc", dname)
str2021 <- rast("data-raw/ERA-5_Land/monthly_means/surface_net_thermal_radiation/era5-land_recent.str.01deg.1m.2021.nc", dname)

str <- c(str2018, str2019, str2020, str2021) # merge years together
str <- ifel(str == -32767, NA, str) # remove fill value
str_mean <- mean(str, na.rm=TRUE) # do interannual mean

# add str and ssr
NETRAD = str_mean + ssr_mean

# resample to SIF + save map
NETRAD_resampled = terra::resample(NETRAD, inter_SIF) # plotted and it is identical after resample

# convert to monthly flux (from J m-2 to W m-2)
inter_NETRAD = NETRAD_resampled / 86400

# save map (J m-2)
writeCDF(inter_NETRAD, "./inter_NETRAD.nc", overwrite=TRUE)

# convert to dataframe for plotting
df_NETRAD <- terra::as.data.frame(inter_NETRAD, xy = TRUE) # xy =TRUE keeps the spatial coordinates
names(df_NETRAD) <- c("lon", "lat", "NETRAD")
saveRDS(df_NETRAD, "df_NETRAD.rds", compress = "xz")

# clean memory
rm(list=ls()) # delete all variables
gc() # return memory to the OS
InputFolder <- "data-raw/TROPOMI/"

# PRECIP ------------------------------------------------------------------

# MSWEP
dname = "precipitation"
MSWEP_rast <- rast(paste0(InputFolder, "precipitation_soilT_pressure_wind.nc"), dname)

# interannual mean
inter_MSWEP <- mean(MSWEP_rast, na.rm=TRUE)
inter_MSWEP <- flip(inter_MSWEP, direction="vertical")
writeCDF(inter_MSWEP, "./inter_MSWEP.nc", overwrite=TRUE)

# convert to dataframe for plotting
df_MSWEP <- terra::as.data.frame(inter_MSWEP, xy = TRUE) # xy =TRUE keeps the spatial coordinates
names(df_MSWEP) <- c("lon", "lat", "PRECIP")
saveRDS(df_MSWEP, "df_MSWEP.rds", compress = "xz")


# ERA-5 Land
# find fillvalue
dname = "tp"
PRECIP_nc <- nc_open("data-raw/ERA-5_Land/monthly_means/total_precipitation/era5-land_recent.tp.01deg.1m.2018.nc")
fillvalue <- ncatt_get(PRECIP_nc, dname, "_FillValue")
fillvalue$value # -32767
PRECIP_units <- ncatt_get(PRECIP_nc, dname,"units")
PRECIP_units$value # "m" actually "m/day"
nc_close(PRECIP_nc)

# load all .nc
tp2018 <- rast("data-raw/ERA-5_Land/monthly_means/total_precipitation/era5-land_recent.tp.01deg.1m.2018.nc", dname)
tp2019 <- rast("data-raw/ERA-5_Land/monthly_means/total_precipitation/era5-land_recent.tp.01deg.1m.2019.nc", dname)
tp2020 <- rast("data-raw/ERA-5_Land/monthly_means/total_precipitation/era5-land_recent.tp.01deg.1m.2020.nc", dname)
tp2021 <- rast("data-raw/ERA-5_Land/monthly_means/total_precipitation/era5-land_recent.tp.01deg.1m.2021.nc", dname)

tp <- c(tp2018, tp2019, tp2020, tp2021) # merge years together
tp <- ifel(tp == -32767, NA, tp) # remove fill value
tp_mean <- mean(tp, na.rm=TRUE) # do interannual mean

# resample to SIF
inter_PRECIP <- resample(tp_mean, inter_SIF)

# save map (m/day)
writeCDF(inter_PRECIP, "./inter_PRECIP.nc", overwrite=TRUE)

# convert to dataframe for plotting
df_PRECIP <- terra::as.data.frame(inter_PRECIP, xy = TRUE) # xy =TRUE keeps the spatial coordinates
names(df_PRECIP) <- c("lon", "lat", "PRECIP")
saveRDS(df_PRECIP, "df_PRECIP.rds", compress = "xz")

# PFT ---------------------------------------------------------------------
dname = "Majority_Land_Cover_Type_1"
PFT_rast <- rast(paste0(InputFolder, "landcover_MCD12C1.nc"), dname)
# Date       : 2018-02-02, 2021-10-24 (min, max)
PFT_rast <- flip(PFT_rast, direction="vertical")


# find fillvalue (runs fast)
PFT_nc <- nc_open(paste0(InputFolder, "landcover_MCD12C1.nc"))
fillvalue <- ncatt_get(PFT_nc, dname, "_FillValue")
fillvalue$value # 255, but all values are already between 0 and 16 --> no need to replace
PFT_units <- ncatt_get(PFT_nc, dname,"units")
PFT_units$value
nc_close(PFT_nc)

# convert to dataframe for plotting
df_PFT <- terra::as.data.frame(PFT_rast, xy = TRUE)
names(df_PFT) <- c("lon", "lat", "2018", "2019", "2020")

# only retain 2019
df_PFT_2019 <- df_PFT %>%
  dplyr::select(1,2,4) #use column number because "2019" won't work (consider as column number instead of column name)
names(df_PFT_2019) <- c("lon", "lat", "PFT")
saveRDS(df_PFT_2019, "df_PFT_2019.rds", compress = "xz")

# MERGE ALL -------------------------------------------------

# load dataframes processed as indicated above
df_SIF <- readRDS("data/reprocessed_intmeans/dataframes/SIF_no_neg/df_SIF.rds") # SIF means calculated without negative values
df_PAR <- readRDS("data/reprocessed_intmeans/dataframes/SIF_no_neg/df_PAR.rds") # PAR matching SIF without negative values
df_PFT_2019 <- readRDS("data/reprocessed_intmeans/dataframes/df_PFT_2019.rds")
df_SM <- readRDS("data/reprocessed_intmeans/dataframes/df_SM_cvSM.rds")
df_NETRAD <- readRDS("data/reprocessed_intmeans/dataframes/df_NETRAD.rds")
df_TEMP <- readRDS("data/reprocessed_intmeans/dataframes/df_TEMP.rds")
df_MSWEP <- readRDS("data/reprocessed_intmeans/dataframes/df_MSWEP.rds")
# df_PRECIP <- readRDS("data/reprocessed_intmeans/dataframes/df_PRECIP.rds") # ERA-5 Land Precip --> outliers in tropics, we use MSWEP as it is based on ERA-5 but better

# create dataframe with all variables, calculate ratios and filter
df_inter <- df_PFT_2019 %>%
  dplyr::filter(PFT != 0) %>% # filter oceans/water bodies (DF is thus lighter/faster)
  left_join(df_SIF, by = c("lon", "lat")) %>% # left_join: keep same pixels as SIF (otherwise super big DF with entire globe)
  left_join(df_PAR, by = c("lon", "lat")) %>%
  left_join(df_SM, by = c("lon", "lat")) %>%
  left_join(df_NETRAD, by = c("lon", "lat")) %>%
  # full_join(df_PRECIP, by = c("lon", "lat")) %>% # to merge ERA-5 precip
  left_join(df_TEMP, by = c("lon", "lat")) %>%
  left_join(df_MSWEP, by = c("lon", "lat"))
# don't remove NAs here: it will share the gaps between all data and increase overall NAs. Better to remove according to the variables needed at each step of the future analysis

coeff = 3 # coefficient to remove outliers in ratios
df_inter_filter <-  df_inter %>%
  dplyr::filter(NETRAD > 0) %>%
  mutate(NETRAD = LE.to.ET(NETRAD, TEMP)) %>% # convert Rn from W/m2 to mm/s
  mutate(NETRAD = NETRAD*86400*365) %>% # convert Rn from mm/s to mm/year
  # mutate(PRECIP = PRECIP*365*1000) %>% # ERA-5 Land: convert P from m/day to mm/year
  mutate(PRECIP = PRECIP*365/8) %>% # MSWEP: convert P from mm/8day to mm/year
  mutate(PRECIP = replace(
    PRECIP,
    PRECIP > 5000, # remove values bigger than 5000 mm/year
    NA
  )) %>%
  mutate(SIF = SIF/1000) %>% # convert SIF from mW to W
  mutate(SIF_over_PAR = SIF/PAR,    # calculate ratios
         P_over_Rn = PRECIP/NETRAD) %>%
  # remove outliers in ratios (replace with NAs) by PFT group
  group_by(PFT) %>%
  mutate(SIF_over_PAR = replace(
    SIF_over_PAR,
    SIF_over_PAR < mean(SIF_over_PAR, na.rm=TRUE) - (coeff * sd(SIF_over_PAR, na.rm=TRUE)),
    NA
  )) %>%
  mutate(SIF_over_PAR = replace(
    SIF_over_PAR,
    SIF_over_PAR > mean(SIF_over_PAR, na.rm=TRUE) + (coeff * sd(SIF_over_PAR, na.rm=TRUE)),
    NA
  )) %>%
  mutate(P_over_Rn = replace(
    P_over_Rn,
    P_over_Rn < mean(P_over_Rn, na.rm=TRUE) - (coeff * sd(P_over_Rn, na.rm=TRUE)),
    NA
  )) %>%
  mutate(P_over_Rn = replace(
    P_over_Rn,
    P_over_Rn > mean(P_over_Rn, na.rm=TRUE) + (coeff * sd(P_over_Rn, na.rm=TRUE)),
    NA
  )) %>%
  ungroup()


df_inter_filter %>% group_by(PFT) %>% summarise(n = n()) # check that there are enough obs by PFT


# adjust PFT column
df_inter_filter$PFT <-
  recode(df_inter_filter$PFT,
         '1' = "ENF",
         '2' = "EBF",
         '3' = "DNF",
         '4' = "DBF",
         '5' = "MF",
         '6' = "CSH",
         '7' = "OSH",
         '8' = "WSA",
         '9' = "SAV",
         '10' = "GRA",
         '11' = "WET",
         '12' = "CRO"
  )


# BINNING -------------------------------------------------------------
# bin by xaxis and calculate lower 25% percentile per bin per PFT
# divide x axis in bins BY PFT
df_bins <- df_inter_filter %>%
  # drop_na(PFT) %>%  # remove NAs in PFTs
  group_by(PFT) %>%
  mutate(bin = as.numeric(cut_number(P_over_Rn,{n()/1000}))) %>% # dynamic number of bins: equal to number of rows per PFT divided by 1000
  ungroup()                                                      # aka at least 1000 points per bin

# calculate quartiles and median in every bin
df_percentiles <- df_bins %>%
  group_by(PFT, bin) %>%
  summarise(
    # SIF
    perc_SIF_10 = stats::quantile(SIF_over_PAR, probs=0.10, na.rm = TRUE),
    perc_SIF_20 = stats::quantile(SIF_over_PAR, probs=0.20, na.rm = TRUE),
    perc_SIF_30 = stats::quantile(SIF_over_PAR, probs=0.30, na.rm = TRUE),
    perc_SIF_40 = stats::quantile(SIF_over_PAR, probs=0.40, na.rm = TRUE),
    perc_SIF_50 = stats::quantile(SIF_over_PAR, probs=0.50, na.rm = TRUE),
    perc_SIF_60 = stats::quantile(SIF_over_PAR, probs=0.60, na.rm = TRUE),
    perc_SIF_70 = stats::quantile(SIF_over_PAR, probs=0.70, na.rm = TRUE),
    perc_SIF_80 = stats::quantile(SIF_over_PAR, probs=0.80, na.rm = TRUE),
    perc_SIF_90 = stats::quantile(SIF_over_PAR, probs=0.90, na.rm = TRUE),

    # SM
    perc_SM_10 = stats::quantile(SM, probs=0.10, na.rm = TRUE),
    perc_SM_20 = stats::quantile(SM, probs=0.20, na.rm = TRUE),
    perc_SM_30 = stats::quantile(SM, probs=0.30, na.rm = TRUE),
    perc_SM_40 = stats::quantile(SM, probs=0.40, na.rm = TRUE),
    perc_SM_50 = stats::quantile(SM, probs=0.50, na.rm = TRUE),
    perc_SM_60 = stats::quantile(SM, probs=0.60, na.rm = TRUE),
    perc_SM_70 = stats::quantile(SM, probs=0.70, na.rm = TRUE),
    perc_SM_80 = stats::quantile(SM, probs=0.80, na.rm = TRUE),
    perc_SM_90 = stats::quantile(SM, probs=0.90, na.rm = TRUE),

    # cvSM
    perc_cvSM_10 = stats::quantile(CV_SM, probs=0.10, na.rm = TRUE),
    perc_cvSM_20 = stats::quantile(CV_SM, probs=0.20, na.rm = TRUE),
    perc_cvSM_30 = stats::quantile(CV_SM, probs=0.30, na.rm = TRUE),
    perc_cvSM_40 = stats::quantile(CV_SM, probs=0.40, na.rm = TRUE),
    perc_cvSM_50 = stats::quantile(CV_SM, probs=0.50, na.rm = TRUE),
    perc_cvSM_60 = stats::quantile(CV_SM, probs=0.60, na.rm = TRUE),
    perc_cvSM_70 = stats::quantile(CV_SM, probs=0.70, na.rm = TRUE),
    perc_cvSM_80 = stats::quantile(CV_SM, probs=0.80, na.rm = TRUE),
    perc_cvSM_90 = stats::quantile(CV_SM, probs=0.90, na.rm = TRUE)
  ) %>%
  ungroup()


# merge two df and flag bin in column
df_int <- df_bins %>%
  left_join(df_percentiles, by = c("bin", "PFT")) %>%
  mutate(
    # create flag to indicate to which percentile SIF belongs
    flag_SIF = if_else(
      SIF_over_PAR < perc_SIF_10,
      10,
      if_else(
        SIF_over_PAR < perc_SIF_20,
        20,
        if_else(
          SIF_over_PAR < perc_SIF_30,
          30,
          if_else(
            SIF_over_PAR < perc_SIF_40,
            40,
            if_else(
              SIF_over_PAR < perc_SIF_50,
              50,
              if_else(
                SIF_over_PAR < perc_SIF_60,
                60,
                if_else(
                  SIF_over_PAR < perc_SIF_70,
                  70,
                  if_else(
                    SIF_over_PAR < perc_SIF_80,
                    80,
                    if_else(
                      SIF_over_PAR < perc_SIF_90,
                      90,
                      100)))))))))) %>%
  mutate(
    # create flag to indicate to which percentile SIF belongs
    flag_SM = if_else(
      SM < perc_SM_10,
      10,
      if_else(
        SM < perc_SM_20,
        20,
        if_else(
          SM < perc_SM_30,
          30,
          if_else(
            SM < perc_SM_40,
            40,
            if_else(
              SM < perc_SM_50,
              50,
              if_else(
                SM < perc_SM_60,
                60,
                if_else(
                  SM < perc_SM_70,
                  70,
                  if_else(
                    SM < perc_SM_80,
                    80,
                    if_else(
                      SM < perc_SM_90,
                      90,
                      100)))))))))) %>%
  mutate(
    # create flag to indicate to which percentile SIF belongs
    flag_cv_SM = if_else(
      CV_SM < perc_cvSM_10,
      10,
      if_else(
        CV_SM < perc_cvSM_20,
        20,
        if_else(
          CV_SM < perc_cvSM_30,
          30,
          if_else(
            CV_SM < perc_cvSM_40,
            40,
            if_else(
              CV_SM < perc_cvSM_50,
              50,
              if_else(
                CV_SM < perc_cvSM_60,
                60,
                if_else(
                  CV_SM < perc_cvSM_70,
                  70,
                  if_else(
                    CV_SM < perc_cvSM_80,
                    80,
                    if_else(
                      CV_SM < perc_cvSM_90,
                      90,
                      100))))))))))
saveRDS(df_int, "df_int_bins.rds", compress = "xz") # XXX change


beep(sound = 2) # beep when it's done














