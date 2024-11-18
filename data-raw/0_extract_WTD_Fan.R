
# Script to extract Water table depth (WTD) data from Yin Fan and transform it to a dataframe with the same resolution as SIF data

# load packages and data --------------------------------------------------

# load packages
library(tidyverse)
library(terra)
library(ncdf4)
library(ncdf4.helpers)
library(sf)

# extract WTD data
dname = "WTD"
# north america
WTD_NAmerica_raw <- rast("data-raw/WTD_global/NAMERICA_WTD_annualmean.nc", dname)
NAmerica_mask <- rast("data-raw/WTD_global/NAMERICA_WTD_annualmean.nc", "mask")

# south america
WTD_SAmerica_raw <- rast("data-raw/WTD_global/SAMERICA_WTD_annualmean.nc", dname)
SAmerica_mask <- rast("data-raw/WTD_global/SAMERICA_WTD_annualmean.nc", "mask")

# africa
WTD_africa_raw <- rast("data-raw/WTD_global/AFRICA_WTD_annualmean.nc", dname)
Africa_mask <- rast("data-raw/WTD_global/AFRICA_WTD_annualmean.nc", "mask")

# eurasia
WTD_eurasia_raw <- rast("data-raw/WTD_global/EURASIA_WTD_annualmean.nc", dname)
Eurasia_mask <- rast("data-raw/WTD_global/EURASIA_WTD_annualmean.nc", "mask")

# oceania
WTD_oceania_raw <- rast("data-raw/WTD_global/OCEANIA_WTD_annualmean.nc", dname)
oceania_mask <- rast("data-raw/WTD_global/OCEANIA_WTD_annualmean.nc", "mask")


# process raw maps --------------------------------------------------------
# mask ocean
WTD_NAmerica <- terra::mask(WTD_NAmerica_raw, NAmerica_mask, maskvalue=0)
WTD_SAmerica <- terra::mask(WTD_SAmerica_raw, SAmerica_mask, maskvalue=0)
WTD_africa <- terra::mask(WTD_africa_raw, Africa_mask, maskvalue=0)
WTD_eurasia <- terra::mask(WTD_eurasia_raw, Eurasia_mask, maskvalue=0)
WTD_oceania <- terra::mask(WTD_oceania_raw, oceania_mask, maskvalue=0)

# remove fillvalue (0)
WTD_NAmerica <- ifel(WTD_NAmerica == 0, NA, WTD_NAmerica)
WTD_SAmerica <- ifel(WTD_SAmerica == 0, NA, WTD_SAmerica)
WTD_africa <- ifel(WTD_africa == 0, NA, WTD_africa)
WTD_eurasia <- ifel(WTD_eurasia == 0, NA, WTD_eurasia)
WTD_oceania <- ifel(WTD_oceania == 0, NA, WTD_oceania)

# aggregate to same resolution as SIF to match rest of dataset
WTD_NAmerica = aggregate(WTD_NAmerica, fact = 10, fun = "mean")
WTD_SAmerica = aggregate(WTD_SAmerica, fact = 10, fun = "mean")
WTD_africa = aggregate(WTD_africa, fact = 10, fun = "mean")
WTD_eurasia = aggregate(WTD_eurasia, fact = 10, fun = "mean")
WTD_oceania = aggregate(WTD_oceania, fact = 10, fun = "mean")

#  merge --------------------------------------------------------------
americas_WTD <- terra::merge(WTD_NAmerica,
                             WTD_SAmerica)

eurasiafrica_WTD <- terra::merge(WTD_africa,
                                 WTD_eurasia)

oceaniamericas_WTD <- terra::merge(americas_WTD,
                                   WTD_oceania)

world_WTD <- terra::merge(eurasiafrica_WTD,
                          oceaniamericas_WTD)

writeCDF(world_WTD, "./world_WTD.nc", overwrite=TRUE)


# resample, convert to DF and save ----------------------------------------
# load SIF map to resample
SIF_map <- rast("data/reprocessed_intmeans/NetCDFs/inter_SIF.nc")

# resample based on SIF (align geometries of NetCDF files)
WTD_resampled <- terra::resample(world_WTD, SIF_map)

# convert to dataframe for plotting
df_WTD <- terra::as.data.frame(WTD_resampled, xy = TRUE)

names(df_WTD) <- c("lon", "lat", "WTD")
saveRDS(df_WTD, "df_WTD.rds", compress = "xz")

