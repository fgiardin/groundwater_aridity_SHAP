# map the land cover code to land cover types
# the land cover map is derived from global 300 m spatial resolution CCI LC map
# reference:
# https://maps.elie.ucl.ac.be/CCI/viewer/download.php
# https://essd.copernicus.org/articles/15/1465/2023/

land_cover_mapping <- c(
  "0" = "no_data",
  "10" = "cropland_rainfed",
  "11" = "cropland_rainfed_herbaceous_cover",
  "12" = "cropland_rainfed_tree_or_shrub_cover",
  "20" = "cropland_irrigated",
  "30" = "mosaic_cropland",
  "40" = "mosaic_natural_vegetation",
  "50" = "tree_broadleaved_evergreen_closed_to_open",
  "60" = "tree_broadleaved_deciduous_closed_to_open",
  "61" = "tree_broadleaved_deciduous_closed",
  "62" = "tree_broadleaved_deciduous_open",
  "70" = "tree_needleleaved_evergreen_closed_to_open",
  "71" = "tree_needleleaved_evergreen_closed",
  "72" = "tree_needleleaved_evergreen_open",
  "80" = "tree_needleleaved_deciduous_closed_to_open",
  "81" = "tree_needleleaved_deciduous_closed",
  "82" = "tree_needleleaved_deciduous_open",
  "90" = "tree_mixed",
  "100" = "mosaic_tree_and_shrub",
  "110" = "mosaic_herbaceous",
  "120" = "shrubland",
  "121" = "shrubland_evergreen",
  "122" = "shrubland_deciduous",
  "130" = "grassland",
  "140" = "lichens_and_mosses",
  "150" = "sparse_vegetation",
  "151" = "sparse_tree",
  "152" = "sparse_shrub",
  "153" = "sparse_herbaceous",
  "160" = "tree_cover_flooded_fresh_or_brakish_water",
  "170" = "tree_cover_flooded_saline_water",
  "180" = "shrub_or_herbaceous_cover_flooded",
  "190" = "urban",
  "200" = "bare_areas",
  "201" = "bare_areas_consolidated",
  "202" = "bare_areas_unconsolidated",
  "210" = "water",
  "220" = "snow_and_ice"
)

major_land_cover_mapping <- c(
  "0" = "other",
  "10" = "croplands",
  "11" = "croplands",
  "12" = "croplands",
  "20" = "croplands",
  "30" = "other",
  "40" = "other",
  "50" = "forests",
  "60" = "forests",
  "61" = "forests",
  "62" = "forests",
  "70" = "forests",
  "71" = "forests",
  "72" = "forests",
  "80" = "forests",
  "81" = "forests",
  "82" = "forests",
  "90" = "forests",
  "100" = "other",
  "110" = "other",
  "120" = "savannas_and_shrublands",
  "121" = "savannas_and_shrublands",
  "122" = "savannas_and_shrublands",
  "130" = "grasslands",
  "140" = "other",
  "150" = "other",
  "151" = "forests",
  "152" = "savannas_and_shrublands",
  "153" = "grasslands",
  "160" = "forests",
  "170" = "forests",
  "180" = "other",
  "190" = "other",
  "200" = "other",
  "201" = "other",
  "202" = "other",
  "210" = "other",
  "220" = "other"
)

vegetation_flag <- c(
  "0" = 0,   # "no_data" -> Not vegetation
  "10" = 1,  # "cropland_rainfed" -> Vegetation
  "11" = 1,  # "cropland_rainfed_herbaceous_cover" -> Vegetation
  "12" = 1,  # "cropland_rainfed_tree_or_shrub_cover" -> Vegetation
  "20" = 1,  # "cropland_irrigated" -> Vegetation
  "30" = 1,  # "mosaic_cropland" -> Vegetation
  "40" = 1,  # "mosaic_natural_vegetation" -> Vegetation
  "50" = 1,  # "tree_broadleaved_evergreen_closed_to_open" -> Vegetation
  "60" = 1,  # "tree_broadleaved_deciduous_closed_to_open" -> Vegetation
  "61" = 1,  # "tree_broadleaved_deciduous_closed" -> Vegetation
  "62" = 1,  # "tree_broadleaved_deciduous_open" -> Vegetation
  "70" = 1,  # "tree_needleleaved_evergreen_closed_to_open" -> Vegetation
  "71" = 1,  # "tree_needleleaved_evergreen_closed" -> Vegetation
  "72" = 1,  # "tree_needleleaved_evergreen_open" -> Vegetation
  "80" = 1,  # "tree_needleleaved_deciduous_closed_to_open" -> Vegetation
  "81" = 1,  # "tree_needleleaved_deciduous_closed" -> Vegetation
  "82" = 1,  # "tree_needleleaved_deciduous_open" -> Vegetation
  "90" = 1,  # "tree_mixed" -> Vegetation
  "100" = 1, # "mosaic_tree_and_shrub" -> Vegetation
  "110" = 1, # "mosaic_herbaceous" -> Vegetation
  "120" = 1, # "shrubland" -> Vegetation
  "121" = 1, # "shrubland_evergreen" -> Vegetation
  "122" = 1, # "shrubland_deciduous" -> Vegetation
  "130" = 1, # "grassland" -> Vegetation
  "140" = 1, # "lichens_and_mosses" -> Vegetation
  "150" = 1, # "sparse_vegetation" -> Vegetation
  "151" = 1, # "sparse_tree" -> Vegetation
  "152" = 1, # "sparse_shrub" -> Vegetation
  "153" = 1, # "sparse_herbaceous" -> Vegetation
  "160" = 1, # "tree_cover_flooded_fresh_or_brakish_water" -> Vegetation
  "170" = 1, # "tree_cover_flooded_saline_water" -> Vegetation
  "180" = 1, # "shrub_or_herbaceous_cover_flooded" -> Vegetation
  "190" = 0, # "urban" -> Not vegetation
  "200" = 0, # "bare_areas" -> Not vegetation
  "201" = 0, # "bare_areas_consolidated" -> Not vegetation
  "202" = 0, # "bare_areas_unconsolidated" -> Not vegetation
  "210" = 0, # "water" -> Not vegetation
  "220" = 0  # "snow_and_ice" -> Not vegetation
)
