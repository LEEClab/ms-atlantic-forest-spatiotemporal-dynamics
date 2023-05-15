#' ----
#' title: grassdb import limits
#' author: mauricio vancine
#' date: 2022-11-01
#' operational system: gnu/linux - ubuntu - pop_os
#' ----

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(rgrass)
library(sf)

# connect grass -----------------------------------------------------------

# connect
rgrass::initGRASS(gisBase = system("grass --config path", inter = TRUE),
                  gisDbase = "01_data/00_grassdb",
                  location = "sirgas2000_albers",
                  mapset = "PERMANENT",
                  override = TRUE)

# import data -------------------------------------------------------------

# import af limits
rgrass::execGRASS(cmd = "v.import",
                  flags = "overwrite",
                  input = "01_data/01_limits/01_af_limit/ma_limite_integrador_muylaert_et_al_2018_wgs84_geodesic_v1_2_0.shp",
                  output = "af_lim")

rgrass::execGRASS(cmd = "v.import",
                  flags = "overwrite",
                  input = "01_data/01_limits/01_af_limit/limit_af_ibge_2004_wgs84_geo.shp",
                  output = "af_lim_ibge2004")

rgrass::execGRASS(cmd = "v.import",
                  flags = "overwrite",
                  input = "01_data/01_limits/01_af_limit/limit_af_ibge_2019_wgs84_geo.shp",
                  output = "af_lim_ibge2019")

rgrass::execGRASS(cmd = "v.import",
                  flags = "overwrite",
                  input = "01_data/01_limits/01_af_limit/limit_af_ribeiroetal2009_gcs_wgs84.shp",
                  output = "af_lim_ribeiro_etal2009")

rgrass::execGRASS(cmd = "v.import",
                  flags = "overwrite",
                  input = "01_data/01_limits/01_af_limit/limit_af_lawaf2006_gcs_wgs84.shp",
                  output = "af_lim_lawaf2006")

rgrass::execGRASS(cmd = "v.import",
                  flags = "overwrite",
                  input = "01_data/01_limits/01_af_limit/limit_af_wwf_terr_ecos_biorregions_2017_gcs_wgs84.shp",
                  output = "af_ecoregions2017")


# import countries limit
rgrass::execGRASS(cmd = "v.import",
                  flags = "overwrite",
                  input = "01_data/01_limits/01_af_limit/ma_limite_integrador_muylaert_et_al_2018_wgs84_geodesic_v1_2_0_br.shp",
                  output = "af_lim_br")

rgrass::execGRASS(cmd = "v.import",
                  flags = "overwrite",
                  input = "01_data/01_limits/01_af_limit/ma_limite_integrador_muylaert_et_al_2018_wgs84_geodesic_v1_2_0_ar.shp",
                  output = "af_lim_ar")

rgrass::execGRASS(cmd = "v.import",
                  flags = "overwrite",
                  input = "01_data/01_limits/01_af_limit/ma_limite_integrador_muylaert_et_al_2018_wgs84_geodesic_v1_2_0_py.shp",
                  output = "af_lim_py")

# import ecoregions limit
rgrass::execGRASS(cmd = "v.import",
                  flags = "overwrite",
                  input = "01_data/01_limits/03_ecoregions/ecoregions_2017_af_wgs84_geodesic.gpkg",
                  output = "af_ecoregions2017_eco")


# rasterize ---------------------------------------------------------------

# limits
rgrass::execGRASS(cmd = "v.to.rast", flags = "overwrite", use = "val", input = "af_lim", output = "af_lim")
rgrass::execGRASS(cmd = "v.to.rast", flags = "overwrite", use = "val", input = "af_lim_br", output = "af_lim_br")
rgrass::execGRASS(cmd = "v.to.rast", flags = "overwrite", use = "val", input = "af_lim_ar", output = "af_lim_ar")
rgrass::execGRASS(cmd = "v.to.rast", flags = "overwrite", use = "val", input = "af_lim_py", output = "af_lim_py")

# end ---------------------------------------------------------------------
