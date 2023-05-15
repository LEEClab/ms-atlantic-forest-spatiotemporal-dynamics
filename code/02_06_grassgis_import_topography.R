#' ----
#' title: grassdb topography
#' author: mauricio vancine
#' date: 2022-12-19
#' operational system: gnu/linux - ubuntu - pop_os
#' ----

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(rgrass)

# connect grass -----------------------------------------------------------

# connect
rgrass::initGRASS(gisBase = system("grass --config path", inter = TRUE),
                  gisDbase = "01_data/00_grassdb",
                  location = "sirgas2000_albers",
                  mapset = "PERMANENT",
                  override = TRUE)

# import data -------------------------------------------------------------

# import fabdem
rgrass::execGRASS("r.import",
                  flags = c("overwrite"),
                  input = "01_data/07_topography/fabdem.tif",
                  output = "fabdem")

# region
rgrass::execGRASS(cmd = "g.region", flags = c("a", "p"), raster = "af_lim", res = "30")

# mask
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", raster = "mapbiomas_brazil_af_trinacional_1986_af_lim_forest")

# align
rgrass::execGRASS(cmd = "r.mapcalc",
                  flags = "overwrite",
                  expression = "fabdem_af_lim = fabdem")

# calculate ---------------------------------------------------------------

# slope
rgrass::execGRASS(cmd = "r.slope.aspect",
                  flags = c("e", "overwrite"),
                  elevation = "fabdem_af_lim",
                  precision = "DCELL",
                  slope = "fabdem_af_lim_slope",
                  nprocs = 10)

# aspect
rgrass::execGRASS(cmd = "r.slope.aspect",
                  flags = c("e", "overwrite"),
                  elevation = "fabdem_af_lim",
                  precision = "DCELL",
                  aspect = "fabdem_af_lim_aspect",
                  nprocs = 10)

# pcurvature
rgrass::execGRASS(cmd = "r.slope.aspect",
                  flags = c("e", "overwrite"),
                  elevation = "fabdem_af_lim",
                  precision = "DCELL",
                  pcurvature = "fabdem_af_lim_pcurvature",
                  nprocs = 10)

# tcurvature
rgrass::execGRASS(cmd = "r.slope.aspect",
                  flags = c("e", "overwrite"),
                  elevation = "fabdem_af_lim",
                  precision = "DCELL",
                  tcurvature = "fabdem_af_lim_tcurvature",
                  nprocs = 10)

# geomorphons
rgrass::execGRASS(cmd = "r.geomorphon",
                  flags = "overwrite",
                  elevation = "fabdem_af_lim",
                  forms = "fabdem_af_lim_geomorph")

# export -------------------------------------------------------------------

# export
rgrass::execGRASS(cmd = "r.out.gdal",
                  flags = "overwrite",
                  input = "fabdem_af_lim",
                  output = "/media/mude/Seagate Expansion Drive/data/cap02/02_topographic/fabdem_af_lim_elevation.tif",
                  createopt = "TFW=TRUE,COMPRESS=DEFLATE,BIGTIFF=YES")

rgrass::execGRASS(cmd = "r.out.gdal",
                  flags = "overwrite",
                  input = "fabdem_af_lim_slope",
                  output = "/media/mude/Seagate Expansion Drive/data/cap02/02_topographic/fabdem_af_lim_slope.tif",
                  createopt = "TFW=TRUE,COMPRESS=DEFLATE,BIGTIFF=YES")

rgrass::execGRASS(cmd = "r.out.gdal",
                  flags = "overwrite",
                  input = "fabdem_af_lim_aspect",
                  output = "/media/mude/Seagate Expansion Drive/data/cap02/02_topographic/fabdem_af_lim_aspect.tif",
                  createopt = "TFW=TRUE,COMPRESS=DEFLATE,BIGTIFF=YES")

rgrass::execGRASS(cmd = "r.out.gdal",
                  flags = "overwrite",
                  input = "fabdem_af_lim_pcurvature",
                  output = "/media/mude/Seagate Expansion Drive/data/cap02/02_topographic/fabdem_af_lim_pcurvature.tif",
                  createopt = "TFW=TRUE,COMPRESS=DEFLATE,BIGTIFF=YES")

rgrass::execGRASS(cmd = "r.out.gdal",
                  flags = "overwrite",
                  input = "fabdem_af_lim_tcurvature",
                  output = "/media/mude/Seagate Expansion Drive/data/cap02/02_topographic/fabdem_af_lim_tcurvature.tif",
                  createopt = "TFW=TRUE,COMPRESS=DEFLATE,BIGTIFF=YES")

rgrass::execGRASS(cmd = "r.out.gdal",
                  flags = "overwrite",
                  input = "fabdem_af_lim_geomorph",
                  output = "/media/mude/Seagate Expansion Drive/data/cap02/02_topographic/fabdem_af_lim_geomorph.tif",
                  createopt = "TFW=TRUE,COMPRESS=DEFLATE,BIGTIFF=YES")

# end ---------------------------------------------------------------------
