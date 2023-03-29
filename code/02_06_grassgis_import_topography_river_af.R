#' ----
#' title: grassdb topography
#' author: mauricio vancine
#' date: 2022-12-19
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
                  input = "01_data/04_topografia/fabdem.tif",
                  output = "fabdem")


# import rivers
rgrass::execGRASS("v.import",
                  flags = c("overwrite"),
                  input = "/home/mude/data/onedrive/doutorado/cap01b/01_data/04_topografia/topo_LL/ca_arc.shp",
                  output = "rivers_if")


# adjust data ---------------------------------------------------------------

# region
rgrass::execGRASS(cmd = "g.region",
                  flags = c("a", "p"),
                  vector = "af_lim",
                  res = "30")

# align
rgrass::execGRASS(cmd = "r.mapcalc",
                  flags = "overwrite",
                  expression = "fabdem_af_lim = fabdem")

# region
rgrass::execGRASS(cmd = "g.region",
                  flags = c("a", "p"),
                  vector = "rivers_if",
                  res = "30")


# calculate ---------------------------------------------------------------

# slope, aspect, curvature
rgrass::execGRASS(cmd = "r.slope.aspect",
                  flags = "overwrite",
                  elevation = "fabdem_af_lim",
                  slope = "fabdem_af_lim_slope",
                  aspect = "fabdem_af_lim_aspect",
                  pcurvature = "fabdem_af_lim_pcurvature",
                  tcurvature = "fabdem_af_lim_tcurvature",
                  nprocs = 10)

# units -------------------------------------------------------------------

# slope units
# rgrass::execGRASS(cmd = "r.slopeunits",
#                   flags = "overwrite",
#                   demmap = "fabdem_af_lim",
#                   slumap = "fabdem_af_lim_slu",
#                   thresh =  10,
#                   areamin = 1000,
#                   areamax = 10000,
#                   cvmin  = 0.5,
#                   rf = 1,
#                   maxiteration = 100)

# tpi
# rgrass::execGRASS(cmd = "g.extension", extension = "r.tpi", operation = "add")
# rgrass::execGRASS(cmd = "r.tpi",
#                   flags = "overwrite",
#                   input = "fabdem_af_lim",
#                   output = "fabdem_af_lim_tpi")

# twi
# rgrass::execGRASS(cmd = "r.topidx",
#                   flags = "overwrite",
#                   input = "fabdem_af_lim",
#                   output = "fabdem_af_lim_twi")

# flow accumulate
# rgrass::execGRASS("r.watershed",
#                   flags = c("m", "overwrite"),
#                   elevation = "fabdem_af_lim",
#                   accumulation = "fabdem_af_lim_acc",
#                   tci = "fabdem_af_lim_tci")

# stream
# rgrass::execGRASS("r.stream.extract",
#                   flags = c("overwrite"),
#                   elevation = "fabdem_af_lim",
#                   accumulation = "fabdem_af_lim_acc",
#                   stream_raster = "fabdem_af_lim_stream",
#                   direction = "fabdem_af_lim_flow_dir",
#                   threshold = 100)

# stream order
# sudo grass --exec g.extension -s extension=r.stream.order operation=add
# rgrass::execGRASS(cmd = "g.extension", extension = "r.stream.order", operation = "add")
# rgrass::execGRASS("r.stream.order",
#                   flags = c("overwrite"),
#                   stream_rast = "fabdem_af_lim_stream",
#                   direction = "fabdem_af_lim_flow_dir",
#                   elevation = "fabdem_af_lim",
#                   accumulation = "fabdem_af_lim_acc",
#                   stream_vect = "fabdem_af_lim_stream_vect",
#                   strahler = "fabdem_af_lim_strahler")

# extract
# rgrass::execGRASS("v.extract",
#                   flags = "overwrite",
#                   input = "fabdem_af_lim_stream_vect",
#                   type = "point",
#                   where = "(strahler = '1') or (strahler = 'null')",
#                   output = "fabdem_af_lim_spring")
#
# rgrass::execGRASS("v.extract",
#                   flags = "overwrite",
#                   input = "fabdem_af_lim_stream_vect",
#                   type = "line",
#                   output = "fabdem_af_lim_stream")
#
# # kernel
# rgrass::execGRASS("v.kernel",
#                   flags = "overwrite",
#                   input = "fabdem_af_lim_spring",
#                   output = "fabdem_af_lim_spring_kde",
#                   radius = 1000)
#
# rgrass::execGRASS("r.mapcalc",
#                   flags = "overwrite",
#                   expression = "fabdem_af_lim_spring_kde = fabdem_af_lim_spring_kde * af_lim")
#
# rgrass::execGRASS("r.colors",
#                   map = "fabdem_af_lim_spring_kde",
#                   color = "bcyr")

# end ---------------------------------------------------------------------
