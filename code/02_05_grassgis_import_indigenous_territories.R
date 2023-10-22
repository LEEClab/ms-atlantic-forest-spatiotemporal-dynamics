#' ----
#' title: atlantic forest spatiotemporal dynamic - import indigenous territories
#' author: mauricio vancine
#' date: 2023-04-10
#' operational system: gnu/linux - ubuntu - pop_os
#' ----

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(rgrass)
library(sf)
library(terra)

# connect grass -----------------------------------------------------------

# connect
rgrass::initGRASS(gisBase = system("grass --config path", inter = TRUE),
                  gisDbase = "01_data/00_grassdb",
                  location = "sirgas2000_albers",
                  mapset = "PERMANENT",
                  override = TRUE)

# read limit
af_lim <- rgrass::read_VECT(vname = "af_lim") %>%
  sf::st_as_sf() %>%
  sf::st_union()
af_lim

# import data -------------------------------------------------------------

# import ----
ti_br <- sf::st_read("01_data/06_indigenous_territories/indigenous_territories_brazil_wgs84_geo.shp") %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(af_lim)) %>%
  sf::st_intersection(af_lim) %>%
  tibble::rowid_to_column(var = "id") %>%
  dplyr::select(id) %>%
  sf::st_make_valid() %>%
  terra::vect()

rgrass::write_VECT(x = ti_br,
                   vname = "indigenous_territories_brazil",
                   flags = c("overwrite", "quiet"))

ti_py <- sf::st_read("01_data/06_indigenous_territories/indigenous_territories_paraguay_wgs84_geo.shp") %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(af_lim)) %>%
  sf::st_intersection(af_lim) %>%
  tibble::rowid_to_column(var = "id") %>%
  dplyr::select(id) %>%
  sf::st_make_valid() %>%
  terra::vect()
ti_py

rgrass::write_VECT(x = ti_py,
                   vname = "indigenous_territories_paraguay",
                   flags = c("overwrite", "quiet"))

rgrass::execGRASS(cmd = "v.patch",
                  flags = c("overwrite", "quiet"),
                  input = "indigenous_territories_brazil,indigenous_territories_paraguay",
                  output = "indigenous_territories")

# rasterize ---------------------------------------------------------------

# region
rgrass::execGRASS(cmd = "g.region", flags = c("a", "p"), vector = "af_lim", res = "30")

# mask
rgrass::execGRASS(cmd = "r.mask", vector = "af_lim")

# rasterize
rgrass::execGRASS(cmd = "v.to.rast",
                  flags = c("overwrite"),
                  input = "indigenous_territories",
                  output = "indigenous_territories_null",
                  use = "val")

# zero
rgrass::execGRASS(cmd = "r.mapcalc",
                  flags = c("overwrite"),
                  expression = "indigenous_territories=if(isnull(indigenous_territories_null), 0, 1)")

# export ------------------------------------------------------------------

# export
rgrass::execGRASS(cmd = "r.out.gdal",
                  flags = "overwrite",
                  input = "indigenous_territories_null",
                  output = "01_data/06_indigenous_territories/indigenous_territories_sirgas2000_albers_brazil.tif",
                  createopt = "TFW=TRUE,COMPRESS=DEFLATE,BIGTIFF=YES")

# area
r <- terra::rast("01_data/06_indigenous_territories/indigenous_territories_sirgas2000_albers_brazil.tif")
rf <- terra::freq(r)
rf[3]*900/1e4
(rf[3]*900/1e4)/162742129*100

# end ---------------------------------------------------------------------
