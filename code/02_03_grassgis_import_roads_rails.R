#' ----
#' title: grassdb import roads
#' author: mauricio vancine
#' date: 2022-11-25
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

# read limit
af_lim <- rgrass::read_VECT(vname = "af_lim") %>%
  sf::st_as_sf() %>%
  sf::st_union()
af_lim

# import data -------------------------------------------------------------

## import roads argentina ----
roads_argentina <- sf::st_read("01_data/03_roads_rails/00_raw/roads_argentina.shp") %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(af_lim)) %>%
  terra::vect()
rgrass::write_VECT(x = roads_argentina, vname = "roads_argentina", flags = c("overwrite", "quiet"))

## import roads paraguay ----
roads_paraguay <- sf::st_read("01_data/03_roads_rails/00_raw/roads_paraguay.shp") %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(af_lim)) %>%
  terra::vect()
rgrass::write_VECT(x = roads_paraguay, vname = "roads_paraguay", flags = c("overwrite", "quiet"))

## import roads brazil ----
roads_brazil <- sf::st_read(dsn = "01_data/03_roads_rails/00_raw/bc250_2021_11_18.gpkg",
                            layer = "rod_trecho_rodoviario_l") %>%
  dplyr::filter(revestimento == "Pavimentado",
                operacional == "Sim",
                situacaofisica == "Construída") %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(af_lim)) %>%
  terra::vect()
rgrass::write_VECT(x = roads_brazil, vname = "roads_brazil", flags = c("overwrite", "quiet"))

## import rails argentina ----
rails_argentina <- sf::st_read(dsn = "01_data/03_roads_rails/00_raw/rails_argentina.shp") %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(af_lim)) %>%
  terra::vect()
rgrass::write_VECT(x = rails_argentina, vname = "rails_argentina", flags = c("overwrite", "quiet"))

## import rails brazil ----
rails_brazil <- sf::st_read(dsn = "01_data/03_roads_rails/00_raw/bc250_2021_11_18.gpkg",
                            layer = "fer_trecho_ferroviario_l") %>%
  dplyr::filter(posicaorelativa == "Superfície",
    tipotrechoferrov == "Trecho para trem",
    operacional == "Sim",
    situacaofisica == "Construída") %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(af_lim)) %>%
  terra::vect()
rgrass::write_VECT(x = rails_brazil, vname = "rails_brazil", flags = c("overwrite", "quiet"))

# patch -------------------------------------------------------------------

# patch
rgrass::execGRASS(cmd = "v.patch",
                  input = "roads_argentina,roads_brazil,roads_paraguay",
                  output = "roads_af")

rgrass::execGRASS(cmd = "v.patch",
                  input = "rails_argentina,rails_brazil",
                  output = "rails_af")

rgrass::execGRASS(cmd = "v.patch",
                  input = "roads_argentina,roads_brazil,roads_paraguay,rails_argentina,rails_brazil",
                  output = "roads_rails_af")

# rasterize ---------------------------------------------------------------

# region
rgrass::execGRASS(cmd = "g.region", flags = c("a", "p"), vector = "af_lim", res = "30")

# mask
rgrass::execGRASS(cmd = "r.mask", vector = "af_lim")

# rasterize
rgrass::execGRASS(cmd = "v.to.rast",
                  flags = c("d", "overwrite"),
                  input = "roads_rails_af",
                  output = "roads_rails_af_null",
                  use = "val")
rgrass::execGRASS(cmd = "r.mapcalc",
                  flags = "overwrite",
                  expression = "roads_rails_af = if(isnull(roads_rails_af_null), 0, 1)")

# remove mask
rgrass::execGRASS(cmd = "r.mask", flags = "r")


# distance ----------------------------------------------------------------

# distance
rgrass::execGRASS(cmd = "r.grow.distance",
                  flags = "overwrite",
                  input = "roads_rails_af_null",
                  distance = "roads_rails_af_distance")

rgrass::execGRASS(cmd = "r.mapcalc",
                  flags = "overwrite",
                  express = "roads_rails_af_distance=int(roads_rails_af_distance)")

# export
rgrass::execGRASS(cmd = "r.out.gdal",
                  flags = "overwrite",
                  input = "roads_rails_af_null",
                  output = "roads_rails_af_null.tif",
                  createopt = "TFW=TRUE,COMPRESS=DEFLATE,BIGTIFF=YES")

rgrass::execGRASS(cmd = "r.out.gdal",
                  flags = "overwrite",
                  input = "roads_rails_af",
                  output = "roads_rails_af.tif",
                  createopt = "TFW=TRUE,COMPRESS=DEFLATE,BIGTIFF=YES")

rgrass::execGRASS(cmd = "r.out.gdal",
                  flags = "overwrite",
                  input = "roads_rails_af_distance",
                  output = "roads_rails_af_distance.tif",
                  createopt = "TFW=TRUE,COMPRESS=DEFLATE,BIGTIFF=YES")

# maps --------------------------------------------------------------------

## limit
af_lim <- sf::st_read("01_data/01_limits/01_af_limit/ma_limite_integrador_muylaert_et_al_2018_wgs84_geodesic_v1_2_0.shp")
af_lim

## import roads argentina ----
roads_argentina_af <- sf::st_read("01_data/03_roads_rails/00_raw/roads_argentina.shp") %>%
  sf::st_intersection(af_lim)
roads_argentina_af

## import roads paraguay ----
roads_paraguay_af <- sf::st_read("01_data/03_roads_rails/00_raw/roads_paraguay.shp") %>%
  sf::st_transform(4326) %>%
  sf::st_intersection(af_lim)
roads_paraguay_af

## import roads brazil ----
roads_brazil_af <- sf::st_read(dsn = "01_data/03_roads_rails/00_raw/bc250_2021_11_18.gpkg",
                            layer = "rod_trecho_rodoviario_l") %>%
  dplyr::filter(revestimento == "Pavimentado",
                operacional == "Sim",
                situacaofisica == "Construída") %>%
  sf::st_transform(4326) %>%
  sf::st_intersection(af_lim)
roads_brazil_af

## import rails argentina ----
rails_argentina_af <- sf::st_read(dsn = "01_data/03_roads_rails/00_raw/rails_argentina.shp") %>%
  sf::st_transform(4326) %>%
  sf::st_intersection(af_lim)
rails_argentina_af

## import rails brazil ----
rails_brazil_af <- sf::st_read(dsn = "01_data/03_roads_rails/00_raw/bc250_2021_11_18.gpkg",
                            layer = "fer_trecho_ferroviario_l") %>%
  dplyr::filter(posicaorelativa == "Superfície",
                tipotrechoferrov == "Trecho para trem",
                operacional == "Sim",
                situacaofisica == "Construída") %>%
  sf::st_transform(4326) %>%
  sf::st_intersection(af_lim)
rails_brazil_af

## export ----
sf::st_write(roads_argentina_af, "01_data/03_roads_rails/map/roads_argentina_af.gpkg")
sf::st_write(roads_brazil_af, "01_data/03_roads_rails/map/roads_brazil_af.gpkg")
sf::st_write(roads_paraguay_af, "01_data/03_roads_rails/map/roads_paraguay_af.gpkg")
sf::st_write(rails_argentina_af, "01_data/03_roads_rails/map/rails_argentina_af.gpkg")
sf::st_write(rails_brazil_af, "01_data/03_roads_rails/map/rails_brazil_af.gpkg")

# end ---------------------------------------------------------------------
