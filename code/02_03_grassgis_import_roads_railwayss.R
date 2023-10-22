#' ----
#' title: atlantic forest spatiotemporal dynamics - import roads and railways
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
roads_argentina_national <- sf::st_read("01_data/03_roads_railways/00_raw/roads_argentina_national.shp") %>%
  dplyr::filter(rst == "1") %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(af_lim)) %>%
  terra::vect()
rgrass::write_VECT(x = roads_argentina_national, vname = "roads_argentina_national", flags = c("overwrite", "quiet"))

roads_argentina_provincial <- sf::st_read("01_data/03_roads_railways/00_raw/roads_argentina_provincial.shp") %>%
  dplyr::filter(rst == "1") %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(af_lim)) %>%
  terra::vect()
rgrass::write_VECT(x = roads_argentina_provincial, vname = "roads_argentina_provincial", flags = c("overwrite", "quiet"))

## import roads paraguay ----
roads_paraguay <- sf::st_read("01_data/03_roads_railways/00_raw/roads_paraguay.shp") %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(af_lim)) %>%
  terra::vect()
rgrass::write_VECT(x = roads_paraguay, vname = "roads_paraguay", flags = c("overwrite", "quiet"))

## import roads brazil ----
roads_brazil <- sf::st_read(dsn = "01_data/01_limits/00_raw/2021_bc_250/bc250_2021_11_18.gpkg",
                            layer = "rod_trecho_rodoviario_l") %>%
  dplyr::filter(revestimento == "Pavimentado",
                operacional == "Sim",
                situacaofisica == "Construída") %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(af_lim)) %>%
  terra::vect()
rgrass::write_VECT(x = roads_brazil, vname = "roads_brazil", flags = c("overwrite", "quiet"))

## import railways argentina ----
railways_argentina <- sf::st_read(dsn = "01_data/03_roads_railways/00_raw/railways_argentina.shp") %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(af_lim)) %>%
  terra::vect()
rgrass::write_VECT(x = railways_argentina, vname = "railways_argentina", flags = c("overwrite", "quiet"))

## import railways brazil ----
railways_brazil <- sf::st_read(dsn = "01_data/01_limits/00_raw/2021_bc_250/bc250_2021_11_18.gpkg",
                            layer = "fer_trecho_ferroviario_l") %>%
  dplyr::filter(posicaorelativa == "Superfície",
    tipotrechoferrov == "Trecho para trem",
    operacional == "Sim",
    situacaofisica == "Construída") %>%
  sf::st_transform(sf::st_crs(af_lim)) %>%
  terra::vect()
rgrass::write_VECT(x = railways_brazil, vname = "railways_brazil", flags = c("overwrite", "quiet"))

# patch -------------------------------------------------------------------

# patch
rgrass::execGRASS(cmd = "v.patch",
                  flags = "overwrite",
                  input = "roads_argentina_national,roads_argentina_provincial",
                  output = "roads_argentina")

rgrass::execGRASS(cmd = "v.patch",
                  flags = "overwrite",
                  input = "roads_argentina,roads_brazil,roads_paraguay",
                  output = "roads_ar_br_py")

rgrass::execGRASS(cmd = "v.patch",
                  flags = "overwrite",
                  input = "railways_argentina,railways_brazil",
                  output = "railways_ar_br")

rgrass::execGRASS(cmd = "v.patch",
                  flags = "overwrite",
                  input = "roads_argentina,roads_brazil,roads_paraguay,railways_argentina,railways_brazil",
                  output = "roads_railways_ar_br_py")

# overlay
rgrass::execGRASS(cmd = "v.overlay",
                  flags = "overwrite",
                  ainput = "roads_argentina",
                  binput = "af_lim",
                  output = "roads_argentina_af_lim",
                  operator = "and")

rgrass::execGRASS(cmd = "v.overlay",
                  flags = "overwrite",
                  ainput = "roads_brazil",
                  binput = "af_lim",
                  output = "roads_brazil_af_lim",
                  operator = "and")

rgrass::execGRASS(cmd = "v.overlay",
                  flags = "overwrite",
                  ainput = "roads_paraguay",
                  binput = "af_lim",
                  output = "roads_paraguay_af_lim",
                  operator = "and")

rgrass::execGRASS(cmd = "v.overlay",
                  flags = "overwrite",
                  ainput = "roads_ar_br_py",
                  binput = "af_lim",
                  output = "roads_af_lim",
                  operator = "and")

rgrass::execGRASS(cmd = "v.overlay",
                  flags = "overwrite",
                  ainput = "railways_argentina",
                  binput = "af_lim",
                  output = "railways_argentina_af_lim",
                  operator = "and")

rgrass::execGRASS(cmd = "v.overlay",
                  flags = "overwrite",
                  ainput = "railways_brazil",
                  binput = "af_lim",
                  output = "railways_brazil_af_lim",
                  operator = "and")

rgrass::execGRASS(cmd = "v.overlay",
                  flags = "overwrite",
                  ainput = "railways_ar_br",
                  binput = "af_lim",
                  output = "railways_af_lim",
                  operator = "and")

rgrass::execGRASS(cmd = "v.overlay",
                  flags = "overwrite",
                  ainput = "roads_railways_ar_br_py",
                  binput = "af_lim",
                  output = "roads_railways_af_lim",
                  operator = "and")

# rasterize ---------------------------------------------------------------

# region
rgrass::execGRASS(cmd = "g.region", flags = c("a", "p"), vector = "af_lim", res = "30")

# mask
rgrass::execGRASS(cmd = "r.mask", vector = "af_lim")

# rasterize
rgrass::execGRASS(cmd = "v.to.rast",
                  flags = c("d", "overwrite"),
                  input = "roads_af_lim",
                  output = "roads_af_lim_null",
                  use = "val")
rgrass::execGRASS(cmd = "r.mapcalc",
                  flags = "overwrite",
                  expression = "roads_af_lim_binary = if(isnull(roads_af_lim_null), 0, 1)")

rgrass::execGRASS(cmd = "v.to.rast",
                  flags = c("d", "overwrite"),
                  input = "railways_af_lim",
                  output = "railways_af_lim_null",
                  use = "val")
rgrass::execGRASS(cmd = "r.mapcalc",
                  flags = "overwrite",
                  expression = "railways_af_lim_binary = if(isnull(railways_af_lim_null), 0, 1)")

rgrass::execGRASS(cmd = "v.to.rast",
                  flags = c("d", "overwrite"),
                  input = "roads_railways_af_lim",
                  output = "roads_railways_af_lim_null",
                  use = "val")
rgrass::execGRASS(cmd = "r.mapcalc",
                  flags = "overwrite",
                  expression = "roads_railways_af_lim_binary = if(isnull(roads_railways_af_lim_null), 0, 1)")

# export ------------------------------------------------------------------

# export
rgrass::execGRASS(cmd = "v.out.ogr", input = "roads_argentina_af_lim", output = "01_data/03_roads_railways/roads_argentina_af_lim.gpkg")
rgrass::execGRASS(cmd = "v.out.ogr", input = "roads_brazil_af_lim", output = "01_data/03_roads_railways/roads_brazil_af_lim.gpkg")
rgrass::execGRASS(cmd = "v.out.ogr", input = "roads_paraguay_af_lim", output = "01_data/03_roads_railways/roads_paraguay_af_lim.gpkg")
rgrass::execGRASS(cmd = "v.out.ogr", input = "roads_af_lim", output = "01_data/03_roads_railways/roads_af_lim.gpkg")

rgrass::execGRASS(cmd = "v.out.ogr", input = "railways_argentina_af_lim", output = "01_data/03_roads_railways/railways_argentina_af_lim.gpkg")
rgrass::execGRASS(cmd = "v.out.ogr", input = "railways_brazil_af_lim", output = "01_data/03_roads_railways/railways_brazil_af_lim.gpkg")
rgrass::execGRASS(cmd = "v.out.ogr", input = "railways_af_lim", output = "01_data/03_roads_railways/railways_af_lim.gpkg")

rgrass::execGRASS(cmd = "r.out.gdal", input = "roads_af_lim_binary", output = "01_data/03_roads_railways/roads_af_lim_binary.tif", createopt = "COMPRESS=DEFLATE,TFW=YES")
rgrass::execGRASS(cmd = "r.out.gdal", input = "roads_af_lim_null", output = "01_data/03_roads_railways/roads_af_lim_null.tif", createopt = "COMPRESS=DEFLATE,TFW=YES")

rgrass::execGRASS(cmd = "r.out.gdal", input = "railways_af_lim_binary", output = "01_data/03_roads_railways/railways_af_lim_binary.tif", createopt = "COMPRESS=DEFLATE,TFW=YES")
rgrass::execGRASS(cmd = "r.out.gdal", input = "railways_af_lim_null", output = "01_data/03_roads_railways/railways_af_lim_null.tif", createopt = "COMPRESS=DEFLATE,TFW=YES")

rgrass::execGRASS(cmd = "r.out.gdal", input = "roads_railways_af_lim_binary", output = "01_data/03_roads_railways/roads_railways_af_lim_binary.tif", createopt = "COMPRESS=DEFLATE,TFW=YES")
rgrass::execGRASS(cmd = "r.out.gdal", input = "roads_railways_af_lim_null", output = "01_data/03_roads_railways/roads_railways_af_lim_null.tif", createopt = "COMPRESS=DEFLATE,TFW=YES")

# end ---------------------------------------------------------------------
