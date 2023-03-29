#' ----
#' title: grassdb mapbiomas 1985-2021
#' author: mauricio vancine
#' date: 2022-10-27
#' ----

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(rgrass)
library(sf)

# grass ----------------------------------------------------------------

# connect
rgrass::initGRASS(gisBase = system("grass --config path", inter = TRUE),
                  gisDbase = "01_data/00_grassdb",
                  location = "sirgas2000_albers",
                  mapset = "PERMANENT",
                  override = TRUE)

# region
rgrass::execGRASS(cmd = "g.region", flags = c("a", "p"), vector = "af_lim", res = "30")

# read limit
af_lim <- rgrass::read_VECT(vname = "af_lim") %>%
  sf::st_as_sf() %>%
  sf::st_union()
af_lim

# import data -------------------------------------------------------------

## import urban argentina ----
urban_argentina <- sf::st_read("01_data/04_urban/00_raw/areas_de_asentamientos_y_edificios_020105.shp") %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(af_lim)) %>%
  terra::vect()
rgrass::write_VECT(x = urban_argentina, vname = "urban_argentina", flags = c("overwrite", "quiet"))

## import urban paraguay ----
urban_paraguay <- sf::st_read("01_data/04_urban/00_raw/Manzanas_Paraguay.shp") %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(af_lim)) %>%
  terra::vect()
rgrass::write_VECT(x = urban_paraguay, vname = "urban_paraguay", flags = c("overwrite", "quiet"))


# buffer ------------------------------------------------------------------

# buffer
rgrass::execGRASS(cmd = "v.buffer",
                  input = "urban_paraguay",
                  output = "urban_paraguay_buffer",
                  distance = 30)

# patch -------------------------------------------------------------------

# patch
rgrass::execGRASS(cmd = "v.patch",
                  flags = "overwrite",
                  input = "urban_argentina,urban_paraguay_buffer",
                  output = "urban_argentina_paraguay")

# rasterize ---------------------------------------------------------------

# rasterize
rgrass::execGRASS(cmd = "v.to.rast",
                  flags = c("d", "overwrite"),
                  input = "urban_argentina_paraguay",
                  output = "urban_argentina_paraguay_null",
                  use = "val")

# binarize
rgrass::execGRASS(cmd = "r.mapcalc",
                  flags = "overwrite",
                  expression = "urban_argentina_paraguay = if(isnull(urban_argentina_paraguay_null), 0, 1)")

# mapcalc -----------------------------------------------------------------

# selection classes
for(i in c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)){

  print(i)

  rgrass::execGRASS("r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_af_lim_urban_br=if(mapbiomas_brazil_af_trinacional_", i, "_af_lim == 24, 1, 0)"))

}

# patch -----------------------------------------------------------------

# patch
for(i in c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)){

  print(i)

  rgrass::execGRASS("r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_af_lim_urban=mapbiomas_brazil_af_trinacional_", i, "_af_lim_urban_br + urban_argentina_paraguay"))

}

# patch
for(i in c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)){

  print(i)

  rgrass::execGRASS("r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_af_lim_urban_null=if(mapbiomas_brazil_af_trinacional_", i, "_af_lim_urban == 1, 1, null())"))

}

# end ---------------------------------------------------------------------
