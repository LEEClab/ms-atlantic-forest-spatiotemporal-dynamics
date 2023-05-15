#' ----
#' title: grassdb protected areas
#' author: mauricio vancine
#' date: 2022-11-01
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

# import protected areas
pa0 <- sf::st_read("01_data/04_protected_areas/00_raw/WDPA_WDOECM_Nov2022_Public_SA_shp_0/WDPA_WDOECM_Nov2022_Public_SA_shp-polygons.shp") %>%
  dplyr::filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV")) %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(af_lim)) %>%
  sf::st_intersection(af_lim) %>%
  sf::st_make_valid() %>%
  terra::vect()

rgrass::write_VECT(x = pa0,
                   vname = "protected_areas0",
                   flags = c("overwrite", "quiet"))

pa1 <- sf::st_read("01_data/04_protected_areas/00_raw/WDPA_WDOECM_Nov2022_Public_SA_shp_1/WDPA_WDOECM_Nov2022_Public_SA_shp-polygons.shp") %>%
  dplyr::filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV")) %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(af_lim)) %>%
  sf::st_intersection(af_lim) %>%
  sf::st_make_valid() %>%
  terra::vect()

rgrass::write_VECT(x = pa1,
                   vname = "protected_areas1",
                   flags = c("overwrite", "quiet"))

pa2 <- sf::st_read("01_data/04_protected_areas/00_raw/WDPA_WDOECM_Nov2022_Public_SA_shp_2/WDPA_WDOECM_Nov2022_Public_SA_shp-polygons.shp") %>%
  dplyr::filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV")) %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(af_lim)) %>%
  sf::st_intersection(af_lim) %>%
  sf::st_make_valid() %>%
  terra::vect()

rgrass::write_VECT(x = pa2,
                   vname = "protected_areas2",
                   flags = c("overwrite", "quiet"))

# patch
rgrass::execGRASS(cmd = "v.patch",
                  flags = c("overwrite", "quiet"),
                  input = "protected_areas0,protected_areas1,protected_areas2",
                  output = "protected_areas")

# rasterize ---------------------------------------------------------------

# region
rgrass::execGRASS(cmd = "g.region", flags = c("a", "p"), vector = "af_lim", res = "30")

# mask
rgrass::execGRASS(cmd = "r.mask", vector = "af_lim")

# rasterize
rgrass::execGRASS(cmd = "v.to.rast",
                  flags = c("overwrite"),
                  input = "protected_areas",
                  output = "protected_areas_null",
                  use = "val")

# zero
rgrass::execGRASS(cmd = "r.mapcalc",
                  flags = c("overwrite"),
                  expression = "protected_areas=if(isnull(protected_areas_null), 0, 1)")

# export ------------------------------------------------------------------

# export
rgrass::execGRASS(cmd = "r.out.gdal",
                  flags = "overwrite",
                  input = "protected_areas_null",
                  output = "01_data/04_protected_areas/protected_areas_sirgas2000_albers_brazil.tif",
                  createopt = "TFW=TRUE,COMPRESS=DEFLATE,BIGTIFF=YES")

# area
nrow(rbind(pa0, pa1, pa2))

r <- terra::rast("01_data/04_protected_areas/protected_areas_sirgas2000_albers_brazil.tif")
rf <- terra::freq(r)
rf[3]*900/1e4
(rf[3]*900/1e4)/162742129*100

# end ---------------------------------------------------------------------
