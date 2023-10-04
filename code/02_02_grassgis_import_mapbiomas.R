#' ----
#' title: atlantic forest spatiotemporal dynamic - import mapbiomas 1986-2020
#' author: mauricio vancine
#' date: 2022-10-27
#' operational system: gnu/linux - ubuntu - pop_os
#' ----

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(rgrass)
library(sf)

# grass wgs84 geodesic ----------------------------------------------------

# years
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
years

# connect
rgrass::initGRASS(gisBase = system("grass --config path", inter = TRUE),
                  gisDbase = "01_data/00_grassdb",
                  location = "wgs84_geodesic",
                  mapset = "PERMANENT",
                  override = TRUE)

# import limit
rgrass::execGRASS("v.in.ogr",
                  flags = c("overwrite"),
                  input = "01_data/01_limits/01_af_limit/ma_limite_integrador_muylaert_et_al_2018_wgs84_geodesic_v1_2_0.shp",
                  output = "af_lim")

# import mapbiomas brazil
for(i in years){

  print(i)
  rgrass::execGRASS("r.in.gdal",
                    flags = "overwrite",
                    input = paste0("01_data/02_mapbiomas/00_raw/brasil_coverage_", i, ".tif"),
                    output = paste0("mapbiomas_brazil_", i))

}

# import mapbiomas trinacional
for(i in years){

  print(i)
  rgrass::execGRASS("r.in.gdal",
                    flags = "overwrite",
                    input = paste0("01_data/02_mapbiomas/00_raw/af_trinacional_", i, "-0000065536-0000000000.tif"),
                    output = paste0("mapbiomas_af_trinacional_", i))

}

# merge mapbiomas brazil and trinacional
for(i in years){

  # information
  print(i)

  # region
  rgrass::execGRASS(cmd = "g.region", flags = c("a", "p"), res = "00:00:01",
                    n = "-21.83", s = "-29.04", e = "-53.52", w = "-58.33")

  # mapcalc
  rgrass::execGRASS(cmd = "r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_", i, "_e = if(mapbiomas_brazil_", i, "> 0, null(), 1)"))

  rgrass::execGRASS(cmd = "r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_af_trinacional_", i, "_e = mapbiomas_af_trinacional_", i, "* mapbiomas_brazil_", i, "_e"))

  # region
  rgrass::execGRASS(cmd = "g.region", flags = c("a", "p"), vector = "af_lim", res = "00:00:01")

  # patch
  rgrass::execGRASS(cmd = "r.patch",
                    flags = "overwrite",
                    input = paste0("mapbiomas_af_trinacional_", i, "_e,mapbiomas_brazil_", i),
                    output = paste0("mapbiomas_brazil_af_trinacional_", i))

}

# export
rgrass::execGRASS(cmd = "g.region", flags = c("a", "p"), raster = "mapbiomas_brazil_af_trinacional_1985", res = "00:00:01")
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim")

for(i in years){

  print(i)
  rgrass::execGRASS(cmd = "r.out.gdal",
                    flags = c("c", "overwrite"),
                    input = paste0("mapbiomas_brazil_af_trinacional_", i),
                    output = paste0("01_data/02_mapbiomas/01_adjusted/mapbiomas_brazil_af_trinacional_af_lim_", i, ".tif"),
                    createopt = "TFW=TRUE,COMPRESS=DEFLATE,BIGTIFF=YES")

  rgrass::execGRASS("r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_forest=if(mapbiomas_brazil_af_trinacional_", i, " == 3 || mapbiomas_brazil_af_trinacional_", i, " == 5 || mapbiomas_brazil_af_trinacional_", i, " == 49, 1, 0)"))
  rgrass::execGRASS(cmd = "r.out.gdal",
                    flags = c("c", "overwrite"),
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_forest"),
                    output = paste0("01_data/02_mapbiomas/01_adjusted/mapbiomas_brazil_af_trinacional_af_lim_", i, "_forest.tif"),
                    createopt = "TFW=TRUE,COMPRESS=DEFLATE,BIGTIFF=YES")

  rgrass::execGRASS("r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_natural=if(mapbiomas_brazil_af_trinacional_", i, " == 3 || mapbiomas_brazil_af_trinacional_", i, " == 4 || mapbiomas_brazil_af_trinacional_", i, " == 5 || mapbiomas_brazil_af_trinacional_", i, " == 11 || mapbiomas_brazil_af_trinacional_", i, " == 12 || mapbiomas_brazil_af_trinacional_", i, " == 13 || mapbiomas_brazil_af_trinacional_", i, " == 32 || mapbiomas_brazil_af_trinacional_", i, " == 49 || mapbiomas_brazil_af_trinacional_", i, " == 50, 1, 0)"))
  rgrass::execGRASS(cmd = "r.out.gdal",
                    flags = c("c", "overwrite"),
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_natural"),
                    output = paste0("01_data/02_mapbiomas/01_adjusted/mapbiomas_brazil_af_trinacional_af_lim_", i, "_natural.tif"),
                    createopt = "TFW=TRUE,COMPRESS=DEFLATE,BIGTIFF=YES")

}

# delete
for(i in years){

  rgrass::execGRASS(cmd = "g.remove",
                    flags = "f",
                    type = "raster",
                    name = paste0("mapbiomas_brazil_", i, ",mapbiomas_brazil_", i, "_e,mapbiomas_af_trinacional_", i, ",mapbiomas_af_trinacional_", i, "_e"))
}


# grass sirgas2000 albers -------------------------------------------------

# years
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
years

# connect
rgrass::initGRASS(gisBase = system("grass --config path", inter = TRUE),
                  gisDbase = "01_data/00_grassdb",
                  location = "sirgas2000_albers",
                  mapset = "PERMANENT",
                  override = TRUE)

# region
rgrass::execGRASS(cmd = "g.region", flags = c("a", "p"), vector = "af_lim", res = "30")

# mask
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim")

# import
for(i in years){

  print(i)
  rgrass::execGRASS(cmd = "r.import",
                    flags = c("overwrite"),
                    input = paste0("01_data/02_mapbiomas/01_adjusted/mapbiomas_brazil_af_trinacional_af_lim_", i, ".tif"),
                    output = paste0("mapbiomas_brazil_af_trinacional_", i))

  rgrass::execGRASS(cmd = "r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_af_lim = mapbiomas_brazil_af_trinacional_", i))

  rgrass::execGRASS(cmd = "g.remove",
                    flags = "f",
                    type = "raster",
                    name = paste0("mapbiomas_brazil_af_trinacional_", i))

}

# end ---------------------------------------------------------------------
