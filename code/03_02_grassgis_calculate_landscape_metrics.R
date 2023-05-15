#' ----
#' title: grassdb landscape metrics
#' author: mauricio vancine
#' date: 2023-03-06
#' operational system: gnu/linux - ubuntu - pop_os
#' ----

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(rgrass)

# options
options(scipen = 1000)

# connect grass -----------------------------------------------------------

# connect
rgrass::initGRASS(gisBase = system("grass --config path", inter = TRUE),
                  gisDbase = "01_data/00_grassdb",
                  location = "sirgas2000_albers",
                  mapset = "PERMANENT",
                  override = TRUE)

# region ------------------------------------------------------------------

# region
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim", res = "30")
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim")

# limit af raster
rgrass::execGRASS(cmd = "v.to.rast",
                  flags = "overwrite",
                  input = "af_lim",
                  output = "af_lim",
                  use = "val")

# list maps
files_vectors <- rgrass::stringexecGRASS("g.list type='vector'", intern=TRUE)
files_vectors

files_rasters <- rgrass::stringexecGRASS("g.list type='raster'", intern=TRUE)
files_rasters

# years ----
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
years

# calculation ---------------------------------------------------------------

# 0 limits, roads and protected areas ----

# region
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim", res = "30")

# af lim
rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "af_lim,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_af_lim_area.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "af_lim_br,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_af_lim_br_area.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "af_lim_ar,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_af_lim_ar_area.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "af_lim_py,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_af_lim_py_area.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "af_lim_lawaf2006,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_af_lim_lawaf2006_area.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "af_lim_dasilvacasteleti2003,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_af_lim_dasilvacasteleti2003_area.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "af_lim_ibge2004,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_af_lim_ibge2004_area.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "af_lim_ibge2019,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_af_lim_ibge2019_area.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "af_lim_ecoregions2017,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_af_lim_ecoregions2017_area.csv")

# roads and rails
rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "roads_rails_af,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_roads_rails_area.csv")

# protected areas
rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "protected_areas,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_protected_area.csv")

# indigenous territory
rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "indigenous_territory,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_indigenous_territory_area.csv")

# 1 habitat cover ----

# years
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
years

# region
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim", res = "30")
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim")

## scenarios ----
scenarios_habitat_cover <- crossing(years,
                                    veg = c("_af_lim_forest_classes", "_af_lim_natural_classes"),
                                    road = c("", "_roads_rails"))
scenarios_habitat_cover <- apply(scenarios_habitat_cover, 1, paste0, collapse = "")
scenarios_habitat_cover

## calculation total ----
for(i in scenarios_habitat_cover){

  print(i)

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "p", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i),
                    output = paste0("02_results/01_mapbiomas_brazil_af_trinacional_", i, "_habitat_cover.csv"))

}

## calculation br ----
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim_br")
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim_br")

for(i in scenarios_habitat_cover[31]){

  print(i)

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "p", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i),
                    output = paste0("02_results/01_mapbiomas_brazil_af_trinacional_", i, "_habitat_cover_br.csv"))

}

## calculation ar ----
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim_ar")
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim_ar")

for(i in scenarios_habitat_cover[31]){

  print(i)

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "p", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i),
                    output = paste0("02_results/01_mapbiomas_brazil_af_trinacional_", i, "_habitat_cover_ar.csv"))

}

## calculation py ----
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim_py")
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim_py")

for(i in scenarios_habitat_cover[31]){

  print(i)

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "p", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i),
                    output = paste0("02_results/01_mapbiomas_brazil_af_trinacional_", i, "_habitat_cover_py.csv"))

}

## calculation limits ----
for(i in c("af_lim_ibge2004", "af_lim_lawaf2006", "af_lim_ribeiro_etal2009",
           "af_lim_ecoregions2017", "af_lim_ibge2019")){

  print(i)
  rgrass::execGRASS("g.region", flags = c("a", "p"), vector = i, res = "30")
  rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = i)

  rgrass::execGRASS(cmd = "v.to.rast", flags = "overwrite", use = "val", input = i, output = i)
  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "p", "overwrite"),
                    separator = ",",
                    input = i,
                    output = paste0("02_results/00_", i, ".csv"))

  for(j in c("forest", "natural", "forest_roads_rails", "natural_roads_rails")){

    rgrass::execGRASS(cmd = "r.stats",
                      flags = c("a", "c", "n", "p", "overwrite"),
                      separator = ",",
                      input = paste0("mapbiomas_brazil_af_trinacional_2020_af_lim_", j),
                      output = paste0("02_results/01_mapbiomas_brazil_af_trinacional_2020_", i, "_", j,  "_habitat_cover.csv"))

  }

}

# 2 number of patch and size distribution ----

# years
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
years

# region
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim", res = "30")
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim")

# list files
files_rasters_pid <- rgrass::stringexecGRASS("g.list type='raster' pattern='*pid'", intern=TRUE)
files_rasters_pid

files_rasters_area <- rgrass::stringexecGRASS("g.list type='raster' pattern='*area*'", intern=TRUE)
files_rasters_area

## scenarios ----
scenarios_area <- crossing(years,
                           veg = c("_af_lim_forest", "_af_lim_natural"),
                           road = c("", "_roads_rails"))
scenarios_area <- apply(scenarios_area, 1, paste0, collapse = "")
scenarios_area

# sudo grass --tmp-location EPSG:4326 --exec g.extension -s extension=r.area operation=add

# region
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim", res = "30")
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim")

## calculation ----
for(i in scenarios_area){

  print(i)

  rgrass::execGRASS(cmd = "r.mapcalc",
                    flags = c("overwrite"),
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_null=if(mapbiomas_brazil_af_trinacional_", i, "==0, null(), 1)"))

  rgrass::execGRASS(cmd = "r.clump",
                    flags = c("d", "quiet", "overwrite"),
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_null"),
                    output = paste0("mapbiomas_brazil_af_trinacional_", i, "_pid"))

  rgrass::execGRASS(cmd = "r.area",
                    flags = c("overwrite"),
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_pid"),
                    output = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_ncell"))

  rgrass::execGRASS(cmd = "r.mapcalc",
                    flags = c("overwrite"),
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_ha = mapbiomas_brazil_af_trinacional_", i, "_area_ncell * 0.09"))

  # area
  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_pid"),
                    output = paste0("02_results/02_mapbiomas_brazil_af_trinacional_", i, "_pid_area.csv"))

  # area raster
  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "n", "overwrite"),
                    separator = "=",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_pid"),
                    output = paste0("02_results/02_mapbiomas_brazil_af_trinacional_", i, "_area.txt"))

  readr::read_delim(paste0("02_results/02_mapbiomas_brazil_af_trinacional_", i, "_area.txt"),
                    delim = "=", col_names = c("pid", "area"), col_types = readr::cols()) %>%
    dplyr::mutate(area = ifelse(area/1e4 < 1, 1, round(area/1e4, digits = 0))) %>%
    dplyr::slice(-1) %>%
    readr::write_delim(paste0("02_results/02_mapbiomas_brazil_af_trinacional_", i, "_area_larger1ha.txt"),
                       delim = "=", col_names = FALSE)

  rgrass::execGRASS(cmd = "r.reclass",
                    flags = c("overwrite"),
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_pid"),
                    output = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger1ha"),
                    rules = paste0("02_results/02_mapbiomas_brazil_af_trinacional_", i, "_area_larger1ha.txt"))

  rgrass::execGRASS(cmd = "r.reclass",
                    flags = c("overwrite"),
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_pid"),
                    output = paste0("mapbiomas_brazil_af_trinacional_", i, "_area"),
                    rules = paste0("02_results/02_mapbiomas_brazil_af_trinacional_", i, "_area.txt"))

  unlink(paste0("02_results/02_mapbiomas_brazil_af_trinacional_", i, "_area.txt"))
  unlink(paste0("02_results/02_mapbiomas_brazil_af_trinacional_", i, "_area_larger1ha.txt"))

  # export
  rgrass::execGRASS(cmd = "r.out.gdal",
                    flags = "overwrite",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger1ha"),
                    output = paste0("01_data/02_mapbiomas/02_albers/area/mapbiomas_brazil_af_trinacional_", i, "_area.tif"),
                    createopt = "TFW=TRUE,COMPRESS=DEFLATE,BIGTIFF=YES")

}

## calculation limits ----
for(i in c("af_lim_ibge2004", "af_lim_lawaf2006", "af_lim_dasilvacasteleti2003",
           "af_lim_ecoregions2017", "af_lim_ibge2019")){

  print(i)
  rgrass::execGRASS("g.region", flags = c("a", "p"), vector = i, res = "30")
  rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = i)

  for(j in c("forest", "natural", "forest_roads_rails", "natural_roads_rails")){

    # area
    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "overwrite"),
                      input = paste0("mapbiomas_brazil_af_trinacional_2020_af_lim_", j, "_null"),
                      output = paste0("mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_pid"))

    rgrass::execGRASS(cmd = "r.stats",
                      flags = c("a", "c", "n", "overwrite"),
                      separator = ",",
                      input = paste0("mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_pid"),
                      output = paste0("02_results/02_mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_pid_area.csv"))


    # area raster
    rgrass::execGRASS(cmd = "r.stats",
                      flags = c("a", "n", "overwrite"),
                      separator = "=",
                      input = paste0("mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_pid"),
                      output = paste0("02_results/02_mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_area.txt"))

    readr::read_delim(paste0("02_results/02_mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_area.txt"),
                      delim = "=", col_names = c("pid", "area"), col_types = readr::cols()) %>%
      dplyr::mutate(area = ifelse(area/1e4 < 1, 1, round(area/1e4, digits = 0))) %>%
      dplyr::slice(-1) %>%
      readr::write_delim(paste0("02_results/02_mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_area_larger1ha.txt"),
                         delim = "=", col_names = FALSE)

    rgrass::execGRASS(cmd = "r.reclass",
                      flags = c("overwrite"),
                      input = paste0("mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_pid"),
                      output = paste0("mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_area_larger1ha"),
                      rules = paste0("02_results/02_mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_area_larger1ha.txt"))

    rgrass::execGRASS(cmd = "r.reclass",
                      flags = c("overwrite"),
                      input = paste0("mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_pid"),
                      output = paste0("mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_area"),
                      rules = paste0("02_results/02_mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_area.txt"))

    unlink(paste0("02_results/02_mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_area.txt"))
    unlink(paste0("02_results/02_mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_area_larger1ha.txt"))

    # export
    rgrass::execGRASS(cmd = "r.out.gdal",
                      flags = "overwrite",
                      input = paste0("mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_area_larger1ha"),
                      output = paste0("01_data/02_mapbiomas/02_albers/area/mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_area_larger1ha.tif"),
                      createopt = "TFW=TRUE,COMPRESS=DEFLATE,BIGTIFF=YES")

  }

}

## calculation new patches ----

# region
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim", res = "30")
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim")

# scenarios
scenarios_new_patches <- crossing(years[c(1, 5, 8)],
                                  veg = c("_af_lim_forest", "_af_lim_natural"),
                                  road = c("", "_roads_rails"))
scenarios_new_patches <- apply(scenarios_new_patches, 1, paste0, collapse = "")
scenarios_new_patches

# subtraction
for(i in scenarios_new_patches){

  # subtraction 2005-1986
  rgrass::execGRASS("r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_2005_1986_af_lim_forest=mapbiomas_brazil_af_trinacional_2005_af_lim_forest - mapbiomas_brazil_af_trinacional_1986_af_lim_forest"))

}

# subtraction 2005-1986
rgrass::execGRASS("r.mapcalc",
                  flags = "overwrite",
                  expression = paste0("mapbiomas_brazil_af_trinacional_2005_1986_af_lim_forest=mapbiomas_brazil_af_trinacional_2005_af_lim_forest - mapbiomas_brazil_af_trinacional_1986_af_lim_forest"))


# 3  core and edge area ----

# years
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
years

# region
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim", res = "30")

# list files
files_rasters_edge_area <- rgrass::stringexecGRASS("g.list type='raster' pattern='*edge_distance_inside'", intern=TRUE)
files_rasters_edge_area

## scenarios ----
scenarios_edge_area <- crossing(years,
                                veg = c("_af_lim_forest", "_af_lim_natural"),
                                road = c("", "_roads_rails"))
scenarios_edge_area <- apply(scenarios_edge_area, 1, paste0, collapse = "")
scenarios_edge_area

## calculation ----
for(i in scenarios_edge_area){

  print(i)

  rgrass::execGRASS("r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_matrix=if(mapbiomas_brazil_af_trinacional_", i, "==1,0,1)"))

  rgrass::execGRASS("r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_matrix_null=if(mapbiomas_brazil_af_trinacional_", i, "==1,null(),1)"))

  rgrass::execGRASS(cmd = "r.grow.distance",
                    flags = "overwrite",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_matrix_null"),
                    distance = paste0("mapbiomas_brazil_af_trinacional_", i, "_matrix_distance"))

  rgrass::execGRASS(cmd = "r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_matrix_distance=int(mapbiomas_brazil_af_trinacional_", i, "_matrix_distance)"))

  rgrass::execGRASS(cmd = "r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_edge_distance_inside=int(mapbiomas_brazil_af_trinacional_", i, "_matrix_distance)"))

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",mapbiomas_brazil_af_trinacional_", i, "_edge_distance_inside"),
                    output = paste0("02_results/03_mapbiomas_brazil_af_trinacional_", i, "_edge_area.csv"))

}


# 4 functional connectivity ----

# years
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
years

# region
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim", res = "30")

# list files
files_rasters_confun_area <- rgrass::stringexecGRASS("g.list type='raster' pattern='*funarea*'", intern=TRUE)
files_rasters_confun_area

## scenarios ----
scenarios_confun_area <- crossing(years,
                                  veg = c("_af_lim_forest", "_af_lim_natural"),
                                  road = c("", "_roads_rails"))
scenarios_confun_area <- apply(scenarios_confun_area, 1, paste0, collapse = "")
scenarios_confun_area

## calculation ----
# calculate
for(i in scenarios_confun_area){

  print(i)

  rgrass::execGRASS(cmd = "r.grow.distance",
                    flags = "overwrite",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_null"),
                    distance = paste0("mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside"))

  rgrass::execGRASS(cmd = "r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside=int(mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside)"))

  for(j in c(60, 120, 180, 240, 300, 600, 900, 1200, 1500)){

    print(j)

    name_j <- ifelse(j < 100, paste0("00", j), ifelse(j < 1000, paste0("0", j), j))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_under", name_j, "=if(mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside>0 && mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside<=", j/2, ",1, 0)"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "=mapbiomas_brazil_af_trinacional_", i, " + mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_under", name_j))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite"),
                      expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_null=if(mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "==0, null(), 1)"))

    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "quiet", "overwrite"),
                      input = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_null"),
                      output = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_pid"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite"),
                      expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_pid_adj=mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_pid * mapbiomas_brazil_af_trinacional_", i, "_null"))

    rgrass::execGRASS(cmd = "r.stats",
                      flags = c("a", "c", "n", "overwrite"),
                      separator = ",",
                      input = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_pid_adj"),
                      output = paste0("02_results/04_mapbiomas_brazil_af_trinacional_", i, "_confun_area", name_j, ".csv"))

    # area raster
    rgrass::execGRASS(cmd = "r.stats",
                      flags = c("a", "n", "overwrite"),
                      separator = "=",
                      input = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_pid_adj"),
                      output = paste0("02_results/04_mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_area.txt"))

    readr::read_delim(paste0("02_results/04_mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_area.txt"),
                      delim = "=", col_names = c("pid", "area"), col_types = readr::cols()) %>%
      dplyr::mutate(area = ifelse(area/1e4 < 1, 1, round(area/1e4, digits = 0))) %>%
      dplyr::slice(-1) %>%
      readr::write_delim(paste0("02_results/04_mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_area_larger1ha.txt"),
                         delim = "=", col_names = FALSE)

    rgrass::execGRASS(cmd = "r.reclass",
                      flags = c("overwrite"),
                      input = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_pid_adj"),
                      output = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "area_larger1ha"),
                      rules = paste0("02_results/04_mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_area_larger1ha.txt"))

    rgrass::execGRASS(cmd = "r.reclass",
                      flags = c("overwrite"),
                      input = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_pid_adj"),
                      output = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_area"),
                      rules = paste0("02_results/04_mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_area.txt"))

    unlink(paste0("02_results/04_mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_area.txt"))
    unlink(paste0("02_results/04_mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_area_larger1ha.txt"))

    # export
    rgrass::execGRASS(cmd = "r.out.gdal",
                      flags = "overwrite",
                      input = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "area_larger1ha"),
                      output = paste0("01_data/02_mapbiomas/02_albers/area/mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_area_larger1ha.tif"),
                      createopt = "TFW=TRUE,COMPRESS=DEFLATE,BIGTIFF=YES")

  }

}

# 5 mean isolation ----

# years
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
years

# region
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim", res = "30")

# list files
files_rasters_isolation <- rgrass::stringexecGRASS("g.list type='raster' pattern='*distance_outside_area_larger*'", intern=TRUE)
files_rasters_isolation

## scenarios ----
scenarios_isolation <- crossing(years,
                                veg = c("_af_lim_forest", "_af_lim_natural"),
                                road = c("_roads_rails"))
scenarios_isolation <- apply(scenarios_isolation, 1, paste0, collapse = "")
scenarios_isolation

## calculation ----
for(i in scenarios_isolation){

  print(i)

  rgrass::execGRASS("r.mapcalc", flags = "overwrite", expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger0000_nonzero=if(mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside>0, mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside, null())"))
  rgrass::execGRASS("r.univar", flags = c("t", "overwrite"), map = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger0000_nonzero"), output = paste0("02_results/05_mapbiomas_brazil_af_trinacional_", i, "_isolation_larger0000.csv"),  separator=",")
  rgrass::execGRASS("r.quantile", flags = c("overwrite"), input = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger0000_nonzero"), percentiles = c(10, 25, 50, 75, 90), file = paste0("02_results/05_mapbiomas_brazil_af_trinacional_", i, "_isolation_larger0000", "_quantile.csv"))

  for(j in c(50, 100, 150, 200, 250, 350, 500)){

    name_j <- ifelse(j < 100, paste0("00", j), ifelse(j < 1000, paste0("0", j), j))

    rgrass::execGRASS("r.mapcalc", flags = "overwrite", expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger", name_j, "=if(mapbiomas_brazil_af_trinacional_", i, "_area_ha>=", j, ",1,null())"))
    rgrass::execGRASS("r.grow.distance", flags = "overwrite", input = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger", name_j), distance = paste0("mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_area_larger", name_j))
    rgrass::execGRASS("r.mapcalc", flags = "overwrite", expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_area_larger", name_j, "=int(mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_area_larger", name_j, ")"))
    rgrass::execGRASS("r.mapcalc", flags = "overwrite", expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_area_larger", name_j, "_nonzero=if(mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_area_larger", name_j, " > 0, mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_area_larger", name_j, ", null())"))
    rgrass::execGRASS("r.univar", flags = c("t", "overwrite"), map = paste0("mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_area_larger", name_j, "_nonzero"), output = paste0("02_results/05_mapbiomas_brazil_af_trinacional_", i, "_isolation_larger", name_j, ".csv"),  separator = ",")
    rgrass::execGRASS("r.quantile", flags = c("overwrite"), input = paste0("mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_area_larger", name_j, "_nonzero"), percentiles = c(10, 25, 50, 75, 90), file = paste0("02_results/05_mapbiomas_brazil_af_trinacional_", i, "_isolation_larger", name_j, "_quantile.csv"))

  }

}


# 6 protected areas and indigenous territory proximity ----

# region
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim", res = "30")

## protected areas ----

# list files
files_rasters_protected_areas <- rgrass::stringexecGRASS("g.list type='raster' pattern='*protected_areas*'", intern=TRUE)
files_rasters_protected_areas

### distance ----
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim")

rgrass::execGRASS(cmd = "r.grow.distance",
                  flags = "overwrite",
                  input = "protected_areas_null",
                  distance = "protected_areas_distance")

rgrass::execGRASS(cmd = "r.mapcalc",
                  flags = "overwrite",
                  expression = "protected_areas_distance=int(protected_areas_distance)")

### scenarios ----
scenarios_pa <- crossing(2020,
                         veg = c("_af_lim_forest", "_af_lim_natural"),
                         road = c("", "_roads_rails"))
scenarios_pa <- apply(scenarios_pa, 1, paste0, collapse = "")
scenarios_pa

### calculation ----
for(i in scenarios_pa){

  print(i)

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",protected_areas_distance"),
                    output = paste0("02_results/06_mapbiomas_brazil_af_trinacional_", i, "_protected_areas.csv"))

}

### classes scenarios ----
scenarios_pa_classes <- crossing(2020,
                                 veg = c("_af_lim_forest_classes", "_af_lim_natural_classes"),
                                 road = c("", "_roads_rails"))
scenarios_pa_classes <- apply(scenarios_pa_classes, 1, paste0, collapse = "")
scenarios_pa_classes

### classes calculation ----
for(i in scenarios_pa_classes){

  print(i)

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",protected_areas_distance"),
                    output = paste0("02_results/06_mapbiomas_brazil_af_trinacional_", i, "_protected_areas_classes.csv"))

}


## indigenous territory ----

# list files
files_rasters_indigenous_territory <- rgrass::stringexecGRASS("g.list type='raster' pattern='*indigenous_territory*'", intern=TRUE)
files_rasters_indigenous_territory

### distance ----
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim")

rgrass::execGRASS(cmd = "r.grow.distance",
                  flags = "overwrite",
                  input = "indigenous_territory_null",
                  distance = "indigenous_territory_distance")

rgrass::execGRASS(cmd = "r.mapcalc",
                  flags = "overwrite",
                  expression = "indigenous_territory_distance=int(indigenous_territory_distance)")

### scenarios ----
scenarios_it <- crossing(2020,
                         veg = c("_af_lim_forest", "_af_lim_natural"),
                         road = c("", "_roads_rails"))
scenarios_it <- apply(scenarios_it, 1, paste0, collapse = "")
scenarios_it

### calculation ----
for(i in scenarios_it){

  print(i)

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",indigenous_territory_distance"),
                    output = paste0("02_results/06_mapbiomas_brazil_af_trinacional_", i, "_indigenous_territory.csv"))

}

### classes scenarios
scenarios_it_classes <- crossing(2020,
                                 veg = c("_af_lim_forest_classes", "_af_lim_natural_classes"),
                                 road = c("", "_roads_rails"))
scenarios_it_classes <- apply(scenarios_it_classes, 1, paste0, collapse = "")
scenarios_it_classes

### classes calculation
for(i in scenarios_it_classes){

  print(i)

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",indigenous_territory_distance"),
                    output = paste0("02_results/06_mapbiomas_brazil_af_trinacional_", i, "_indigenous_territory_classes.csv"))

}


# 7 landscape and topographic index ----

# years
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
years

# region
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim", res = "30")

# list files
files_rasters_fabdem <- rgrass::stringexecGRASS("g.list type='raster' pattern='*fabdem*'", intern=TRUE)
files_rasters_fabdem

## total distribution ----
rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "A", "overwrite"),
                  separator = ",",
                  input = "fabdem_af_lim",
                  output = "02_results/07_fabdem_elevation_original.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "i", "overwrite"),
                  separator = ",",
                  input = "fabdem_af_lim_slope",
                  output = "02_results/07_fabdem_slope_original.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "i", "overwrite"),
                  separator = ",",
                  input = "fabdem_af_lim_aspect",
                  output = "02_results/07_fabdem_aspect_original.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "A", "overwrite"),
                  separator = ",",
                  input = "fabdem_af_lim_pcurvature",
                  output = "02_results/07_fabdem_pcurvature_original.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "A", "overwrite"),
                  separator = ",",
                  input = "fabdem_af_lim_tcurvature",
                  output = "02_results/07_fabdem_tcurvature_original.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "overwrite"),
                  separator = ",",
                  input = "fabdem_af_lim_geomorph",
                  output = "02_results/07_fabdem_geomorph_original.csv")

## scenarios ----
scenarios_topographic <- crossing(years,
                                  veg = c("_af_lim_forest", "_af_lim_natural"),
                                  road = c("", "_roads_rails"))
scenarios_topographic <- apply(scenarios_topographic, 1, paste0, collapse = "")
scenarios_topographic

# calculation cover
for(i in scenarios_topographic[23:32]){

  print(i)

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "A", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",fabdem_af_lim"),
                    output = paste0("02_results/07_mapbiomas_brazil_af_trinacional_", i, "_elevation.csv"))

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "i", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",fabdem_af_lim_slope"),
                    output = paste0("02_results/07_mapbiomas_brazil_af_trinacional_", i, "_slope.csv"))

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "i", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",fabdem_af_lim_aspect"),
                    output = paste0("02_results/07_mapbiomas_brazil_af_trinacional_", i, "_aspect.csv"))

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "A", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",fabdem_af_lim_pcurvature"),
                    output = paste0("02_results/07_mapbiomas_brazil_af_trinacional_", i, "_pcurvature.csv"))

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "A", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",fabdem_af_lim_tcurvature"),
                    output = paste0("02_results/07_mapbiomas_brazil_af_trinacional_", i, "_tcurvature.csv"))

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",fabdem_af_lim_geomorph"),
                    output = paste0("02_results/07_mapbiomas_brazil_af_trinacional_", i, "_geomorph.csv"))

}

# 8 project rasters ----------------------------------------------------

# connect
rgrass::initGRASS(gisBase = system("grass --config path", inter = TRUE),
                  gisDbase = "01_data/00_grassdb",
                  location = "wgs84_geodesic",
                  mapset = "PERMANENT",
                  override = TRUE)

# region
rgrass::execGRASS("g.region", flags = "p")

# project
rgrass::execGRASS(cmd = "r.proj",
                  flags = "overwrite",
                  location = "sirgas2000_albers",
                  mapset = "PERMANENT",
                  input = "mapbiomas_brazil_af_trinacional_2020_af_lim_forest_roads_rails_area_larger1ha")

# export
rgrass::execGRASS(cmd = "r.out.gdal",
                  flags = "overwrite",
                  input = "mapbiomas_brazil_af_trinacional_2020_af_lim_forest_roads_rails_area_larger1ha",
                  output = "01_data/02_mapbiomas/03_geo/mapbiomas_brazil_af_trinacional_2020_af_lim_forest_roads_rails_area.tif",
                  createopt = "COMPRESS=DEFLATE")

# end ---------------------------------------------------------------------
