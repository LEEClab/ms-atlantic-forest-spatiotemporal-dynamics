#' ----
#' title: grassdb landscape metrics
#' author: mauricio vancine
#' date: 2022-11-23
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
files_vectors <- attributes(rgrass::execGRASS(cmd = "g.list", type = "vector"))$resOut
files_vectors

files_rasters <- attributes(rgrass::execGRASS(cmd = "g.list", type = "raster"))$resOut
files_rasters

# rename
files_rename <- attributes(rgrass::execGRASS(cmd = "g.list", type = "raster", pattern = "*lim_af*"))$resOut
files_rename

for(i in files_rename){

  name <- sub("lim_af", "af_lim", i)

  rgrass::execGRASS(cmd = "g.rename", raster = paste0(i, ",", name))

}

# calculate ---------------------------------------------------------------

# 0 limits, roads and protected areas ----

# af lim
rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "af_lim",
                  output = "02_results/00_af_lim.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "af_lim_br",
                  output = "02_results/00_af_lim_br.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "af_lim_ar",
                  output = "02_results/00_af_lim_ar.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "af_lim_py",
                  output = "02_results/00_af_lim_py.csv")


# roads and rails
rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "roads_rails_af",
                  output = "02_results/00_roads_rails_af.csv")

# protected areas
rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "protected_areas",
                  output = "02_results/00_protected_areas.csv")

# years ----
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)[c(1, 8)]
years

# 1 habitat cover ----
# scenarios
scenarios_habitat_cover <- crossing(years,
                                    veg = c("_af_lim_forest_classes", "_af_lim_natural_classes"),
                                    road = c("", "_roads_rails"))
scenarios_habitat_cover <- apply(scenarios_habitat_cover, 1, paste0, collapse = "")
scenarios_habitat_cover

# calculate total
for(i in scenarios_habitat_cover){

  print(i)

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "p", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i),
                    output = paste0("02_results/01_mapbiomas_brazil_af_trinacional_", i, "_habitat_cover.csv"))

}

# calculate br
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim_br")
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim_br")

for(i in scenarios_habitat_cover){

  print(i)

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "p", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i),
                    output = paste0("02_results/01_mapbiomas_brazil_af_trinacional_", i, "_habitat_cover_br.csv"))

}

# calculate ar
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim_ar")
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim_ar")

for(i in scenarios_habitat_cover){

  print(i)

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "p", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i),
                    output = paste0("02_results/01_mapbiomas_brazil_af_trinacional_", i, "_habitat_cover_ar.csv"))

}

# calculate py
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim_py")
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim_py")

for(i in scenarios_habitat_cover){

  print(i)

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "p", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i),
                    output = paste0("02_results/01_mapbiomas_brazil_af_trinacional_", i, "_habitat_cover_py.csv"))

}

# 2 number of patch and size distribution ----

# scenarios
scenarios_area <- crossing(years,
                           veg = c("_af_lim_forest", "_af_lim_natural"),
                           road = c("", "_roads_rails"))
scenarios_area <- apply(scenarios_area, 1, paste0, collapse = "")
scenarios_area

# sudo grass --tmp-location EPSG:4326 --exec g.extension -s extension=r.area operation=add

# region
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim", res = "30")
# rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim")

# area
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

  }

# export
for(i in scenarios_area){

  print(i)
  rgrass::execGRASS(cmd = "r.out.gdal",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_area"),
                    output = paste0("01_data/02_mapbiomas/02_albers/area/mapbiomas_brazil_af_trinacional_", i, "_area.tif"),
                    createopt = "TFW=TRUE,COMPRESS=DEFLATE")
}


# 3  core and edge area ----
# scenarios
scenarios_edge_area <- crossing(years,
                                veg = c("_af_lim_forest", "_af_lim_natural"),
                                road = c("", "_roads_rails"))
scenarios_edge_area <- apply(scenarios_edge_area, 1, paste0, collapse = "")
scenarios_edge_area

# calculate
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


# 4 connectivity ----

# scenarios
scenarios_confun_area <- crossing(years,
                                  veg = c("_af_lim_forest", "_af_lim_natural"),
                                  road = c("", "_roads_rails"))
scenarios_confun_area <- apply(scenarios_confun_area, 1, paste0, collapse = "")
scenarios_confun_area

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

  for(d in c(90, 180, 300, 390, 510, 1020, 1500, 2010, 3000)){

    print(d)

    name_d <- ifelse(d < 100, paste0("00", d), ifelse(d < 1000, paste0("00", d), d))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_under", name_d, "=if(mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside>0 && mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside<=", d, ",1, 0)"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_d, "=mapbiomas_brazil_af_trinacional_", i, " + mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_under", name_d))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite"),
                      expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_d, "_null=if(mapbiomas_brazil_af_trinacional_", i, "_confun", name_d, "==0, null(), 1)"))

    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "quiet", "overwrite"),
                      input = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_d, "_null"),
                      output = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_d, "_pid"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite"),
                      expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_d, "_pid_adj=mapbiomas_brazil_af_trinacional_", i, "_confun", name_d, "_pid * mapbiomas_brazil_af_trinacional_", i, "_null"))

    rgrass::execGRASS(cmd = "r.stats",
                      flags = c("a", "c", "n", "overwrite"),
                      separator = ",",
                      input = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_d, "_pid_adj"),
                      output = paste0("02_results/04_mapbiomas_brazil_af_trinacional_", i, "_confun_area", name_d, ".csv"))

  }

}

# 5 mean isolation ----

# scenarios
scenarios_isolation <- crossing(years,
                                veg = c("_af_lim_forest", "_af_lim_natural"),
                                road = c("_roads_rails"))
scenarios_isolation <- apply(scenarios_isolation, 1, paste0, collapse = "")
scenarios_isolation

# distance
for(i in scenarios_isolation){

  print(i)

  for(s in c(50, 100, 150, 200, 350, 500)){

    name_s <- ifelse(s < 100, paste0("0", s))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger", name_s, "=if(mapbiomas_brazil_af_trinacional_", i, "_area>=", s, ",1,null())"))

    rgrass::execGRASS(cmd = "r.grow.distance",
                      flags = "overwrite",
                      input = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger", name_s),
                      distance = paste0("mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_area_larger", name_s))
  }

}

# isolation
for(i in scenarios_isolation){

  print(i)

  rgrass::execGRASS(cmd = "r.univar",
                    flags = c("t", "overwrite"),
                    separator = ",",
                    map = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger0000"),
                    output = paste0("02_results/05_mapbiomas_brazil_af_trinacional_", i, "_area_larger0000_isolation_dist.csv"))

  for(s in c(50, 100, 150, 200, 350, 500)){

    name_s <- ifelse(s < 100, paste0("0", s))


    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = "overwrite",
                      expression = paste0("mapbiomas_brazil_af_trinacional_", i, "edge_distance_outside_area_larger", name_s, "=int(mapbiomas_brazil_af_trinacional", i, "_edge_distance_outside_area_larger", name_s, ")"))

    rgrass::execGRASS(cmd = "r.univar",
                      flags = c("e", "t", "overwrite"),
                      map = paste0("mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_area_larger", name_s),
                      output = paste0("02_results/05_mapbiomas_brazil_af_trinacional_", i, "_area_larger", name_s, "_teste.csv"),
                      separator = ",")

  }


}


# 6 protected area and indigenous territory proximity ----

# protected areas distance
# rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim")

rgrass::execGRASS(cmd = "r.grow.distance",
                  flags = "overwrite",
                  input = "protected_areas_null",
                  distance = "protected_areas_distance")

rgrass::execGRASS(cmd = "r.mapcalc",
                  flags = "overwrite",
                  expression = "protected_areas_distance=int(protected_areas_distance)")

# scenarios
scenarios_protected_areas <- crossing(2020,
                                      veg = c("_af_lim_forest", "_af_lim_natural"),
                                      road = c("", "_roads_rails"))
scenarios_protected_areas <- apply(scenarios_protected_areas, 1, paste0, collapse = "")
scenarios_protected_areas

# calculation
for(i in scenarios_protected_areas){

  print(i)

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_null,protected_areas_distance"),
                    output = paste0("02_results/06_mapbiomas_brazil_af_trinacional_", i, "_protected_areas2.csv"))

  # rgrass::execGRASS(cmd = "r.stats",
  #                   flags = c("a", "c", "n", "overwrite"),
  #                   separator = ",",
  #                   input = paste0("mapbiomas_brazil_af_trinacional_", i, ",protected_areas,fabdem_af_lim"),
  #                   output = paste0("02_results/06_mapbiomas_brazil_af_trinacional_", i, "_protected_areas_elevation.csv"))
  #
  # rgrass::execGRASS(cmd = "r.stats",
  #                   flags = c("a", "c", "n", "overwrite"),
  #                   separator = ",",
  #                   input = paste0("mapbiomas_brazil_af_trinacional_", i, ",protected_areas,fabdem_af_lim_slope"),
  #                   output = paste0("02_results/06_mapbiomas_brazil_af_trinacional_", i, "_protected_areas_slope.csv"))
  #
  # rgrass::execGRASS(cmd = "r.stats",
  #                   flags = c("a", "c", "n", "overwrite"),
  #                   separator = ",",
  #                   input = paste0("mapbiomas_brazil_af_trinacional_", i, ",protected_areas,fabdem_af_lim_aspect"),
  #                   output = paste0("02_results/06_mapbiomas_brazil_af_trinacional_", i, "_protected_areas_aspect.csv"))
  #
  # rgrass::execGRASS(cmd = "r.stats",
  #                   flags = c("a", "c", "n", "overwrite"),
  #                   separator = ",",
  #                   input = paste0("mapbiomas_brazil_af_trinacional_", i, ",protected_areas,fabdem_af_lim_pcurvature"),
  #                   output = paste0("02_results/06_mapbiomas_brazil_af_trinacional_", i, "_protected_areas_pcurvature.csv"))
  #
  # rgrass::execGRASS(cmd = "r.stats",
  #                   flags = c("a", "c", "n", "overwrite"),
  #                   separator = ",",
  #                   input = paste0("mapbiomas_brazil_af_trinacional_", i, ",protected_areas,fabdem_af_lim_tcurvature"),
  #                   output = paste0("02_results/06_mapbiomas_brazil_af_trinacional_", i, "_protected_areas_tcurvature.csv"))

}

# classes
rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "overwrite"),
                  separator = ",",
                  input = paste0("mapbiomas_brazil_af_trinacional_2020_af_lim_forest_classes,protected_areas"),
                  output = paste0("02_results/06_mapbiomas_brazil_af_trinacional_2020_af_lim_forest_classes_protected_areas.csv"))

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "overwrite"),
                  separator = ",",
                  input = paste0("mapbiomas_brazil_af_trinacional_2020_af_lim_forest_classes_roads_rails,protected_areas"),
                  output = paste0("02_results/06_mapbiomas_brazil_af_trinacional_2020_af_lim_forest_classes_roads_rails_protected_areas.csv"))

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "overwrite"),
                  separator = ",",
                  input = paste0("mapbiomas_brazil_af_trinacional_2020_af_lim_natural_classes,protected_areas"),
                  output = paste0("02_results/06_mapbiomas_brazil_af_trinacional_2020_af_lim_natural_classes_protected_areas.csv"))

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "overwrite"),
                  separator = ",",
                  input = paste0("mapbiomas_brazil_af_trinacional_2020_af_lim_natural_classes_roads_rails,protected_areas"),
                  output = paste0("02_results/06_mapbiomas_brazil_af_trinacional_2020_af_lim_natural_classes_roads_rails_protected_areas.csv"))


# 7 landscape and topographic index ----

# originals
rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "overwrite"),
                  separator = ",",
                  input = "fabdem_af_lim",
                  output = "02_results/07_fabdem_elevation_original.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "i", "overwrite"),
                  separator = ",",
                  input = "fabdem_float_af_lim_slope",
                  output = "02_results/07_fabdem_float_slope_original.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "i", "overwrite"),
                  separator = ",",
                  input = "fabdem_float_af_lim_aspect",
                  output = "02_results/07_fabdem_float_aspect_original.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "A", "overwrite"),
                  separator = ",",
                  input = "fabdem_float_af_lim_pcurvature",
                  output = "02_results/07_fabdem_float_pcurvature_original.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "A", "overwrite"),
                  separator = ",",
                  input = "fabdem_float_af_lim_tcurvature",
                  output = "02_results/07_fabdem_float_tcurvature_original.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "overwrite"),
                  separator = ",",
                  input = "fabdem_float_af_lim_geomorph",
                  output = "02_results/07_fabdem_float_geomorph_original.csv")

# scenarios
scenarios_topographic <- crossing(years,
                                  veg = c("_af_lim_forest", "_af_lim_natural"),
                                  road = c("", "_roads_rails"))
scenarios_topographic <- apply(scenarios_topographic, 1, paste0, collapse = "")
scenarios_topographic

# calculation cover
for(i in scenarios_topographic){

  print(i)

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",fabdem_af_lim"),
                    output = paste0("02_results/07_mapbiomas_brazil_af_trinacional_", i, "_elevation.csv"))

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "i", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",fabdem_float_af_lim_slope"),
                    output = paste0("02_results/07_mapbiomas_brazil_af_trinacional_", i, "_slope.csv"))

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "i", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",fabdem_float_af_lim_aspect"),
                    output = paste0("02_results/07_mapbiomas_brazil_af_trinacional_", i, "_aspect.csv"))

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "A", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",fabdem_float_af_lim_pcurvature"),
                    output = paste0("02_results/07_mapbiomas_brazil_af_trinacional_", i, "_pcurvature.csv"))

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "A", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",fabdem_float_af_lim_tcurvature"),
                    output = paste0("02_results/07_mapbiomas_brazil_af_trinacional_", i, "_tcurvature.csv"))

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",fabdem_float_af_lim_geomorph"),
                    output = paste0("02_results/07_mapbiomas_brazil_af_trinacional_", i, "_geomorph.csv"))

}

# calculation area
scenarios_topographic_area <- crossing(years,
                                       veg = c("_af_lim_forest", "_af_lim_natural"),
                                       road = c("_pid", "_roads_rails_pid"))
scenarios_topographic_area <- apply(scenarios_topographic_area, 1, paste0, collapse = "")
scenarios_topographic_area

for(i in scenarios_topographic_area){

  print(i)

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "i", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",fabdem_af_lim"),
                    output = paste0("02_results/07_mapbiomas_brazil_af_trinacional_", i, "_elevation_area.csv"))

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "i", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",fabdem_float_af_lim_slope"),
                    output = paste0("02_results/07_mapbiomas_brazil_af_trinacional_", i, "_slope.csv"))

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "i", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",fabdem_float_af_lim_aspect"),
                    output = paste0("02_results/07_mapbiomas_brazil_af_trinacional_", i, "_aspect.csv"))

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "A", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",fabdem_float_af_lim_pcurvature"),
                    output = paste0("02_results/07_mapbiomas_brazil_af_trinacional_", i, "_pcurvature.csv"))

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "A", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",fabdem_float_af_lim_tcurvature"),
                    output = paste0("02_results/07_mapbiomas_brazil_af_trinacional_", i, "_tcurvature.csv"))

}


# 8. limits ------------------------------------------------------------------

# calculate
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
                    input = paste0("mapbiomas_brazil_af_trinacional_2021_af_lim_", j),
                    output = paste0("02_results/01_mapbiomas_brazil_af_trinacional_2021_", i, "_", j,  "_habitat_cover.csv"))

  rgrass::execGRASS(cmd = "r.clump",
                    flags = c("d", "overwrite"),
                    input = paste0("mapbiomas_brazil_af_trinacional_2021_af_lim_", j, "_null"),
                    output = paste0("mapbiomas_brazil_af_trinacional_2021_", i, "_", j, "_pid"))

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_2021_", i, "_", j, "_pid"),
                    output = paste0("02_results/02_mapbiomas_brazil_af_trinacional_2021_", i, "_", j, "_pid_area.csv"))

  }

}

# end ---------------------------------------------------------------------

# 5 mean isolation ----

# scenarios
scenarios_isolation <- crossing(years,
                                veg = c("_af_lim_forest", "_af_lim_natural"),
                                road = c("", "_roads_rails"))
scenarios_isolation <- apply(scenarios_isolation, 1, paste0, collapse = "")
scenarios_isolation

# isolation
for(i in scenarios_isolation){

  print(i)

  rgrass::execGRASS(cmd = "r.to.vect",
                    flags = "overwrite",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_area"),
                    output = paste0("mapbiomas_brazil_af_trinacional_", i, "_area"),
                    type = "area")

  rgrass::execGRASS(cmd = "v.random",
                    flags = c("b", "overwrite"),
                    output = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger0000_random_points"),
                    npoints = 1e3,
                    restrict = "af_lim",
                    column = "distance",
                    column_type = "integer")

  rgrass::execGRASS(cmd = "v.build",
                    map = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger0000_random_points"))

  rgrass::execGRASS(cmd = "v.distance",
                    flags = "overwrite",
                    from = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger0000_random_points"),
                    to = paste0("mapbiomas_brazil_af_trinacional_", i, "_area"),
                    upload = "dist",
                    column = "distance")

  rgrass::execGRASS(cmd = "v.out.ogr",
                    flags = "overwrite",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger0000_random_points"),
                    output = paste0("02_results/05_mapbiomas_brazil_af_trinacional_", i, "_area_larger0000_isolation.csv"),
                    format = "CSV")

  for(s in c(30, 90, 180, 210, 360, 510, 1020)){

    name_s <- ifelse(s < 100, paste0("00", s), ifelse(s < 1000, paste0("0", s), s))
    print(name_s)

    rgrass::execGRASS("v.extract",
                      flags = "overwrite",
                      input = paste0("mapbiomas_brazil_af_trinacional_", i, "_area"),
                      type = "area",
                      where = paste0("(value >= '", s, "')"),
                      output = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger", name_s))

    rgrass::execGRASS(cmd = "v.random",
                      flags = c("b", "overwrite"),
                      output = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger", name_s, "_random_points"),
                      npoints = 1e3,
                      column = "distance",
                      column_type = "integer")

    rgrass::execGRASS(cmd = "v.build",
                      map = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger", name_s, "_random_points"))

    rgrass::execGRASS(cmd = "v.distance",
                      flags = "overwrite",
                      from = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger", name_s, "_random_points"),
                      to = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger", name_s),
                      upload = "dist",
                      column = "distance")

    rgrass::execGRASS(cmd = "v.out.ogr",
                      flags = "overwrite",
                      input = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger", name_s, "_random_points"),
                      output = paste0("02_results/05_mapbiomas_brazil_af_trinacional", i, "_area_larger", name_s, "_isolation.csv"),
                      format = "CSV")

  }

}
