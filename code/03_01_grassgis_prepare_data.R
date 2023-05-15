#' ----
#' title: grass prepare data
#' author: mauricio vancine
#' date: 2022-11-23
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

# years
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
years

# region
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim", res = "30")

# select forest and natural classes ---------------------------------------

# selection classes
for(i in years){

  print(i)

  rgrass::execGRASS("r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_af_lim_forest=if(mapbiomas_brazil_af_trinacional_", i, "_af_lim == 3 || mapbiomas_brazil_af_trinacional_", i, "_af_lim == 5 || mapbiomas_brazil_af_trinacional_", i, "_af_lim == 49, 1, 0)"))

  rgrass::execGRASS("r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_af_lim_natural=if(mapbiomas_brazil_af_trinacional_", i, "_af_lim == 3 || mapbiomas_brazil_af_trinacional_", i, "_af_lim == 4 || mapbiomas_brazil_af_trinacional_", i, "_af_lim == 5 || mapbiomas_brazil_af_trinacional_", i, "_af_lim == 11 || mapbiomas_brazil_af_trinacional_", i, "_af_lim == 12 || mapbiomas_brazil_af_trinacional_", i, "_af_lim == 13 || mapbiomas_brazil_af_trinacional_", i, "_af_lim == 32 || mapbiomas_brazil_af_trinacional_", i, "_af_lim == 49 || mapbiomas_brazil_af_trinacional_", i, "_af_lim == 50, 1, 0)"))

  rgrass::execGRASS("r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_af_lim_forest_classes=mapbiomas_brazil_af_trinacional_", i, "_af_lim * mapbiomas_brazil_af_trinacional_", i, "_af_lim_forest"))

  rgrass::execGRASS("r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_af_lim_natural_classes=mapbiomas_brazil_af_trinacional_", i, "_af_lim * mapbiomas_brazil_af_trinacional_", i, "_af_lim_natural"))

}

# delete forest in roads ---------------------------------------------------

# delete national roads
for(i in years){

  print(i)

  rgrass::execGRASS("r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_af_lim_forest_roads_rails=if(roads_rails_af == 1 & mapbiomas_brazil_af_trinacional_", i, "_af_lim_forest == 1, 0, mapbiomas_brazil_af_trinacional_", i, "_af_lim_forest)"))

  rgrass::execGRASS("r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_af_lim_natural_roads_rails=if(roads_rails_af == 1 & mapbiomas_brazil_af_trinacional_", i, "_af_lim_natural == 1, 0, mapbiomas_brazil_af_trinacional_", i, "_af_lim_natural)"))

  rgrass::execGRASS("r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_af_lim_forest_classes_roads_rails=if(roads_rails_af == 1 & mapbiomas_brazil_af_trinacional_", i, "_af_lim_forest_classes != 0 , 0, mapbiomas_brazil_af_trinacional_", i, "_af_lim_forest_classes)"))

  rgrass::execGRASS("r.mapcalc",
                    flags = "overwrite",
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_af_lim_natural_classes_roads_rails=if(roads_rails_af == 1 & mapbiomas_brazil_af_trinacional_", i, "_af_lim_natural_classes != 0 , 0, mapbiomas_brazil_af_trinacional_", i, "_af_lim_natural_classes)"))


}

# end ---------------------------------------------------------------------
