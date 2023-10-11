#' ----
#' title: atlantic forest spatiotemporal dynamics - calculate landscape metrics
#' author: mauricio vancine
#' date: 2023-03-06
#' operational system: gnu/linux - ubuntu - pop_os
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

# calculate ---------------------------------------------------------------

# 0 limits, roads and railways, protected areas and indigenous territories ----

# region
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim", res = "30")

## af limits ----
rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "af_lim,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_af_lim_area.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "af_lim_brazil,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_af_lim_area_brazil.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "af_lim_argentina,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_af_lim_area_argentina.csv")

rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "af_lim_paraguay,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_af_lim_area_paraguay.csv")

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

## roads and railways ----
rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "roads_railways_af,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_roads_railways_area.csv")

## protected areas ----
rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "protected_areas,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_protected_areas_area.csv")

## indigenous territories ----
rgrass::execGRASS(cmd = "r.stats",
                  flags = c("a", "c", "n", "p", "overwrite"),
                  separator = ",",
                  input = "indigenous_territories,mapbiomas_brazil_af_trinacional_2020_af_lim_forest",
                  output = "02_results/00_indigenous_territories_area.csv")

# 1 number of patch and size distribution ---------------------------------

# years
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
years

# region
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim", res = "30")
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim")

## scenarios ----
scenarios_area <- crossing(years,
                           veg = c("_af_lim_forest", "_af_lim_natural"),
                           road = c("", "_roads_railways"))
scenarios_area <- apply(scenarios_area, 1, paste0, collapse = "")
scenarios_area

# addon- installation on linux
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
                    output = paste0("mapbiomas_brazil_af_trinacional_", i, "_fid"))

  rgrass::execGRASS(cmd = "r.area",
                    flags = c("overwrite"),
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_fid"),
                    output = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_ncell"))

  rgrass::execGRASS(cmd = "r.mapcalc",
                    flags = c("overwrite"),
                    expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_ha = mapbiomas_brazil_af_trinacional_", i, "_area_ncell * 0.09"))

  # area
  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_fid"),
                    output = paste0("02_results/01_mapbiomas_brazil_af_trinacional_", i, "_fid_area.csv"))

  # area raster
  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "n", "overwrite"),
                    separator = "=",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_fid"),
                    output = paste0("02_results/01_mapbiomas_brazil_af_trinacional_", i, "_area.txt"))

  readr::read_delim(paste0("02_results/01_mapbiomas_brazil_af_trinacional_", i, "_area.txt"),
                    delim = "=", col_names = c("fid", "area"), col_types = readr::cols()) %>%
    dplyr::mutate(area = ifelse(area/1e4 < 1, 1, round(area/1e4, digits = 0))) %>%
    dplyr::slice(-1) %>%
    readr::write_delim(paste0("02_results/01_mapbiomas_brazil_af_trinacional_", i, "_area_larger1ha.txt"),
                       delim = "=", col_names = FALSE)

  rgrass::execGRASS(cmd = "r.reclass",
                    flags = c("overwrite"),
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_fid"),
                    output = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger1ha"),
                    rules = paste0("02_results/01_mapbiomas_brazil_af_trinacional_", i, "_area_larger1ha.txt"))

  rgrass::execGRASS(cmd = "r.reclass",
                    flags = c("overwrite"),
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, "_fid"),
                    output = paste0("mapbiomas_brazil_af_trinacional_", i, "_area"),
                    rules = paste0("02_results/01_mapbiomas_brazil_af_trinacional_", i, "_area.txt"))

  unlink(paste0("02_results/01_mapbiomas_brazil_af_trinacional_", i, "_area.txt"))
  unlink(paste0("02_results/01_mapbiomas_brazil_af_trinacional_", i, "_area_larger1ha.txt"))

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

  for(j in c("forest", "natural", "forest_roads_railways", "natural_roads_railways")){

    # area
    rgrass::execGRASS(cmd = "r.clump",
                      flags = c("d", "overwrite"),
                      input = paste0("mapbiomas_brazil_af_trinacional_2020_af_lim_", j, "_null"),
                      output = paste0("mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_fid"))

    rgrass::execGRASS(cmd = "r.stats",
                      flags = c("a", "c", "n", "overwrite"),
                      separator = ",",
                      input = paste0("mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_fid"),
                      output = paste0("02_results/01_mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_fid_area.csv"))


    # area raster
    rgrass::execGRASS(cmd = "r.stats",
                      flags = c("a", "n", "overwrite"),
                      separator = "=",
                      input = paste0("mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_fid"),
                      output = paste0("02_results/01_mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_area.txt"))

    readr::read_delim(paste0("02_results/01_mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_area.txt"),
                      delim = "=", col_names = c("fid", "area"), col_types = readr::cols()) %>%
      dplyr::mutate(area = ifelse(area/1e4 < 1, 1, round(area/1e4, digits = 0))) %>%
      dplyr::slice(-1) %>%
      readr::write_delim(paste0("02_results/01_mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_area_larger1ha.txt"),
                         delim = "=", col_names = FALSE)

    rgrass::execGRASS(cmd = "r.reclass",
                      flags = c("overwrite"),
                      input = paste0("mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_fid"),
                      output = paste0("mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_area_larger1ha"),
                      rules = paste0("02_results/01_mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_area_larger1ha.txt"))

    rgrass::execGRASS(cmd = "r.reclass",
                      flags = c("overwrite"),
                      input = paste0("mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_fid"),
                      output = paste0("mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_area"),
                      rules = paste0("02_results/01_mapbiomas_brazil_af_trinacional_2020_", i, "_", j, "_area.txt"))

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

## new patches ----

### function ----
grass_temporal_patches <- function(x1, x2, path) {

  # rename
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = paste0("x1 = ", x1))
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = paste0("x2 = ", x2))

  # binary
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = "x2_10 = if(x2 == 1, 10, 0)")

  # stable
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = "x1_x2_stable = x1 + x2_10")

  # stable null
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = "x1_x2_stable_null = if(x1_x2_stable > 0, 1, null())")

  # fid
  rgrass::execGRASS(cmd = "r.clump", flags = c("d", "overwrite"), input = "x1_x2_stable_null", output = "x1_x2_stable_fid")

  # table
  rgrass::execGRASS(cmd = "r.stats", flags = c("c", "n", "overwrite"), input = "x1_x2_stable_fid,x1_x2_stable", output = "x1_x2_stable_fid.csv", separator = ",")

  table_x1_x2 <- readr::read_csv("x1_x2_stable_fid.csv", col_names = c("fid", "stable", "n"), show_col_types = FALSE) %>%
    dplyr::mutate(stable = paste0("stable_", stable)) %>%
    tidyr::pivot_wider(names_from = "stable", values_from = "n", values_fill = 0)

  # gain patches
  table_x1_x2_gain_patches <- table_x1_x2 %>%
    dplyr::filter(stable_1 == 0, stable_10 > 0, stable_11 == 0) %>%
    dplyr::select(fid) %>%
    dplyr::mutate(fid = paste0(fid, "=1")) %>%
    dplyr::bind_rows(tibble::tibble(fid = "*=0"))

  readr::write_delim(table_x1_x2_gain_patches, "table_x1_x2_gain_patches.txt", col_names = NA)

  rgrass::execGRASS(cmd = "r.reclass", flags = c("overwrite"), input = "x1_x2_stable_fid", output = "x1_x2_stable_fid_gain_patches", rules = "table_x1_x2_gain_patches.txt")
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = "x1_x2_stable_fid_gain_patches=x1_x2_stable_fid_gain_patches")
  rgrass::execGRASS(cmd = "r.null", map = "x1_x2_stable_fid_gain_patches", null = 0)
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = "x1_x2_stable_fid_gain_patches_null=if(x1_x2_stable_fid_gain_patches == 1, 1, null())")

  rgrass::execGRASS(cmd = "r.clump", flags = c("d", "overwrite"), input = "x1_x2_stable_fid_gain_patches_null", output = "x1_x2_stable_fid_gain_patches_fid")
  rgrass::execGRASS(cmd = "r.stats", flags = c("a", "c", "n", "overwrite"), input = "x1_x2_stable_fid_gain_patches_fid", output = paste0(x1, "_", x2, "_stable_fid_gain_patches_fid.csv"), separator = ",")

  # loss patches
  table_x1_x2_loss_patches <- table_x1_x2 %>%
    dplyr::filter(stable_1 > 0, stable_10 == 0, stable_11 == 0) %>%
    dplyr::select(fid) %>%
    dplyr::mutate(fid = paste0(fid, "=1")) %>%
    dplyr::bind_rows(tibble::tibble(fid = "*=0"))

  readr::write_delim(table_x1_x2_loss_patches, "table_x1_x2_loss_patches.txt", col_names = NA)

  rgrass::execGRASS(cmd = "r.reclass", flags = c("overwrite"), input = "x1_x2_stable_fid", output = "x1_x2_stable_fid_loss_patches", rules = "table_x1_x2_loss_patches.txt")
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = "x1_x2_stable_fid_loss_patches=x1_x2_stable_fid_loss_patches")
  rgrass::execGRASS(cmd = "r.null", map = "x1_x2_stable_fid_loss_patches", null = 0)
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = "x1_x2_stable_fid_loss_patches_null=if(x1_x2_stable_fid_loss_patches == 1, 1, null())")

  rgrass::execGRASS(cmd = "r.clump", flags = c("d", "overwrite"), input = "x1_x2_stable_fid_loss_patches_null", output = "x1_x2_stable_fid_loss_patches_fid")
  rgrass::execGRASS(cmd = "r.stats", flags = c("a", "c", "n", "overwrite"), input = "x1_x2_stable_fid_loss_patches_fid", output = paste0(x1, "_", x2, "_stable_fid_loss_patches_fid.csv"), separator = ",")

  # gain area
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = "x1_x2_stable_fid_gain_area = if(x1_x2_stable == 10, 1, 0) - x1_x2_stable_fid_gain_patches")

  # loss area
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = "x1_x2_stable_fid_loss_area = if(x1_x2_stable == 1, 1, 0) - x1_x2_stable_fid_loss_patches")

  # stable area
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = "x1_x2_stable_fid_stable_area = if(x1_x2_stable == 11, 1, 0)")

  rgrass::execGRASS(cmd = "r.stats", flags = c("a", "c", "overwrite"), input = "x1_x2_stable_fid_gain_area", output = paste0(x1, "_", x2, "_stable_fid_gain_area.csv"), separator = ",")
  rgrass::execGRASS(cmd = "r.stats", flags = c("a", "c", "overwrite"), input = "x1_x2_stable_fid_loss_area", output = paste0(x1, "_", x2, "_stable_fid_loss_area.csv"), separator = ",")
  rgrass::execGRASS(cmd = "r.stats", flags = c("a", "c", "overwrite"), input = "x1_x2_stable_fid_stable_area", output = paste0(x1, "_", x2, "_stable_fid_stable_area.csv"), separator = ",")

  # rename
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_stable,", x1, "_", x2, "_stable"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_stable_fid_gain_patches,", x1, "_", x2, "_stable_fid_gain_patches"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_stable_fid_gain_patches_fid,", x1, "_", x2, "_stable_fid_gain_patches_fid"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_stable_fid_loss_patches,", x1, "_", x2, "_stable_fid_loss_patches"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_stable_fid_loss_patches_fid,", x1, "_", x2, "_stable_fid_loss_patches_fid"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_stable_fid_gain_area,", x1, "_", x2, "_stable_fid_gain_area"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_stable_fid_loss_area,", x1, "_", x2, "_stable_fid_loss_area"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_stable_fid_stable_area,", x1, "_", x2, "_stable_fid_stable_area"))

  # metrics
  gain_area <- readr::read_csv(paste0(x1, "_", x2, "_stable_fid_gain_area.csv"), col_names = c("classes", "area", "number"), show_col_types = FALSE)
  gain_area_value <- gain_area$area[2]/1e4
  gain_area_percentage <- gain_area$number[2]/sum(gain_area$number) * 100

  loss_area <- readr::read_csv(paste0(x1, "_", x2, "_stable_fid_loss_area.csv"), col_names = c("classes", "area", "number"), show_col_types = FALSE)
  loss_area_value <- loss_area$area[2]/1e4
  loss_area_percentage <- loss_area$number[2]/sum(loss_area$number) * 100

  stable_area <- readr::read_csv(paste0(x1, "_", x2, "_stable_fid_stable_area.csv"), col_names = c("classes", "area", "number"), show_col_types = FALSE)
  stable_area_value <- stable_area$area[2]/1e4
  stable_area_percentage <- stable_area$number[2]/sum(stable_area$number) * 100

  gain_patches <- readr::read_csv(paste0(x1, "_", x2, "_stable_fid_gain_patches_fid.csv"), col_names = c("classes", "area", "number"), show_col_types = FALSE)
  gain_patches_number <- nrow(gain_patches)
  gain_patches_area <- sum(gain_patches$area)/1e4
  gain_patches_percentage <- sum(gain_patches$number)/sum(gain_area$number) * 100
  gain_patches_area_mn <- mean(gain_patches$area)/1e4
  gain_patches_area_sd <- sd(gain_patches$area)/1e4
  gain_patches_area_md <- median(gain_patches$area)/1e4
  gain_patches_area_mx <- max(gain_patches$area)/1e4

  loss_patches <- readr::read_csv(paste0(x1, "_", x2, "_stable_fid_loss_patches_fid.csv"), col_names = c("classes", "area", "number", "percentage"), show_col_types = FALSE)
  loss_patches_number <- nrow(loss_patches)
  loss_patches_area <- sum(loss_patches$area)/1e4
  loss_patches_percentage <- sum(loss_patches$number)/sum(loss_area$number) * 100
  loss_patches_area_mn <- mean(loss_patches$area)/1e4
  loss_patches_area_sd <- sd(loss_patches$area)/1e4
  loss_patches_area_md <- median(loss_patches$area)/1e4
  loss_patches_area_mx <- max(loss_patches$area)/1e4

  gain_area_value_total <- gain_area_value + gain_patches_area
  loss_area_value_total <- loss_area_value + loss_patches_area
  gain_area_percentage_total <- gain_area_percentage + gain_patches_percentage
  loss_area_percentage_total <- loss_area_percentage + loss_patches_percentage

  balance_area <- gain_area_value - loss_area_value
  balance_area_total <- gain_area_value_total - loss_area_value_total
  balance_area_percentage <- gain_area_percentage - loss_area_percentage
  balance_area_percentage_total <- gain_area_percentage_total - loss_area_percentage_total
  balance_patches_number <- gain_patches_number - loss_patches_number
  balance_patches_area <- gain_patches_area - loss_patches_area
  balance_patches_percentage <- gain_patches_percentage - loss_patches_percentage

  metrics <- tibble::tibble(metric = c("gain_area",
                                       "gain_area_total",
                                       "loss_area",
                                       "loss_area_total",
                                       "balance_area",
                                       "balance_area_total",
                                       "stable_area",
                                       "gain_patches_area",
                                       "gain_patches_area_mn",
                                       "gain_patches_area_sd",
                                       "gain_patches_area_md",
                                       "gain_patches_area_mx",
                                       "gain_patches_number",
                                       "loss_patches_area",
                                       "loss_patches_area_mn",
                                       "loss_patches_area_sd",
                                       "loss_patches_area_md",
                                       "loss_patches_area_mx",
                                       "loss_patches_number",
                                       "balance_patches_number",
                                       "balance_patches_area"),
                            value = c(gain_area_value,
                                      gain_area_value_total,
                                      loss_area_value,
                                      loss_area_value_total,
                                      balance_area,
                                      balance_area_total,
                                      stable_area_value,
                                      gain_patches_area,
                                      gain_patches_area_mn,
                                      gain_patches_area_sd,
                                      gain_patches_area_md,
                                      gain_patches_area_mx,
                                      gain_patches_number,
                                      loss_patches_area,
                                      loss_patches_area_mn,
                                      loss_patches_area_sd,
                                      loss_patches_area_md,
                                      loss_patches_area_mx,
                                      loss_patches_number,
                                      balance_patches_number,
                                      balance_patches_area),
                            percentage = c(gain_area_percentage,
                                           gain_area_percentage_total,
                                           loss_area_percentage,
                                           loss_area_percentage_total,
                                           balance_area_percentage,
                                           balance_area_percentage_total,
                                           stable_area_percentage,
                                           gain_patches_percentage,
                                           NA,
                                           NA,
                                           NA,
                                           NA,
                                           NA,
                                           loss_patches_percentage,
                                           NA,
                                           NA,
                                           NA,
                                           NA,
                                           NA,
                                           NA,
                                           balance_patches_percentage)) %>%
    dplyr::mutate(value = round(value, 2),
                  percentage = round(percentage, 2))
  metrics

  readr::write_csv(metrics, paste0(path, x1, "_", x2, "_temporal_patches_metrics.csv"))

  # clean
  unlink("x1_x2_stable_fid.csv")
  unlink("table_x1_x2_gain_patches.txt")
  unlink("table_x1_x2_loss_patches.txt")
  unlink(paste0(x1, "_", x2, "_stable_fid_gain_area.csv"))
  unlink(paste0(x1, "_", x2, "_stable_fid_loss_area.csv"))
  unlink(paste0(x1, "_", x2, "_stable_fid_stable_area.csv"))
  unlink(paste0(x1, "_", x2, "_stable_fid_gain_patches_fid.csv"))
  unlink(paste0(x1, "_", x2, "_stable_fid_loss_patches_fid.csv"))

  rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = "x1")
  rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = "x2")
  rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = "x1_x2_stable_null")
  rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = "x1_x2_stable_fid_gain_patches_null")
  rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = "x1_x2_stable_fid_loss_patches_null")
  rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = "x1_x2_stable_fid")

}

### calculation ----
grass_temporal_patches(x1 = "mapbiomas_brazil_af_trinacional_1986_af_lim_forest_roads_railways",
                       x2 = "mapbiomas_brazil_af_trinacional_2005_af_lim_forest_roads_railways",
                       path = "02_results/01_")

grass_temporal_patches(x1 = "mapbiomas_brazil_af_trinacional_1986_af_lim_natural_roads_railways",
                       x2 = "mapbiomas_brazil_af_trinacional_2005_af_lim_natural_roads_railways",
                       path = "02_results/01_")

grass_temporal_patches(x1 = "mapbiomas_brazil_af_trinacional_1986_af_lim_forest_roads_railways",
                       x2 = "mapbiomas_brazil_af_trinacional_2020_af_lim_forest_roads_railways",
                       path = "02_results/01_")

grass_temporal_patches(x1 = "mapbiomas_brazil_af_trinacional_1986_af_lim_natural_roads_railways",
                       x2 = "mapbiomas_brazil_af_trinacional_2020_af_lim_natural_roads_railways",
                       path = "02_results/01_")

grass_temporal_patches(x1 = "mapbiomas_brazil_af_trinacional_2005_af_lim_forest_roads_railways",
                       x2 = "mapbiomas_brazil_af_trinacional_2020_af_lim_forest_roads_railways",
                       path = "02_results/01_")

grass_temporal_patches(x1 = "mapbiomas_brazil_af_trinacional_2005_af_lim_natural_roads_railways",
                       x2 = "mapbiomas_brazil_af_trinacional_2020_af_lim_natural_roads_railways",
                       path = "02_results/01_")

# 2 habitat cover ---------------------------------------------------------

# years
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
years

# region
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim", res = "30")
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim")

## scenarios ----
scenarios_habitat_cover <- crossing(years,
                                    veg = c("_af_lim_forest_classes", "_af_lim_natural_classes"),
                                    road = c("", "_roads_railways"))
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

## calculation brazil ----
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

## calculation ar ----
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

## calculation py ----
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

  for(j in c("forest", "natural", "forest_roads_railways", "natural_roads_railways")){

    rgrass::execGRASS(cmd = "r.stats",
                      flags = c("a", "c", "n", "p", "overwrite"),
                      separator = ",",
                      input = paste0("mapbiomas_brazil_af_trinacional_2020_af_lim_", j),
                      output = paste0("02_results/01_mapbiomas_brazil_af_trinacional_2020_", i, "_", j,  "_habitat_cover.csv"))

  }

}

# 3  core and edge area ---------------------------------------------------

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
                                road = c("", "_roads_railways"))
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


# 4 functional connectivity -----------------------------------------------

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
                                  road = c("", "_roads_railways"))
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
                      output = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_fid"))

    rgrass::execGRASS(cmd = "r.mapcalc",
                      flags = c("overwrite"),
                      expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_fid_adj=mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_fid * mapbiomas_brazil_af_trinacional_", i, "_null"))

    rgrass::execGRASS(cmd = "r.stats",
                      flags = c("a", "c", "n", "overwrite"),
                      separator = ",",
                      input = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_fid_adj"),
                      output = paste0("02_results/04_mapbiomas_brazil_af_trinacional_", i, "_confun_area", name_j, ".csv"))

    # area raster
    rgrass::execGRASS(cmd = "r.stats",
                      flags = c("a", "n", "overwrite"),
                      separator = "=",
                      input = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_fid_adj"),
                      output = paste0("02_results/04_mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_area.txt"))

    readr::read_delim(paste0("02_results/04_mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_area.txt"),
                      delim = "=", col_names = c("fid", "area"), col_types = readr::cols()) %>%
      dplyr::mutate(area = ifelse(area/1e4 < 1, 1, round(area/1e4, digits = 0))) %>%
      dplyr::slice(-1) %>%
      readr::write_delim(paste0("02_results/04_mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_area_larger1ha.txt"),
                         delim = "=", col_names = FALSE)

    rgrass::execGRASS(cmd = "r.reclass",
                      flags = c("overwrite"),
                      input = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_fid_adj"),
                      output = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "area_larger1ha"),
                      rules = paste0("02_results/04_mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_area_larger1ha.txt"))

    rgrass::execGRASS(cmd = "r.reclass",
                      flags = c("overwrite"),
                      input = paste0("mapbiomas_brazil_af_trinacional_", i, "_confun", name_j, "_fid_adj"),
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

# 5 mean isolation --------------------------------------------------------

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
                                road = c("_roads_railways"))
scenarios_isolation <- apply(scenarios_isolation, 1, paste0, collapse = "")
scenarios_isolation

## calculation ----
for(i in scenarios_isolation){

  print(i)

  rgrass::execGRASS("r.mapcalc", flags = "overwrite", expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger0000_nonzero=if(mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside>0, mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside, null())"))
  rgrass::execGRASS("r.univar", flags = c("t", "overwrite"), map = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger0000_nonzero"), output = paste0("02_results/05_mapbiomas_brazil_af_trinacional_", i, "_isolation_larger0000.csv"),  separator=",")
  rgrass::execGRASS("r.quantile", flags = c("overwrite"), input = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger0000_nonzero"), percentiles = c(10, 25, 50, 75, 90), file = paste0("02_results/05_mapbiomas_brazil_af_trinacional_", i, "_isolation_larger0000", "_quantile.csv"))

  for(j in c(50, 100, 150, 200, 250, 350, 500, 1000)){

    name_j <- ifelse(j < 100, paste0("00", j), ifelse(j < 1000, paste0("0", j), j))

    rgrass::execGRASS("r.mapcalc", flags = "overwrite", expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger", name_j, "=if(mapbiomas_brazil_af_trinacional_", i, "_area_ha>=", j, ",1,null())"))
    rgrass::execGRASS("r.grow.distance", flags = "overwrite", input = paste0("mapbiomas_brazil_af_trinacional_", i, "_area_larger", name_j), distance = paste0("mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_area_larger", name_j))
    rgrass::execGRASS("r.mapcalc", flags = "overwrite", expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_area_larger", name_j, "=int(mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_area_larger", name_j, ")"))
    rgrass::execGRASS("r.mapcalc", flags = "overwrite", expression = paste0("mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_area_larger", name_j, "_nonzero=if(mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_area_larger", name_j, " > 0, mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_area_larger", name_j, ", null())"))
    rgrass::execGRASS("r.univar", flags = c("t", "overwrite"), map = paste0("mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_area_larger", name_j, "_nonzero"), output = paste0("02_results/05_mapbiomas_brazil_af_trinacional_", i, "_isolation_larger", name_j, ".csv"),  separator = ",")
    rgrass::execGRASS("r.quantile", flags = c("overwrite"), input = paste0("mapbiomas_brazil_af_trinacional_", i, "_edge_distance_outside_area_larger", name_j, "_nonzero"), percentiles = c(10, 25, 50, 75, 90), file = paste0("02_results/05_mapbiomas_brazil_af_trinacional_", i, "_isolation_larger", name_j, "_quantile.csv"))

  }

}


# 6 protected areas and indigenous territories proximity ------------------

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
                         road = c("", "_roads_railways"))
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
                                 road = c("", "_roads_railways"))
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


## indigenous territories ----

# list files
files_rasters_indigenous_territories <- rgrass::stringexecGRASS("g.list type='raster' pattern='*indigenous_territories*'", intern=TRUE)
files_rasters_indigenous_territories

### distance ----
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim")

rgrass::execGRASS(cmd = "r.grow.distance",
                  flags = "overwrite",
                  input = "indigenous_territories_null",
                  distance = "indigenous_territories_distance")

rgrass::execGRASS(cmd = "r.mapcalc",
                  flags = "overwrite",
                  expression = "indigenous_territories_distance=int(indigenous_territories_distance)")

### scenarios ----
scenarios_it <- crossing(2020,
                         veg = c("_af_lim_forest", "_af_lim_natural"),
                         road = c("", "_roads_railways"))
scenarios_it <- apply(scenarios_it, 1, paste0, collapse = "")
scenarios_it

### calculation ----
for(i in scenarios_it){

  print(i)

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",indigenous_territories_distance"),
                    output = paste0("02_results/06_mapbiomas_brazil_af_trinacional_", i, "_indigenous_territories.csv"))

}

### classes scenarios ----
scenarios_it_classes <- crossing(2020,
                                 veg = c("_af_lim_forest_classes", "_af_lim_natural_classes"),
                                 road = c("", "_roads_railways"))
scenarios_it_classes <- apply(scenarios_it_classes, 1, paste0, collapse = "")
scenarios_it_classes

### classes calculation ----
for(i in scenarios_it_classes){

  print(i)

  rgrass::execGRASS(cmd = "r.stats",
                    flags = c("a", "c", "n", "overwrite"),
                    separator = ",",
                    input = paste0("mapbiomas_brazil_af_trinacional_", i, ",indigenous_territories_distance"),
                    output = paste0("02_results/06_mapbiomas_brazil_af_trinacional_", i, "_indigenous_territories_classes.csv"))

}

# end ---------------------------------------------------------------------
