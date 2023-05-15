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
options(scipen = 10)

# connect grass -----------------------------------------------------------

# connect
rgrass::initGRASS(gisBase = system("grass --config path", inter = TRUE),
                  gisDbase = "01_data/00_grassdb",
                  location = "sirgas2000_albers",
                  mapset = "PERMANENT",
                  override = TRUE)

rgrass::initGRASS(gisBase = system("grass --config path", inter = TRUE),
                  gisDbase = "/home/mude/Downloads/grassdb/",
                  location = "newLocation",
                  mapset = "PERMANENT",
                  override = TRUE)



# region ------------------------------------------------------------------

# region
rgrass::execGRASS("g.region", flags = c("a", "p"), vector = "af_lim", res = "30")
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim")

# calculation -------------------------------------------------------------

## function ----
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

  # pid
  rgrass::execGRASS(cmd = "r.clump", flags = c("d", "overwrite"), input = "x1_x2_stable_null", output = "x1_x2_stable_pid")

  # table
  rgrass::execGRASS(cmd = "r.stats", flags = c("c", "n", "overwrite"), input = "x1_x2_stable_pid,x1_x2_stable", output = "x1_x2_stable_pid.csv", separator = ",")

  table_x1_x2 <- readr::read_csv("x1_x2_stable_pid.csv", col_names = c("pid", "stable", "n"), show_col_types = FALSE) %>%
    dplyr::mutate(stable = paste0("stable_", stable)) %>%
    tidyr::pivot_wider(names_from = "stable", values_from = "n", values_fill = 0)

  # gain patches
  table_x1_x2_gain_patches <- table_x1_x2 %>%
    dplyr::filter(stable_1 == 0, stable_10 > 0, stable_11 == 0) %>%
    dplyr::select(pid) %>%
    dplyr::mutate(pid = paste0(pid, "=1")) %>%
    dplyr::bind_rows(tibble::tibble(pid = "*=0"))

  readr::write_delim(table_x1_x2_gain_patches, "table_x1_x2_gain_patches.txt", col_names = NA)

  rgrass::execGRASS(cmd = "r.reclass", flags = c("overwrite"), input = "x1_x2_stable_pid", output = "x1_x2_stable_pid_gain_patches", rules = "table_x1_x2_gain_patches.txt")
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = "x1_x2_stable_pid_gain_patches=x1_x2_stable_pid_gain_patches")
  rgrass::execGRASS(cmd = "r.null", map = "x1_x2_stable_pid_gain_patches", null = 0)
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = "x1_x2_stable_pid_gain_patches_null=if(x1_x2_stable_pid_gain_patches == 1, 1, null())")

  rgrass::execGRASS(cmd = "r.clump", flags = c("d", "overwrite"), input = "x1_x2_stable_pid_gain_patches_null", output = "x1_x2_stable_pid_gain_patches_pid")
  rgrass::execGRASS(cmd = "r.stats", flags = c("a", "c", "n", "overwrite"), input = "x1_x2_stable_pid_gain_patches_pid", output = paste0(x1, "_", x2, "_stable_pid_gain_patches_pid.csv"), separator = ",")

  # loss patches
  table_x1_x2_loss_patches <- table_x1_x2 %>%
    dplyr::filter(stable_1 > 0, stable_10 == 0, stable_11 == 0) %>%
    dplyr::select(pid) %>%
    dplyr::mutate(pid = paste0(pid, "=1")) %>%
    dplyr::bind_rows(tibble::tibble(pid = "*=0"))

  readr::write_delim(table_x1_x2_loss_patches, "table_x1_x2_loss_patches.txt", col_names = NA)

  rgrass::execGRASS(cmd = "r.reclass", flags = c("overwrite"), input = "x1_x2_stable_pid", output = "x1_x2_stable_pid_loss_patches", rules = "table_x1_x2_loss_patches.txt")
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = "x1_x2_stable_pid_loss_patches=x1_x2_stable_pid_loss_patches")
  rgrass::execGRASS(cmd = "r.null", map = "x1_x2_stable_pid_loss_patches", null = 0)
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = "x1_x2_stable_pid_loss_patches_null=if(x1_x2_stable_pid_loss_patches == 1, 1, null())")

  rgrass::execGRASS(cmd = "r.clump", flags = c("d", "overwrite"), input = "x1_x2_stable_pid_loss_patches_null", output = "x1_x2_stable_pid_loss_patches_pid")
  rgrass::execGRASS(cmd = "r.stats", flags = c("a", "c", "n", "overwrite"), input = "x1_x2_stable_pid_loss_patches_pid", output = paste0(x1, "_", x2, "_stable_pid_loss_patches_pid.csv"), separator = ",")

  # gain area
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = "x1_x2_stable_pid_gain_area = if(x1_x2_stable == 10, 1, 0) - x1_x2_stable_pid_gain_patches")

  # loss area
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = "x1_x2_stable_pid_loss_area = if(x1_x2_stable == 1, 1, 0) - x1_x2_stable_pid_loss_patches")

  # stable area
  rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = "x1_x2_stable_pid_stable_area = if(x1_x2_stable == 11, 1, 0)")

  rgrass::execGRASS(cmd = "r.stats", flags = c("a", "c", "overwrite"), input = "x1_x2_stable_pid_gain_area", output = paste0(x1, "_", x2, "_stable_pid_gain_area.csv"), separator = ",")
  rgrass::execGRASS(cmd = "r.stats", flags = c("a", "c", "overwrite"), input = "x1_x2_stable_pid_loss_area", output = paste0(x1, "_", x2, "_stable_pid_loss_area.csv"), separator = ",")
  rgrass::execGRASS(cmd = "r.stats", flags = c("a", "c", "overwrite"), input = "x1_x2_stable_pid_stable_area", output = paste0(x1, "_", x2, "_stable_pid_stable_area.csv"), separator = ",")

  # rename
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_stable,", x1, "_", x2, "_stable"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_stable_pid_gain_patches,", x1, "_", x2, "_stable_pid_gain_patches"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_stable_pid_gain_patches_pid,", x1, "_", x2, "_stable_pid_gain_patches_pid"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_stable_pid_loss_patches,", x1, "_", x2, "_stable_pid_loss_patches"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_stable_pid_loss_patches_pid,", x1, "_", x2, "_stable_pid_loss_patches_pid"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_stable_pid_gain_area,", x1, "_", x2, "_stable_pid_gain_area"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_stable_pid_loss_area,", x1, "_", x2, "_stable_pid_loss_area"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_stable_pid_stable_area,", x1, "_", x2, "_stable_pid_stable_area"))

  # metrics
  gain_area <- readr::read_csv(paste0(x1, "_", x2, "_stable_pid_gain_area.csv"), col_names = c("classes", "area", "number"), show_col_types = FALSE)
  gain_area_value <- gain_area$area[2]/1e4
  gain_area_percentage <- gain_area$number[2]/sum(gain_area$number) * 100

  loss_area <- readr::read_csv(paste0(x1, "_", x2, "_stable_pid_loss_area.csv"), col_names = c("classes", "area", "number"), show_col_types = FALSE)
  loss_area_value <- loss_area$area[2]/1e4
  loss_area_percentage <- loss_area$number[2]/sum(loss_area$number) * 100

  stable_area <- readr::read_csv(paste0(x1, "_", x2, "_stable_pid_stable_area.csv"), col_names = c("classes", "area", "number"), show_col_types = FALSE)
  stable_area_value <- stable_area$area[2]/1e4
  stable_area_percentage <- stable_area$number[2]/sum(stable_area$number) * 100

  gain_patches <- readr::read_csv(paste0(x1, "_", x2, "_stable_pid_gain_patches_pid.csv"), col_names = c("classes", "area", "number"), show_col_types = FALSE)
  gain_patches_number <- nrow(gain_patches)
  gain_patches_area <- sum(gain_patches$area)/1e4
  gain_patches_percentage <- sum(gain_patches$number)/sum(gain_area$number) * 100
  gain_patches_area_mn <- mean(gain_patches$area)/1e4
  gain_patches_area_sd <- sd(gain_patches$area)/1e4
  gain_patches_area_md <- median(gain_patches$area)/1e4
  gain_patches_area_mx <- max(gain_patches$area)/1e4

  loss_patches <- readr::read_csv(paste0(x1, "_", x2, "_stable_pid_loss_patches_pid.csv"), col_names = c("classes", "area", "number", "percentage"), show_col_types = FALSE)
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
  unlink("x1_x2_stable_pid.csv")
  unlink("table_x1_x2_gain_patches.txt")
  unlink("table_x1_x2_loss_patches.txt")
  unlink(paste0(x1, "_", x2, "_stable_pid_gain_area.csv"))
  unlink(paste0(x1, "_", x2, "_stable_pid_loss_area.csv"))
  unlink(paste0(x1, "_", x2, "_stable_pid_stable_area.csv"))
  unlink(paste0(x1, "_", x2, "_stable_pid_gain_patches_pid.csv"))
  unlink(paste0(x1, "_", x2, "_stable_pid_loss_patches_pid.csv"))

  rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = "x1")
  rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = "x2")
  rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = "x1_x2_stable_null")
  rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = "x1_x2_stable_pid_gain_patches_null")
  rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = "x1_x2_stable_pid_loss_patches_null")
  rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = "x1_x2_stable_pid")

}

# calculation ----
grass_temporal_patches(x1 = "mapbiomas_brazil_af_trinacional_1986_af_lim_forest_roads_rails",
                       x2 = "mapbiomas_brazil_af_trinacional_2005_af_lim_forest_roads_rails",
                       path = "02_results/08_")

grass_temporal_patches(x1 = "mapbiomas_brazil_af_trinacional_1986_af_lim_natural_roads_rails",
                       x2 = "mapbiomas_brazil_af_trinacional_2005_af_lim_natural_roads_rails",
                       path = "02_results/08_")

grass_temporal_patches(x1 = "mapbiomas_brazil_af_trinacional_1986_af_lim_forest_roads_rails",
                       x2 = "mapbiomas_brazil_af_trinacional_2020_af_lim_forest_roads_rails",
                       path = "02_results/08_")

grass_temporal_patches(x1 = "mapbiomas_brazil_af_trinacional_1986_af_lim_natural_roads_rails",
                       x2 = "mapbiomas_brazil_af_trinacional_2020_af_lim_natural_roads_rails",
                       path = "02_results/08_")

grass_temporal_patches(x1 = "mapbiomas_brazil_af_trinacional_2005_af_lim_forest_roads_rails",
                       x2 = "mapbiomas_brazil_af_trinacional_2020_af_lim_forest_roads_rails",
                       path = "02_results/08_")

grass_temporal_patches(x1 = "mapbiomas_brazil_af_trinacional_2005_af_lim_natural_roads_rails",
                       x2 = "mapbiomas_brazil_af_trinacional_2020_af_lim_natural_roads_rails",
                       path = "02_results/08_")


# analyses ----------------------------------------------------------------

# import
forest_1986_2005 <- readr::read_csv("02_results/08_mapbiomas_brazil_af_trinacional_1986_af_lim_forest_roads_rails_mapbiomas_brazil_af_trinacional_2005_af_lim_forest_roads_rails_temporal_patches_metrics.csv") %>%
  dplyr::mutate(period = "1986_2005",
                scenario = "forest", .before = 1)
forest_1986_2005

forest_1986_2020 <- readr::read_csv("02_results/08_mapbiomas_brazil_af_trinacional_1986_af_lim_forest_roads_rails_mapbiomas_brazil_af_trinacional_2020_af_lim_forest_roads_rails_temporal_patches_metrics.csv") %>%
  dplyr::mutate(period = "1986_2020",
                scenario = "forest", .before = 1)
forest_1986_2020

forest_2005_2020 <- readr::read_csv("02_results/08_mapbiomas_brazil_af_trinacional_2005_af_lim_forest_roads_rails_mapbiomas_brazil_af_trinacional_2020_af_lim_forest_roads_rails_temporal_patches_metrics.csv") %>%
  dplyr::mutate(period = "2005_2020",
                scenario = "forest", .before = 1)
forest_2005_2020

natural_1986_2005 <- readr::read_csv("02_results/08_mapbiomas_brazil_af_trinacional_1986_af_lim_natural_roads_rails_mapbiomas_brazil_af_trinacional_2005_af_lim_natural_roads_rails_temporal_patches_metrics.csv") %>%
  dplyr::mutate(period = "1986_2005",
                scenario = "natural", .before = 1)
natural_1986_2005

natural_1986_2020 <- readr::read_csv("02_results/08_mapbiomas_brazil_af_trinacional_1986_af_lim_natural_roads_rails_mapbiomas_brazil_af_trinacional_2020_af_lim_natural_roads_rails_temporal_patches_metrics.csv") %>%
  dplyr::mutate(period = "1986_2020",
                scenario = "natural", .before = 1)
natural_1986_2020

natural_2005_2020 <- readr::read_csv("02_results/08_mapbiomas_brazil_af_trinacional_2005_af_lim_natural_roads_rails_mapbiomas_brazil_af_trinacional_2020_af_lim_natural_roads_rails_temporal_patches_metrics.csv") %>%
  dplyr::mutate(period = "2005_2020",
                scenario = "natural", .before = 1)
natural_2005_2020

# organize
data_temporal <- dplyr::bind_rows(forest_1986_2005, forest_1986_2020, forest_2005_2020,
                                  natural_1986_2005, natural_1986_2020, natural_2005_2020)
data_temporal

data_temporal_wide <- tidyr::pivot_wider(data_temporal,
                                         id_cols = c(period, scenario),
                                         names_from = metric,
                                         values_from = c(value, percentage)) %>%
  dplyr::relocate(percentage_gain_area, percentage_loss_area, percentage_balance_area, .after = value_balance_area) %>%
  dplyr::relocate(percentage_gain_area_total, percentage_loss_area_total, percentage_balance_area_total, .after = value_balance_area_total) %>%
  dplyr::relocate(percentage_stable_area, .after = value_stable_area) %>%
  dplyr::relocate(percentage_gain_patches_area, percentage_loss_patches_area, percentage_balance_patches_area, .after = value_balance_patches_area) %>%
  dplyr::select(1:34)
data_temporal_wide

readr::write_csv(data_temporal_wide, "02_results/08_data_mapbiomas_brazil_af_trinacional_temporal.csv")

# end ---------------------------------------------------------------------
