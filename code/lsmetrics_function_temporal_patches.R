 q## function ----
lsmetrics_temporal_patches <- function(x1, x2, path) {

  # packages
  if(!require(tidyverse)) install.packages("tidyverse")
  if(!require(rgrass)) install.packages("rgrass")

  # rename
  rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = paste0("x1 = int(", x1, ")"))
  rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = paste0("x2 = int(", x2, ")"))

  # binary
  rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "x2_2 = if(x2 == 1, 2, 0)")

  # temporal
  rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "x1_x2_temporal = x1 + x2_2")

  # temporal null
  rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "x1_x2_temporal_null = if(x1_x2_temporal > 0, 1, null())")

  # pid
  rgrass::execGRASS(cmd = "r.clump", flags = c("d", "overwrite"), input = "x1_x2_temporal_null", output = "x1_x2_temporal_pid")

  # table
  rgrass::execGRASS(cmd = "r.stats", flags = c("c", "n", "overwrite"), input = "x1_x2_temporal_pid,x1_x2_temporal", output = "x1_x2_temporal_pid.csv", separator = ",")

  table_x1_x2 <- readr::read_csv("x1_x2_temporal_pid.csv", col_names = c("pid", "temporal", "n"), show_col_types = FALSE) %>%
    dplyr::mutate(temporal = paste0("temporal_", temporal)) %>%
    tidyr::pivot_wider(names_from = "temporal", values_from = "n", values_fill = 0)
  table_x1_x2

  # gain patches
  table_x1_x2_gain_patches <- table_x1_x2 %>%
    dplyr::filter(temporal_1 == 0, temporal_2 > 0, temporal_3 == 0) %>%
    dplyr::select(pid) %>%
    dplyr::mutate(pid = paste0(pid, "=1")) %>%
    dplyr::bind_rows(tibble::tibble(pid = "*=0"))

  readr::write_delim(table_x1_x2_gain_patches, "table_x1_x2_gain_patches.txt", col_names = NA)

  rgrass::execGRASS(cmd = "r.reclass", flags = "overwrite", input = "x1_x2_temporal_pid", output = "x1_x2_temporal_pid_gain_patches", rules = "table_x1_x2_gain_patches.txt")
  rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "x1_x2_temporal_pid_gain_patches=x1_x2_temporal_pid_gain_patches")
  rgrass::execGRASS(cmd = "r.null", map = "x1_x2_temporal_pid_gain_patches", null = 0)
  rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "x1_x2_temporal_pid_gain_patches_null=if(x1_x2_temporal_pid_gain_patches == 1, 1, null())")

  rgrass::execGRASS(cmd = "r.clump", flags = c("d", "overwrite"), input = "x1_x2_temporal_pid_gain_patches_null", output = "x1_x2_temporal_pid_gain_patches_pid")
  rgrass::execGRASS(cmd = "r.stats", flags = c("a", "c", "n", "overwrite"), input = "x1_x2_temporal_pid_gain_patches_pid", output = paste0(x1, "_", x2, "_temporal_pid_gain_patches_pid.csv"), separator = ",")

  # loss patches
  table_x1_x2_loss_patches <- table_x1_x2 %>%
    dplyr::filter(temporal_1 > 0, temporal_2 == 0, temporal_3 == 0) %>%
    dplyr::select(pid) %>%
    dplyr::mutate(pid = paste0(pid, "=1")) %>%
    dplyr::bind_rows(tibble::tibble(pid = "*=0"))

  readr::write_delim(table_x1_x2_loss_patches, "table_x1_x2_loss_patches.txt", col_names = NA)

  rgrass::execGRASS(cmd = "r.reclass", flags = "overwrite", input = "x1_x2_temporal_pid", output = "x1_x2_temporal_pid_loss_patches", rules = "table_x1_x2_loss_patches.txt")
  rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "x1_x2_temporal_pid_loss_patches=x1_x2_temporal_pid_loss_patches")
  rgrass::execGRASS(cmd = "r.null", map = "x1_x2_temporal_pid_loss_patches", null = 0)
  rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "x1_x2_temporal_pid_loss_patches_null=if(x1_x2_temporal_pid_loss_patches == 1, 1, null())")

  rgrass::execGRASS(cmd = "r.clump", flags = c("d", "overwrite"), input = "x1_x2_temporal_pid_loss_patches_null", output = "x1_x2_temporal_pid_loss_patches_pid")
  rgrass::execGRASS(cmd = "r.stats", flags = c("a", "c", "n", "overwrite"), input = "x1_x2_temporal_pid_loss_patches_pid", output = paste0(x1, "_", x2, "_temporal_pid_loss_patches_pid.csv"), separator = ",")

  # gain area
  rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "x1_x2_temporal_pid_gain_area = if(x1_x2_temporal == 2, 1, 0) - x1_x2_temporal_pid_gain_patches")

  # loss area
  rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "x1_x2_temporal_pid_loss_area = if(x1_x2_temporal == 1, 1, 0) - x1_x2_temporal_pid_loss_patches")

  # stable area
  rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "x1_x2_temporal_pid_stable_area = if(x1_x2_temporal == 3, 1, 0)")

  rgrass::execGRASS(cmd = "r.stats", flags = c("a", "c", "n", "overwrite"), input = "x1_x2_temporal_pid_gain_area", output = paste0(x1, "_", x2, "_temporal_pid_gain_area.csv"), separator = ",")
  rgrass::execGRASS(cmd = "r.stats", flags = c("a", "c", "n", "overwrite"), input = "x1_x2_temporal_pid_loss_area", output = paste0(x1, "_", x2, "_temporal_pid_loss_area.csv"), separator = ",")
  rgrass::execGRASS(cmd = "r.stats", flags = c("a", "c", "n", "overwrite"), input = "x1_x2_temporal_pid_stable_area", output = paste0(x1, "_", x2, "_temporal_pid_stable_area.csv"), separator = ",")

  # temporal
  rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "x1_x2_temporal = x1_x2_temporal + if(x1_x2_temporal_pid_gain_patches == 1, 3, 0) + if(x1_x2_temporal_pid_loss_patches == 1, 3, 0)")
  readr::write_delim(x = tibble::tibble(values = 0:5, colors = c("white", "orange", "green", "gray", "red", "blue")),
                     file = "table_color.txt", delim = " ", col_names = FALSE)
  rgrass::execGRASS(cmd = "r.colors", map = "x1_x2_temporal", rules = "table_color.txt")

  # rename
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_temporal,", x1, "_", x2, "_temporal"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_temporal_pid_gain_patches,", x1, "_", x2, "_temporal_pid_gain_patches"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_temporal_pid_gain_patches_pid,", x1, "_", x2, "_temporal_pid_gain_patches_pid"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_temporal_pid_loss_patches,", x1, "_", x2, "_temporal_pid_loss_patches"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_temporal_pid_loss_patches_pid,", x1, "_", x2, "_temporal_pid_loss_patches_pid"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_temporal_pid_gain_area,", x1, "_", x2, "_temporal_pid_gain_area"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_temporal_pid_loss_area,", x1, "_", x2, "_temporal_pid_loss_area"))
  rgrass::execGRASS(cmd = "g.rename", flags = c("overwrite", "quiet"), raster = paste0("x1_x2_temporal_pid_stable_area,", x1, "_", x2, "_temporal_pid_stable_area"))

  # metrics
  gain <- readr::read_csv(paste0(x1, "_", x2, "_temporal_pid_gain_area.csv"), col_names = c("classes", "area", "number"), show_col_types = FALSE)
  gain_npixel <- gain$number[2]
  gain_area <- gain$area[2]/1e4
  gain_percentage <- gain$area[2]/sum(gain$area) * 100

  loss <- readr::read_csv(paste0(x1, "_", x2, "_temporal_pid_loss_area.csv"), col_names = c("classes", "area", "number"), show_col_types = FALSE)
  loss_npixel <- loss$number[2]
  loss_area <- loss$area[2]/1e4
  loss_percentage <- loss$area[2]/sum(loss$area) * 100

  stable <- readr::read_csv(paste0(x1, "_", x2, "_temporal_pid_stable_area.csv"), col_names = c("classes", "area", "number"), show_col_types = FALSE)
  stable_npixel <- stable$number[2]
  stable_area <- stable$area[2]/1e4
  stable_percentage <- stable$area[2]/sum(stable$area) * 100

  gain_patches <- readr::read_csv(paste0(x1, "_", x2, "_temporal_pid_gain_patches_pid.csv"), col_names = c("classes", "area", "number"), show_col_types = FALSE)
  gain_patches_number <- nrow(gain_patches)
  gain_patches_npixel <- sum(gain_patches$number)
  gain_patches_area <- sum(gain_patches$area)/1e4
  gain_patches_percentage <- sum(gain_patches$area)/sum(gain$area) * 100
  gain_patches_area_mn <- mean(gain_patches$area)/1e4
  gain_patches_area_sd <- sd(gain_patches$area)/1e4
  gain_patches_area_md <- median(gain_patches$area)/1e4
  gain_patches_area_mx <- max(gain_patches$area)/1e4

  loss_patches <- readr::read_csv(paste0(x1, "_", x2, "_temporal_pid_loss_patches_pid.csv"), col_names = c("classes", "area", "number", "percentage"), show_col_types = FALSE)
  loss_patches_number <- nrow(loss_patches)
  loss_patches_npixel <- sum(loss_patches$number)
  loss_patches_area <- sum(loss_patches$area)/1e4
  loss_patches_percentage <- sum(loss_patches$area)/sum(loss$area) * 100
  loss_patches_area_mn <- mean(loss_patches$area)/1e4
  loss_patches_area_sd <- sd(loss_patches$area)/1e4
  loss_patches_area_md <- median(loss_patches$area)/1e4
  loss_patches_area_mx <- max(loss_patches$area)/1e4

  gain_area_total <- gain_area + gain_patches_area
  loss_area_total <- loss_area + loss_patches_area
  gain_percentage_total <- gain_percentage + gain_patches_percentage
  loss_percentage_total <- loss_percentage + loss_patches_percentage

  balance_area <- gain_area - loss_area
  balance_area_total <- gain_area_total - loss_area_total
  balance_percentage <- gain_percentage - loss_percentage
  balance_percentage_total <- gain_percentage_total - loss_percentage_total
  balance_patches_number <- gain_patches_number - loss_patches_number
  balance_patches_area <- gain_patches_area - loss_patches_area
  balance_patches_percentage <- gain_patches_percentage - loss_patches_percentage

  area_total <- sum(gain$area)/1e4

  metrics <- tibble::tibble(metric = c("area_total",

                                       "gain_area",
                                       "loss_area",
                                       "balance_area",

                                       "gain_area_total",
                                       "loss_area_total",
                                       "balance_area_total",

                                       "stable_area",

                                       "gain_patches_area",
                                       "loss_patches_area",
                                       "balance_patches_area",

                                       "gain_patches_area_mn",
                                       "loss_patches_area_mn",

                                       "gain_patches_area_sd",
                                       "loss_patches_area_sd",

                                       "gain_patches_area_md",
                                       "loss_patches_area_md",

                                       "gain_patches_area_mx",
                                       "loss_patches_area_mx",

                                       "gain_patches_number",
                                       "loss_patches_number",
                                       "balance_patches_number"),
                            value = c(area_total,

                                      gain_area,
                                      loss_area,
                                      balance_area,

                                      gain_area_total,
                                      loss_area_total,
                                      balance_area_total,

                                      stable_area,

                                      gain_patches_area,
                                      loss_patches_area,
                                      balance_patches_area,

                                      gain_patches_area_mn,
                                      loss_patches_area_mn,

                                      gain_patches_area_sd,
                                      loss_patches_area_sd,

                                      gain_patches_area_md,
                                      loss_patches_area_md,

                                      gain_patches_area_mx,
                                      loss_patches_area_mx,

                                      gain_patches_number,
                                      loss_patches_number,
                                      balance_patches_number),
                            percentage = c(100,

                                           gain_percentage,
                                           loss_percentage,
                                           balance_percentage,

                                           gain_percentage_total,
                                           loss_percentage_total,
                                           balance_percentage_total,

                                           stable_percentage,

                                           gain_patches_percentage,
                                           loss_patches_percentage,
                                           balance_patches_percentage,

                                           NA,
                                           NA,
                                           NA,
                                           NA,
                                           NA,
                                           NA,
                                           NA,
                                           NA,
                                           NA,
                                           NA,
                                           NA)) %>%
    dplyr::mutate(value = round(value, 2),
                  percentage = round(percentage, 2))

  # export
  readr::write_csv(metrics, paste0(path, x1, "_", x2, "_temporal_patches_metrics.csv"))

  # clean
  unlink("x1_x2_temporal_pid.csv")
  unlink("table_x1_x2_gain_patches.txt")
  unlink("table_x1_x2_loss_patches.txt")
  unlink(paste0(x1, "_", x2, "_temporal_pid_gain_area.csv"))
  unlink(paste0(x1, "_", x2, "_temporal_pid_loss_area.csv"))
  unlink(paste0(x1, "_", x2, "_temporal_pid_stable_area.csv"))
  unlink(paste0(x1, "_", x2, "_temporal_pid_gain_patches_pid.csv"))
  unlink(paste0(x1, "_", x2, "_temporal_pid_loss_patches_pid.csv"))
  unlink("table_color.txt")

  rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = "x1")
  rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = "x2")
  rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = "x2_2")
  rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = "x1_x2_temporal_null")
  rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = "x1_x2_temporal_pid_gain_patches_null")
  rgrass::execGRASS(cmd = "g.remove", flags = c("f", "quiet"), type = "raster", name = "x1_x2_temporal_pid_loss_patches_null")
  rgrass::execGRASS(cmd = "g.remove", flags = c("b", "f", "quiet"), type = "raster", name = "x1_x2_temporal_pid")

}
