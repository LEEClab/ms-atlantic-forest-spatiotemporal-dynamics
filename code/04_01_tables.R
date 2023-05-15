#' ----
#' title: tables
#' author: mauricio vancine
#' date: 2022-12-09
#' operational system: gnu/linux - ubuntu - pop_os
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(paletteer)
library(fmsb)

# options
options(scipen = 1000)

# area --------------------------------------------------------------------

# areas
area_af_lim <- readr::read_csv("02_results/00_af_lim_area.csv",
                               col_names = c("class_area", "class_forest", "area", "n", "p")) %>%
  dplyr::summarise(area = sum(area)/1e4)
area_af_lim

area_af_lim_ar <- readr::read_csv("02_results/00_af_lim_ar_area.csv",
                                  col_names = c("class_area", "class_forest", "area", "n", "p")) %>%
  dplyr::summarise(area = sum(area)/1e4) %>%
  dplyr::mutate(p = area/area_af_lim*100)
area_af_lim_ar

area_af_lim_br <- readr::read_csv("02_results/00_af_lim_br_area.csv",
                                  col_names = c("class_area", "class_forest", "area", "n", "p")) %>%
  dplyr::summarise(area = sum(area)/1e4) %>%
  dplyr::mutate(p = area/area_af_lim*100)
area_af_lim_br

area_af_lim_py <- readr::read_csv("02_results/00_af_lim_py_area.csv",
                                  col_names = c("class_area", "class_forest", "area", "n", "p")) %>%
  dplyr::summarise(area = sum(area)/1e4) %>%
  dplyr::mutate(p = area/area_af_lim*100)
area_af_lim_py

area_af_lim_lawaf2006 <- readr::read_csv("02_results/00_af_lim_lawaf2006_area.csv",
                                         col_names = c("class_area", "class_forest", "area", "n", "p")) %>%
  dplyr::summarise(area = sum(area)/1e4) %>%
  dplyr::mutate(p = area/area_af_lim*100)
area_af_lim_lawaf2006

area_af_lim_dasilva_casteleti_2003 <- readr::read_csv("02_results/00_af_lim_dasilvacasteleti2003_area.csv",
                                                      col_names = c("class_area", "class_forest", "area", "n", "p")) %>%
  dplyr::summarise(area = sum(area)/1e4) %>%
  dplyr::mutate(p = area/area_af_lim*100)
area_af_lim_dasilva_casteleti_2003

area_af_lim_ibge2004 <- readr::read_csv("02_results/00_af_lim_ibge2004_area.csv",
                                        col_names = c("class_area", "class_forest", "area", "n", "p")) %>%
  dplyr::summarise(area = sum(area)/1e4) %>%
  dplyr::mutate(p = area/area_af_lim*100)
area_af_lim_ibge2004

area_af_lim_ibge2019 <- readr::read_csv("02_results/00_af_lim_ibge2019_area.csv",
                                        col_names = c("class_area", "class_forest", "area", "n", "p")) %>%
  dplyr::summarise(area = sum(area)/1e4) %>%
  dplyr::mutate(p = area/area_af_lim*100)
area_af_lim_ibge2019

area_af_lim_ecoregions2017 <- readr::read_csv("02_results/00_af_lim_ecoregions2017_area.csv",
                                              col_names = c("class_area", "class_forest", "area", "n", "p")) %>%
  dplyr::summarise(area = sum(area)/1e4) %>%
  dplyr::mutate(p = area/area_af_lim*100)
area_af_lim_ecoregions2017

area_roads_rails <- readr::read_csv("02_results/00_roads_rails_area.csv",
                                    col_names = c("roads_area", "class_forest", "area", "n", "p")) %>%
  dplyr::group_by(roads_area) %>%
  dplyr::summarise(area = sum(area)/1e4)
area_roads_rails

area_protected_area <- readr::read_csv("02_results/00_protected_area.csv",
                                       col_names = c("roads_area", "class_forest", "area", "n", "p")) %>%
  dplyr::group_by(roads_area) %>%
  dplyr::summarise(area = sum(area)/1e4)
area_protected_area

area_indigenous_territory <- readr::read_csv("02_results/00_indigenous_territory_area.csv",
                                             col_names = c("roads_area", "class_forest", "area", "n", "p")) %>%
  dplyr::group_by(roads_area) %>%
  dplyr::summarise(area = sum(area)/1e4)
area_indigenous_territory

# 1 number of patch and size distribution ---------------------------------

# years
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
years

### data ----
# list files
files_area <- dir(path = "02_results", pattern = "01_", full.names = TRUE) %>%
  stringr::str_subset("resume", negate = TRUE) %>%
  stringr::str_subset("af_lim_forest|af_lim_natural") %>%
  stringr::str_subset(paste0(years, collapse = "|"))
files_area

# import data
data_area_resume <- NULL

for(i in files_area){

  print(i)

  name_area <- basename(i) %>%
    stringr::str_replace("01_mapbiomas_brazil_af_trinacional_", "") %>%
    stringr::str_replace(".csv", "") %>%
    stringr::str_replace("_af_lim", "") %>%
    stringr::str_replace("_pid", "") %>%
    stringr::str_replace("_area", "")

  year <- gsub("[^0-9]", " ", name_area) %>%
    stringr::str_trim()

  scenario <- gsub("[^a-zA-Z]", " ", name_area) %>%
    stringr::str_trim()

  data_area_i <- readr::read_csv(i, col_names = c("values", "area", "n"), col_types = cols()) %>%
    dplyr::mutate(area_ha = area/1e4) %>%
    dplyr::mutate(scenario = name_area)

  data_area_resume_i <- tibble::tibble(year = year,
                                       scenario = scenario,
                                       n_patches = nrow(data_area_i),
                                       total_area_ha = round(sum(data_area_i$area_ha), 2),
                                       mean_area_ha = round(mean(data_area_i$area_ha), 2),
                                       sd_area_ha = round(sd(data_area_i$area_ha), 2),
                                       cv_area_ha = round(sd_area_ha/mean_area_ha, 2),
                                       min_area_ha = min(data_area_i$area_ha),
                                       first_quarter_area_ha = quantile(data_area_i$area_ha, probs = .25),
                                       median_area_ha = median(data_area_i$area_ha),
                                       third_quarter_area_ha = quantile(data_area_i$area_ha, probs = .75),
                                       max_area_ha = max(data_area_i$area_ha))

  data_area_resume <- dplyr::bind_rows(data_area_resume, data_area_resume_i)

}

data_area_resume

# resume
data_area_resume <- data_area_resume %>%
  dplyr::mutate(total_area_ha = round(total_area_ha, 0),
                max_area_ha = round(max_area_ha, 0)) %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
    scenario == "forest roads rails" ~ "Forest vegetation (trimmed by roads and rails)",
    scenario == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
    scenario == "natural roads rails" ~ "Natural vegetation (trimmed by roads and rails)")
  )
data_area_resume

# export
readr::write_csv(data_area_resume,
                 paste0("04_tables/01_data_pid_area_resume.csv"))

# percentage
((data_area_resume[17, ]$n_patches - data_area_resume[1, ]$n_patches)/data_area_resume[1, ]$n_patches) * 100 # forest n patches percent - 1986-2020
((data_area_resume[18, ]$n_patches - data_area_resume[2, ]$n_patches)/data_area_resume[2, ]$n_patches) * 100 # forest roads n patches percent - 1986-2020
((data_area_resume[19, ]$n_patches - data_area_resume[3, ]$n_patches)/data_area_resume[3, ]$n_patches) * 100 # natural n patches percent - 1986-2020
((data_area_resume[20, ]$n_patches - data_area_resume[4, ]$n_patches)/data_area_resume[4, ]$n_patches) * 100 # natural roads n patches percent - 1986-2020

((data_area_resume[29, ]$n_patches - data_area_resume[17, ]$n_patches)/data_area_resume[17, ]$n_patches) * 100 # forest n patches percent - 1986-2020
((data_area_resume[30, ]$n_patches - data_area_resume[18, ]$n_patches)/data_area_resume[18, ]$n_patches) * 100 # forest roads n patches percent - 1986-2020
((data_area_resume[31, ]$n_patches - data_area_resume[19, ]$n_patches)/data_area_resume[19, ]$n_patches) * 100 # natural n patches percent - 1986-2020
((data_area_resume[32, ]$n_patches - data_area_resume[20, ]$n_patches)/data_area_resume[20, ]$n_patches) * 100 # natural roads n patches percent - 1986-2020

((data_area_resume[29, ]$n_patches - data_area_resume[1, ]$n_patches)/data_area_resume[1, ]$n_patches) * 100 # forest n patches percent - 1986-2020
((data_area_resume[30, ]$n_patches - data_area_resume[2, ]$n_patches)/data_area_resume[2, ]$n_patches) * 100 # forest roads n patches percent - 1986-2020
((data_area_resume[31, ]$n_patches - data_area_resume[3, ]$n_patches)/data_area_resume[3, ]$n_patches) * 100 # natural n patches percent - 1986-2020
((data_area_resume[32, ]$n_patches - data_area_resume[4, ]$n_patches)/data_area_resume[4, ]$n_patches) * 100 # natural roads n patches percent - 1986-2020

((data_area_resume[17, ]$mean_area_ha - data_area_resume[1, ]$mean_area_ha)/data_area_resume[1, ]$mean_area_ha) * 100 # forest n patches percent - 1986-2020
((data_area_resume[18, ]$mean_area_ha - data_area_resume[2, ]$mean_area_ha)/data_area_resume[2, ]$mean_area_ha) * 100 # forest roads n patches percent - 1986-2020
((data_area_resume[19, ]$mean_area_ha - data_area_resume[3, ]$mean_area_ha)/data_area_resume[3, ]$mean_area_ha) * 100 # natural n patches percent - 1986-2020
((data_area_resume[20, ]$mean_area_ha - data_area_resume[4, ]$mean_area_ha)/data_area_resume[4, ]$mean_area_ha) * 100 # natural roads n patches percent - 1986-2020

((data_area_resume[29, ]$mean_area_ha - data_area_resume[17, ]$mean_area_ha)/data_area_resume[17, ]$mean_area_ha) * 100 # forest n patches percent - 1986-2020
((data_area_resume[30, ]$mean_area_ha - data_area_resume[18, ]$mean_area_ha)/data_area_resume[18, ]$mean_area_ha) * 100 # forest roads n patches percent - 1986-2020
((data_area_resume[31, ]$mean_area_ha - data_area_resume[19, ]$mean_area_ha)/data_area_resume[19, ]$mean_area_ha) * 100 # natural n patches percent - 1986-2020
((data_area_resume[32, ]$mean_area_ha - data_area_resume[20, ]$mean_area_ha)/data_area_resume[20, ]$mean_area_ha) * 100 # natural roads n patches percent - 1986-2020

((data_area_resume[29, ]$mean_area_ha - data_area_resume[1, ]$mean_area_ha)/data_area_resume[1, ]$mean_area_ha) * 100 # forest mean area patches percent - 1986-2020
((data_area_resume[30, ]$mean_area_ha - data_area_resume[2, ]$mean_area_ha)/data_area_resume[2, ]$mean_area_ha) * 100 # forest roads mean area patches percent - 1986-2020
((data_area_resume[31, ]$mean_area_ha - data_area_resume[3, ]$mean_area_ha)/data_area_resume[3, ]$mean_area_ha) * 100 # natural mean area patches percent - 1986-2020
((data_area_resume[32, ]$mean_area_ha - data_area_resume[4, ]$mean_area_ha)/data_area_resume[4, ]$mean_area_ha) * 100 # natural roads mean area patches percent - 1986-2020

max_t <- tibble()
for(i in seq(1, 31, 2)){

  max_i <- ((data_area_resume[i+1, ]$max_area_ha - data_area_resume[i, ]$max_area_ha)/data_area_resume[i, ]$max_area_ha) * 100 # forest max area patches 1986 percent - trimmed roads
  max_p <- tibble(scenario = paste0(data_area_resume[i, 1], "_", data_area_resume[i, 2]),
                  max_por = max_i)
  max_t <- dplyr::bind_rows(max_t, max_p)

}
max_t

((data_area_resume[29, ]$max_area_ha - data_area_resume[1, ]$max_area_ha)/data_area_resume[1, ]$max_area_ha) * 100 # forest max area patches percent - 1986-2020
((data_area_resume[30, ]$max_area_ha - data_area_resume[2, ]$max_area_ha)/data_area_resume[2, ]$max_area_ha) * 100 # forest roads max area patches percent - 1986-2020
((data_area_resume[31, ]$max_area_ha - data_area_resume[3, ]$max_area_ha)/data_area_resume[3, ]$max_area_ha) * 100 # natural max area patches percent - 1986-2020
((data_area_resume[32, ]$max_area_ha - data_area_resume[4, ]$max_area_ha)/data_area_resume[4, ]$max_area_ha) * 100 # natural roads max area patches percent - 1986-2020

# data
data_area_resume_plot <- readr::read_csv("04_tables/01_data_area_resume_plot.csv")
data_area_resume_plot

data_area_resume_plot_25kha <- data_area_resume_plot %>%
  dplyr::filter(year %in% c(1986, 2020),
                scenario %in% c("Forest Roads Rails", "Natural Roads Rails"),
                class %in% c("25000-50000", "50000-100000", "100000-250000", "250000-500000", "500000-1000000", ">1000000")) %>%
  group_by(year, scenario) %>%
  summarise(class_area_per_sum = sum(class_area_per),
            class_n_per_sum = round(sum(class_n_per), 1))
data_area_resume_plot_25kha


data_area_resume_plot_50ha <- data_area_resume_plot %>%
  dplyr::filter(year %in% c(1986, 2020),
                class %in% c("<1", "1-5", "5-10", "10-50")) %>%
  group_by(year, scenario) %>%
  summarise(class_area_per_sum = sum(class_area_per),
            class_n_per_sum = round(sum(class_n_per), 1))
data_area_resume_plot_50ha

data_area_resume_plot_10ha <- data_area_resume_plot %>%
  dplyr::filter(year %in% c(1986, 2020),
                class %in% c("<1", "1-5", "5-10")) %>%
  group_by(year, scenario) %>%
  summarise(class_area_per_sum = sum(class_area_per),
            class_n_per_sum = round(sum(class_n_per), 0))
data_area_resume_plot_10ha

data_area_resume_plot_5ha <- data_area_resume_plot %>%
  dplyr::filter(year %in% c(1986, 2020),
                class %in% c("<1", "1-5")) %>%
  group_by(year, scenario) %>%
  summarise(class_area_per_sum = sum(class_area_per),
            class_n_per_sum = round(sum(class_n_per), 0))
data_area_resume_plot_5ha

data_area_resume_plot_1ha <- data_area_resume_plot %>%
  dplyr::filter(year %in% c(1986, 2020),
                class %in% c("<1")) %>%
  group_by(year, scenario) %>%
  summarise(class_area_per_sum = sum(class_area_per),
            class_n_per_sum = round(sum(class_n_per), 0))
data_area_resume_plot_1ha

data_area_resume_plot_50ha_fv <- data_area_resume_plot %>%
  dplyr::filter(year %in% c(1986, 2020),
                class %in% c("<1", "1-5", "5-10", "10-50"),
                scenario %in% c("Forest", "Forest Roads Rails")) %>%
  group_by(year, scenario) %>%
  summarise(class_area_per_sum = sum(class_area_per),
            class_n_per_sum = round(sum(class_n_per), 0))
data_area_resume_plot_50ha_fv

data_area_resume_plot_50ha_nv <- data_area_resume_plot %>%
  dplyr::filter(year %in% c(1986, 2020),
                class %in% c("<1", "1-5", "5-10", "10-50"),
                scenario %in% c("Natural", "Natural Roads Rails")) %>%
  group_by(year, scenario) %>%
  summarise(class_area_per_sum = sum(class_area_per),
            class_n_per_sum = round(sum(class_n_per), 0))
data_area_resume_plot_50ha_nv

## temporal analyses ----



# 2 habitat cover ---------------------------------------------------------

# years
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
years

# classes
classes_habitat_cover <- tibble::tibble(
  classes = c(0, 1, 10, 3, 4, 5, 11, 12, 13, 32, 49, 50),
  class_abbreviation = c("MA", "FV", "NV", "FF", "SF", "MG", "WT", "GL", "OF",
                         "ST", "WS", "HS"),
  class_description = c(
    "Matrix", "Forest vegetation", "Natural vegetation", "Forest formation",
    "Savanna formation", "Mangrove", "Wetland", "Grassland",
    "Other non forest formations", "Hypersaline Tidal Flat",
    "Wooded sandbank vegetation", "Herbaceous sandbank vegetation"))
classes_habitat_cover

### values total ----

# list files
files_habitat_cover <- dir(path = "02_results", pattern = "02_mapbiomas", full.names = TRUE) %>%
  stringr::str_subset("ar.csv|br.csv|py.csv", negate = TRUE) %>%
  stringr::str_subset("classes", negate = FALSE) %>%
  stringr::str_subset(paste0(years, collapse = "|"))
files_habitat_cover

# import data
data_habitat_cover <- NULL
for(i in files_habitat_cover){

  names_habitat_cover <- i %>%
    basename() %>%
    stringr::str_split("_", simplify = TRUE)

  year <- names_habitat_cover[, 6]
  scenario <- ifelse(length(names_habitat_cover) > 12,
                     paste0(names_habitat_cover[, 9], "_", names_habitat_cover[, 11], "_", names_habitat_cover[, 12]),
                     names_habitat_cover[, 9])


  data_habitat_cover_i <- readr::read_csv(i, col_names = c("classes", "area", "n", "p"), col_types = cols()) %>%
    dplyr::mutate(year = year,
                  scenario = scenario,
                  .before = 1) %>%
    dplyr::mutate(area_ha = area/1e4, .after = area) %>%
    dplyr::select(-area, -n) %>%
    dplyr::left_join(classes_habitat_cover, by = "classes") %>%
    dplyr::arrange(classes) %>%
    dplyr::mutate(p = as.numeric(stringr::str_replace(p, "%", ""))) %>%
    dplyr::relocate(class_abbreviation, class_description, .after = classes)

  data_habitat_cover <- dplyr::bind_rows(data_habitat_cover, data_habitat_cover_i)

}
data_habitat_cover

data_habitat_cover_resume <- data_habitat_cover %>%
  dplyr::filter(classes != 0) %>%
  dplyr::group_by(year, scenario) %>%
  dplyr::summarise(p = sum(p),
                   area_ha = sum(area_ha)) %>%
  dplyr::bind_cols(classes = rep(c(1, 1, 10, 10), times = 8),
                   class_abbreviation = rep(c("FV", "FV", "NV", "NV"), times = 8),
                   class_description = rep(c("Forest vegetation","Forest vegetation",
                                             "Natural vegetation", "Natural vegetation"), times = 8))
data_habitat_cover_resume

data_habitat_cover_resume_total <- dplyr::bind_rows(data_habitat_cover, data_habitat_cover_resume) %>%
  dplyr::arrange(year, scenario) %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
    scenario == "forest_roads_rails" ~ "Forest vegetation (trimmed by roads and rails)",
    scenario == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
    scenario == "natural_roads_rails" ~ "Natural vegetation (trimmed by roads and rails)"))
data_habitat_cover_resume_total

# export
readr::write_csv(data_habitat_cover_resume_total, "04_tables/02_data_habitat_cover_resume_total.csv")


### values br ----

# list files
files_habitat_cover_br <- dir(path = "02_results", pattern = "02_mapbiomas", full.names = TRUE) %>%
  stringr::str_subset("br.csv") %>%
  stringr::str_subset(paste0(years, collapse = "|"))
files_habitat_cover_br

# import data
data_habitat_cover_br <- NULL
for(i in files_habitat_cover_br){

  names_habitat_cover_br <- i %>%
    basename() %>%
    stringr::str_split("_", simplify = TRUE)

  year_br <- names_habitat_cover_br[, 6]
  scenario_br <- ifelse(length(names_habitat_cover_br) > 13,
                        paste0(names_habitat_cover_br[, 9], "_", names_habitat_cover_br[, 11], "_", names_habitat_cover_br[, 12]),
                        names_habitat_cover_br[, 9])


  data_habitat_cover_i_br <- readr::read_csv(i, col_names = c("classes", "area", "n", "p"), col_types = cols()) %>%
    dplyr::mutate(year = year_br,
                  scenario = scenario_br,
                  .before = 1) %>%
    dplyr::mutate(area_ha = area/1e4, .after = area) %>%
    dplyr::select(-area, -n) %>%
    dplyr::left_join(classes_habitat_cover, by = "classes") %>%
    dplyr::arrange(classes) %>%
    dplyr::mutate(p = as.numeric(stringr::str_replace(p, "%", "")))

  data_habitat_cover_br <- dplyr::bind_rows(data_habitat_cover_br, data_habitat_cover_i_br)

}
data_habitat_cover_br

data_habitat_cover_br_resume <- data_habitat_cover_br %>%
  dplyr::filter(classes != 0) %>%
  dplyr::group_by(year, scenario) %>%
  dplyr::summarise(p = sum(p),
                   area_ha = sum(area_ha)) %>%
  dplyr::bind_cols(classes = rep(c(1, 1, 10, 10), times = 8),
                   class_abbreviation = rep(c("FV", "FV", "NV", "NV"), times = 8),
                   class_description = rep(c("Forest vegetation","Forest vegetation",
                                             "Natural vegetation", "Natural vegetation"), times = 8))
data_habitat_cover_br_resume

data_habitat_cover_br_resume_total <- dplyr::bind_rows(data_habitat_cover_br, data_habitat_cover_br_resume) %>%
  dplyr::arrange(classes) %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
    scenario == "forest_roads_rails" ~ "Forest vegetation (trimmed by roads and rails)",
    scenario == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
    scenario == "natural_roads_rails" ~ "Natural vegetation (trimmed by roads and rails)"))
data_habitat_cover_br_resume_total

readr::write_csv(data_habitat_cover_br_resume_total,
                 paste0("04_tables/02_data_habitat_cover_br_resume.csv"))

### values ar ----

# list files
files_habitat_cover_ar <- dir(path = "02_results", pattern = "02_mapbiomas", full.names = TRUE) %>%
  stringr::str_subset("ar.csv") %>%
  stringr::str_subset(paste0(years, collapse = "|"))
files_habitat_cover_ar

# import data
data_habitat_cover_ar <- NULL
for(i in files_habitat_cover_ar){

  names_habitat_cover_ar <- i %>%
    basename() %>%
    stringr::str_split("_", simplify = TRUE)

  year_ar <- names_habitat_cover_ar[, 6]
  scenario_ar <- ifelse(length(names_habitat_cover_ar) > 13,
                        paste0(names_habitat_cover_ar[, 9], "_", names_habitat_cover_ar[, 11], "_", names_habitat_cover_ar[, 12]),
                        names_habitat_cover_ar[, 9])


  data_habitat_cover_i_ar <- readr::read_csv(i, col_names = c("classes", "area", "n", "p"), col_types = cols()) %>%
    dplyr::mutate(year = year_ar,
                  scenario = scenario_ar,
                  .before = 1) %>%
    dplyr::mutate(area_ha = area/1e4, .after = area) %>%
    dplyr::select(-area, -n) %>%
    dplyr::left_join(classes_habitat_cover, by = "classes") %>%
    dplyr::arrange(classes) %>%
    dplyr::mutate(p = as.numeric(stringr::str_replace(p, "%", "")))

  data_habitat_cover_ar <- dplyr::bind_rows(data_habitat_cover_ar, data_habitat_cover_i_ar)

}
data_habitat_cover_ar

data_habitat_cover_ar_resume <- data_habitat_cover_ar %>%
  dplyr::filter(classes != 0) %>%
  dplyr::group_by(year, scenario) %>%
  dplyr::summarise(p = sum(p),
                   area_ha = sum(area_ha)) %>%
  dplyr::bind_cols(classes = rep(c(1, 1, 10, 10), times = 8),
                   class_abbreviation = rep(c("FV", "FV", "NV", "NV"), times = 8),
                   class_description = rep(c("Forest vegetation","Forest vegetation",
                                             "Natural vegetation", "Natural vegetation"), times = 8))
data_habitat_cover_ar_resume

data_habitat_cover_ar_resume_total <- dplyr::bind_rows(data_habitat_cover_ar, data_habitat_cover_ar_resume) %>%
  dplyr::arrange(classes) %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
    scenario == "forest_roads_rails" ~ "Forest vegetation (trimmed by roads and rails)",
    scenario == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
    scenario == "natural_roads_rails" ~ "Natural vegetation (trimmed by roads and rails)"))
data_habitat_cover_ar_resume_total

readr::write_csv(data_habitat_cover_ar_resume_total,
                 paste0("04_tables/02_data_habitat_cover_ar_resume.csv"))

### values py ----

# list files
files_habitat_cover_py <- dir(path = "02_results", pattern = "02_mapbiomas", full.names = TRUE) %>%
  stringr::str_subset("py.csv") %>%
  stringr::str_subset(paste0(years, collapse = "|"))
files_habitat_cover_py

# import data
data_habitat_cover_py <- NULL
for(i in files_habitat_cover_py){

  names_habitat_cover_py <- i %>%
    basename() %>%
    stringr::str_split("_", simplify = TRUE)

  year_py <- names_habitat_cover_py[, 6]
  scenario_py <- ifelse(length(names_habitat_cover_py) > 13,
                          paste0(names_habitat_cover_py[, 9], "_", names_habitat_cover_py[, 11], "_", names_habitat_cover_py[, 12]),
                          names_habitat_cover_py[, 9])


  data_habitat_cover_i_py <- readr::read_csv(i, col_names = c("classes", "area", "n", "p"), col_types = cols()) %>%
    dplyr::mutate(year = year_py,
                  scenario = scenario_py,
                  .before = 1) %>%
    dplyr::mutate(area_ha = area/1e4, .after = area) %>%
    dplyr::select(-area, -n) %>%
    dplyr::left_join(classes_habitat_cover, by = "classes") %>%
    dplyr::arrange(classes) %>%
    dplyr::mutate(p = as.numeric(stringr::str_replace(p, "%", "")))

  data_habitat_cover_py <- dplyr::bind_rows(data_habitat_cover_py, data_habitat_cover_i_py)

}
data_habitat_cover_py

data_habitat_cover_py_resume <- data_habitat_cover_py %>%
  dplyr::filter(classes != 0) %>%
  dplyr::group_by(year, scenario) %>%
  dplyr::summarise(p = sum(p),
                   area_ha = sum(area_ha)) %>%
  dplyr::bind_cols(classes = rep(c(1, 1, 10, 10), times = 8),
                   class_abbreviation = rep(c("FV", "FV", "NV", "NV"), times = 8),
                   class_description = rep(c("Forest vegetation","Forest vegetation",
                                               "Natural vegetation", "Natural vegetation"), times = 8))
data_habitat_cover_py_resume

data_habitat_cover_py_resume_total <- dplyr::bind_rows(data_habitat_cover_py, data_habitat_cover_py_resume) %>%
  dplyr::arrange(classes) %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
    scenario == "forest_roads_rails" ~ "Forest vegetation (trimmed by roads and rails)",
    scenario == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
    scenario == "natural_roads_rails" ~ "Natural vegetation (trimmed by roads and rails)"))
data_habitat_cover_py_resume_total

readr::write_csv(data_habitat_cover_py_resume_total,
                 paste0("04_tables/02_data_habitat_cover_py_resume.csv"))


### values limits ----
# list files
files_limits_habitat_cover <- dir(path = "02_results", pattern = "02_", full.names = TRUE) %>%
  stringr::str_subset("_2020_") %>%
  stringr::str_subset("af_lim_forest|af_lim_natural", negate = TRUE)
files_limits_habitat_cover

files_limits_area <- dir(path = "02_results", pattern = "01_", full.names = TRUE) %>%
  stringr::str_subset("_2020_") %>%
  stringr::str_subset("af_lim_forest|af_lim_natural", negate = TRUE)
files_limits_area

data_limits_habitat_cover <- NULL
for(i in files_limits_habitat_cover){

  names_habitat_cover <- i %>%
    basename() %>%
    stringr::str_split("_", simplify = TRUE)

  limit <- ifelse(length(names_habitat_cover) > 12,
                  paste0(names_habitat_cover[, 9]),
                  names_habitat_cover[, 9])

  scenario <- ifelse(length(names_habitat_cover) > 12,
                       paste0(names_habitat_cover[, 10], "_", names_habitat_cover[, 11], "_", names_habitat_cover[, 12]),
                       names_habitat_cover[, 10])


  data_limits_habitat_cover_i <- readr::read_csv(i, col_names = c("values", "area", "n", "p"), col_types = cols()) %>%
    dplyr::mutate(limit = limit,
                  scenario = scenario,
                  .before = 1) %>%
    dplyr::mutate(area_ha = area/1e4, .after = area) %>%
    dplyr::select(-area, -n) %>%
    dplyr::arrange(values) %>%
    dplyr::mutate(p = as.numeric(stringr::str_replace(p, "%", "")))

  data_limits_habitat_cover <- dplyr::bind_rows(data_limits_habitat_cover, data_limits_habitat_cover_i)

}
data_limits_habitat_cover

data_limits_habitat_cover_filter <- data_limits_habitat_cover %>%
  dplyr::filter(values == 1) %>%
  dplyr::select(-values) %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
    scenario == "forest_roads_rails" ~ "Forest vegetation (trimmed by roads and rails)",
    scenario == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
    scenario == "natural_roads_rails" ~ "Natural vegetation (trimmed by roads and rails)"))
data_limits_habitat_cover_filter

data_limits_area <- NULL
for(i in files_limits_area){

  name_area <- basename(i) %>%
    stringr::str_split("_", simplify = TRUE)

  limit <- ifelse(length(name_area) > 12,
                  paste0(name_area[, 9]),
                  name_area[, 9])

  scenario <- ifelse(length(name_area) > 12,
                       paste0(name_area[, 10], "_", name_area[, 11], "_", name_area[, 12]),
                       name_area[, 10])

  data_area <- readr::read_csv(i, col_names = c("values", "area", "n"), col_types = cols()) %>%
    dplyr::mutate(area_ha = area/1e4)

  data_limits_area_i <- tibble::tibble(limit = limit,
                                       scenario = scenario,
                                       n_patches = nrow(data_area),
                                       total_area_ha = round(sum(data_area$area_ha), 2),
                                       mean_area_ha = round(mean(data_area$area_ha), 2),
                                       sd_area_ha = round(sd(data_area$area_ha), 2),
                                       cv_area_ha = round(sd_area_ha/mean_area_ha, 2),
                                       min_area_ha = min(data_area$area_ha),
                                       first_quarter_area_ha = quantile(data_area$area_ha, probs = .25),
                                       median_area_ha = median(data_area$area_ha),
                                       third_quarter_area_ha = quantile(data_area$area_ha, probs = .75),
                                       max_area_ha = max(data_area$area_ha))

  data_limits_area <- dplyr::bind_rows(data_limits_area, data_limits_area_i)

}
data_limits_area

data_limits_habitat_cover_area <- data_limits_area %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
    scenario == "forest_roads_rails" ~ "Forest vegetation (trimmed by roads and rails)",
    scenario == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
    scenario == "natural_roads_rails" ~ "Natural vegetation (trimmed by roads and rails)")) %>%
  dplyr::left_join(data_limits_habitat_cover_filter)
data_limits_habitat_cover_area

data_limits_habitat_cover_area_sel <- data_limits_habitat_cover_area %>%
  dplyr::select(1, 2, p, 3, 4, 5) %>%
  dplyr::slice(c(17:20, 1:4, 9:16, 5:8)) %>%
  dplyr::mutate(total_area_ha = round(total_area_ha, 0))
data_limits_habitat_cover_area_sel

readr::write_csv(data_limits_habitat_cover_area_sel, "04_tables/02_data_area_limits.csv")

# 3  core and edge area ---------------------------------------------------

# years
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
years

### data ----
# list files
files_core_edge_area <- dir(path = "02_results", pattern = "_edge_area.csv", full.names = TRUE) %>%
  stringr::str_subset("data_core", negate = TRUE) %>%
  stringr::str_subset(paste0(years, collapse = "|"))
files_core_edge_area

# import data
data_core_edge_area <- NULL
for(i in files_core_edge_area){

  print(i)

  name_core_edge_area <- basename(i) %>%
    stringr::str_replace("03_mapbiomas_brazil_af_trinacional_", "") %>%
    stringr::str_replace(".csv", "") %>%
    stringr::str_replace("_af_lim", "") %>%
    stringr::str_replace("_edge_area", "")

  name_core_edge_area_scenario <- gsub("[^a-zA-Z]", " ", name_core_edge_area) %>%
    stringr::str_trim()

  name_core_edge_area_year <- gsub("[^0-9]", " ", name_core_edge_area) %>%
    stringr::str_trim()

  # data
  data_core_edge_area_i <- readr::read_csv(i, col_names = c("vegetation", "edge_dist", "area", "n"),
                                           col_types = readr::cols()) %>%
    dplyr::slice(-1) %>%
    dplyr::mutate(area_ha = area/1e4, .after = area) %>%
    dplyr::mutate(per = n/sum(n)*100) %>%
    dplyr::mutate(per_cumsum = cumsum(per)) %>%
    dplyr::mutate(year = name_core_edge_area_year,
                  scenario = name_core_edge_area_scenario,
                  .before = 1)

  # combine
  data_core_edge_area <- dplyr::bind_rows(data_core_edge_area, data_core_edge_area_i)

}

data_core_edge_area <- data_core_edge_area %>%
  dplyr::select(year, scenario, edge_dist, area_ha, per, per_cumsum)
data_core_edge_area

# resume
data_core_edge_area_30m_forest <- data_core_edge_area %>%
  dplyr::filter(scenario == "forest roads rails", edge_dist == 30) %>%
  dplyr::group_by(year, scenario)
data_core_edge_area_30m_forest

data_core_edge_area_30m_natural <- data_core_edge_area %>%
  dplyr::filter(scenario == "natural roads rails", edge_dist == 30)
data_core_edge_area_30m_natural

data_core_edge_area_90m_forest <- data_core_edge_area %>%
  dplyr::filter(scenario == "forest roads rails", edge_dist == 90)
data_core_edge_area_90m_forest

data_core_edge_area_90m_natural <- data_core_edge_area %>%
  dplyr::filter(scenario == "natural roads rails", edge_dist == 90)
data_core_edge_area_90m_natural

data_core_edge_area_240m_forest <- data_core_edge_area %>%
  dplyr::filter(scenario == "forest roads rails", edge_dist == 240)
data_core_edge_area_240m_forest

data_core_edge_area_240m_natural <- data_core_edge_area %>%
  dplyr::filter(scenario == "natural roads rails", edge_dist == 240)
data_core_edge_area_240m_natural

data_core_edge_area_500m_forest <- data_core_edge_area %>%
  dplyr::filter(scenario == "forest roads rails", edge_dist >= 500) %>%
  group_by(scenario, year) %>%
  summarise(per_sum = sum(per))
data_core_edge_area_500m_forest

data_core_edge_area_500m_natural <- data_core_edge_area %>%
  dplyr::filter(scenario == "natural roads rails", edge_dist > 500) %>%
  group_by(scenario, year) %>%
  summarise(per_sum = sum(per))
data_core_edge_area_500m_natural

data_core_edge_area %>%
  dplyr::filter(scenario == "forest roads rails") %>%
  dplyr::pull(edge_dist) %>%
  max()

data_core_edge_area %>%
  dplyr::filter(scenario == "natural roads rails") %>%
  dplyr::pull(edge_dist) %>%
  max()

# export
data_core_edge_area <- data_core_edge_area %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
    scenario == "forest roads rails" ~ "Forest vegetation (trimmed by roads and rails)",
    scenario == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
    scenario == "natural roads rails" ~ "Natural vegetation (trimmed by roads and rails)"))
data_core_edge_area

data_core_edge_area_filter <- data_core_edge_area %>%
  dplyr::mutate(per = round(per, 4),
                per_cumsum = round(per_cumsum, 2)) %>%
  dplyr::filter(edge_dist %in% c(30, 90, 240, 510, 1020, 2520, 5010, 10553, 30360)) %>%
  dplyr::select(year, scenario, edge_dist, area_ha, per, per_cumsum)
data_core_edge_area_filter

readr::write_csv(data_core_edge_area, "04_tables/03_data_core_edge_area.csv")
readr::write_csv(data_core_edge_area_filter, "04_tables/03_data_core_edge_area_filter.csv")

# 4 functional connectivity ---------------------------------------------

### data ----

# list files
files_connectivity <- dir(path = "02_results", pattern = "confun", full.names = TRUE)
files_connectivity

# import data
scenarios <- c("forest", "natural", "forest_roads_rails", "natural_roads_rails")
gaps <- c("0060", "0120", "0180", "0240", "0300", "0600", "0900", "1200", "1500")

data_connectivity <- NULL

for(i in years){

  print(i)

  files_connectivity_i <- stringr::str_subset(files_connectivity, paste0("_", i, "_"))
  data_connectivity_i <- NULL

  for(j in scenarios){

    area_i <- readr::read_csv(paste0("02_results/01_mapbiomas_brazil_af_trinacional_", i, "_af_lim_", sub("_confun", "", j), "_pid_area.csv"),
                              col_names = c("pid", "area", "n")) %>%
      dplyr::mutate(area_ha = area/1e4) %>%
      dplyr::pull(area_ha)

    total_area_i <- sum(area_i)
    mean_area_i <- mean(area_i)
    max_area_i <- max(area_i)

    files_connectivity_j <- stringr::str_subset(files_connectivity_i, j)
    data_connectivity_j <- NULL

    for(k in gaps){

      files_connectivity_k <- stringr::str_subset(files_connectivity_j, paste0("area", k))

      data_connectivity_k <- readr::read_csv(files_connectivity_k, col_names = c("pid", "area", "n")) %>%
        dplyr::mutate(area_ha = area/1e4,
                      gap_crossing = k) %>%
        dplyr::group_by(gap_crossing) %>%
        dplyr::summarise(area_mean = mean(area_ha),
                         high_cluster = max(area_ha)/total_area_i * 100) %>%
        dplyr::mutate(year = i, scenario = sub("_confun", "", j), .before = 1)

      data_connectivity_j <- dplyr::bind_rows(data_connectivity_j, data_connectivity_k)

    }

    data_connectivity_i <- dplyr::bind_rows(data_connectivity_i, data_connectivity_j) %>%
      dplyr::bind_rows(tibble::tibble(year = i, scenario = sub("_confun", "", j), gap_crossing = "0",
                                      area_mean = mean_area_i, high_cluster = max_area_i/total_area_i * 100)) %>%
      dplyr::arrange(gap_crossing)

  }

  data_connectivity <- dplyr::bind_rows(data_connectivity, data_connectivity_i)

}

data_connectivity

# summary
data_connectivity_summary <- data_connectivity %>%
  dplyr::arrange(year, scenario) %>%
  dplyr::mutate(area_mean = round(area_mean, 2),
                high_cluster = round(high_cluster, 2)) %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
    scenario == "forest_roads_rails" ~ "Forest vegetation (trimmed by roads and rails)",
    scenario == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
    scenario == "natural_roads_rails" ~ "Natural vegetation (trimmed by roads and rails)"))
data_connectivity_summary

# export
readr::write_csv(data_connectivity_summary, "04_tables/04_data_connectivity_resume.csv")

# 5 mean isolation --------------------------------------------------------

### data ----

# import mean data
files_isolation <- dir(path = "02_results", pattern = "isolation", full.names = TRUE) %>%
  stringr::str_subset("_quantile", negate = TRUE) %>%
  stringr::str_subset("data_isolation", negate = TRUE) %>%
  sort()
files_isolation

data_isolation_mean <- NULL
for(i in files_isolation){

  names_isolation <- i %>%
    basename() %>%
    stringr::str_split("_", simplify = TRUE)

  data_isolation_mean_i <- readr::read_csv(i) %>%
    dplyr::mutate(scenario = ifelse(length(names_isolation) == 13,
                                      paste0(names_isolation[, 9], "_", names_isolation[, 10], "_", names_isolation[, 11]),
                                      names_isolation[, 9]),
                  year = names_isolation[, 6],
                  area = ifelse(length(names_isolation) == 13,
                                stringr::str_extract(names_isolation[, 13], pattern = "[0-9]+"),
                                stringr::str_extract(names_isolation[, 11], pattern = "[0-9]+")),
                  .before = 1)
  data_isolation_mean_i

  data_isolation_mean <- dplyr::bind_rows(data_isolation_mean, data_isolation_mean_i)

}
data_isolation_mean

# resume
data_isolation_resume <- data_isolation_mean %>%
  dplyr::select(year, scenario, area, mean, stddev) %>%
  dplyr::arrange(year, scenario, area) %>%
  dplyr::mutate(mean = round(mean, 2),
                stddev = round(stddev, 2)) %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
    scenario == "forest_roads_rails" ~ "Forest vegetation (trimmed by roads and rails)",
    scenario == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
    scenario == "natural_roads_rails" ~ "Natural vegetation (trimmed by roads and rails)"))
data_isolation_resume

readr::write_csv(data_isolation_resume, "04_tables/05_data_isolation_resume.csv")

# filter
data_isolation_resume_000_forest <- data_isolation_resume %>%
  dplyr::filter(area == "0000",
                scenario == "Forest vegetation (trimmed by roads and rails)") %>%
  dplyr::arrange(scenario, year)
data_isolation_resume_000_forest

data_isolation_resume_000_natural <- data_isolation_resume %>%
  dplyr::filter(area == "0000",
                scenario == "Natural vegetation (trimmed by roads and rails)") %>%
  dplyr::arrange(vegetation, year)
data_isolation_resume_000_natural

round((data_isolation_resume_000_forest$mean - data_isolation_resume_000_natural$mean)/data_isolation_resume_000_forest$mean, 2)

data_isolation_resume_050 <- data_isolation_resume %>%
  dplyr::filter(area == "0050") %>%
  dplyr::arrange(vegetation, year)
data_isolation_resume_050

data_isolation_resume_100 <- data_isolation_resume %>%
  dplyr::filter(area == "0100") %>%
  dplyr::arrange(vegetation, year)
data_isolation_resume_100

data_isolation_resume_150 <- data_isolation_resume %>%
  dplyr::filter(area == "0150") %>%
  dplyr::arrange(vegetation, year)
data_isolation_resume_150

data_isolation_resume_200 <- data_isolation_resume %>%
  dplyr::filter(area == "0200") %>%
  dplyr::arrange(vegetation, year)
data_isolation_resume_200

data_isolation_resume_250 <- data_isolation_resume %>%
  dplyr::filter(area == "0250") %>%
  dplyr::arrange(vegetation, year)
data_isolation_resume_250

data_isolation_resume_350 <- data_isolation_resume %>%
  dplyr::filter(area == "0350") %>%
  dplyr::arrange(vegetation, year)
data_isolation_resume_350

data_isolation_resume_500 <- data_isolation_resume %>%
  dplyr::filter(area == "0500") %>%
  dplyr::arrange(vegetation, year)
data_isolation_resume_500

data_isolation_resume_1000 <- data_isolation_resume %>%
  dplyr::filter(area == "1000") %>%
  dplyr::arrange(vegetation, year)
data_isolation_resume_1000

# 6 protected area and indigenous territories ------------------------------

## protected area ----

### vegetation ----

# import mean data
files_protected_area <- dir(path = "02_results", pattern = "06_mapbiomas", full.names = TRUE) %>%
  stringr::str_subset("protected_areas") %>%
  stringr::str_subset("classes", negate = TRUE)
files_protected_area

data_protected_area <- NULL
for(i in files_protected_area){

  names_protected_area <- i %>%
    basename() %>%
    stringr::str_split("_", simplify = TRUE)

  data_protected_area_i <- readr::read_csv(i, col_names = c("vegetation", "dist_pa", "area", "n"), col_types = readr::cols()) %>%
    dplyr::mutate(area_ha = area/1e4) %>%
    dplyr::filter(vegetation == 1) %>%
    dplyr::mutate(class = case_when(dist_pa == 0 ~ "PA",
                                    dist_pa > 0 & dist_pa <= 100 ~ "<100",
                                    dist_pa > 100 & dist_pa <= 250 ~ "100-250",
                                    dist_pa > 250 & dist_pa <= 500 ~ "250-500",
                                    dist_pa > 500 & dist_pa <= 1000 ~ "500-\n1000",
                                    dist_pa > 1000 & dist_pa <= 2500 ~ "1000-\n2500",
                                    dist_pa > 2500 & dist_pa <= 5000 ~ "2500-\n5000",
                                    dist_pa > 5000 & dist_pa <= 10000 ~ "5000-\n10000",
                                    dist_pa > 10000 & dist_pa <= 25000 ~ "10000-\n25000",
                                    dist_pa > 25000 & dist_pa <= 50000 ~ "25000-\n50000",
                                    dist_pa > 50000 ~ ">50000")) %>%
    dplyr::mutate(class = forcats::as_factor(class)) %>%
    dplyr::mutate(class = forcats::fct_relevel(class,
                                               c("PA", "<100", "100-250", "250-500",
                                                 "500-\n1000", "1000-\n2500", "2500-\n5000",
                                                 "5000-\n10000", "10000-\n25000",
                                                 "25000-\n50000", ">50000"))) %>%
    dplyr::group_by(class) %>%
    dplyr::summarise(n_total = sum(n),
                     area_ha_total = sum(area_ha)) %>%
    dplyr::mutate(n_total_per = round(n_total/sum(n_total)*100, 1)) %>%
    dplyr::mutate(scenario = ifelse(length(names_protected_area) == 13,
                                    paste0(names_protected_area[, 9], "_", names_protected_area[, 10], "_", names_protected_area[, 11]),
                                    names_protected_area[, 9]),
                  .before = 1)
  data_protected_area_i

  data_protected_area <- dplyr::bind_rows(data_protected_area, data_protected_area_i)

}
data_protected_area

# percentage
data_area_forest <- readr::read_csv("04_tables/01_data_pid_area_resume.csv") %>%
  dplyr::filter(year == 2020, scenario == "Forest vegetation (trimmed by roads and rails)") %>%
  dplyr::pull(total_area_ha)
data_area_forest

data_area_natural <- readr::read_csv("04_tables/01_data_pid_area_resume.csv") %>%
  dplyr::filter(year == 2020, scenario == "Natural vegetation (trimmed by roads and rails)") %>%
  dplyr::pull(total_area_ha)
data_area_natural

data_protected_area_area <- readr::read_csv("02_results/00_protected_area.csv",
                                            col_names = c("pa", "ve", "area", "n", "p")) %>%
  dplyr::mutate(area_ha = area/1e4, .after = area) %>%
  dplyr::filter(pa == 1) %>%
  dplyr::pull(area_ha) %>%
  sum()
data_protected_area_area

data_protected_area_area/data_area_forest*100
data_protected_area_area/data_area_natural*100

# analysis
data_protected_area %>%
  dplyr::filter(scenario == "forest_roads_rails",
                class == "PA") %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_protected_area %>%
  dplyr::filter(scenario == "natural_roads_rails",
                class == "PA") %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_protected_area %>%
  dplyr::filter(scenario == "forest_roads_rails",
                class %in% c("<100", "100-250", "250-500", "500-\n1000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_protected_area %>%
  dplyr::filter(scenario == "natural_roads_rails",
                class %in% c("<100", "100-250", "250-500", "500-\n1000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_protected_area %>%
  dplyr::filter(scenario == "forest_roads_rails",
                class %in% c("<100", "100-250", "250-500", "500-\n1000", "1000-\n2500", "2500-\n5000", "5000-\n10000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_protected_area %>%
  dplyr::filter(scenario == "natural_roads_rails",
                class %in% c("<100", "100-250", "250-500", "500-\n1000", "1000-\n2500", "2500-\n5000", "5000-\n10000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_protected_area %>%
  dplyr::filter(scenario == "forest_roads_rails",
                class %in% c("10000-\n25000", "25000-\n50000", ">50000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_protected_area %>%
  dplyr::filter(scenario == "natural_roads_rails",
                class %in% c("10000-\n25000", "25000-\n50000", ">50000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

# export
data_protected_area_summary <- data_protected_area %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
    scenario == "forest_roads_rails" ~ "Forest vegetation (trimmed by roads and rails)",
    scenario == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
    scenario == "natural_roads_rails" ~ "Natural vegetation (trimmed by roads and rails)"))
data_protected_area_summary

readr::write_csv(data_protected_area_summary, "04_tables/06_data_protected_area.csv")

### classes ----

# classes
classes_habitat_cover <- tibble::tibble(
  values = c(0, 3, 4, 5, 11, 12, 13, 32, 49, 50),
  classes = c("Matrix", "Forest formation", "Savanna formation", "Mangrove",
              "Wetland", "Grassland", "Other non forest formations",
              "Salt flat", "Wooded sandbank vegetation",
              "Herbaceous sandbank vegetation"))
classes_habitat_cover

# not trimmed by roads and rails
data_protected_area_natural_classes <- readr::read_csv("02_results/06_mapbiomas_brazil_af_trinacional_2020_af_lim_natural_classes_protected_areas.csv",
                                                       col_names = c("values", "pa", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(area_ha = area/1e4,
                per_total = round(area_ha/sum(area_ha) * 100, 3)) %>%
  dplyr::select(1, 2, 5, 6)
data_protected_area_natural_classes

data_protected_area_natural_classes_per <- data_protected_area_natural_classes %>%
  dplyr::group_by(values) %>%
  dplyr::summarise(area_ha_class_sum = sum(area_ha))
data_protected_area_natural_classes_per

data_protected_area_natural_classes_per_veg <- data_protected_area_natural_classes_per %>%
  filter(values != 0) %>%
  pull(area_ha_class_sum) %>%
  sum()
data_protected_area_natural_classes_per_veg

data_protected_area_natural_classes_per_total <- data_protected_area_natural_classes %>%
  dplyr::left_join(data_protected_area_natural_classes_per) %>%
  dplyr::left_join(classes_habitat_cover) %>%
  dplyr::mutate(per_protec = round(area_ha/area_ha_class_sum * 100, 2),
                per_class = round(area_ha_class_sum/data_protected_area_natural_classes_per_veg * 100, 2),
                scenario = "not_trimmed") %>%
  dplyr::filter(values > 0, pa == 0) %>%
  dplyr::select(scenario, classes, values, area_ha, area_ha_class_sum, per_protec, per_class, per_total)
data_protected_area_natural_classes_per_total

# trimmed by roads and rails
data_protected_area_natural_classes_roads <- readr::read_csv("02_results/06_mapbiomas_brazil_af_trinacional_2020_af_lim_natural_classes_roads_rails_protected_areas.csv",
                                                             col_names = c("values", "pa", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(area_ha = area/1e4,
                per_total = round(area_ha/sum(area_ha) * 100, 3)) %>%
  dplyr::select(1, 2, 5, 6)
data_protected_area_natural_classes_roads

data_protected_area_natural_classes_roads_per <- data_protected_area_natural_classes_roads %>%
  dplyr::group_by(values) %>%
  dplyr::summarise(area_ha_class_sum = sum(area_ha))
data_protected_area_natural_classes_roads_per

data_protected_area_natural_classes_roads_per_veg <- data_protected_area_natural_classes_roads_per %>%
  filter(values != 0) %>%
  pull(area_ha_class_sum) %>%
  sum()
data_protected_area_natural_classes_roads_per_veg

data_protected_area_natural_classes_roads_per_total <- data_protected_area_natural_classes_roads %>%
  dplyr::left_join(data_protected_area_natural_classes_roads_per) %>%
  dplyr::left_join(classes_habitat_cover) %>%
  dplyr::mutate(per_protec = round(area_ha/area_ha_class_sum * 100, 2),
                per_class = round(area_ha_class_sum/data_protected_area_natural_classes_roads_per_veg * 100, 2),
                scenario = "trimmed") %>%
  dplyr::filter(values > 0, pa == 0) %>%
  dplyr::select(scenario, classes, values, area_ha, area_ha_class_sum, per_protec, per_class, per_total)
data_protected_area_natural_classes_roads_per_total

# analyses
data_protected_area_natural_classes_roads_per_total %>%
  dplyr::arrange(-per_protec)

data_protected_area_natural_classes_roads_per_total %>%
  dplyr::arrange(-per_class)

# bind
data_protected_area_classes <- data_protected_area_natural_classes_per_total %>%
  dplyr::bind_rows(data_protected_area_natural_classes_roads_per_total)
data_protected_area_classes

# export
readr::write_csv(data_protected_area_classes, "04_tables/06_data_protected_area_classes.csv")

## indigenous territory ----

# import mean data
files_indigenous_territory <- dir(path = "02_results", pattern = "06_mapbiomas", full.names = TRUE) %>%
  stringr::str_subset("indigenous_territory") %>%
  stringr::str_subset("classes", negate = TRUE)
files_indigenous_territory

data_indigenous_territory <- NULL
for(i in files_indigenous_territory){

  names_indigenous_territory <- i %>%
    basename() %>%
    stringr::str_split("_", simplify = TRUE)

  data_indigenous_territory_i <- readr::read_csv(i, col_names = c("vegetation", "dist_pa", "area", "n"), col_types = readr::cols()) %>%
    dplyr::mutate(area_ha = area/1e4) %>%
    dplyr::filter(vegetation == 1) %>%
    dplyr::mutate(class = case_when(dist_pa == 0 ~ "IT",
                                    dist_pa > 0 & dist_pa <= 100 ~ "<100",
                                    dist_pa > 100 & dist_pa <= 250 ~ "100-250",
                                    dist_pa > 250 & dist_pa <= 500 ~ "250-500",
                                    dist_pa > 500 & dist_pa <= 1000 ~ "500-\n1000",
                                    dist_pa > 1000 & dist_pa <= 2500 ~ "1000-\n2500",
                                    dist_pa > 2500 & dist_pa <= 5000 ~ "2500-\n5000",
                                    dist_pa > 5000 & dist_pa <= 10000 ~ "5000-\n10000",
                                    dist_pa > 10000 & dist_pa <= 25000 ~ "10000-\n25000",
                                    dist_pa > 25000 & dist_pa <= 50000 ~ "25000-\n50000",
                                    dist_pa > 50000 ~ ">50000")) %>%
    dplyr::mutate(class = forcats::as_factor(class)) %>%
    dplyr::mutate(class = forcats::fct_relevel(class,
                                               c("IT", "<100", "100-250", "250-500",
                                                 "500-\n1000", "1000-\n2500", "2500-\n5000",
                                                 "5000-\n10000", "10000-\n25000",
                                                 "25000-\n50000", ">50000"))) %>%
    dplyr::group_by(class) %>%
    dplyr::summarise(n_total = sum(n),
                     area_ha_total = sum(area_ha)) %>%
    dplyr::mutate(n_total_per = round(n_total/sum(n_total)*100, 1)) %>%
    dplyr::mutate(scenario = ifelse(length(names_indigenous_territory) == 13,
                                    paste0(names_indigenous_territory[, 9], "_", names_indigenous_territory[, 10], "_", names_indigenous_territory[, 11]),
                                    names_indigenous_territory[, 9]),
                  .before = 1)
  data_indigenous_territory_i

  data_indigenous_territory <- dplyr::bind_rows(data_indigenous_territory, data_indigenous_territory_i)

}
data_indigenous_territory

# percentage
data_area_forest <- readr::read_csv("04_tables/01_data_pid_area_resume.csv") %>%
  dplyr::filter(year == 2020, scenario == "Forest vegetation (trimmed by roads and rails)") %>%
  dplyr::pull(total_area_ha)
data_area_forest

data_area_natural <- readr::read_csv("04_tables/01_data_pid_area_resume.csv") %>%
  dplyr::filter(year == 2020, scenario == "Natural vegetation (trimmed by roads and rails)") %>%
  dplyr::pull(total_area_ha)
data_area_natural

data_indigenous_territory_area <- readr::read_csv("02_results/00_indigenous_territory_area.csv",
                                            col_names = c("pa", "ve", "area", "n", "p")) %>%
  dplyr::mutate(area_ha = area/1e4, .after = area) %>%
  dplyr::filter(pa == 1) %>%
  dplyr::pull(area_ha) %>%
  sum()
data_indigenous_territory_area

data_indigenous_territory_area/data_area_forest*100
data_indigenous_territory_area/data_area_natural*100

# analysis
data_indigenous_territory %>%
  dplyr::filter(scenario == "forest_roads_rails",
                class == "IT") %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_indigenous_territory %>%
  dplyr::filter(scenario == "natural_roads_rails",
                class == "IT") %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_indigenous_territory %>%
  dplyr::filter(scenario == "forest_roads_rails",
                class %in% c("<100", "100-250", "250-500", "500-\n1000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_indigenous_territory %>%
  dplyr::filter(scenario == "natural_roads_rails",
                class %in% c("<100", "100-250", "250-500", "500-\n1000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_indigenous_territory %>%
  dplyr::filter(scenario == "forest_roads_rails",
                class %in% c("<100", "100-250", "250-500", "500-\n1000", "1000-\n2500", "2500-\n5000", "5000-\n10000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_indigenous_territory %>%
  dplyr::filter(scenario == "natural_roads_rails",
                class %in% c("<100", "100-250", "250-500", "500-\n1000", "1000-\n2500", "2500-\n5000", "5000-\n10000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_indigenous_territory %>%
  dplyr::filter(scenario == "forest_roads_rails",
                class %in% c("10000-\n25000", "25000-\n50000", ">50000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_indigenous_territory %>%
  dplyr::filter(scenario == "natural_roads_rails",
                class %in% c("10000-\n25000", "25000-\n50000", ">50000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

# export
data_indigenous_territory_summary <- data_indigenous_territory %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
    scenario == "forest_roads_rails" ~ "Forest vegetation (trimmed by roads and rails)",
    scenario == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
    scenario == "natural_roads_rails" ~ "Natural vegetation (trimmed by roads and rails)"))
data_indigenous_territory_summary

readr::write_csv(data_indigenous_territory_summary, "04_tables/06_data_indigenous_territory.csv")

### classes ----

# classes
classes_habitat_cover <- tibble::tibble(
  values = c(0, 3, 4, 5, 11, 12, 13, 32, 49, 50),
  classes = c("Matrix", "Forest formation", "Savanna formation", "Mangrove",
              "Wetland", "Grassland", "Other non forest formations",
              "Salt flat", "Wooded sandbank vegetation",
              "Herbaceous sandbank vegetation"))
classes_habitat_cover

# not trimmed by roads and rails
data_indigenous_territory_natural_classes <- readr::read_csv("02_results/06_mapbiomas_brazil_af_trinacional_2020_af_lim_natural_classes_indigenous_territory_classes.csv",
                                                             col_names = c("values", "pa", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(area_ha = area/1e4,
                per_total = round(area_ha/sum(area_ha) * 100, 3)) %>%
  dplyr::select(1, 2, 5, 6)
data_indigenous_territory_natural_classes

data_indigenous_territory_natural_classes_per <- data_indigenous_territory_natural_classes %>%
  dplyr::group_by(values) %>%
  dplyr::summarise(area_ha_class_sum = sum(area_ha))
data_indigenous_territory_natural_classes_per

data_indigenous_territory_natural_classes_per_veg <- data_indigenous_territory_natural_classes_per %>%
  filter(values != 0) %>%
  pull(area_ha_class_sum) %>%
  sum()
data_indigenous_territory_natural_classes_per_veg

data_indigenous_territory_natural_classes_per_total <- data_indigenous_territory_natural_classes %>%
  dplyr::left_join(data_indigenous_territory_natural_classes_per) %>%
  dplyr::left_join(classes_habitat_cover) %>%
  dplyr::mutate(per_protec = round(area_ha/area_ha_class_sum * 100, 2),
                per_class = round(area_ha_class_sum/data_indigenous_territory_natural_classes_per_veg * 100, 2),
                scenario = "not_trimmed") %>%
  dplyr::filter(values > 0, pa == 0) %>%
  dplyr::select(scenario, classes, values, area_ha, area_ha_class_sum, per_protec, per_class, per_total)
data_indigenous_territory_natural_classes_per_total

# trimmed by roads and rails
data_indigenous_territory_natural_classes_roads <- readr::read_csv("02_results/06_mapbiomas_brazil_af_trinacional_2020_af_lim_natural_classes_roads_rails_indigenous_territory_classes.csv",
                                                                   col_names = c("values", "pa", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(area_ha = area/1e4,
                per_total = round(area_ha/sum(area_ha) * 100, 3)) %>%
  dplyr::select(1, 2, 5, 6)
data_indigenous_territory_natural_classes_roads

data_indigenous_territory_natural_classes_roads_per <- data_indigenous_territory_natural_classes_roads %>%
  dplyr::group_by(values) %>%
  dplyr::summarise(area_ha_class_sum = sum(area_ha))
data_indigenous_territory_natural_classes_roads_per

data_indigenous_territory_natural_classes_roads_per_veg <- data_indigenous_territory_natural_classes_roads_per %>%
  filter(values != 0) %>%
  pull(area_ha_class_sum) %>%
  sum()
data_indigenous_territory_natural_classes_roads_per_veg

data_indigenous_territory_natural_classes_roads_per_total <- data_indigenous_territory_natural_classes_roads %>%
  dplyr::left_join(data_indigenous_territory_natural_classes_roads_per) %>%
  dplyr::left_join(classes_habitat_cover) %>%
  dplyr::mutate(per_protec = round(area_ha/area_ha_class_sum * 100, 2),
                per_class = round(area_ha_class_sum/data_indigenous_territory_natural_classes_roads_per_veg * 100, 2),
                scenario = "trimmed") %>%
  dplyr::filter(values > 0, pa == 0) %>%
  dplyr::select(scenario, classes, values, area_ha, area_ha_class_sum, per_protec, per_class, per_total)
data_indigenous_territory_natural_classes_roads_per_total

data_indigenous_territory_natural_classes_roads_per_total %>%
  dplyr::arrange(-per_class)

data_indigenous_territory_natural_classes_roads_per_total %>%
  dplyr::arrange(-per_protec)


# bind
data_indigenous_territory_classes <- data_indigenous_territory_natural_classes_per_total %>%
  dplyr::bind_rows(data_indigenous_territory_natural_classes_roads_per_total)
data_indigenous_territory_classes

# export
readr::write_csv(data_indigenous_territory_classes, "04_tables/06_data_indigenous_territory_classes.csv")

# 7 landscape and topographic index ---------------------------------------

# years
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)

## import original data ----

### elevation ----
ele <- readr::read_csv("02_results/07_fabdem_elevation_original.csv",
                       col_names = c("value", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(per = area/sum(area), .after = area) %>%
  dplyr::mutate(class = case_when(
    value <= 100 ~ "≤100",
    value > 100 & value <= 200 ~ ">100-\n200",
    value > 200 & value <= 400 ~ ">200-\n400",
    value > 400 & value <= 800 ~ ">400-\n800",
    value > 800 & value <= 1200 ~ ">800-\n1200",
    value > 1200 & value <= 1600 ~ ">1200-\n1600",
    value > 1600 ~ ">1600")) %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(area_class_ha = sum(area)/1e4,
                   per_class = sum(per)*100) %>%
  dplyr::mutate(class = forcats::as_factor(class)) %>%
  dplyr::mutate(area_total_ha = sum(area_class_ha), .after = area_class_ha) %>%
  dplyr::mutate(class = forcats::fct_relevel(class,
                                             c("≤100", ">100-\n200", ">200-\n400",
                                               ">400-\n800", ">800-\n1200",
                                               ">1200-\n1600", ">1600"))) %>%
  dplyr::arrange(class) %>%
  dplyr::mutate(scenario = "Original distribution", topography = "elevation", .before = 1)
ele

### slope ----
slo <- readr::read_csv("02_results/07_fabdem_slope_original.csv",
                       col_names = c("value", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(per = area/sum(area), .after = n) %>%
  dplyr::mutate(class = case_when(
    value <= 5 ~ "≤5°",
    value > 5 & value <= 10 ~ ">5°-10°",
    value > 10 & value <= 15 ~ ">10°-15°",
    value > 15 & value <= 20 ~ ">15°-20°",
    value > 20 & value <= 25 ~ ">20°-25°",
    value > 25 ~ ">25°")) %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(area_class_ha = sum(area)/1e4,
                   per_class = sum(per)*100) %>%
  dplyr::mutate(area_total_ha = sum(area_class_ha), .after = area_class_ha) %>%
  dplyr::mutate(class = forcats::as_factor(class)) %>%
  dplyr::mutate(class = forcats::fct_relevel(class,
                                             c("≤5°", ">5°-10°",
                                               ">10°-15°", ">15°-20°",
                                               ">20°-25°", ">25°"))) %>%
  dplyr::arrange(class) %>%
  dplyr::mutate(scenario = "Original distribution", topography = "slope", .before = 1)
slo

### aspect ----
asp_original <- readr::read_csv("02_results/07_fabdem_aspect_original.csv",
                                col_names = c("value", "area", "n"), col_types = readr::cols())

asp <- readr::read_csv("02_results/07_fabdem_aspect_original.csv",
                       col_names = c("value", "area", "n"), col_types = readr::cols()) %>%
  dplyr::filter(value != 0) %>%
  dplyr::mutate(per = area/sum(area), .after = n) %>%
  dplyr::mutate(class = case_when(
    value >= 22.5 & value < 67.5 ~ "NE",
    value >= 67.5 & value < 112.5 ~ "N",
    value >= 112.5 & value < 157.5 ~ "NW",
    value >= 157.5 & value < 202.5 ~ "W",
    value >= 202.5 & value < 247.5 ~ "SW",
    value >= 247.5 & value < 292.5 ~ "S",
    value >= 292.5 & value < 337.5 ~ "SE",
    value >= 337.5 ~ "E",
    value >= 1 & value < 22.5 ~ "E")) %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(area_class_ha = sum(area)/1e4,
                   per_class = sum(per)*100) %>%
  dplyr::mutate(area_total_ha = sum(area_class_ha), .after = area_class_ha) %>%
  dplyr::mutate(class = forcats::as_factor(class)) %>%
  dplyr::mutate(class = forcats::fct_relevel(class,
                                             c("NE", "N", "NW", "W", "SW", "S", "SE", "E"))) %>%
  dplyr::arrange(class) %>%
  dplyr::mutate(scenario = "Original distribution", topography = "slope", .before = 1)
asp

asp_figure <- asp_original %>%
  dplyr::filter(value != 0) %>%
  dplyr::mutate(per = area/sum(area), .after = n) %>%
  dplyr::mutate(class = case_when(
    value >= 22.5 & value < 67.5 ~ "NE",
    value >= 67.5 & value < 112.5 ~ "N",
    value >= 112.5 & value < 157.5 ~ "NW",
    value >= 157.5 & value < 202.5 ~ "W",
    value >= 202.5 & value < 247.5 ~ "SW",
    value >= 247.5 & value < 292.5 ~ "S",
    value >= 292.5 & value < 337.5 ~ "SE",
    value >= 337.5 ~ "E",
    value >= 1 & value < 22.5 ~ "E")) %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(per_total = sum(per)) %>%
  dplyr::mutate(class = forcats::as_factor(class)) %>%
  dplyr::mutate(class = forcats::fct_relevel(class,
                                             c("NE", "N", "NW", "W", "SW", "S", "SE", "E"))) %>%
  dplyr::arrange(class) %>%
  tidyr::pivot_wider(names_from = class, values_from = per_total) %>%
  rbind(rep(.16, 8) , rep(0, 8), .) %>%
  dplyr::select(N, NW, W, SW, S, SE, E, NE) %>%
  dplyr::mutate(scenario = c("1", "2", "Original"), .before = 1)
asp_figure

### pcurvature ----
pcu <- readr::read_csv("02_results/07_fabdem_pcurvature_original.csv",
                       col_names = c("value", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(per = area/sum(area), .after = n) %>%
  dplyr::mutate(class = case_when(
    value <= -0.005 ~ "≤-0.005",
    value > -0.005 & value <= -0.002 ~ ">-0.005-\n-0.002",
    value > -0.002 & value <= -0.001 ~ ">-0.002-\n-0.001",
    value > -0.001 & value <= 0.000 ~ ">-0.001-\n0.000",
    value > 0.000 & value <= 0.001 ~ ">0.000-\n0.001",
    value > 0.001 & value <= 0.002 ~ ">0.001-\n0.002",
    value > 0.002 & value <= 0.005 ~ ">0.002-\n0.005",
    value > 0.005 ~ ">0.005")) %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(area_class_ha = sum(area)/1e4,
                   per_class = sum(per)*100) %>%
  dplyr::mutate(area_total_ha = sum(area_class_ha), .after = area_class_ha) %>%
  dplyr::mutate(class = forcats::as_factor(class)) %>%
  dplyr::mutate(class = forcats::fct_relevel(class,
                                             c("≤-0.005", ">-0.005-\n-0.002",
                                               ">-0.002-\n-0.001", ">-0.001-\n0.000",
                                               ">0.000-\n0.001", ">0.001-\n0.002",
                                               ">0.002-\n0.005", ">0.005"))) %>%
  dplyr::arrange(class) %>%
  dplyr::mutate(scenario = "Original distribution", topography = "pcurvature", .before = 1)
pcu

### tcurvature ----
tcu <- readr::read_csv("02_results/07_fabdem_tcurvature_original.csv",
                       col_names = c("value", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(per = area/sum(area), .after = n) %>%
  dplyr::mutate(class = case_when(
    value <= -0.005 ~ "≤-0.005",
    value > -0.005 & value <= -0.002 ~ ">-0.005-\n-0.002",
    value > -0.002 & value <= -0.001 ~ ">-0.002-\n-0.001",
    value > -0.001 & value <= 0.000 ~ ">-0.001-\n0.000",
    value > 0.000 & value <= 0.001 ~ ">0.000-\n0.001",
    value > 0.001 & value <= 0.002 ~ ">0.001-\n0.002",
    value > 0.002 & value <= 0.005 ~ ">0.002-\n0.005",
    value > 0.005 ~ ">0.005")) %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(area_class_ha = sum(area)/1e4,
                   per_class = sum(per)*100) %>%
  dplyr::mutate(area_total_ha = sum(area_class_ha), .after = area_class_ha) %>%
  dplyr::mutate(class = forcats::as_factor(class)) %>%
  dplyr::mutate(class = forcats::fct_relevel(class,
                                             c("≤-0.005", ">-0.005-\n-0.002",
                                               ">-0.002-\n-0.001", ">-0.001-\n0.000",
                                               ">0.000-\n0.001", ">0.001-\n0.002",
                                               ">0.002-\n0.005", ">0.005"))) %>%
  dplyr::arrange(class) %>%
  dplyr::mutate(scenario = "Original distribution", topography = "tcurvature", .before = 1)
tcu

### geomorph ----
geomorph <- readr::read_csv("02_results/07_fabdem_geomorph_original.csv",
                            col_names = c("value", "area", "n"), col_types = readr::cols()) %>%
  dplyr::bind_cols(geo = c("flat", "peak", "ridge", "shoulder", "spur", "slope",
                           "hollow", "footslope", "valley", "pit")) %>%
  dplyr::mutate(geo = forcats::as_factor(geo),
                area_class_ha = area/1e4) %>%
  dplyr::mutate(area_total_ha = sum(area_class_ha), .after = area_class_ha) %>%
  dplyr::mutate(per_class = area/sum(area)*100, .after = area_total_ha) %>%
  dplyr::select(-n, -area) %>%
  dplyr::mutate(geo = forcats::fct_relevel(geo,
                                           c("flat", "peak", "ridge", "shoulder", "spur", "slope",
                                             "hollow", "footslope", "valley", "pit"))) %>%
  dplyr::mutate(scenario = "Original distribution", topography = "geomorph", .before = 1) %>%
  dplyr::relocate(geo, .after = value)
geomorph

## import data ----

# list files
files_topography <- dir(path = "02_results", pattern = "07_mapbiomas", full.names = TRUE) %>%
  stringr::str_subset("original", negate = TRUE)
files_topography

# import vegetation data
data_topography_elevation <- NULL
data_topography_slope <- NULL
data_topography_aspect <- NULL
data_topography_pcurvature <- NULL
data_topography_tcurvature <- NULL
data_topography_geomorph <- NULL

for(i in years){

  print(i)

  files_topography_i <- stringr::str_subset(files_topography, paste0("_", i, "_"))
  data_topography_elevation_i <- NULL
  data_topography_slope_i <- NULL
  data_topography_aspect_i <- NULL
  data_topography_pcurvature_i <- NULL
  data_topography_tcurvature_i <- NULL
  data_topography_geomorph_i <- NULL

  for(j in c("forest", "forest_roads_rails", "natural", "natural_roads_rails")){

    ### elevation ----
    data_topography_elevation_partial <- stringr::str_subset(files_topography_i, paste0(j, "_elevation")) %>%
      readr::read_csv(col_names = c("vegetation", "value", "area", "n")) %>%
      dplyr::mutate(class = case_when(
        value <= 100 ~ "≤100",
        value > 100 & value <= 200 ~ ">100-\n200",
        value > 200 & value <= 400 ~ ">200-\n400",
        value > 400 & value <= 800 ~ ">400-\n800",
        value > 800 & value <= 1200 ~ ">800-\n1200",
        value > 1200 & value <= 1600 ~ ">1200-\n1600",
        value > 1600 ~ ">1600")) %>%
      dplyr::group_by(vegetation, class) %>%
      dplyr::summarise(area_class_ha = sum(area)/1e4) %>%
      dplyr::mutate(class = forcats::as_factor(class)) %>%
      dplyr::mutate(class = forcats::fct_relevel(class,
                                                 c("≤100", ">100-\n200", ">200-\n400",
                                                   ">400-\n800", ">800-\n1200",
                                                   ">1200-\n1600", ">1600"))) %>%
      dplyr::arrange(class) %>%
      dplyr::mutate(scenario = j, topography = "elevation", year = i, .before = 1)
    data_topography_elevation_partial

    data_topography_elevation_total <- data_topography_elevation_partial %>%
      dplyr::group_by(class) %>%
      dplyr::summarise(original_distribution = sum(area_class_ha)) %>%
      dplyr::pull(original_distribution)
    data_topography_elevation_total

    data_topography_elevation_j <- data_topography_elevation_partial %>%
      dplyr::filter(vegetation == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-vegetation) %>%
      dplyr::mutate(area_total_ha = data_topography_elevation_total,
                    per_class = area_class_ha/area_total_ha * 100)
    data_topography_elevation_j

    data_topography_elevation_i <- dplyr::bind_rows(data_topography_elevation_i, data_topography_elevation_j)


    ### slope ----
    data_topography_slope_partial <- stringr::str_subset(files_topography_i, paste0(j, "_slope")) %>%
      readr::read_csv(col_names = c("vegetation", "value", "area", "n")) %>%
      dplyr::mutate(class = case_when(
        value <= 5 ~ "≤5°",
        value > 5 & value <= 10 ~ ">5°-10°",
        value > 10 & value <= 15 ~ ">10°-15°",
        value > 15 & value <= 20 ~ ">15°-20°",
        value > 20 & value <= 25 ~ ">20°-25°",
        value > 25 ~ ">25°")) %>%
      dplyr::group_by(vegetation, class) %>%
      dplyr::summarise(area_class_ha = sum(area)/1e4) %>%
      dplyr::mutate(class = forcats::as_factor(class)) %>%
      dplyr::mutate(class = forcats::fct_relevel(class,
                                                 c("≤5°", ">5°-10°",
                                                   ">10°-15°", ">15°-20°",
                                                   ">20°-25°", ">25°"))) %>%
      dplyr::arrange(class) %>%
      dplyr::mutate(scenario = j, topography = "slope", year = i, .before = 1)
    data_topography_slope_partial

    data_topography_slope_total <- data_topography_slope_partial %>%
      dplyr::group_by(class) %>%
      dplyr::summarise(original_distribution = sum(area_class_ha)) %>%
      dplyr::pull(original_distribution)
    data_topography_slope_total

    data_topography_slope_j <- data_topography_slope_partial %>%
      dplyr::filter(vegetation == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-vegetation) %>%
      dplyr::mutate(area_total_ha = data_topography_slope_total,
                    per_class = area_class_ha/area_total_ha * 100)
    data_topography_slope_j

    data_topography_slope_i <- dplyr::bind_rows(data_topography_slope_i, data_topography_slope_j)

    ###  aspect ----
    data_topography_aspect_partial <- stringr::str_subset(files_topography_i, paste0(j, "_aspect")) %>%
      readr::read_csv(col_names = c("vegetation", "value", "area", "n")) %>%
      dplyr::filter(value != 0) %>%
      dplyr::mutate(class = case_when(
        value >= 22.5 & value < 67.5 ~ "NE",
        value >= 67.5 & value < 112.5 ~ "N",
        value >= 112.5 & value < 157.5 ~ "NW",
        value >= 157.5 & value < 202.5 ~ "W",
        value >= 202.5 & value < 247.5 ~ "SW",
        value >= 247.5 & value < 292.5 ~ "S",
        value >= 292.5 & value < 337.5 ~ "SE",
        value >= 337.5 ~ "E",
        value >= 1 & value < 22.5 ~ "E")) %>%
      dplyr::group_by(vegetation, class) %>%
      dplyr::summarise(area_class_ha = sum(area)/1e4) %>%
      dplyr::mutate(class = forcats::as_factor(class)) %>%
      dplyr::mutate(class = forcats::fct_relevel(class,
                                                 c("NE", "N", "NW", "W", "SW", "S", "SE", "E"))) %>%
      dplyr::arrange(class) %>%
      dplyr::mutate(scenario = j, topography = "aspect", year = i, .before = 1)
    data_topography_aspect_partial

    data_topography_aspect_total <- data_topography_aspect_partial %>%
      dplyr::group_by(class) %>%
      dplyr::summarise(original_distribution = sum(area_class_ha)) %>%
      dplyr::pull(original_distribution)
    data_topography_aspect_total

    data_topography_aspect_j <- data_topography_aspect_partial %>%
      dplyr::filter(vegetation == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-vegetation) %>%
      dplyr::mutate(area_total_ha = data_topography_aspect_total,
                    per_class = area_class_ha/sum(area_class_ha)*100)
    data_topography_aspect_j

    data_topography_aspect_i <- dplyr::bind_rows(data_topography_aspect_i, data_topography_aspect_j)


    ### pcurvature ----
    data_topography_pcurvature_partial <- stringr::str_subset(files_topography_i, paste0(j, "_pcurvature")) %>%
      readr::read_csv(col_names = c("vegetation", "value", "area", "n")) %>%
      dplyr::mutate(class = case_when(
        value <= -0.005 ~ "≤-0.005",
        value > -0.005 & value <= -0.002 ~ ">-0.005-\n-0.002",
        value > -0.002 & value <= -0.001 ~ ">-0.002-\n-0.001",
        value > -0.001 & value <= 0.000 ~ ">-0.001-\n0.000",
        value > 0.000 & value <= 0.001 ~ ">0.000-\n0.001",
        value > 0.001 & value <= 0.002 ~ ">0.001-\n0.002",
        value > 0.002 & value <= 0.005 ~ ">0.002-\n0.005",
        value > 0.005 ~ ">0.005")) %>%
      dplyr::group_by(vegetation, class) %>%
      dplyr::summarise(area_class_ha = sum(area)/1e4) %>%
      dplyr::mutate(class = forcats::as_factor(class)) %>%
      dplyr::mutate(class = forcats::fct_relevel(class,
                                                 c("≤-0.005", ">-0.005-\n-0.002",
                                                   ">-0.002-\n-0.001", ">-0.001-\n0.000",
                                                   ">0.000-\n0.001", ">0.001-\n0.002",
                                                   ">0.002-\n0.005", ">0.005"))) %>%
      dplyr::arrange(class) %>%
      dplyr::mutate(scenario = j, topography = "pcurvature", year = i, .before = 1)
    data_topography_pcurvature_partial

    data_topography_pcurvature_total <- data_topography_pcurvature_partial %>%
      dplyr::group_by(class) %>%
      dplyr::summarise(original_distribution = sum(area_class_ha)) %>%
      dplyr::pull(original_distribution)
    data_topography_pcurvature_total

    data_topography_pcurvature_j <- data_topography_pcurvature_partial %>%
      dplyr::filter(vegetation == 1) %>%
      dplyr::mutate(area_total_ha = data_topography_pcurvature_total,
                    per_class = area_class_ha/area_total_ha*100)
    data_topography_pcurvature_j

    data_topography_pcurvature_i <- dplyr::bind_rows(data_topography_pcurvature_i, data_topography_pcurvature_j)

    ### tcurvature ----
    data_topography_tcurvature_partial <- stringr::str_subset(files_topography_i, paste0(j, "_tcurvature")) %>%
      readr::read_csv(col_names = c("vegetation", "value", "area", "n")) %>%
      dplyr::mutate(class = case_when(
        value <= -0.005 ~ "≤-0.005",
        value > -0.005 & value <= -0.002 ~ ">-0.005-\n-0.002",
        value > -0.002 & value <= -0.001 ~ ">-0.002-\n-0.001",
        value > -0.001 & value <= 0.000 ~ ">-0.001-\n0.000",
        value > 0.000 & value <= 0.001 ~ ">0.000-\n0.001",
        value > 0.001 & value <= 0.002 ~ ">0.001-\n0.002",
        value > 0.002 & value <= 0.005 ~ ">0.002-\n0.005",
        value > 0.005 ~ ">0.005")) %>%
      dplyr::group_by(vegetation, class) %>%
      dplyr::summarise(area_class_ha = sum(area)/1e4) %>%
      dplyr::mutate(class = forcats::as_factor(class)) %>%
      dplyr::mutate(class = forcats::fct_relevel(class,
                                                 c("≤-0.005", ">-0.005-\n-0.002",
                                                   ">-0.002-\n-0.001", ">-0.001-\n0.000",
                                                   ">0.000-\n0.001", ">0.001-\n0.002",
                                                   ">0.002-\n0.005", ">0.005"))) %>%
      dplyr::arrange(class) %>%
      dplyr::mutate(scenario = j, topography = "tcurvature", year = i, .before = 1)
    data_topography_tcurvature_partial

    data_topography_tcurvature_total <- data_topography_tcurvature_partial %>%
      dplyr::group_by(class) %>%
      dplyr::summarise(original_distribution = sum(area_class_ha)) %>%
      dplyr::pull(original_distribution)
    data_topography_tcurvature_total

    data_topography_tcurvature_j <- data_topography_tcurvature_partial %>%
      dplyr::filter(vegetation == 1) %>%
      dplyr::mutate(area_total_ha = data_topography_tcurvature_total,
                    per_class = area_class_ha/data_topography_tcurvature_total*100)
    data_topography_tcurvature_j

    data_topography_tcurvature_i <- dplyr::bind_rows(data_topography_tcurvature_i, data_topography_tcurvature_j)


    ### geomorph ----
    data_topography_geomorph_partial <- stringr::str_subset(files_topography_i, paste0(j, "_geomorph")) %>%
      readr::read_csv(col_names = c("vegetation", "value", "area", "n")) %>%
      dplyr::mutate(scenario = j, topography = "geomorph", year = i, area_class_ha = area/1e4, .before = 1)
    data_topography_geomorph_partial

    data_topography_geomorph_total <- data_topography_geomorph_partial %>%
      dplyr::group_by(value) %>%
      dplyr::summarise(original_distribution = sum(area_class_ha)) %>%
      dplyr::pull(original_distribution)
    data_topography_geomorph_total

    data_topography_geomorph_j <- data_topography_geomorph_partial %>%
      dplyr::filter(vegetation == 1) %>%
      dplyr::mutate(area_total_ha = data_topography_geomorph_total,
                    per_class = area_class_ha/data_topography_geomorph_total*100) %>%
      dplyr::select(-vegetation, -area, -n) %>%
      dplyr::relocate(value, .after = year)
    data_topography_geomorph_j

    data_topography_geomorph_i <- dplyr::bind_rows(data_topography_geomorph_i, data_topography_geomorph_j)


  }

  data_topography_elevation <- dplyr::bind_rows(data_topography_elevation, data_topography_elevation_i)
  data_topography_slope <- dplyr::bind_rows(data_topography_slope, data_topography_slope_i)
  data_topography_aspect <- dplyr::bind_rows(data_topography_aspect, data_topography_aspect_i)
  data_topography_pcurvature <- dplyr::bind_rows(data_topography_pcurvature, data_topography_pcurvature_i)
  data_topography_tcurvature <- dplyr::bind_rows(data_topography_tcurvature, data_topography_tcurvature_i)
  data_topography_geomorph <- dplyr::bind_rows(data_topography_geomorph, data_topography_geomorph_i)

}

data_topography_elevation <- dplyr::bind_rows(ele, data_topography_elevation)
data_topography_slope <- dplyr::bind_rows(slo, data_topography_slope)
data_topography_aspect <- dplyr::bind_rows(asp, data_topography_aspect)
data_topography_pcurvature <- dplyr::bind_rows(pcu, data_topography_pcurvature)
data_topography_tcurvature <- dplyr::bind_rows(tcu, data_topography_tcurvature)
data_topography_geomorph <- data_topography_geomorph %>%
  dplyr::left_join(., geomorph[, c("geo", "value")]) %>%
  dplyr::bind_rows(geomorph, .)

# export
data_topography_elevation_export <- data_topography_elevation %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
    scenario == "forest_roads_rails" ~ "Forest vegetation (trimmed by roads and rails)",
    scenario == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
    scenario == "natural_roads_rails" ~ "Natural vegetation (trimmed by roads and rails)",
    .default = scenario))
data_topography_elevation_export

data_topography_slope_export <- data_topography_slope %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
    scenario == "forest_roads_rails" ~ "Forest vegetation (trimmed by roads and rails)",
    scenario == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
    scenario == "natural_roads_rails" ~ "Natural vegetation (trimmed by roads and rails)",
    .default = scenario))
data_topography_slope_export

data_topography_aspect_export <- data_topography_aspect %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
    scenario == "forest_roads_rails" ~ "Forest vegetation (trimmed by roads and rails)",
    scenario == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
    scenario == "natural_roads_rails" ~ "Natural vegetation (trimmed by roads and rails)",
    .default = scenario))
data_topography_aspect_export

data_topography_pcurvature_export <- data_topography_pcurvature %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
    scenario == "forest_roads_rails" ~ "Forest vegetation (trimmed by roads and rails)",
    scenario == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
    scenario == "natural_roads_rails" ~ "Natural vegetation (trimmed by roads and rails)",
    .default = scenario))
data_topography_pcurvature_export

data_topography_tcurvature_export <- data_topography_tcurvature %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
    scenario == "forest_roads_rails" ~ "Forest vegetation (trimmed by roads and rails)",
    scenario == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
    scenario == "natural_roads_rails" ~ "Natural vegetation (trimmed by roads and rails)",
    .default = scenario))
data_topography_tcurvature_export

data_topography_geomorph_export <- data_topography_geomorph %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
    scenario == "forest_roads_rails" ~ "Forest vegetation (trimmed by roads and rails)",
    scenario == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
    scenario == "natural_roads_rails" ~ "Natural vegetation (trimmed by roads and rails)",
    .default = scenario))
data_topography_geomorph_export

readr::write_csv(data_topography_elevation_export, "04_tables/07_data_topography_elevation.csv")
readr::write_csv(data_topography_slope_export, "04_tables/07_data_topography_slope.csv")
readr::write_csv(data_topography_aspect_export, "04_tables/07_data_topography_aspect.csv")
readr::write_csv(data_topography_pcurvature_export, "04_tables/07_data_topography_pcurvature.csv")
readr::write_csv(data_topography_tcurvature_export, "04_tables/07_data_topography_tcurvature.csv")
readr::write_csv(data_topography_geomorph_export, "04_tables/07_data_topography_geomorph.csv")

## statistics ----

### elevation ----
data_topography_elevation %>%
  dplyr::filter(scenario == "Original distribution",
                class %in% c(">200-\n400", ">400-\n800", ">800-\n1200")) %>%
  dplyr::pull(per_class) %>%
  sum()

data_topography_elevation %>%
  dplyr::filter(scenario == "Original distribution",
                class %in% c(">400-\n800")) %>%
  dplyr::pull(per_class) %>%
  sum()

data_topography_elevation %>%
  dplyr::filter(scenario == "forest_roads_rails",
                year == 2020)

data_topography_elevation_1986_2020_forest <- data_topography_elevation %>%
  dplyr::filter(scenario == "forest_roads_rails",
                year %in% c(1986, 2005, 2020))
data_topography_elevation_1986_2020_forest

data_topography_elevation %>%
  dplyr::filter(scenario == "natural_roads_rails",
                year == 2020)

data_topography_elevation_1986_2020_natural <- data_topography_elevation %>%
  dplyr::filter(scenario == "natural_roads_rails",
                year %in% c(1986, 2005, 2020))
data_topography_elevation_1986_2020_natural


### slope ----
data_topography_slope %>%
  dplyr::filter(scenario == "Original distribution",
                class %in% c("≤5°", ">5°-10°")) %>%
  dplyr::pull(per_class) %>%
  sum()

data_topography_slope_1986_2020_forest <- data_topography_slope %>%
  dplyr::filter(scenario == "forest_roads_rails",
                year %in% c(1986, 2005, 2020))

data_topography_slope_1986_2020_natural <- data_topography_slope %>%
  dplyr::filter(scenario == "natural_roads_rails",
                year %in% c(1986, 2005, 2020))


### aspect ----
data_topography_aspect %>%
  dplyr::filter(scenario == "Original distribution") %>%
  dplyr::arrange(per_class)

data_topography_aspect %>%
  dplyr::filter(scenario == "forest_roads_rails", year == 2020) %>%
  dplyr::arrange(-per_class)

data_topography_aspect_forest <- data_topography_aspect %>%
  dplyr::filter(scenario == "forest_roads_rails",
                year %in% c(1986, 2020))
data_topography_aspect_forest

data_topography_aspect %>%
  dplyr::filter(scenario == "natural_roads_rails", year == 2020) %>%
  dplyr::arrange(-per_class)

data_topography_aspect_natural <- data_topography_aspect %>%
  dplyr::filter(scenario == "natural_roads_rails",
                year %in% c(1986, 2020))
data_topography_aspect_natural


### curvatures ----
data_topography_pcurvature %>%
  dplyr::filter(scenario == "Original distribution",
                class %in% c(">0.000-\n0.001", ">-0.001-\n0.000")) %>%
  dplyr::arrange(per_class) %>%
  dplyr::pull(per_class) %>%
  sum()

data_topography_tcurvature %>%
  dplyr::filter(scenario == "Original distribution",
                class %in% c(">0.000-\n0.001", ">-0.001-\n0.000")) %>%
  dplyr::arrange(per_class) %>%
  dplyr::pull(per_class) %>%
  sum()


data_topography_pcurvature_forest <- data_topography_pcurvature %>%
  dplyr::filter(scenario == "forest_roads_rails",
                year %in% c(1986, 2005, 2020)) %>%
  dplyr::arrange(class)

data_topography_pcurvature_forest_currently <- data_topography_pcurvature %>%
  dplyr::filter(scenario == "forest_roads_rails",
                !class %in% c(">-0.002-\n-0.001", ">-0.001-\n0.000", ">0.000-\n0.001", ">0.001-\n0.002"),
                year %in% c(2020)) %>%
  dplyr::arrange(class)


data_topography_pcurvature_natural <- data_topography_pcurvature %>%
  dplyr::filter(scenario == "natural_roads_rails",
                year %in% c(1986, 2005, 2020)) %>%
  dplyr::arrange(class)

data_topography_pcurvature_natural_currently <- data_topography_pcurvature %>%
  dplyr::filter(scenario == "natural_roads_rails",
                !class %in% c(">-0.002-\n-0.001", ">-0.001-\n0.000", ">0.000-\n0.001", ">0.001-\n0.002"),
                year %in% c(2020)) %>%
  dplyr::arrange(class)


data_topography_tcurvature_forest <- data_topography_tcurvature %>%
  dplyr::filter(scenario == "forest_roads_rails",
                year %in% c(1986, 2005, 2020)) %>%
  dplyr::arrange(class)

data_topography_tcurvature_forest_currently <- data_topography_tcurvature %>%
  dplyr::filter(scenario == "forest_roads_rails",
                !class %in% c(">-0.002-\n-0.001", ">-0.001-\n0.000", ">0.000-\n0.001", ">0.001-\n0.002"),
                year %in% c(2020)) %>%
  dplyr::arrange(class)

data_topography_tcurvature_natural <- data_topography_tcurvature %>%
  dplyr::filter(scenario == "natural_roads_rails",
                year %in% c(1986, 2005, 2020)) %>%
  dplyr::arrange(class)

data_topography_tcurvature_natural_currently <- data_topography_tcurvature %>%
  dplyr::filter(scenario == "natural_roads_rails",
                !class %in% c(">-0.002-\n-0.001", ">-0.001-\n0.000", ">0.000-\n0.001", ">0.001-\n0.002"),
                year %in% c(2020)) %>%
  dplyr::arrange(class)


### geomorph ----
data_topography_geomorph %>%
  dplyr::filter(scenario == "Original distribution") %>%
  dplyr::arrange(per_class)

data_topography_geomorph %>%
  dplyr::filter(scenario == "Original distribution",
                per_class < 10) %>%
  dplyr::pull(per_class) %>%
  sum()

data_topography_geomorph %>%
  dplyr::filter(scenario == "forest_roads_rails", year == 2020) %>%
  dplyr::arrange(-per_class)

data_topography_geomorph_forest <- data_topography_geomorph %>%
  dplyr::filter(scenario == "forest_roads_rails",
                year %in% c(1986, 2005, 2020)) %>%
  dplyr::arrange(geo)


data_topography_geomorph_forest_balance <- NULL
for(i in unique(data_topography_geomorph_forest$geo)){

data_topography_geomorph_forest_geo <- data_topography_geomorph_forest %>%
    dplyr::filter(geo == i)

data_topography_geomorph_forest_balance <- dplyr::bind_rows(tibble::tibble(
  geo = i,
  balance = data_topography_geomorph_forest_geo[data_topography_geomorph_forest_geo$year == 2020, ]$per_class - data_topography_geomorph_forest_geo[data_topography_geomorph_forest_geo$year == 1986, ]$per_class),
data_topography_geomorph_forest_balance)

}
data_topography_geomorph_forest_balance %>%
  dplyr::arrange(balance)

data_topography_geomorph %>%
  dplyr::filter(scenario == "natural_roads_rails", year == 2020) %>%
  dplyr::arrange(-per_class)

data_topography_geomorph_natural <- data_topography_geomorph %>%
  dplyr::filter(scenario == "natural_roads_rails",
                year %in% c(1986, 2005, 2020)) %>%
  dplyr::arrange(geo)

data_topography_geomorph_natural_balance <- NULL
for(i in unique(data_topography_geomorph_natural$geo)){

  data_topography_geomorph_natural_geo <- data_topography_geomorph_natural %>%
    dplyr::filter(geo == i)

  data_topography_geomorph_natural_balance <- dplyr::bind_rows(tibble::tibble(
    geo = i,
    balance = data_topography_geomorph_natural_geo[data_topography_geomorph_natural_geo$year == 2020, ]$per_class - data_topography_geomorph_natural_geo[data_topography_geomorph_natural_geo$year == 1986, ]$per_class),
    data_topography_geomorph_natural_balance)

}
data_topography_geomorph_natural_balance %>%
  dplyr::arrange(balance)


# end ---------------------------------------------------------------------
