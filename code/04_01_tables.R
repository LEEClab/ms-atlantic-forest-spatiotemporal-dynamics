#' ----
#' title: atlantic forest spatiotemporal dynamic - tables
#' author: mauricio vancine
#' date: 2022-12-09
#' operational system: gnu/linux - ubuntu - pop_os
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(paletteer)

# 0 areas ----------------------------------------------------------------

## limits ----
area_af_lim <- readr::read_csv("02_results/00_af_lim_area.csv",
                               col_names = c("class_area", "class_forest", "area", "n", "p")) %>%
  dplyr::summarise(area_ha = sum(area)/1e4) %>%
  dplyr::mutate(limit = "vancine_etal_inprep",
                country = "Argentina, Brazil, and Paraguay",
                .before = 1) %>%
  dplyr::mutate(percentage_vancine_etal_inprep = 100)
area_af_lim

area_af_lim_ar <- readr::read_csv("02_results/00_af_lim_area_argentina.csv",
                                  col_names = c("class_area", "class_forest", "area", "n", "p")) %>%
  dplyr::summarise(area_ha = sum(area)/1e4) %>%
  dplyr::mutate(percentage_vancine_etal_inprep = area_ha/area_af_lim$area_ha*100) %>%
  dplyr::mutate(limit = "vancine_etal_inprep",
                country = "Argentina",
                .before = 1)
area_af_lim_ar

area_af_lim_br <- readr::read_csv("02_results/00_af_lim_area_brazil.csv",
                                  col_names = c("class_area", "class_forest", "area", "n", "p")) %>%
  dplyr::summarise(area_ha = sum(area)/1e4) %>%
  dplyr::mutate(percentage_vancine_etal_inprep = area_ha/area_af_lim$area_ha*100) %>%
  dplyr::mutate(limit = "vancine_etal_inprep",
                country = "Brazil",
                .before = 1)
area_af_lim_br

area_af_lim_py <- readr::read_csv("02_results/00_af_lim_area_paraguay.csv",
                                  col_names = c("class_area", "class_forest", "area", "n", "p")) %>%
  dplyr::summarise(area_ha = sum(area)/1e4) %>%
  dplyr::mutate(percentage_vancine_etal_inprep = area_ha/area_af_lim$area_ha*100) %>%
  dplyr::mutate(limit = "vancine_etal_inprep",
                country = "Paraguay",
                .before = 1)
area_af_lim_py

area_af_lim_lawaf2006 <- readr::read_csv("02_results/00_af_lim_lawaf2006_area.csv",
                                         col_names = c("class_area", "class_forest", "area", "n", "p")) %>%
  dplyr::summarise(area_ha = sum(area)/1e4) %>%
  dplyr::mutate(percentage_vancine_etal_inprep = area_ha/area_af_lim$area_ha*100) %>%
  dplyr::mutate(limit = "Atlantic Fores Law (2006)",
                country = "Brazil",
                .before = 1)
area_af_lim_lawaf2006

area_af_lim_dasilva_casteleti_2003 <- readr::read_csv("02_results/00_af_lim_dasilvacasteleti2003_area.csv",
                                                      col_names = c("class_area", "class_forest", "area", "n", "p")) %>%
  dplyr::summarise(area_ha = sum(area)/1e4) %>%
  dplyr::mutate(percentage_vancine_etal_inprep = area_ha/area_af_lim$area_ha*100) %>%
  dplyr::mutate(limit = "Da Silva & Casteleti (2003)",
                country = "Brazil",
                .before = 1)
area_af_lim_dasilva_casteleti_2003

area_af_lim_ibge2004 <- readr::read_csv("02_results/00_af_lim_ibge2004_area.csv",
                                        col_names = c("class_area", "class_forest", "area", "n", "p")) %>%
  dplyr::summarise(area_ha = sum(area)/1e4) %>%
  dplyr::mutate(percentage_vancine_etal_inprep = area_ha/area_af_lim$area_ha*100) %>%
  dplyr::mutate(limit = "IBGE (2004)",
                country = "Brazil",
                .before = 1)
area_af_lim_ibge2004

area_af_lim_ibge2019 <- readr::read_csv("02_results/00_af_lim_ibge2019_area.csv",
                                        col_names = c("class_area", "class_forest", "area", "n", "p")) %>%
  dplyr::summarise(area_ha = sum(area)/1e4) %>%
  dplyr::mutate(percentage_vancine_etal_inprep = area_ha/area_af_lim$area_ha*100) %>%
  dplyr::mutate(limit = "IBGE (2019)",
                country = "Brazil",
                .before = 1)
area_af_lim_ibge2019

area_af_lim_dinerstein2017 <- readr::read_csv("02_results/00_af_lim_ecoregions2017_area.csv",
                                              col_names = c("class_area", "class_forest", "area", "n", "p")) %>%
  dplyr::summarise(area_ha = sum(area)/1e4) %>%
  dplyr::mutate(percentage_vancine_etal_inprep = area_ha/area_af_lim$area_ha*100) %>%
  dplyr::mutate(limit = "Dinerstein et al. (2017)",
                country = "Argentina, Brazil, and Paraguay",
                .before = 1)
area_af_lim_dinerstein2017

area_limits <- dplyr::bind_rows(area_af_lim,
                                area_af_lim_ar,
                                area_af_lim_br,
                                area_af_lim_py,
                                area_af_lim_lawaf2006,
                                area_af_lim_dasilva_casteleti_2003,
                                area_af_lim_ibge2004,
                                area_af_lim_ibge2019,
                                area_af_lim_dinerstein2017)
area_limits

## roads railways ----
area_roads_railways <- readr::read_csv("02_results/00_roads_railways_area.csv",
                                       col_names = c("anthropic", "class_forest", "area", "n", "p")) %>%
  dplyr::group_by(anthropic) %>%
  dplyr::summarise(area_ha = sum(area)/1e4) %>%
  dplyr::filter(anthropic == 1) %>%
  dplyr::mutate(percentage_vancine_etal_inprep = area_ha/area_af_lim$area_ha*100,
                anthropic = "Roads and railways")
area_roads_railways

length_roads <- sf::st_read("01_data/03_roads_railways/roads_af_lim.gpkg") %>%
  sf::st_length() %>%
  sum()/1e3
length_roads

length_railways <- sf::st_read("01_data/03_roads_railways/railways_af_lim.gpkg") %>%
  sf::st_length() %>%
  sum()/1e3
length_railways

length_roads_railways <- length_roads + length_railways
length_roads_railways

## protected areas ----
area_protected_areas <- readr::read_csv("02_results/00_protected_areas_area.csv",
                                        col_names = c("anthropic", "class_forest", "area", "n", "p")) %>%
  dplyr::group_by(anthropic) %>%
  dplyr::summarise(area_ha = sum(area)/1e4) %>%
  dplyr::filter(anthropic == 1) %>%
  dplyr::mutate(percentage_vancine_etal_inprep = area_ha/area_af_lim$area_ha*100,
                anthropic = "Protected areas")
area_protected_areas

## indigenous territories
area_indigenous_territories <- readr::read_csv("02_results/00_indigenous_territories_area.csv",
                                               col_names = c("anthropic", "class_forest", "area", "n", "p")) %>%
  dplyr::group_by(anthropic) %>%
  dplyr::summarise(area_ha = sum(area)/1e4) %>%
  dplyr::filter(anthropic == 1) %>%
  dplyr::mutate(percentage_vancine_etal_inprep = area_ha/area_af_lim$area_ha*100,
                anthropic = "Indigenous territories")
area_indigenous_territories

area_antropic <- dplyr::bind_rows(area_roads_railways,
                                  area_protected_areas,
                                  area_indigenous_territories)
area_antropic

# 1 number of patch and size distribution ---------------------------------

## data ----

# files
files_fid_area <- dir(path = "02_results", pattern = "01_mapbiomas", full.names = TRUE) %>%
  stringr::str_subset("af_lim_forest|af_lim_natural") %>%
  stringr::str_subset("fid")
files_fid_area

# data
data_fid_area <- NULL
data_fid_area_mean <- NULL
data_fid_area_resume <- NULL

for(i in files_fid_area){

  print(i)

  name_fid_area <- basename(i) %>%
    stringr::str_replace("01_mapbiomas_brazil_af_trinacional_", "") %>%
    stringr::str_replace(".csv", "") %>%
    stringr::str_replace("_af_lim", "") %>%
    stringr::str_replace("_fid", "")

  title_fid_area <- stringr::str_replace_all(name_fid_area, "_", " ") %>%
    stringr::str_replace("area", "") %>%
    stringr::str_trim() %>%
    stringr::str_to_title()

  title_fid_area_letters <- gsub("[^a-zA-Z]", " ", title_fid_area) %>%
    stringr::str_trim()

  scenario <- ifelse(title_fid_area_letters == "Forest", "Forest vegetation (not trimmed)",
                     ifelse(title_fid_area_letters == "Forest Roads Railways", "Forest vegetation (trimmed)",
                            ifelse(title_fid_area_letters == "Natural", "Natural vegetation (not trimmed)",
                                   ifelse(title_fid_area_letters == "Natural Roads Railways", "Natural vegetation (trimmed)", NA))))


  year <- gsub("[^0-9]", " ", title_fid_area) %>%
    stringr::str_trim()

  data_fid_area_i <- readr::read_csv(i, col_names = c("values", "area", "n"), col_types = cols()) %>%
    dplyr::mutate(area_ha = area/1e4)

  # figure 01
  data_fid_area_i_class <- data_fid_area_i %>%
    dplyr::mutate(class = case_when(area_ha < 1 ~ "<1",
                                    area_ha >= 1 & area_ha < 5 ~ "1-5",
                                    area_ha >= 5 & area_ha < 10 ~ "5-10",
                                    area_ha >= 10 & area_ha < 50 ~ "10-50",
                                    area_ha >= 50 & area_ha < 100 ~ "50-100",
                                    area_ha >= 100 & area_ha < 250 ~ "100-250",
                                    area_ha >= 250 & area_ha < 500 ~ "250-500",
                                    area_ha >= 500 & area_ha < 1000 ~ "500-1,000",
                                    area_ha >= 1000 & area_ha < 2500 ~ "1,000-2,500",
                                    area_ha >= 2500 & area_ha < 5000 ~ "2,500-5,000",
                                    area_ha >= 5000 & area_ha < 10000 ~ "5,000-10,000",
                                    area_ha >= 10000 & area_ha < 25000 ~ "10,000-25,000",
                                    area_ha >= 25000 & area_ha < 50000 ~ "25,000-50,000",
                                    area_ha >= 50000 & area_ha < 100000 ~ "50,000-100,000",
                                    area_ha >= 100000 & area_ha < 250000 ~ "100,000-250,000",
                                    area_ha >= 250000 & area_ha < 500000 ~ "250,000-500,000",
                                    area_ha >= 500000 & area_ha < 1000000 ~ "500,000-1,000,000",
                                    area_ha >= 1000000 ~ ">1,000,000")) %>%
    dplyr::bind_rows(tibble::tibble(values = 0, area = 0, n = 0, area_ha = 0, class = ">1,000,000")) %>%
    dplyr::mutate(class = forcats::as_factor(class)) %>%
    dplyr::mutate(class = forcats::fct_relevel(class,
                                               c("<1", "1-5", "5-10", "10-50",
                                                 "50-100", "100-250",
                                                 "250-500", "500-1,000",
                                                 "1,000-2,500", "2,500-5,000",
                                                 "5,000-10,000", "10,000-25,000",
                                                 "25,000-50,000", "50,000-100,000",
                                                 "100,000-250,000", "250,000-500,000",
                                                 "500,000-1,000,000", ">1,000,000")))
  data_fid_area_i_class_resume <- data_fid_area_i_class %>%
    dplyr::group_by(class) %>%
    dplyr::summarise(class_area = sum(area_ha),
                     class_n = n()) %>%
    dplyr::mutate(class_area_per = round(class_area/sum(class_area)*100, 1),
                  class_n_per = round(class_n/sum(class_n)*100, 2)) %>%
    dplyr::mutate(year = year,
                  scenario = scenario)
  data_fid_area <- dplyr::bind_rows(data_fid_area, data_fid_area_i_class_resume)


  # figure 03
  data_fid_area_mean_i <- data_fid_area_i %>%
    dplyr::mutate(year = year,
                  scenario = scenario) %>%
    dplyr::group_by(year, scenario) %>%
    dplyr::summarise(n_patches = n(),
                     mean_area_ha = mean(area_ha))
  data_fid_area_mean <- dplyr::bind_rows(data_fid_area_mean, data_fid_area_mean_i)

  # resume
  data_fid_area_resume_i <- tibble::tibble(year = year,
                                           scenario = scenario,
                                           n_patches = nrow(data_fid_area_i),
                                           total_area_ha = round(sum(data_fid_area_i$area_ha), 0),
                                           percentage = round(total_area_ha/162742129*100, 2),
                                           mean_area_ha = round(mean(data_fid_area_i$area_ha), 0),
                                           sd_area_ha = round(sd(data_fid_area_i$area_ha), 0),
                                           cv_area_ha = round(sd_area_ha/mean_area_ha, 0),
                                           min_area_ha = min(data_fid_area_i$area_ha),
                                           first_quarter_area_ha = quantile(data_fid_area_i$area_ha, probs = .25),
                                           median_area_ha = median(data_fid_area_i$area_ha),
                                           third_quarter_area_ha = quantile(data_fid_area_i$area_ha, probs = .75),
                                           max_area_ha = round(max(data_fid_area_i$area_ha), 0))

  data_fid_area_resume <- dplyr::bind_rows(data_fid_area_resume, data_fid_area_resume_i)

}

data_fid_area
data_fid_area_mean
data_fid_area_resume

# export
readr::write_csv(data_fid_area, "03_tables/data_fig01.csv")
readr::write_csv(data_fid_area_mean, "03_tables/data_fig03.csv")

## analysis ----

#### size classes -----
data_fid_area_resume_1ha <- data_fid_area %>%
  dplyr::filter(year %in% c(1986, 2005, 2020),
                scenario %in% c("Forest vegetation (trimmed)", "Natural vegetation (trimmed)"),
                class %in% c("<1")) %>%
  group_by(year, scenario) %>%
  summarise(class_fid_area_per_sum = sum(class_area_per),
            class_n_per_sum = sum(class_n_per)) %>%
  dplyr::arrange(scenario)
data_fid_area_resume_1ha

data_fid_area_resume_5ha <- data_fid_area %>%
  dplyr::filter(year %in% c(1986, 2005, 2020),
                scenario %in% c("Forest vegetation (trimmed)", "Natural vegetation (trimmed)"),
                class %in% c("<1", "1-5")) %>%
  group_by(year, scenario) %>%
  summarise(class_fid_area_per_sum = sum(class_area_per),
            class_n_per_sum = sum(class_n_per)) %>%
  dplyr::arrange(scenario)
data_fid_area_resume_5ha

data_fid_area_resume_10ha <- data_fid_area %>%
  dplyr::filter(year %in% c(1986, 2005, 2020),
                scenario %in% c("Forest vegetation (trimmed)", "Natural vegetation (trimmed)"),
                class %in% c("<1", "1-5", "5-10")) %>%
  group_by(year, scenario) %>%
  summarise(class_fid_area_per_sum = sum(class_area_per),
            class_n_per_sum = sum(class_n_per)) %>%
  dplyr::arrange(scenario)
data_fid_area_resume_10ha

data_fid_area_resume_1_50ha <- data_fid_area %>%
  dplyr::filter(year %in% c(1986, 2005, 2020),
                scenario %in% c("Forest vegetation (trimmed)", "Natural vegetation (trimmed)"),
                class %in% c("<1", "1-5", "5-10", "10-50")) %>%
  group_by(year, scenario) %>%
  summarise(class_fid_area_per_sum = sum(class_area_per),
            class_n_per_sum = sum(class_n_per)) %>%
  dplyr::arrange(scenario)
data_fid_area_resume_1_50ha

data_fid_area_resume_50_25kha <- data_fid_area %>%
  dplyr::filter(year %in% c(1986, 2005, 2020),
                scenario %in% c("Forest vegetation (trimmed)", "Natural vegetation (trimmed)"),
                class %in% c("50-100", "100-250", "250-500", "500-1,000", "1,000-2,500", "2,500-5,000", "5,000-10,000", "10,000-25,000")) %>%
  group_by(year, scenario) %>%
  summarise(class_fid_area_per_sum = sum(class_area_per),
            class_n_per_sum = sum(class_n_per)) %>%
  dplyr::arrange(scenario)
data_fid_area_resume_50_25kha

data_fid_area_resume_25kha <- data_fid_area %>%
  dplyr::filter(year %in% c(1986, 2005, 2020),
                scenario %in% c("Forest vegetation (trimmed)", "Natural vegetation (trimmed)"),
                class %in% c("25,000-50,000", "50,000-100,000", "100,000-250,000", "250,000-500,000", "500,000-1,000,000", ">1,000,000")) %>%
  group_by(year, scenario) %>%
  summarise(class_fid_area_per_sum = sum(class_area_per),
            class_n_per_sum = sum(class_n_per)) %>%
  dplyr::arrange(scenario)
data_fid_area_resume_25kha

data_fid_area_resume_500kha <- data_fid_area %>%
  dplyr::filter(year %in% c(1986, 2005, 2020),
                scenario %in% c("Forest vegetation (trimmed)", "Natural vegetation (trimmed)"),
                class %in% c("500,000-1,000,000", ">1,000,000")) %>%
  group_by(year, scenario) %>%
  summarise(class_fid_area_per_sum = sum(class_area_per),
            class_n_per_sum = sum(class_n_per)) %>%
  dplyr::arrange(scenario)
data_fid_area_resume_500kha

data_fid_area_resume_1mha <- data_fid_area %>%
  dplyr::filter(year %in% c(1986, 2005, 2020),
                scenario %in% c("Forest vegetation (trimmed)", "Natural vegetation (trimmed)"),
                class %in% c(">1,000,000")) %>%
  group_by(year, scenario) %>%
  summarise(class_fid_area_per_sum = sum(class_area_per),
            class_n_per_sum = sum(class_n_per)) %>%
  dplyr::arrange(scenario)
data_fid_area_resume_1mha

#### number of patches and mean area -----
data_fid_area_resume %>%
  dplyr::filter(scenario %in% c("Forest vegetation (not trimmed)", "Forest vegetation (trimmed)"),
                year %in% c("1986", "2005", "2020")) %>%
  dplyr::arrange(scenario) %>%
  dplyr::select(year, scenario, n_patches, mean_area_ha)

data_fid_area_resume %>%
  dplyr::filter(scenario %in% c("Natural vegetation (not trimmed)", "Natural vegetation (trimmed)"),
                year %in% c("1986", "2005", "2020")) %>%
  dplyr::arrange(scenario) %>%
  dplyr::select(year, scenario, n_patches, mean_area_ha)

data_fid_area_resume %>%
  dplyr::select(year, scenario, n_patches) %>%
  dplyr::filter(year %in% c(1986, 2005, 2020)) %>%
  tidyr::pivot_wider(names_from = scenario, values_from = n_patches) %>%
  janitor::clean_names()

#### percentage change ----
data_fid_area_resume_pct_change <- NULL

for(i in unique(data_fid_area_resume$scenario)){

  data_fid_area_resume_i <- data_fid_area_resume %>%
    dplyr::select(year, scenario, n_patches, mean_area_ha, max_area_ha) %>%
    dplyr::filter(scenario == i)

  data_fid_area_resume_pct_change_i_2005_1986 <- data_fid_area_resume_i %>%
    dplyr::filter(year %in% c(1986, 2005)) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::mutate(pct_change_n_patches = (n_patches/lag(n_patches) - 1) * 100,
                  pct_change_mean_area_ha = (mean_area_ha/lag(mean_area_ha) - 1) * 100,
                  pct_change_max_area_ha = (max_area_ha/lag(max_area_ha) - 1) * 100) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(year = "2005-1986")

  data_fid_area_resume_pct_change_i_2020_1986 <- data_fid_area_resume_i %>%
    dplyr::filter(year %in% c(1986, 2020)) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::mutate(pct_change_n_patches = (n_patches/lag(n_patches) - 1) * 100,
                  pct_change_mean_area_ha = (mean_area_ha/lag(mean_area_ha) - 1) * 100,
                  pct_change_max_area_ha = (max_area_ha/lag(max_area_ha) - 1) * 100) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(year = "2020-1986")

  data_fid_area_resume_pct_change_i_2020_2005 <- data_fid_area_resume_i %>%
    dplyr::filter(year %in% c(2005, 2020)) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::mutate(pct_change_n_patches = (n_patches/lag(n_patches) - 1) * 100,
                  pct_change_mean_area_ha = (mean_area_ha/lag(mean_area_ha) - 1) * 100,
                  pct_change_max_area_ha = (max_area_ha/lag(max_area_ha) - 1) * 100) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(year = "2020-2005")

  data_fid_area_resume_pct_change_i <- dplyr::bind_rows(data_fid_area_resume_pct_change_i_2005_1986,
                                                        data_fid_area_resume_pct_change_i_2020_1986,
                                                        data_fid_area_resume_pct_change_i_2020_2005)

  data_fid_area_resume_pct_change <- dplyr::bind_rows(data_fid_area_resume_pct_change, data_fid_area_resume_pct_change_i)

}
data_fid_area_resume_pct_change

# maximum area trimmed
data_fid_area_resume_max_area_trimmed <- tibble()
for(i in seq(1, 31, 2)){

  max_i <- ((data_fid_area_resume[i+1, ]$max_area_ha - data_fid_area_resume[i, ]$max_area_ha)/data_fid_area_resume[i, ]$max_area_ha) * 100 # forest max area patches 1986 percent - trimmed roads
  max_p <- tibble(scenario = paste0(data_fid_area_resume[i, 1], "_", data_fid_area_resume[i, 2]),
                  max_por = max_i)
  data_fid_area_resume_max_area_trimmed <- dplyr::bind_rows(data_fid_area_resume_max_area_trimmed, max_p)

}
dplyr::arrange(data_fid_area_resume_max_area_trimmed, max_por)

#### temporal analysis ----

# import
forest_1986_2005 <- readr::read_csv("02_results/01_mapbiomas_brazil_af_trinacional_1986_af_lim_forest_mapbiomas_brazil_af_trinacional_2005_af_lim_forest_temporal_patches_metrics.csv") %>%
  dplyr::mutate(period = "1986_2005",
                scenario = "forest", .before = 1)
forest_1986_2005

forest_1986_2020 <- readr::read_csv("02_results/01_mapbiomas_brazil_af_trinacional_1986_af_lim_forest_mapbiomas_brazil_af_trinacional_2020_af_lim_forest_temporal_patches_metrics.csv") %>%
  dplyr::mutate(period = "1986_2020",
                scenario = "forest", .before = 1)
forest_1986_2020

forest_2005_2020 <- readr::read_csv("02_results/01_mapbiomas_brazil_af_trinacional_2005_af_lim_forest_mapbiomas_brazil_af_trinacional_2020_af_lim_forest_temporal_patches_metrics.csv") %>%
  dplyr::mutate(period = "2005_2020",
                scenario = "forest", .before = 1)
forest_2005_2020

natural_1986_2005 <- readr::read_csv("02_results/01_mapbiomas_brazil_af_trinacional_1986_af_lim_natural_mapbiomas_brazil_af_trinacional_2005_af_lim_natural_temporal_patches_metrics.csv") %>%
  dplyr::mutate(period = "1986_2005",
                scenario = "natural", .before = 1)
natural_1986_2005

natural_1986_2020 <- readr::read_csv("02_results/01_mapbiomas_brazil_af_trinacional_1986_af_lim_natural_mapbiomas_brazil_af_trinacional_2020_af_lim_natural_temporal_patches_metrics.csv") %>%
  dplyr::mutate(period = "1986_2020",
                scenario = "natural", .before = 1)
natural_1986_2020

natural_2005_2020 <- readr::read_csv("02_results/01_mapbiomas_brazil_af_trinacional_2005_af_lim_natural_mapbiomas_brazil_af_trinacional_2020_af_lim_natural_temporal_patches_metrics.csv") %>%
  dplyr::mutate(period = "2005_2020",
                scenario = "natural", .before = 1)
natural_2005_2020

# import roads
forest_1986_2005_roads_railways <- readr::read_csv("02_results/01_mapbiomas_brazil_af_trinacional_1986_af_lim_forest_roads_railways_mapbiomas_brazil_af_trinacional_2005_af_lim_forest_roads_railways_temporal_patches_metrics.csv") %>%
  dplyr::mutate(period = "1986_2005",
                scenario = "forest_roads_railways", .before = 1)
forest_1986_2005_roads_railways

forest_1986_2020_roads_railways <- readr::read_csv("02_results/01_mapbiomas_brazil_af_trinacional_1986_af_lim_forest_roads_railways_mapbiomas_brazil_af_trinacional_2020_af_lim_forest_roads_railways_temporal_patches_metrics.csv") %>%
  dplyr::mutate(period = "1986_2020",
                scenario = "forest_roads_railways", .before = 1)
forest_1986_2020_roads_railways

forest_2005_2020_roads_railways <- readr::read_csv("02_results/01_mapbiomas_brazil_af_trinacional_2005_af_lim_forest_roads_railways_mapbiomas_brazil_af_trinacional_2020_af_lim_forest_roads_railways_temporal_patches_metrics.csv") %>%
  dplyr::mutate(period = "2005_2020",
                scenario = "forest_roads_railways", .before = 1)
forest_2005_2020_roads_railways

natural_1986_2005_roads_railways <- readr::read_csv("02_results/01_mapbiomas_brazil_af_trinacional_1986_af_lim_natural_roads_railways_mapbiomas_brazil_af_trinacional_2005_af_lim_natural_roads_railways_temporal_patches_metrics.csv") %>%
  dplyr::mutate(period = "1986_2005",
                scenario = "natural_roads_railways", .before = 1)
natural_1986_2005_roads_railways

natural_1986_2020_roads_railways <- readr::read_csv("02_results/01_mapbiomas_brazil_af_trinacional_1986_af_lim_natural_roads_railways_mapbiomas_brazil_af_trinacional_2020_af_lim_natural_roads_railways_temporal_patches_metrics.csv") %>%
  dplyr::mutate(period = "1986_2020",
                scenario = "natural_roads_railways", .before = 1)
natural_1986_2020_roads_railways

natural_2005_2020_roads_railways <- readr::read_csv("02_results/01_mapbiomas_brazil_af_trinacional_2005_af_lim_natural_roads_railways_mapbiomas_brazil_af_trinacional_2020_af_lim_natural_roads_railways_temporal_patches_metrics.csv") %>%
  dplyr::mutate(period = "2005_2020",
                scenario = "natural_roads_railways", .before = 1)
natural_2005_2020_roads_railways

# organize
data_temporal <- dplyr::bind_rows(forest_1986_2005, forest_1986_2020, forest_2005_2020,
                                  natural_1986_2005, natural_1986_2020, natural_2005_2020,

                                  forest_1986_2005_roads_railways, forest_1986_2020_roads_railways, forest_2005_2020_roads_railways,
                                  natural_1986_2005_roads_railways, natural_1986_2020_roads_railways, natural_2005_2020_roads_railways)
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
    scenario == "forest" ~ "Forest vegetation (not trimmed)",
    scenario == "forest_roads_railways" ~ "Forest vegetation (trimmed)",
    scenario == "natural" ~ "Natural vegetation (not trimmed)",
    scenario == "natural_roads_railways" ~ "Natural vegetation (trimmed)"))
data_habitat_cover_resume_total

# export
readr::write_csv(data_habitat_cover_resume_total, "03_tables/data_fig04.csv")

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

data_habitat_cover_resume_total_br <- dplyr::bind_rows(data_habitat_cover_br, data_habitat_cover_br_resume) %>%
  dplyr::arrange(classes) %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed)",
    scenario == "forest_roads_railways" ~ "Forest vegetation (trimmed)",
    scenario == "natural" ~ "Natural vegetation (not trimmed)",
    scenario == "natural_roads_railways" ~ "Natural vegetation (trimmed)"))
data_habitat_cover_resume_total_br

readr::write_csv(data_habitat_cover_resume_total_br, paste0("03_tables/data_fig04_br.csv"))

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

data_habitat_cover_resume_total_ar <- dplyr::bind_rows(data_habitat_cover_ar, data_habitat_cover_ar_resume) %>%
  dplyr::arrange(classes) %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed)",
    scenario == "forest_roads_railways" ~ "Forest vegetation (trimmed)",
    scenario == "natural" ~ "Natural vegetation (not trimmed)",
    scenario == "natural_roads_railways" ~ "Natural vegetation (trimmed)"))
data_habitat_cover_resume_total_ar

readr::write_csv(data_habitat_cover_resume_total_ar, paste0("03_tables/data_fig04_ar.csv"))

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

data_habitat_cover_resume_total_py <- dplyr::bind_rows(data_habitat_cover_py, data_habitat_cover_py_resume) %>%
  dplyr::arrange(classes) %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed)",
    scenario == "forest_roads_railways" ~ "Forest vegetation (trimmed)",
    scenario == "natural" ~ "Natural vegetation (not trimmed)",
    scenario == "natural_roads_railways" ~ "Natural vegetation (trimmed)"))
data_habitat_cover_resume_total_py

readr::write_csv(data_habitat_cover_resume_total_py, paste0("03_tables/data_fig04_py.csv"))

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
    scenario == "forest" ~ "Forest vegetation (not trimmed)",
    scenario == "forest_roads_railways" ~ "Forest vegetation (trimmed)",
    scenario == "natural" ~ "Natural vegetation (not trimmed)",
    scenario == "natural_roads_railways" ~ "Natural vegetation (trimmed)"))
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
    scenario == "forest" ~ "Forest vegetation (not trimmed)",
    scenario == "forest_roads_railways" ~ "Forest vegetation (trimmed)",
    scenario == "natural" ~ "Natural vegetation (not trimmed)",
    scenario == "natural_roads_railways" ~ "Natural vegetation (trimmed)")) %>%
  dplyr::left_join(data_limits_habitat_cover_filter)
data_limits_habitat_cover_area

# 3  core and edge area ---------------------------------------------------

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
  dplyr::filter(scenario == "forest roads railways", edge_dist == 30) %>%
  dplyr::group_by(year, scenario)
data_core_edge_area_30m_forest

data_core_edge_area_30m_natural <- data_core_edge_area %>%
  dplyr::filter(scenario == "natural roads railways", edge_dist == 30)
data_core_edge_area_30m_natural

data_core_edge_area_90m_forest <- data_core_edge_area %>%
  dplyr::filter(scenario == "forest roads railways", edge_dist == 90)
data_core_edge_area_90m_forest

data_core_edge_area_90m_natural <- data_core_edge_area %>%
  dplyr::filter(scenario == "natural roads railways", edge_dist == 90)
data_core_edge_area_90m_natural

data_core_edge_area_240m_forest <- data_core_edge_area %>%
  dplyr::filter(scenario == "forest roads railways", edge_dist == 240)
data_core_edge_area_240m_forest

data_core_edge_area_240m_natural <- data_core_edge_area %>%
  dplyr::filter(scenario == "natural roads railways", edge_dist == 240)
data_core_edge_area_240m_natural

data_core_edge_area_500m_forest <- data_core_edge_area %>%
  dplyr::filter(scenario == "forest roads railways", edge_dist >= 500) %>%
  group_by(scenario, year) %>%
  summarise(per_sum = sum(per))
data_core_edge_area_500m_forest

data_core_edge_area_500m_natural <- data_core_edge_area %>%
  dplyr::filter(scenario == "natural roads railways", edge_dist > 500) %>%
  group_by(scenario, year) %>%
  summarise(per_sum = sum(per))
data_core_edge_area_500m_natural

data_core_edge_area %>%
  dplyr::filter(scenario == "forest roads railways") %>%
  dplyr::pull(edge_dist) %>%
  max()

data_core_edge_area %>%
  dplyr::filter(scenario == "natural roads railways") %>%
  dplyr::pull(edge_dist) %>%
  max()

# export
data_core_edge_area <- data_core_edge_area %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed)",
    scenario == "forest roads railways" ~ "Forest vegetation (trimmed)",
    scenario == "natural" ~ "Natural vegetation (not trimmed)",
    scenario == "natural roads railways" ~ "Natural vegetation (trimmed)"))
data_core_edge_area

readr::write_csv(data_core_edge_area, "03_tables/data_fig05.csv")

# 4 functional connectivity ---------------------------------------------

### data ----

# years
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
years

# list files
files_connectivity <- dir(path = "02_results", pattern = "confun", full.names = TRUE)
files_connectivity

# import data
scenarios <- c("forest", "natural", "forest_roads_railways", "natural_roads_railways")
gaps <- c("0060", "0120", "0180", "0240", "0300", "0600", "0900", "1200", "1500")

data_connectivity <- NULL

for(i in years){

  print(i)

  files_connectivity_i <- stringr::str_subset(files_connectivity, paste0("_", i, "_"))
  data_connectivity_i <- NULL

  for(j in scenarios){

    area_i <- readr::read_csv(paste0("02_results/01_mapbiomas_brazil_af_trinacional_", i, "_af_lim_", sub("_confun", "", j), "_fid_area.csv"),
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
    scenario == "forest" ~ "Forest vegetation (not trimmed)",
    scenario == "forest_roads_railways" ~ "Forest vegetation (trimmed)",
    scenario == "natural" ~ "Natural vegetation (not trimmed)",
    scenario == "natural_roads_railways" ~ "Natural vegetation (trimmed)"))
data_connectivity_summary

# export
readr::write_csv(data_connectivity_summary, "03_tables/data_fig06.csv")

## summary ----
data_connectivity_summary_sel <- data_connectivity_summary %>%
  dplyr::filter(year %in% c(1986, 2005, 2020),
                scenario %in% c("Forest vegetation (trimmed)",
                                "Natural vegetation (trimmed)"),
                gap_crossing == 0)
data_connectivity_summary_sel

# percentage change
(data_connectivity_summary_sel[5, 4] - data_connectivity_summary_sel[1, 4])/data_connectivity_summary_sel[1, 4]*100
(data_connectivity_summary_sel[5, 4] - data_connectivity_summary_sel[3, 4])/data_connectivity_summary_sel[3, 4]*100

(data_connectivity_summary_sel[4, 4] - data_connectivity_summary_sel[2, 4])/data_connectivity_summary_sel[2, 4]*100
(data_connectivity_summary_sel[6, 4] - data_connectivity_summary_sel[2, 4])/data_connectivity_summary_sel[2, 4]*100

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
    scenario == "forest" ~ "Forest vegetation (not trimmed)",
    scenario == "forest_roads_railways" ~ "Forest vegetation (trimmed)",
    scenario == "natural" ~ "Natural vegetation (not trimmed)",
    scenario == "natural_roads_railways" ~ "Natural vegetation (trimmed)"))
data_isolation_resume

readr::write_csv(data_isolation_resume, "03_tables/data_fig07.csv")

## statistics ----
data_isolation_resume_000_forest <- data_isolation_resume %>%
  dplyr::filter(area == "0000",
                scenario == "Forest vegetation (trimmed)") %>%
  dplyr::arrange(scenario, year)
data_isolation_resume_000_forest

data_isolation_resume_000_natural <- data_isolation_resume %>%
  dplyr::filter(area == "0000",
                scenario == "Natural vegetation (trimmed)") %>%
  dplyr::arrange(scenario, year)
data_isolation_resume_000_natural

data_isolation_resume_050_forest <- data_isolation_resume %>%
  dplyr::filter(area == "0050",
                scenario == "Forest vegetation (trimmed)") %>%
  dplyr::arrange(scenario, year)
data_isolation_resume_050_forest

data_isolation_resume_050_natural <- data_isolation_resume %>%
  dplyr::filter(area == "0050",
                scenario == "Natural vegetation (trimmed)") %>%
  dplyr::arrange(scenario, year)
data_isolation_resume_050_natural

sort(data_isolation_resume_000_forest$mean - data_isolation_resume_050_forest$mean)/data_isolation_resume_050_forest$mean * 100
sort(data_isolation_resume_000_natural$mean - data_isolation_resume_050_natural$mean)/data_isolation_resume_050_natural$mean * 100
sort(data_isolation_resume_000_natural$mean - data_isolation_resume_000_forest$mean)/data_isolation_resume_000_forest$mean * 100

data_isolation_resume_100 <- data_isolation_resume %>%
  dplyr::filter(area == "0100",
                scenario %in% c("Forest vegetation (trimmed)",
                                "Natural vegetation (trimmed)")) %>%
  dplyr::arrange(scenario, year)
data_isolation_resume_100

data_isolation_resume_150 <- data_isolation_resume %>%
  dplyr::filter(area == "0150",
                scenario %in% c("Forest vegetation (trimmed)",
                                "Natural vegetation (trimmed)")) %>%
  dplyr::arrange(scenario, year)
data_isolation_resume_150

data_isolation_resume_200 <- data_isolation_resume %>%
  dplyr::filter(area == "0200",
                scenario %in% c("Forest vegetation (trimmed)",
                                "Natural vegetation (trimmed)")) %>%
  dplyr::arrange(scenario, year)
data_isolation_resume_200

data_isolation_resume_250 <- data_isolation_resume %>%
  dplyr::filter(area == "0250",
                scenario %in% c("Forest vegetation (trimmed)",
                                "Natural vegetation (trimmed)")) %>%
  dplyr::arrange(scenario, year)
data_isolation_resume_250

data_isolation_resume_350 <- data_isolation_resume %>%
  dplyr::filter(area == "0350",
                scenario %in% c("Forest vegetation (trimmed)",
                                "Natural vegetation (trimmed)")) %>%
  dplyr::arrange(scenario, year)
data_isolation_resume_350

data_isolation_resume_500 <- data_isolation_resume %>%
  dplyr::filter(area == "0500",
                scenario %in% c("Forest vegetation (trimmed)",
                                "Natural vegetation (trimmed)")) %>%
  dplyr::arrange(scenario, year)
data_isolation_resume_500

data_isolation_resume_1000 <- data_isolation_resume %>%
  dplyr::filter(area == "1000",
                scenario %in% c("Forest vegetation (trimmed)",
                                "Natural vegetation (trimmed)")) %>%
  dplyr::arrange(scenario, year)
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

# analysis
data_protected_area %>%
  dplyr::filter(scenario == "forest_roads_railways",
                class == "PA") %>%
  dplyr::pull(area_ha_total) %>%
  sum()/1e6

data_protected_area %>%
  dplyr::filter(scenario == "forest_roads_railways",
                class == "PA") %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_protected_area %>%
  dplyr::filter(scenario == "natural_roads_railways",
                class == "PA") %>%
  dplyr::pull(area_ha_total) %>%
  sum()/1e6

data_protected_area %>%
  dplyr::filter(scenario == "natural_roads_railways",
                class == "PA") %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_protected_area %>%
  dplyr::filter(scenario == "forest_roads_railways",
                class %in% c("<100", "100-250", "250-500", "500-\n1000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_protected_area %>%
  dplyr::filter(scenario == "natural_roads_railways",
                class %in% c("<100", "100-250", "250-500", "500-\n1000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_protected_area %>%
  dplyr::filter(scenario == "forest_roads_railways",
                class %in% c("<100", "100-250", "250-500", "500-\n1000", "1000-\n2500", "2500-\n5000", "5000-\n10000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_protected_area %>%
  dplyr::filter(scenario == "natural_roads_railways",
                class %in% c("<100", "100-250", "250-500", "500-\n1000", "1000-\n2500", "2500-\n5000", "5000-\n10000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_protected_area %>%
  dplyr::filter(scenario == "forest_roads_railways",
                class %in% c("10000-\n25000", "25000-\n50000", ">50000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_protected_area %>%
  dplyr::filter(scenario == "natural_roads_railways",
                class %in% c("10000-\n25000", "25000-\n50000", ">50000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

# export
data_protected_area_summary <- data_protected_area %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed)",
    scenario == "forest_roads_railways" ~ "Forest vegetation (trimmed)",
    scenario == "natural" ~ "Natural vegetation (not trimmed)",
    scenario == "natural_roads_railways" ~ "Natural vegetation (trimmed)"))
data_protected_area_summary

readr::write_csv(data_protected_area_summary, "03_tables/data_fig08_pa.csv")

### classes ----

# classes
classes_habitat_cover <- tibble::tibble(
  values = c(0, 3, 4, 5, 11, 12, 13, 32, 49, 50),
  classes = c("Matrix", "Forest formation", "Savanna formation", "Mangrove",
              "Wetland", "Grassland", "Other non forest formations",
              "Salt flat", "Wooded sandbank vegetation",
              "Herbaceous sandbank vegetation"))
classes_habitat_cover

# not trimmed
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

# trimmed
data_protected_area_natural_classes_roads <- readr::read_csv("02_results/06_mapbiomas_brazil_af_trinacional_2020_af_lim_natural_classes_roads_railways_protected_areas.csv",
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
readr::write_csv(data_protected_area_classes, "03_tables/data_fig08_classes_pa.csv")


## indigenous territories ----

### vegetation ----

# import mean data
files_indigenous_territories <- dir(path = "02_results", pattern = "06_mapbiomas", full.names = TRUE) %>%
  stringr::str_subset("indigenous_territories") %>%
  stringr::str_subset("classes", negate = TRUE)
files_indigenous_territories

data_indigenous_territories <- NULL
for(i in files_indigenous_territories){

  names_indigenous_territories <- i %>%
    basename() %>%
    stringr::str_split("_", simplify = TRUE)

  data_indigenous_territories_i <- readr::read_csv(i, col_names = c("vegetation", "dist_pa", "area", "n"), col_types = readr::cols()) %>%
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
    dplyr::mutate(scenario = ifelse(length(names_indigenous_territories) == 13,
                                    paste0(names_indigenous_territories[, 9], "_", names_indigenous_territories[, 10], "_", names_indigenous_territories[, 11]),
                                    names_indigenous_territories[, 9]),
                  .before = 1)
  data_indigenous_territories_i

  data_indigenous_territories <- dplyr::bind_rows(data_indigenous_territories, data_indigenous_territories_i)

}
data_indigenous_territories

# analysis
data_indigenous_territories %>%
  dplyr::filter(scenario == "forest_roads_railways",
                class == "IT") %>%
  dplyr::pull(area_ha_total) %>%
  sum()/1e6

data_indigenous_territories %>%
  dplyr::filter(scenario == "forest_roads_railways",
                class == "IT") %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_indigenous_territories %>%
  dplyr::filter(scenario == "natural_roads_railways",
                class == "IT") %>%
  dplyr::pull(area_ha_total) %>%
  sum()/1e6

data_indigenous_territories %>%
  dplyr::filter(scenario == "natural_roads_railways",
                class == "IT") %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_indigenous_territories %>%
  dplyr::filter(scenario == "forest_roads_railways",
                class %in% c("<100", "100-250", "250-500", "500-\n1000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_indigenous_territories %>%
  dplyr::filter(scenario == "natural_roads_railways",
                class %in% c("<100", "100-250", "250-500", "500-\n1000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_indigenous_territories %>%
  dplyr::filter(scenario == "forest_roads_railways",
                class %in% c("<100", "100-250", "250-500", "500-\n1000", "1000-\n2500", "2500-\n5000", "5000-\n10000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_indigenous_territories %>%
  dplyr::filter(scenario == "natural_roads_railways",
                class %in% c("<100", "100-250", "250-500", "500-\n1000", "1000-\n2500", "2500-\n5000", "5000-\n10000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_indigenous_territories %>%
  dplyr::filter(scenario == "forest_roads_railways",
                class %in% c("10000-\n25000", "25000-\n50000", ">50000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

data_indigenous_territories %>%
  dplyr::filter(scenario == "natural_roads_railways",
                class %in% c("10000-\n25000", "25000-\n50000", ">50000")) %>%
  dplyr::pull(n_total_per) %>%
  sum()

# export
data_indigenous_territories_summary <- data_indigenous_territories %>%
  dplyr::mutate(scenario = dplyr::case_when(
    scenario == "forest" ~ "Forest vegetation (not trimmed)",
    scenario == "forest_roads_railways" ~ "Forest vegetation (trimmed)",
    scenario == "natural" ~ "Natural vegetation (not trimmed)",
    scenario == "natural_roads_railways" ~ "Natural vegetation (trimmed)"))
data_indigenous_territories_summary

readr::write_csv(data_indigenous_territories_summary, "03_tables/data_fig08_it.csv")

### classes ----

# classes
classes_habitat_cover <- tibble::tibble(
  values = c(0, 3, 4, 5, 11, 12, 13, 32, 49, 50),
  classes = c("Matrix", "Forest formation", "Savanna formation", "Mangrove",
              "Wetland", "Grassland", "Other non forest formations",
              "Salt flat", "Wooded sandbank vegetation",
              "Herbaceous sandbank vegetation"))
classes_habitat_cover

# not trimmed
data_indigenous_territories_natural_classes <- readr::read_csv("02_results/06_mapbiomas_brazil_af_trinacional_2020_af_lim_natural_classes_indigenous_territories_classes.csv",
                                                               col_names = c("values", "pa", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(area_ha = area/1e4,
                per_total = round(area_ha/sum(area_ha) * 100, 3)) %>%
  dplyr::select(1, 2, 5, 6)
data_indigenous_territories_natural_classes

data_indigenous_territories_natural_classes_per <- data_indigenous_territories_natural_classes %>%
  dplyr::group_by(values) %>%
  dplyr::summarise(area_ha_class_sum = sum(area_ha))
data_indigenous_territories_natural_classes_per

data_indigenous_territories_natural_classes_per_veg <- data_indigenous_territories_natural_classes_per %>%
  filter(values != 0) %>%
  pull(area_ha_class_sum) %>%
  sum()
data_indigenous_territories_natural_classes_per_veg

data_indigenous_territories_natural_classes_per_total <- data_indigenous_territories_natural_classes %>%
  dplyr::left_join(data_indigenous_territories_natural_classes_per) %>%
  dplyr::left_join(classes_habitat_cover) %>%
  dplyr::mutate(per_protec = round(area_ha/area_ha_class_sum * 100, 2),
                per_class = round(area_ha_class_sum/data_indigenous_territories_natural_classes_per_veg * 100, 2),
                scenario = "not_trimmed") %>%
  dplyr::filter(values > 0, pa == 0) %>%
  dplyr::select(scenario, classes, values, area_ha, area_ha_class_sum, per_protec, per_class, per_total)
data_indigenous_territories_natural_classes_per_total

# trimmed
data_indigenous_territories_natural_classes_roads <- readr::read_csv("02_results/06_mapbiomas_brazil_af_trinacional_2020_af_lim_natural_classes_roads_railways_indigenous_territories_classes.csv",
                                                                     col_names = c("values", "pa", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(area_ha = area/1e4,
                per_total = round(area_ha/sum(area_ha) * 100, 3)) %>%
  dplyr::select(1, 2, 5, 6)
data_indigenous_territories_natural_classes_roads

data_indigenous_territories_natural_classes_roads_per <- data_indigenous_territories_natural_classes_roads %>%
  dplyr::group_by(values) %>%
  dplyr::summarise(area_ha_class_sum = sum(area_ha))
data_indigenous_territories_natural_classes_roads_per

data_indigenous_territories_natural_classes_roads_per_veg <- data_indigenous_territories_natural_classes_roads_per %>%
  filter(values != 0) %>%
  pull(area_ha_class_sum) %>%
  sum()
data_indigenous_territories_natural_classes_roads_per_veg

data_indigenous_territories_natural_classes_roads_per_total <- data_indigenous_territories_natural_classes_roads %>%
  dplyr::left_join(data_indigenous_territories_natural_classes_roads_per) %>%
  dplyr::left_join(classes_habitat_cover) %>%
  dplyr::mutate(per_protec = round(area_ha/area_ha_class_sum * 100, 2),
                per_class = round(area_ha_class_sum/data_indigenous_territories_natural_classes_roads_per_veg * 100, 2),
                scenario = "trimmed") %>%
  dplyr::filter(values > 0, pa == 0) %>%
  dplyr::select(scenario, classes, values, area_ha, area_ha_class_sum, per_protec, per_class, per_total)
data_indigenous_territories_natural_classes_roads_per_total

data_indigenous_territories_natural_classes_roads_per_total %>%
  dplyr::arrange(-per_class)

data_indigenous_territories_natural_classes_roads_per_total %>%
  dplyr::arrange(-per_protec)

# bind
data_indigenous_territories_classes <- data_indigenous_territories_natural_classes_per_total %>%
  dplyr::bind_rows(data_indigenous_territories_natural_classes_roads_per_total)
data_indigenous_territories_classes

# export
readr::write_csv(data_indigenous_territories_classes, "03_tables/data_fig08_classes_it.csv")

# tables ------------------------------------------------------------------

## table s01 ----
readr::write_csv(area_limits, "03_tables/table_s01.csv")

## table s04 ----
data_fid_area_resume %>%
  dplyr::select(year, scenario, percentage, n_patches, total_area_ha,
                mean_area_ha, sd_area_ha, median_area_ha, max_area_ha) %>%
  readr::write_csv("03_tables/table_s04.csv")

## table s05 ----
data_temporal_wide %>%
  dplyr::filter(period %in% c("1986_2005", "2005_2020")) %>%
  dplyr::arrange(period, scenario) %>%
  dplyr::mutate(period = case_when(period == "1986_2005" ~ "1986-2005",
                                   period == "2005_2020" ~ "2005-2020"),
                scenario = case_when(scenario == "forest" ~ "FV not trimmed",
                                     scenario == "forest_roads_railways" ~ "FV trimmed",
                                     scenario == "natural" ~ "NV not trimmed",
                                     scenario == "natural_roads_railways" ~ "NV trimmed"),
                value_balance_area_total = round(value_balance_area_total, 0)) %>%
  dplyr::select(period, scenario, value_balance_area_total, percentage_balance_area_total,
                value_balance_patches_number, value_gain_patches_area_mn, value_loss_patches_area_mn) %>%
  readr::write_csv("03_tables/table_s05.csv")

## table s06 ----
data_limits_habitat_cover_area %>%
  dplyr::select(limit, scenario, p, n_patches, total_area_ha, mean_area_ha) %>%
  dplyr::slice(c(17:20, 1:4, 9:16, 5:8)) %>%
  dplyr::mutate(total_area_ha = round(total_area_ha, 0)) %>%
  readr::write_csv("03_tables/table_s06.csv")

# end ---------------------------------------------------------------------
