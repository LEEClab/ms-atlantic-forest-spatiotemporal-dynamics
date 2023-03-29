#' ----
#' title: graphics
#' author: mauricio vancine
#' date: 2022-12-09
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(paletteer)

# options
options(scipen = 1000)

# 1 habitat cover ---------------------------------------------------------

# classes
classes_habitat_cover <- tibble::tibble(
  values = c(0, 3, 4, 5, 11, 12, 13, 32, 49, 50),
  classes = c("MA", "FF", "SF", "MG", "WT", "GL", "OF",
              "ST", "WS", "HS"))
classes_habitat_cover

### values total ----

# years
years <- c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2021)
years

# list files
files_habitat_cover <- dir(path = "02_results", pattern = "01_mapbiomas", full.names = TRUE) %>%
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
  vegetation <- ifelse(length(names_habitat_cover) > 12,
                       paste0(names_habitat_cover[, 9], "_", names_habitat_cover[, 11], "_", names_habitat_cover[, 12]),
                       names_habitat_cover[, 9])


  data_habitat_cover_i <- readr::read_csv(i, col_names = c("values", "area", "n", "p"), col_types = cols()) %>%
    dplyr::mutate(year = year,
                  vegetation = vegetation,
                  .before = 1) %>%
    dplyr::mutate(area_ha = area/1e4, .after = area) %>%
    dplyr::select(-area, -n) %>%
    dplyr::left_join(classes_habitat_cover, by = "values") %>%
    dplyr::arrange(values) %>%
    dplyr::mutate(classes = forcats::fct_reorder(classes, values),
                  p = as.numeric(stringr::str_replace(p, "%", "")))

  data_habitat_cover <- dplyr::bind_rows(data_habitat_cover, data_habitat_cover_i)

}
data_habitat_cover

data_habitat_cover_resume <- data_habitat_cover %>%
  dplyr::filter(values != 0) %>%
  dplyr::group_by(year, vegetation) %>%
  dplyr::summarise(p = sum(p),
                   area_ha = sum(area_ha)) %>%
  # dplyr::bind_cols(values = rep(c(1, 10), times = 8),
  #                  classes = rep(c("Forest vegetation", "Natural vegetation"), times = 8))
  dplyr::bind_cols(values = rep(c(1, 1, 10, 10), times = 8),
                   classes = rep(c("FV", "FV", "NV", "NV"), times = 8))
data_habitat_cover_resume

readr::write_csv(data_habitat_cover_resume, "02_results/01_data_habitat_cover_resume.csv")

data_habitat_cover_resume_total <- dplyr::bind_rows(data_habitat_cover, data_habitat_cover_resume) %>%
  dplyr::arrange(year, vegetation)
data_habitat_cover_resume_total

data_habitat_cover_resume_total_area <- data_habitat_cover %>%
  dplyr::group_by(year, vegetation) %>%
  dplyr::summarise(area_ha = sum(area_ha))
data_habitat_cover_resume_total_area


### values br ----

# list files
files_habitat_cover_br <- dir(path = "02_results", pattern = "01_mapbiomas", full.names = TRUE) %>%
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
  vegetation_br <- ifelse(length(names_habitat_cover_br) > 13,
                          paste0(names_habitat_cover_br[, 9], "_", names_habitat_cover_br[, 11], "_", names_habitat_cover_br[, 12]),
                          names_habitat_cover_br[, 9])


  data_habitat_cover_i_br <- readr::read_csv(i, col_names = c("values", "area", "n", "p"), col_types = cols()) %>%
    dplyr::mutate(year = year_br,
                  vegetation = vegetation_br,
                  .before = 1) %>%
    dplyr::mutate(area_ha = area/1e4, .after = area) %>%
    dplyr::select(-area, -n) %>%
    dplyr::left_join(classes_habitat_cover, by = "values") %>%
    dplyr::arrange(values) %>%
    dplyr::mutate(classes = forcats::fct_reorder(classes, values),
                  p = as.numeric(stringr::str_replace(p, "%", "")))

  data_habitat_cover_br <- dplyr::bind_rows(data_habitat_cover_br, data_habitat_cover_i_br)

}
data_habitat_cover_br

data_habitat_cover_br_resume <- data_habitat_cover_br %>%
  dplyr::filter(values != 0) %>%
  dplyr::group_by(year, vegetation) %>%
  dplyr::summarise(p = sum(p),
                   area_ha = sum(area_ha)) %>%
  dplyr::bind_cols(values = rep(c(1, 1, 10, 10), times = 8),
                   classes = rep(c("FV", "FV", "NV", "NV"), times = 8))
data_habitat_cover_br_resume

readr::write_csv(data_habitat_cover_br_resume, "02_results/01_data_habitat_cover_br_resume.csv")

data_habitat_cover_br_resume_total <- dplyr::bind_rows(data_habitat_cover_br, data_habitat_cover_br_resume) %>%
  dplyr::arrange(values)
data_habitat_cover_br_resume_total

data_habitat_cover_br_resume_total_area <- data_habitat_cover_br %>%
  dplyr::group_by(year, vegetation) %>%
  dplyr::summarise(area_ha = sum(area_ha))
data_habitat_cover_br_resume_total_area


### values ar ----

# list files
files_habitat_cover_ar <- dir(path = "02_results", pattern = "01_mapbiomas", full.names = TRUE) %>%
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
  vegetation_ar <- ifelse(length(names_habitat_cover_ar) > 13,
                          paste0(names_habitat_cover_ar[, 9], "_", names_habitat_cover_ar[, 11], "_", names_habitat_cover_ar[, 12]),
                          names_habitat_cover_ar[, 9])


  data_habitat_cover_i_ar <- readr::read_csv(i, col_names = c("values", "area", "n", "p"), col_types = cols()) %>%
    dplyr::mutate(year = year_ar,
                  vegetation = vegetation_ar,
                  .before = 1) %>%
    dplyr::mutate(area_ha = area/1e4, .after = area) %>%
    dplyr::select(-area, -n) %>%
    dplyr::left_join(classes_habitat_cover, by = "values") %>%
    dplyr::arrange(values) %>%
    dplyr::mutate(classes = forcats::fct_reorder(classes, values),
                  p = as.numeric(stringr::str_replace(p, "%", "")))

  data_habitat_cover_ar <- dplyr::bind_rows(data_habitat_cover_ar, data_habitat_cover_i_ar)

}
data_habitat_cover_ar

data_habitat_cover_ar_resume <- data_habitat_cover_ar %>%
  dplyr::filter(values != 0) %>%
  dplyr::group_by(year, vegetation) %>%
  dplyr::summarise(p = sum(p),
                   area_ha = sum(area_ha)) %>%
  dplyr::bind_cols(values = rep(c(1, 1, 10, 10), times = 8),
                   classes = rep(c("FV", "FV", "NV", "NV"), times = 8))
data_habitat_cover_ar_resume

readr::write_csv(data_habitat_cover_ar_resume, "02_results/01_data_habitat_cover_ar_resume.csv")

data_habitat_cover_ar_resume_total <- dplyr::bind_rows(data_habitat_cover_ar, data_habitat_cover_ar_resume) %>%
  dplyr::arrange(values)
data_habitat_cover_ar_resume_total

data_habitat_cover_ar_resume_total_area <- data_habitat_cover_ar %>%
  dplyr::group_by(year, vegetation) %>%
  dplyr::summarise(area_ha = sum(area_ha))
data_habitat_cover_ar_resume_total_area


### values py ----

# list files
files_habitat_cover_py <- dir(path = "02_results", pattern = "01_mapbiomas", full.names = TRUE) %>%
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
  vegetation_py <- ifelse(length(names_habitat_cover_py) > 13,
                          paste0(names_habitat_cover_py[, 9], "_", names_habitat_cover_py[, 11], "_", names_habitat_cover_py[, 12]),
                          names_habitat_cover_py[, 9])


  data_habitat_cover_i_py <- readr::read_csv(i, col_names = c("values", "area", "n", "p"), col_types = cols()) %>%
    dplyr::mutate(year = year_py,
                  vegetation = vegetation_py,
                  .before = 1) %>%
    dplyr::mutate(area_ha = area/1e4, .after = area) %>%
    dplyr::select(-area, -n) %>%
    dplyr::left_join(classes_habitat_cover, by = "values") %>%
    dplyr::arrange(values) %>%
    dplyr::mutate(classes = forcats::fct_reorder(classes, values),
                  p = as.numeric(stringr::str_replace(p, "%", "")))

  data_habitat_cover_py <- dplyr::bind_rows(data_habitat_cover_py, data_habitat_cover_i_py)

}
data_habitat_cover_py

data_habitat_cover_py_resume <- data_habitat_cover_py %>%
  dplyr::filter(values != 0) %>%
  dplyr::group_by(year, vegetation) %>%
  dplyr::summarise(p = sum(p),
                   area_ha = sum(area_ha)) %>%
  dplyr::bind_cols(values = rep(c(1, 1, 10, 10), times = 8),
                   classes = rep(c("FV", "FV", "NV", "NV"), times = 8))
data_habitat_cover_py_resume

readr::write_csv(data_habitat_cover_py_resume, "02_results/01_data_habitat_cover_py_resume.csv")

data_habitat_cover_py_resume_total <- dplyr::bind_rows(data_habitat_cover_py, data_habitat_cover_py_resume) %>%
  dplyr::arrange(values)
data_habitat_cover_py_resume_total

data_habitat_cover_py_resume_total_area <- data_habitat_cover_py %>%
  dplyr::group_by(year, vegetation) %>%
  dplyr::summarise(area_ha = sum(area_ha))
data_habitat_cover_py_resume_total_area

### plot total ----
plot_habitat_cover_resume_total_forest <- data_habitat_cover_resume_total %>%
  dplyr::filter(values != 0,
                vegetation == "forest") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Forest vegetation", "Forest formation",
  #                                                         "Mangroove", "Wooded sandbank vegetation"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes, "FV", "FF", "MG", "WS"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .9, label = paste0(p, "%")), size = 5, color = "black") +
  scale_fill_manual(values = c("#129912", "#006400", "#687537", "#6b9932")) +
  # facet_grid(vars(forcats::fct_relevel(classes,
  #                                    "Forest vegetation", "Forest formation",
  #                                    "Mangroove", "Wooded sandbank vegetation")), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(classes, "FV", "FF", "MG", "WS")), scales = "free") +
  labs(title = "Forest vegetation (not trimmed by roads and rails)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(size = 10))
plot_habitat_cover_resume_total_forest
ggsave("03_figures/fig02a_habitat_cover_resume_total_forest.png",
       plot_habitat_cover_resume_total_forest, wi = 25, he = 20, un = "cm", dpi = 300)

plot_habitat_cover_resume_total_forest_roads_rails <- data_habitat_cover_resume_total %>%
  dplyr::filter(values != 0,
                vegetation == "forest_roads_rails") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Forest vegetation", "Forest formation",
  #                                                         "Mangroove", "Wooded sandbank vegetation"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes, "FV", "FF", "MG", "WS"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .9, label = paste0(p, "%")), size = 5, color = "black") +
  scale_fill_manual(values = c("#129912", "#006400", "#687537", "#6b9932")) +
  # facet_grid(vars(forcats::fct_relevel(classes,
  #                                    "Forest vegetation", "Forest formation",
  #                                    "Mangroove", "Wooded sandbank vegetation")), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(classes, "FV", "FF", "MG", "WS")), scales = "free") +
  labs(title = "Forest vegetation (trimmed by roads and rails)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(size = 10))
plot_habitat_cover_resume_total_forest_roads_rails
ggsave("03_figures/fig02b_habitat_cover_resume_total_forest_roads_rails.png",
       plot_habitat_cover_resume_total_forest_roads_rails, wi = 25, he = 20, un = "cm", dpi = 300)

plot_habitat_cover_resume_total_natural <- data_habitat_cover_resume_total %>%
  dplyr::filter(values != 0,
                vegetation == "natural") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Natural vegetation", "Forest formation",
  #                                                         "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                                                         "Wetland", "Grassland", "Salt flat",
  #                                                         "Herbaceous sandbank vegetation",
  #                                                         "Other non forest formations"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes, "NV", "FF",
                                                          "SF", "MG", "WS", "WT",
                                                          "GL", "ST", "HS", "OF"))) +

  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .7, label = paste0(p, "%")), size = 4, color = "black") +
  scale_fill_manual(values = c("#ff8800", "#006400", "#00ff00", "#687537",
                                        "#6b9932", "#45c2a5", "#b8af4f", "#968c46",
                                        "#66ffcc", "#bdb76b")) +
                                          # facet_grid(vars(
  #   forcats::fct_relabel(forcats::fct_relevel(classes,
  #                        "Natural vegetation", "Forest formation",
  #                        "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                        "Wetland", "Grassland", "Salt flat",
  #                        "Herbaceous sandbank vegetation",
  #                        "Other non forest formations"))), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(classes,
                                       "NV", "FF",
                                       "SF", "MG", "WS", "WT",
                                       "GL", "ST", "HS", "OF")),
             scales = "free") +
  labs(title = "Natural vegetation (not trimmed by roads and rails)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10))
plot_habitat_cover_resume_total_natural
ggsave("03_figures/fig02c_habitat_cover_resume_total_natural.png",
       plot_habitat_cover_resume_total_natural, wi = 25, he = 20, un = "cm", dpi = 300)

plot_habitat_cover_resume_total_natural_roads_rails <- data_habitat_cover_resume_total %>%
  dplyr::filter(values != 0,
                vegetation == "natural_roads_rails") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Natural vegetation", "Forest formation",
  #                                                         "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                                                         "Wetland", "Grassland", "Salt flat",
  #                                                         "Herbaceous sandbank vegetation",
  #                                                         "Other non forest formations"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes, "NV", "FF",
                                                          "SF", "MG", "WS", "WT",
                                                          "GL", "ST", "HS", "OF"))) +

  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .7, label = paste0(p, "%")), size = 4, color = "black") +
  scale_fill_manual(values = c("#ff8800", "#006400", "#00ff00", "#687537",
                                        "#6b9932", "#45c2a5", "#b8af4f", "#968c46",
                                        "#66ffcc", "#bdb76b")) +
                                          # facet_grid(vars(
  #   forcats::fct_relabel(forcats::fct_relevel(classes,
  #                        "Natural vegetation", "Forest formation",
  #                        "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                        "Wetland", "Grassland", "Salt flat",
  #                        "Herbaceous sandbank vegetation",
  #                        "Other non forest formations"))), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(classes,
                                       "NV", "FF",
                                       "SF", "MG", "WS", "WT",
                                       "GL", "ST", "HS", "OF")),
             scales = "free") +
  labs(title = "Natural vegetation (trimmed by roads and rails)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10))
plot_habitat_cover_resume_total_natural_roads_rails
ggsave("03_figures/fig02d_habitat_cover_resume_total_natural_roads_rails.png",
       plot_habitat_cover_resume_total_natural_roads_rails, wi = 25, he = 20, un = "cm", dpi = 300)


### plot br ----
plot_habitat_cover_br_resume_total_forest <- data_habitat_cover_br_resume_total %>%
  dplyr::filter(values != 0,
                vegetation == "forest") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Forest vegetation", "Forest formation",
  #                                                         "Mangroove", "Wooded sandbank vegetation"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes, "FV", "FF", "MG", "WS"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .9, label = paste0(p, "%")), size = 5, color = "black") +
  scale_fill_manual(values = c("#129912", "#006400", "#687537", "#6b9932")) +
  # facet_grid(vars(forcats::fct_relevel(classes,
  #                                    "Forest vegetation", "Forest formation",
  #                                    "Mangroove", "Wooded sandbank vegetation")), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(classes, "FV", "FF", "MG", "WS")), scales = "free") +
  labs(title = "Forest vegetation (not trimmed by roads and rails) (Brazil)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(size = 10))
plot_habitat_cover_br_resume_total_forest
ggsave("03_figures/fig02e_habitat_cover_br_resume_total_forest.png",
       plot_habitat_cover_br_resume_total_forest, wi = 25, he = 20, un = "cm", dpi = 300)

plot_habitat_cover_br_resume_total_forest_roads_rails <- data_habitat_cover_br_resume_total %>%
  dplyr::filter(values != 0,
                vegetation == "forest_roads_rails") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Forest vegetation", "Forest formation",
  #                                                         "Mangroove", "Wooded sandbank vegetation"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes, "FV", "FF", "MG", "WS"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .9, label = paste0(p, "%")), size = 5, color = "black") +
  scale_fill_manual(values = c("#129912", "#006400", "#687537", "#6b9932")) +
  # facet_grid(vars(forcats::fct_relevel(classes,
  #                                    "Forest vegetation", "Forest formation",
  #                                    "Mangroove", "Wooded sandbank vegetation")), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(classes, "FV", "FF", "MG", "WS")), scales = "free") +
  labs(title = "Forest vegetation (trimmed by roads and rails) (Brazil)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(size = 10))
plot_habitat_cover_br_resume_total_forest_roads_rails
ggsave("03_figures/fig02f_habitat_cover_br_resume_total_forest_roads_rails.png",
       plot_habitat_cover_br_resume_total_forest_roads_rails, wi = 25, he = 20, un = "cm", dpi = 300)


plot_habitat_cover_br_resume_total_natural <- data_habitat_cover_br_resume_total %>%
  dplyr::filter(values != 0,
                vegetation == "natural") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Natural vegetation", "Forest formation",
  #                                                         "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                                                         "Wetland", "Grassland", "Salt flat",
  #                                                         "Herbaceous sandbank vegetation",
  #                                                         "Other non forest formations"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes, "NV", "FF",
                                                          "SF", "MG", "WS", "WT",
                                                          "GL", "ST", "HS", "OF"))) +

  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .7, label = paste0(p, "%")), size = 4, color = "black") +
  scale_fill_manual(values = c("#ff8800", "#006400", "#00ff00", "#687537",
                                        "#6b9932", "#45c2a5", "#b8af4f", "#968c46",
                                        "#66ffcc", "#bdb76b")) +
                                          # facet_grid(vars(
  #   forcats::fct_relabel(forcats::fct_relevel(classes,
  #                        "Natural vegetation", "Forest formation",
  #                        "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                        "Wetland", "Grassland", "Salt flat",
  #                        "Herbaceous sandbank vegetation",
  #                        "Other non forest formations"))), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(classes,
                                       "NV", "FF",
                                       "SF", "MG", "WS", "WT",
                                       "GL", "ST", "HS", "OF")),
             scales = "free") +
  labs(title = "Natural vegetation (not trimmed by roads and rails) (Brazil)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10))
plot_habitat_cover_br_resume_total_natural
ggsave("03_figures/fig02g_habitat_cover_br_resume_total_natural.png",
       plot_habitat_cover_br_resume_total_natural, wi = 25, he = 20, un = "cm", dpi = 300)

plot_habitat_cover_br_resume_total_natural_roads_rails <- data_habitat_cover_br_resume_total %>%
  dplyr::filter(values != 0,
                vegetation == "natural_roads_rails") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Natural vegetation", "Forest formation",
  #                                                         "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                                                         "Wetland", "Grassland", "Salt flat",
  #                                                         "Herbaceous sandbank vegetation",
  #                                                         "Other non forest formations"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes, "NV", "FF",
                                                          "SF", "MG", "WS", "WT",
                                                          "GL", "ST", "HS", "OF"))) +

  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .7, label = paste0(p, "%")), size = 4, color = "black") +
  scale_fill_manual(values = c("#ff8800", "#006400", "#00ff00", "#687537",
                                        "#6b9932", "#45c2a5", "#b8af4f", "#968c46",
                                        "#66ffcc", "#bdb76b")) +
                                          # facet_grid(vars(
  #   forcats::fct_relabel(forcats::fct_relevel(classes,
  #                        "Natural vegetation", "Forest formation",
  #                        "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                        "Wetland", "Grassland", "Salt flat",
  #                        "Herbaceous sandbank vegetation",
  #                        "Other non forest formations"))), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(classes,
                                       "NV", "FF",
                                       "SF", "MG", "WS", "WT",
                                       "GL", "ST", "HS", "OF")),
             scales = "free") +
  labs(title = "Natural vegetation (trimmed by roads and rails) (Brazil)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10))
plot_habitat_cover_br_resume_total_natural_roads_rails
ggsave("03_figures/fig02h_habitat_cover_br_resume_total_natural_roads_rails.png",
       plot_habitat_cover_br_resume_total_natural_roads_rails, wi = 25, he = 20, un = "cm", dpi = 300)



### plot ar ----
plot_habitat_cover_ar_resume_total_forest <- data_habitat_cover_ar_resume_total %>%
  dplyr::filter(values != 0,
                vegetation == "forest") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Forest vegetation", "Forest formation",
  #                                                         "Mangroove", "Wooded sandbank vegetation"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes, "FV", "FF", "MG", "WS"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .9, label = paste0(p, "%")), size = 5, color = "black") +
  scale_fill_manual(values = c("#129912", "#006400", "#687537", "#6b9932")) +
  # facet_grid(vars(forcats::fct_relevel(classes,
  #                                    "Forest vegetation", "Forest formation",
  #                                    "Mangroove", "Wooded sandbank vegetation")), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(classes, "FV", "FF", "MG", "WS")), scales = "free") +
  labs(title = "Forest vegetation (not trimmed by roads and rails) (Argentina)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(size = 10))
plot_habitat_cover_ar_resume_total_forest
ggsave("03_figures/fig02i_habitat_cover_ar_resume_total_forest.png",
       plot_habitat_cover_ar_resume_total_forest, wi = 25, he = 20, un = "cm", dpi = 300)


plot_habitat_cover_ar_resume_total_forest_roads_rails <- data_habitat_cover_ar_resume_total %>%
  dplyr::filter(values != 0,
                vegetation == "forest_roads_rails") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Forest vegetation", "Forest formation",
  #                                                         "Mangroove", "Wooded sandbank vegetation"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes, "FV", "FF", "MG", "WS"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .9, label = paste0(p, "%")), size = 5, color = "black") +
  scale_fill_manual(values = c("#129912", "#006400", "#687537", "#6b9932")) +
  # facet_grid(vars(forcats::fct_relevel(classes,
  #                                    "Forest vegetation", "Forest formation",
  #                                    "Mangroove", "Wooded sandbank vegetation")), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(classes, "FV", "FF", "MG", "WS")), scales = "free") +
  labs(title = "Forest vegetation (trimmed by roads and rails) (Argentina)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(size = 10))
plot_habitat_cover_ar_resume_total_forest_roads_rails
ggsave("03_figures/fig02i_habitat_cover_ar_resume_total_forest_roads_rails.png",
       plot_habitat_cover_ar_resume_total_forest_roads_rails, wi = 25, he = 20, un = "cm", dpi = 300)

plot_habitat_cover_ar_resume_total_natural <- data_habitat_cover_ar_resume_total %>%
  dplyr::filter(values != 0,
                vegetation == "natural") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Natural vegetation", "Forest formation",
  #                                                         "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                                                         "Wetland", "Grassland", "Salt flat",
  #                                                         "Herbaceous sandbank vegetation",
  #                                                         "Other non forest formations"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes, "NV", "FF",
                                                          "SF", "MG", "WS", "WT",
                                                          "GL", "ST", "HS", "OF"))) +

  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .7, label = paste0(p, "%")), size = 4, color = "black") +
  scale_fill_manual(values = c("#ff8800",
                               "#006400",
                               # "#00ff00",
                               # "#687537",
                               # "#6b9932",
                               "#45c2a5",
                               "#b8af4f"
                               # "#968c46",
                               # "#66ffcc",
                                #"#bdb76b"
                                )) +
                                          # facet_grid(vars(
  #   forcats::fct_relabel(forcats::fct_relevel(classes,
  #                        "Natural vegetation", "Forest formation",
  #                        "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                        "Wetland", "Grassland", "Salt flat",
  #                        "Herbaceous sandbank vegetation",
  #                        "Other non forest formations"))), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(classes,
                                       "NV", "FF",
                                       "SF", "MG", "WS", "WT",
                                       "GL", "ST", "HS", "OF")),
             scales = "free") +
  labs(title = "Natural vegetation (not trimmed by roads and rails) (Argentina)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10))
plot_habitat_cover_ar_resume_total_natural
ggsave("03_figures/fig02j_habitat_cover_ar_resume_total_natural.png",
       plot_habitat_cover_ar_resume_total_natural, wi = 25, he = 20, un = "cm", dpi = 300)


plot_habitat_cover_ar_resume_total_natural_roads_rails <- data_habitat_cover_ar_resume_total %>%
  dplyr::filter(values != 0,
                vegetation == "natural_roads_rails") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Natural vegetation", "Forest formation",
  #                                                         "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                                                         "Wetland", "Grassland", "Salt flat",
  #                                                         "Herbaceous sandbank vegetation",
  #                                                         "Other non forest formations"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes, "NV", "FF",
                                                          "SF", "MG", "WS", "WT",
                                                          "GL", "ST", "HS", "OF"))) +

  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .7, label = paste0(p, "%")), size = 4, color = "black") +
  scale_fill_manual(values = c("#ff8800",
                                        "#006400",
                                        # "#00ff00",
                                        # "#687537",
                                        # "#6b9932",
                                        "#45c2a5",
                                        "#b8af4f"
                                        # "#968c46",
                                        # "#66ffcc",
                                        #"#bdb76b"
  )) +
  # facet_grid(vars(
  #   forcats::fct_relabel(forcats::fct_relevel(classes,
  #                        "Natural vegetation", "Forest formation",
  #                        "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                        "Wetland", "Grassland", "Salt flat",
  #                        "Herbaceous sandbank vegetation",
  #                        "Other non forest formations"))), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(classes,
                                       "NV", "FF",
                                       "SF", "MG", "WS", "WT",
                                       "GL", "ST", "HS", "OF")),
             scales = "free") +
  labs(title = "Natural vegetation (trimmed by roads and rails) (Argentina)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10))
plot_habitat_cover_ar_resume_total_natural_roads_rails
ggsave("03_figures/fig02k_habitat_cover_ar_resume_total_natural_roads_rails.png",
       plot_habitat_cover_ar_resume_total_natural_roads_rails, wi = 25, he = 20, un = "cm", dpi = 300)

### plot py ----
plot_habitat_cover_py_resume_total_forest <- data_habitat_cover_py_resume_total %>%
  dplyr::filter(values != 0,
                vegetation == "forest") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Forest vegetation", "Forest formation",
  #                                                         "Mangroove", "Wooded sandbank vegetation"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes, "FV", "FF", "MG", "WS"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .9, label = paste0(p, "%")), size = 5, color = "black") +
  scale_fill_manual(values = c("#129912", "#006400", "#687537", "#6b9932")) +
  # facet_grid(vars(forcats::fct_relevel(classes,
  #                                    "Forest vegetation", "Forest formation",
  #                                    "Mangroove", "Wooded sandbank vegetation")), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(classes, "FV", "FF", "MG", "WS")), scales = "free") +
  labs(title = "Forest vegetation (not trimmed by roads and rails) (Paraguay)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(size = 10))
plot_habitat_cover_py_resume_total_forest
ggsave("03_figures/fig02l_habitat_cover_py_resume_total_forest.png",
       plot_habitat_cover_py_resume_total_forest, wi = 25, he = 20, un = "cm", dpi = 300)


plot_habitat_cover_py_resume_total_forest_roads_rails <- data_habitat_cover_py_resume_total %>%
  dplyr::filter(values != 0,
                vegetation == "forest_roads_rails") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Forest vegetation", "Forest formation",
  #                                                         "Mangroove", "Wooded sandbank vegetation"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes, "FV", "FF", "MG", "WS"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .9, label = paste0(p, "%")), size = 5, color = "black") +
  scale_fill_manual(values = c("#129912", "#006400", "#687537", "#6b9932")) +
  # facet_grid(vars(forcats::fct_relevel(classes,
  #                                    "Forest vegetation", "Forest formation",
  #                                    "Mangroove", "Wooded sandbank vegetation")), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(classes, "FV", "FF", "MG", "WS")), scales = "free") +
  labs(title = "Forest vegetation (trimmed by roads and rails) (Paraguay)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(size = 10))
plot_habitat_cover_py_resume_total_forest_roads_rails
ggsave("03_figures/fig02m_habitat_cover_py_resume_total_forest_roads_rails.png",
       plot_habitat_cover_py_resume_total_forest_roads_rails, wi = 25, he = 20, un = "cm", dpi = 300)


plot_habitat_cover_py_resume_total_natural <- data_habitat_cover_py_resume_total %>%
  dplyr::filter(values != 0,
                vegetation == "natural") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Natural vegetation", "Forest formation",
  #                                                         "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                                                         "Wetland", "Grassland", "Salt flat",
  #                                                         "Herbaceous sandbank vegetation",
  #                                                         "Other non forest formations"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes, "NV", "FF",
                                                          "SF", "MG", "WS", "WT",
                                                          "GL", "ST", "HS", "OF"))) +

  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .7, label = paste0(p, "%")), size = 4, color = "black") +
  scale_fill_manual(values = c("#ff8800",
                               "#006400",
                               "#00ff00",
                               # "#687537",
                               # "#6b9932",
                               "#45c2a5",
                               "#b8af4f"
                               # "#968c46",
                                #"#66ffcc",
                               # "#bdb76b"
                               )) +
                                          # facet_grid(vars(
  #   forcats::fct_relabel(forcats::fct_relevel(classes,
  #                        "Natural vegetation", "Forest formation",
  #                        "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                        "Wetland", "Grassland", "Salt flat",
  #                        "Herbaceous sandbank vegetation",
  #                        "Other non forest formations"))), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(classes,
                                       "NV", "FF",
                                       "SF", "MG", "WS", "WT",
                                       "GL", "ST", "HS", "OF")),
             scales = "free") +
  labs(title = "Natural vegetation (not trimmed by roads and rails) (Paraguay)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10))
plot_habitat_cover_py_resume_total_natural
ggsave("03_figures/fig02n_habitat_cover_py_resume_total_natural.png",
       plot_habitat_cover_py_resume_total_natural, wi = 25, he = 20, un = "cm", dpi = 300)

plot_habitat_cover_py_resume_total_natural_roads_rails <- data_habitat_cover_py_resume_total %>%
  dplyr::filter(values != 0,
                vegetation == "natural_roads_rails") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Natural vegetation", "Forest formation",
  #                                                         "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                                                         "Wetland", "Grassland", "Salt flat",
  #                                                         "Herbaceous sandbank vegetation",
  #                                                         "Other non forest formations"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes, "NV", "FF",
                                                          "SF", "MG", "WS", "WT",
                                                          "GL", "ST", "HS", "OF"))) +

  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .7, label = paste0(p, "%")), size = 4, color = "black") +
  scale_fill_manual(values = c("#ff8800",
                                        "#006400",
                                        "#00ff00",
                                        # "#687537",
                                        # "#6b9932",
                                        "#45c2a5",
                                        "#b8af4f"
                                        # "#968c46",
                                        #"#66ffcc",
                                        # "#bdb76b"
  )) +
                                          # facet_grid(vars(
  #   forcats::fct_relabel(forcats::fct_relevel(classes,
  #                        "Natural vegetation", "Forest formation",
  #                        "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                        "Wetland", "Grassland", "Salt flat",
  #                        "Herbaceous sandbank vegetation",
  #                        "Other non forest formations"))), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(classes,
                                       "NV", "FF",
                                       "SF", "MG", "WS", "WT",
                                       "GL", "ST", "HS", "OF")),
             scales = "free") +
  labs(title = "Natural vegetation (trimmed by roads and rails) (Paraguay)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10))
plot_habitat_cover_py_resume_total_natural_roads_rails
ggsave("03_figures/fig02o_habitat_cover_py_resume_total_natural_roads_rails.png",
       plot_habitat_cover_py_resume_total_natural_roads_rails, wi = 25, he = 20, un = "cm", dpi = 300)


# 2 number of patch and size distribution ---------------------------------

### data ----
# list files
files_area <- dir(path = "02_results", pattern = "02_", full.names = TRUE) %>%
  stringr::str_subset("resume", negate = TRUE) %>%
  stringr::str_subset("af_lim_forest|af_lim_natural") %>%
  stringr::str_subset("1985|2021", negate = TRUE)
files_area

# import data
data_area_resume <- NULL
for(i in files_area){

  print(i)

  name_area <- basename(i) %>%
    stringr::str_replace("02_mapbiomas_brazil_af_trinacional_", "") %>%
    stringr::str_replace(".csv", "") %>%
    stringr::str_replace("_af_lim", "") %>%
    stringr::str_replace("_pid", "") %>%
    stringr::str_replace("_area", "")

  year <- gsub("[^0-9]", " ", name_area) %>%
    stringr::str_trim()

  vegetation <- gsub("[^a-zA-Z]", " ", name_area) %>%
    stringr::str_trim()

  data_area <- readr::read_csv(i, col_names = c("values", "area", "n"), col_types = cols()) %>%
    dplyr::mutate(area_ha = area/1e4)

  data_area_resume_i <- tibble::tibble(year = year,
                                       vegetation = vegetation,
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

  data_area_resume <- dplyr::bind_rows(data_area_resume, data_area_resume_i)

  data_area_total_i <- tibble::tibble(year = year,
                                      vegetation = vegetation,
                                      area_ha = data_area$area_ha)


}

# resume
data_area_resume <- data_area_resume %>%
  dplyr::mutate(percentage = data_habitat_cover_resume$p, .after = 2)
data_area_resume

readr::write_csv(data_area_resume, "02_results/02_data_area_resume.csv")

# percentage
((data_area_resume[29, ]$n_patches - data_area_resume[1, ]$n_patches)/data_area_resume[1, ]$n_patches) * 100
((data_area_resume[30, ]$n_patches - data_area_resume[2, ]$n_patches)/data_area_resume[2, ]$n_patches) * 100
((data_area_resume[31, ]$n_patches - data_area_resume[3, ]$n_patches)/data_area_resume[3, ]$n_patches) * 100
((data_area_resume[32, ]$n_patches - data_area_resume[4, ]$n_patches)/data_area_resume[4, ]$n_patches) * 100

((data_area_resume[29, ]$mean_area_ha - data_area_resume[1, ]$mean_area_ha)/data_area_resume[1, ]$mean_area_ha) * 100
((data_area_resume[30, ]$mean_area_ha - data_area_resume[2, ]$mean_area_ha)/data_area_resume[2, ]$mean_area_ha) * 100
((data_area_resume[31, ]$mean_area_ha - data_area_resume[3, ]$mean_area_ha)/data_area_resume[3, ]$mean_area_ha) * 100
((data_area_resume[32, ]$mean_area_ha - data_area_resume[4, ]$mean_area_ha)/data_area_resume[4, ]$mean_area_ha) * 100

((data_area_resume[2, ]$max_area_ha - data_area_resume[1, ]$max_area_ha)/data_area_resume[1, ]$max_area_ha) * 100
((data_area_resume[4, ]$max_area_ha - data_area_resume[3, ]$max_area_ha)/data_area_resume[3, ]$max_area_ha) * 100
((data_area_resume[30, ]$max_area_ha - data_area_resume[29, ]$max_area_ha)/data_area_resume[29, ]$max_area_ha) * 100
((data_area_resume[32, ]$max_area_ha - data_area_resume[31, ]$max_area_ha)/data_area_resume[31, ]$max_area_ha) * 100

((data_area_resume[29, ]$max_area_ha - data_area_resume[1, ]$max_area_ha)/data_area_resume[1, ]$max_area_ha) * 100
((data_area_resume[30, ]$max_area_ha - data_area_resume[2, ]$max_area_ha)/data_area_resume[2, ]$max_area_ha) * 100
((data_area_resume[31, ]$max_area_ha - data_area_resume[3, ]$max_area_ha)/data_area_resume[3, ]$max_area_ha) * 100
((data_area_resume[32, ]$max_area_ha - data_area_resume[4, ]$max_area_ha)/data_area_resume[4, ]$max_area_ha) * 100

### limits ----
# list files
files_limits_habitat_cover <- dir(path = "02_results", pattern = "01_", full.names = TRUE) %>%
  stringr::str_subset("_2020_") %>%
  stringr::str_subset("af_lim_forest|af_lim_natural", negate = TRUE)
files_limits_habitat_cover

files_limits_area <- dir(path = "02_results", pattern = "02_", full.names = TRUE) %>%
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

  vegetation <- ifelse(length(names_habitat_cover) > 12,
                       paste0(names_habitat_cover[, 10], "_", names_habitat_cover[, 11], "_", names_habitat_cover[, 12]),
                       names_habitat_cover[, 10])


  data_limits_habitat_cover_i <- readr::read_csv(i, col_names = c("values", "area", "n", "p"), col_types = cols()) %>%
    dplyr::mutate(limit = limit,
                  vegetation = vegetation,
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
  dplyr::select(-values)
data_limits_habitat_cover_filter

data_limits_area <- NULL
for(i in files_limits_area){

  name_area <- basename(i) %>%
    stringr::str_split("_", simplify = TRUE)

  limit <- ifelse(length(name_area) > 12,
                  paste0(name_area[, 9]),
                  name_area[, 9])

  vegetation <- ifelse(length(name_area) > 12,
                       paste0(name_area[, 10], "_", name_area[, 11], "_", name_area[, 12]),
                       name_area[, 10])

  data_area <- readr::read_csv(i, col_names = c("values", "area", "n"), col_types = cols()) %>%
    dplyr::mutate(area_ha = area/1e4)

  data_limits_area_i <- tibble::tibble(limit = limit,
                                       vegetation = vegetation,
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
  dplyr::left_join(data_limits_habitat_cover_filter)
data_limits_habitat_cover_area


### plot ----
plot_data_area_resume_n_patches <- data_area_resume %>%
  dplyr::mutate(vegetation = dplyr::case_when(vegetation == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
                                              vegetation == "forest roads rails" ~ "Forest vegetation (trimmed by roads and rails)",
                                              vegetation == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
                                              vegetation == "natural roads rails" ~ "Natural vegetation (trimmed by roads and rails)")) %>%
  ggplot(aes(x = year, y = n_patches, color = vegetation, fill = vegetation, group = vegetation)) +
  geom_line(aes(linetype = vegetation), linewidth = 2) +
  geom_point(size = 4, shape = 21, stroke = 1.5) +
  scale_color_manual(values = c("#129912", "#129912", "#ff8800", "#ff8800")) +
  scale_fill_manual(values = c("#129912", "gray30", "#ff8800", "gray30")) +
  scale_linetype_manual(values = c("solid", "dotted", "solid", "dotted")) +
  annotate(geom = "text", x = 1, y = 2350000, label = "a", size = 10, fontface = "bold") +
  labs(x = "Year", y = "Number of Patches", color = "Scenarios", fill = "Scenarios", linetype = "Scenarios") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.47, .84),
        legend.title = element_blank(),
        legend.text = element_text(size = 15))
plot_data_area_resume_n_patches
ggsave("03_figures/fig03a_n_patches_1986_2020.png",
       plot_data_area_resume_n_patches, wi = 25, he = 20, un = "cm", dpi = 300)

plot_data_area_resume_avg_size <- data_area_resume %>%
  dplyr::mutate(vegetation = dplyr::case_when(vegetation == "forest" ~ "Forest vegetation (not trimmed by roads and rails)",
                                              vegetation == "forest roads rails" ~ "Forest vegetation (trimmed by roads and rails)",
                                              vegetation == "natural" ~ "Natural vegetation (not trimmed by roads and rails)",
                                              vegetation == "natural roads rails" ~ "Natural vegetation (trimmed by roads and rails)")) %>%
  ggplot(aes(x = year, y = mean_area_ha, color = vegetation, fill = vegetation, group = vegetation)) +
  geom_line(aes(linetype = vegetation), size = 2) +
  geom_point(size = 4, shape = 21, stroke = 1.5) +
  scale_color_manual(values = c("#129912", "#129912", "#ff8800", "#ff8800")) +
  scale_fill_manual(values = c("#129912", "gray30", "#ff8800", "gray30")) +
  scale_linetype_manual(values = c("solid", "dotted", "solid", "dotted")) +
  annotate(geom = "text", x = 1, y = 35, label = "b", size = 10, fontface = "bold") +
  labs(x = "Year", y = "Average size of patches (ha)",
       color = "Scenarios", fill = "Scenarios", linetype = "Scenarios") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.55, .84),
        legend.title = element_blank(),
        legend.text = element_text(size = 15))
plot_data_area_resume_avg_size
ggsave("03_figures/fig03b_avg_size_patches_1986_2020.png",
       plot_data_area_resume_avg_size, wi = 25, he = 20, un = "cm", dpi = 300)


plot_data_area_resume_n_patches_avg_size <- data_area_resume %>%
  dplyr::mutate(vegetation = stringr::str_to_title(vegetation)) %>%
  ggplot(aes(x = n_patches, y = mean_area_ha, color = vegetation,
             fill = vegetation, group = vegetation)) +
  geom_line(aes(linetype = vegetation), size = 2) +
  geom_point(size = 4, shape = 21, stroke = 1.5) +
  geom_text(mapping = aes(y = mean_area_ha + .5, label = year), color = "black") +
  scale_color_manual(values = c("#129912", "#129912", "#ff8800", "#ff8800")) +
  scale_fill_manual(values = c("#129912", "gray30", "#ff8800", "gray30")) +
  scale_linetype_manual(values = c("solid", "dotted", "solid", "dotted")) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.83, .87))
plot_data_area_resume_n_patches_avg_size


data_area_resume_plot <- NULL
for(i in files_area){

  print(i)

  name_area <- basename(i) %>%
    stringr::str_replace("02_mapbiomas_brazil_af_trinacional_", "") %>%
    stringr::str_replace(".csv", "") %>%
    stringr::str_replace("_af_lim", "") %>%
    stringr::str_replace("_pid", "")

  title_area <- stringr::str_replace_all(name_area, "_", " ") %>%
    stringr::str_replace("area", "") %>%
    stringr::str_trim() %>%
    stringr::str_to_title()

  title_area_letters <- gsub("[^a-zA-Z]", " ", title_area) %>%
    stringr::str_trim()

  title_area_letters_adj <- ifelse(title_area_letters == "Forest", "Forest vegetation (not trimmed by roads and rails)",
                            ifelse(title_area_letters == "Forest Roads Rails", "Forest vegetation (trimmed by roads and rails)",
                            ifelse(title_area_letters == "Natural", "Natural vegetation (not trimmed by roads and rails)",
                            ifelse(title_area_letters == "Natural Roads Rails", "Natural vegetation (trimmed by roads and rails)", NA))))


  title_area_numbers <- gsub("[^0-9]", " ", title_area) %>%
    stringr::str_trim()

  title_area <- paste0(title_area_letters_adj, " (", title_area_numbers, ")")

  data_area <- readr::read_csv(i, col_names = c("values", "area", "n"), col_types = cols()) %>%
    dplyr::mutate(area_ha = area/1e4) %>%
    dplyr::mutate(class = case_when(area_ha < 1 ~ "<1 ha",
                                    area_ha >= 1 & area_ha < 5 ~ "1-5 ha",
                                    area_ha >= 5 & area_ha < 10 ~ "5-10 ha",
                                    area_ha >= 10 & area_ha < 50 ~ "10-50 ha",
                                    area_ha >= 50 & area_ha < 100 ~ "50-100 ha",
                                    area_ha >= 100 & area_ha < 250 ~ "100-250 ha",
                                    area_ha >= 250 & area_ha < 500 ~ "250-500 ha",
                                    area_ha >= 500 & area_ha < 1000 ~ "500-1000 ha",
                                    area_ha >= 1000 & area_ha < 2500 ~ "1000-2500 ha",
                                    area_ha >= 2500 & area_ha < 5000 ~ "2500-5000 ha",
                                    area_ha >= 5000 & area_ha < 10000 ~ "5000-10000 ha",
                                    area_ha >= 10000 & area_ha < 25000 ~ "10000-25000 ha",
                                    area_ha >= 25000 & area_ha < 50000 ~ "25000-50000 ha",
                                    area_ha >= 50000 & area_ha < 100000 ~ "50000-100000 ha",
                                    area_ha >= 100000 & area_ha < 250000 ~ "100000-250000 ha",
                                    area_ha >= 250000 & area_ha < 500000 ~ "250000-500000 ha",
                                    area_ha >= 500000 & area_ha < 1000000 ~ "500000-1000000 ha",
                                    area_ha >= 1000000 ~ ">1000000 ha")) %>%
    dplyr::mutate(class = forcats::as_factor(class)) %>%
    dplyr::mutate(class = forcats::fct_relevel(class,
                                               c("<1 ha", "1-5 ha", "5-10 ha", "10-50 ha",
                                                 "50-100 ha", "100-250 ha",
                                                 "250-500 ha", "500-1000 ha",
                                                 "1000-2500 ha", "2500-5000 ha",
                                                 "5000-10000 ha", "10000-25000 ha",
                                                 "25000-50000 ha", "50000-100000 ha",
                                                 "100000-250000 ha", "250000-500000 ha",
                                                 "500000-1000000 ha", ">1000000 ha")))
  data_area

  # resume data
  data_area_resume_plot_i <- data_area %>%
    dplyr::group_by(class) %>%
    dplyr::summarise(class_area = sum(area_ha),
                     class_n = n()) %>%
    dplyr::mutate(class_area_per = round(class_area/sum(class_area)*100, 2),
                  class_n_per = round(class_n/sum(class_n)*100, 4)) %>%
    dplyr::mutate(year = title_area_numbers,
                  vegetation = title_area_letters)
  data_area_resume_plot_i

  data_area_resume_plot <- dplyr::bind_rows(data_area_resume_plot, data_area_resume_plot_i)

  # plot
  plot_number_patch_size_dist <- ggplot(data = data_area_resume_plot_i,
                                        aes(x = class, y = class_area)) +
    geom_bar(stat = "identity", fill = ifelse(!is.na(str_extract(i, "forest")), "#129912", "#ff8800")) +
    geom_text(aes(label = paste0(class_area_per, "%A / ", class_n_per, "%NP")),
              nudge_y = max(data_area_resume_plot_i$class_area) * .3, size = 5) +
    # scale_fill_manual(values = c("#d7191c", "#e24430", "#ed6e43", "#f89957", "#fdba6f",
    #                              "#fed18a", "#fee8a5", "#ffffc0", "#e6f4a7", "#cce98f",
    #                              "#b3de76", "#92cf64", "#6abc58", "#42a94d", "#1a9641")) +
    coord_flip() +
    labs(title = title_area,
         x = "Class of patch size (ha)", y = "Area (ha)") +
    ylim(c(0, max(data_area_resume_plot_i$class_area) * 1.7)) +
    theme_bw(base_size = 20) +
    theme(title = element_text(size = 15),
          legend.position = "none",
          axis.text = element_text(size = 12))
  plot_number_patch_size_dist

  ggsave(filename = paste0("03_figures/fig03_", name_area, ".png"),
         plot = plot_number_patch_size_dist,
         wi = 25, he = 20, un = "cm", dpi = 300)
}

# data
data_area_resume_plot_50ha <- data_area_resume_plot %>%
  dplyr::filter(class %in% c("<1 ha", "1-5 ha", "5-10 ha", "10-50 ha")) %>%
  group_by(year, vegetation) %>%
  summarise(class_area_per_sum = sum(class_area_per),
            class_n_per_sum = sum(class_n_per))
data_area_resume_plot_50ha

data_area_resume_plot_10ha <- data_area_resume_plot %>%
  dplyr::filter(class %in% c("<1 ha", "1-5 ha", "5-10 ha")) %>%
  group_by(year, vegetation) %>%
  summarise(class_area_per_sum = sum(class_area_per),
            class_n_per_sum = sum(class_n_per))
data_area_resume_plot_10ha

data_area_resume_plot_5ha <- data_area_resume_plot %>%
  dplyr::filter(class %in% c("<1 ha", "1-5 ha")) %>%
  group_by(year, vegetation) %>%
  summarise(class_area_per_sum = sum(class_area_per),
            class_n_per_sum = sum(class_n_per))
data_area_resume_plot_5ha

data_area_resume_plot_1ha <- data_area_resume_plot %>%
  dplyr::filter(class %in% c("<1 ha")) %>%
  group_by(year, vegetation) %>%
  summarise(class_area_per_sum = sum(class_area_per),
            class_n_per_sum = sum(class_n_per))
data_area_resume_plot_1ha

# export
readr::write_csv(data_area_resume_plot, "02_data_area_resume_plot.csv")

# 3  core and edge area ---------------------------------------------------

### data ----
# list files
files_core_edge_area <- dir(path = "02_results", pattern = "_edge_area.csv", full.names = TRUE) %>%
  stringr::str_subset("data_core", negate = TRUE)
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

  name_core_edge_area_vegetation <- gsub("[^a-zA-Z]", " ", name_core_edge_area) %>%
    stringr::str_trim()

  name_core_edge_area_year <- gsub("[^0-9]", " ", name_core_edge_area) %>%
    stringr::str_trim()

  # data
  data_core_edge_area_i <- readr::read_csv(i, col_names = c("forest", "edge_dist", "area", "n"),
                                           col_types = readr::cols()) %>%
    dplyr::slice(-1) %>%
    dplyr::mutate(area_ha = area/1e4, .after = area) %>%
    dplyr::mutate(per = n/sum(n)*100) %>%
    dplyr::mutate(per_cumsum = cumsum(per)) %>%
    dplyr::mutate(year = name_core_edge_area_year,
                  vegetation = name_core_edge_area_vegetation,
                  .before = 1)

  # combine
  data_core_edge_area <- dplyr::bind_rows(data_core_edge_area, data_core_edge_area_i)

}

data_core_edge_area <- data_core_edge_area %>%
  dplyr::select(year, vegetation, edge_dist, area_ha, per, per_cumsum)
data_core_edge_area

# resume
data_core_edge_area_30m_forest <- data_core_edge_area %>%
  dplyr::filter(vegetation == "forest roads rails", edge_dist == 30) %>%
  dplyr::group_by(year, vegetation)
data_core_edge_area_30m_forest

data_core_edge_area_30m_natural <- data_core_edge_area %>%
  dplyr::filter(vegetation == "natural roads rails", edge_dist == 30)
data_core_edge_area_30m_natural

data_core_edge_area_90m_forest <- data_core_edge_area %>%
  dplyr::filter(vegetation == "forest roads rails", edge_dist == 90)
data_core_edge_area_90m_forest[1, ]$per_cumsum
data_core_edge_area_90m_forest[8, ]$per_cumsum
data_core_edge_area_90m_forest[8, ]$per_cumsum - data_core_edge_area_90m_forest[1, ]$per_cumsum

data_core_edge_area_90m_natural <- data_core_edge_area %>%
  dplyr::filter(vegetation == "natural roads rails", edge_dist == 90)
data_core_edge_area_90m_natural[1, ]$per_cumsum
data_core_edge_area_90m_natural[8, ]$per_cumsum
data_core_edge_area_90m_natural[8, ]$per_cumsum - data_core_edge_area_90m_natural[1, ]$per_cumsum

data_core_edge_area_240m_forest <- data_core_edge_area %>%
  dplyr::filter(vegetation == "forest roads rails", edge_dist == 240)
data_core_edge_area_240m_forest

data_core_edge_area_240m_natural <- data_core_edge_area %>%
  dplyr::filter(vegetation == "natural roads rails", edge_dist == 240)
data_core_edge_area_240m_natural

data_core_edge_area_500m_forest <- data_core_edge_area %>%
  dplyr::filter(vegetation == "forest roads rails", edge_dist >= 500) %>%
  group_by(vegetation, year) %>%
  summarise(per_sum = sum(per))
data_core_edge_area_l1000m_forest

data_core_edge_area_500m_natural <- data_core_edge_area %>%
  dplyr::filter(vegetation == "natural roads rails", edge_dist > 500) %>%
  group_by(vegetation, year) %>%
  summarise(per_sum = sum(per))
data_core_edge_area_l1000m_natural

# export
data_core_edge_area_filter <- data_core_edge_area %>%
  dplyr::mutate(per = round(per, 4),
                per_cumsum = round(per_cumsum, 2)) %>%
  dplyr::filter(edge_dist %in% c(30, 90, 240, 510, 1020, 2520, 5010, 10553, 30360)) %>%
  dplyr::select(year, vegetation, edge_dist, area_ha, per, per_cumsum)
data_core_edge_area_filter

readr::write_csv(data_core_edge_area, "02_results/03_data_core_edge_area.csv")
readr::write_csv(data_core_edge_area_filter, "02_results/03_data_core_edge_area_filter.csv")

### plot ----
plot_core_edge_area_forest <- data_core_edge_area %>%
  dplyr::filter(vegetation == "forest") %>%
  ggplot(aes(x = log10(edge_dist), y = per_cumsum, color = year)) +
  geom_line() +
  geom_point(size = 2) +
  scale_colour_manual(values = paletteer::paletteer_c("ggthemes::Green-Gold", 8)) +
  scale_x_continuous(breaks = log10(c(30, 90, 240, 510, 1020, 2520, 5010, 11010, 32010)),
                     labels = as.character(c(30, 90, 240, 510, 1020, 2520, 5010, 11010, 32010)),
                     expand = c(.02, .02)) +
  scale_y_continuous(expand = c(.02, .02)) +
  geom_segment(aes(x = log10(30), y = 50, xend = log10(90), yend = 50), linetype = "dashed", color = "gray50") +
  geom_segment(aes(x = log10(30), y = 75, xend = log10(240), yend = 75), linetype = "dashed", color = "gray50") +
  geom_segment(aes(x = log10(90), y = 15, xend = log10(90), yend = 50), linetype = "dashed", color = "gray50") +
  geom_segment(aes(x = log10(240), y = 15, xend = log10(240), yend = 75), linetype = "dashed", color = "gray50") +  annotate(geom = "text", x = log10(35), y = 97, label = "a", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (not trimmed by roads and rails)",
       x = "Edge distance (m) log10", y = "Cumulative area (%)", color = "Year") +
  ylim(15, 100) +
  theme_bw(base_size = 20) +
  theme(title = element_text(size = 20),
        legend.position = c(.9, .25))
plot_core_edge_area_forest
ggsave(filename = "03_figures/fig04a_forest_core_edge_area.png", plot = plot_core_edge_area_forest,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_core_edge_area_forest_road <- data_core_edge_area %>%
  dplyr::filter(vegetation == "forest roads rails") %>%
  ggplot(aes(x = log10(edge_dist), y = per_cumsum, color = year)) +
  geom_line() +
  geom_point(size = 2) +
  scale_colour_manual(values = paletteer::paletteer_c("ggthemes::Green-Gold", 8)) +
  scale_x_continuous(breaks = log10(c(30, 90, 240, 510, 1020, 2520, 5010, 11010)),
                     labels = as.character(c(30, 90, 240, 510, 1020, 2520, 5010, 11010)),
                     expand = c(.02, .02)) +
  geom_segment(aes(x = log10(30), y = 50, xend = log10(90), yend = 50), linetype = "dashed", color = "gray50") +
  geom_segment(aes(x = log10(30), y = 75, xend = log10(240), yend = 75), linetype = "dashed", color = "gray50") +
  geom_segment(aes(x = log10(90), y = 15, xend = log10(90), yend = 50), linetype = "dashed", color = "gray50") +
  geom_segment(aes(x = log10(240), y = 15, xend = log10(240), yend = 75), linetype = "dashed", color = "gray50") +
  annotate(geom = "text", x = log10(35), y = 97, label = "a", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (trimmed by roads and rails)",
       x = "Edge distance (m) log10", y = "Cumulative area (%)", color = "Year") +
  ylim(15, 100) +
  theme_bw(base_size = 20) +
  theme(title = element_text(size = 20),
        legend.position = c(.9, .25))
plot_core_edge_area_forest_road
ggsave(filename = "03_figures/fig04a_forest_roads_rails_core_edge_area.png",
       plot = plot_core_edge_area_forest_road,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_core_edge_area_natural <- data_core_edge_area %>%
  dplyr::filter(vegetation == "natural") %>%
  ggplot(aes(x = log10(edge_dist), y = per_cumsum, color = year)) +
  geom_line() +
  geom_point(size = 2) +
  scale_colour_manual(values = paletteer::paletteer_c("ggthemes::Orange-Gold", 8)) +
  scale_x_continuous(breaks = log10(c(30, 90, 240, 510, 1020, 2520, 5010, 11010, 32010)),
                     labels = as.character(c(30, 90, 240, 510, 1020, 2520, 5010, 11010, 32010)),
                     expand = c(.02, .02)) +
  geom_segment(aes(x = log10(30), y = 50, xend = log10(90), yend = 50), linetype = "dashed", color = "gray50") +
  geom_segment(aes(x = log10(30), y = 75, xend = log10(240), yend = 75), linetype = "dashed", color = "gray50") +
  geom_segment(aes(x = log10(90), y = 15, xend = log10(90), yend = 50), linetype = "dashed", color = "gray50") +
  geom_segment(aes(x = log10(240), y = 15, xend = log10(240), yend = 75), linetype = "dashed", color = "gray50") +
  annotate(geom = "text", x = log10(35), y = 97, label = "b", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (not trimmed by roads and rails)",
       x = "Edge distance (m) log10", y = "Cumulative area (%)", color = "Year") +
  theme_bw(base_size = 20) +
  theme(title = element_text(size = 20),
        legend.position = c(.9, .25))
plot_core_edge_area_natural
ggsave(filename = "03_figures/fig04b_forest_core_edge_area.png",
       plot = plot_core_edge_area_natural,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_core_edge_area_natural_road <- data_core_edge_area %>%
  dplyr::filter(vegetation == "natural roads rails") %>%
  ggplot(aes(x = log10(edge_dist), y = per_cumsum, color = year)) +
  geom_line() +
  geom_point(size = 2) +
  scale_colour_manual(values = paletteer::paletteer_c("ggthemes::Orange-Gold", 8)) +
  scale_x_continuous(breaks = log10(c(30, 90, 240, 510, 1020, 2520, 5010, 11010, 32010)),
                     labels = as.character(c(30, 90, 240, 510, 1020, 2520, 5010, 11010, 32010)),
                     expand = c(.02, .02)) +
  geom_segment(aes(x = log10(30), y = 50, xend = log10(90), yend = 50), linetype = "dashed", color = "gray50") +
  geom_segment(aes(x = log10(30), y = 75, xend = log10(240), yend = 75), linetype = "dashed", color = "gray50") +
  geom_segment(aes(x = log10(90), y = 15, xend = log10(90), yend = 50), linetype = "dashed", color = "gray50") +
  geom_segment(aes(x = log10(240), y = 15, xend = log10(240), yend = 75), linetype = "dashed", color = "gray50") +
  annotate(geom = "text", x = log10(35), y = 97, label = "b", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (trimmed by roads and rails)",
       x = "Edge distance (m) log10", y = "Cumulative area (%)", color = "Year") +
  theme_bw(base_size = 20) +
  theme(title = element_text(size = 20),
        legend.position = c(.9, .25))
plot_core_edge_area_natural_road
ggsave(filename = "03_figures/fig04b_natural_roads_rails_core_edge_area.png",
       plot = plot_core_edge_area_natural_road,
       wi = 25, he = 20, un = "cm", dpi = 300)

# prepare data
data_core_edge_area_class <- data_core_edge_area %>%
  dplyr::mutate(class = case_when(
    edge_dist <= 30 ~ "0-30",
    edge_dist > 30 & edge_dist <= 90 ~ "30-90",
    edge_dist > 90 & edge_dist <= 240 ~ "90-240",
    edge_dist > 240 & edge_dist <= 510 ~ "240-510",
    edge_dist > 510 & edge_dist <= 1020 ~ "510-\n1020",
    edge_dist > 1020 & edge_dist <= 2520 ~ "1020-\n2520",
    edge_dist > 2520 & edge_dist <= 5010 ~ "2520-\n5010",
    edge_dist > 5010 & edge_dist <= 11010 ~ "5010-\n11010",
    edge_dist > 11010 & edge_dist <= 32010 ~ "11010-\n32010")
  ) %>%
  dplyr::group_by(vegetation, year, class) %>%
  dplyr::summarise(per_total = sum(per)) %>%
  dplyr::mutate(class = forcats::as_factor(class)) %>%
  dplyr::mutate(class = forcats::fct_relevel(class,
                                             c("0-30", "30-90", "90-240",
                                               "240-510", "510-\n1020",
                                               "1020-\n2520", "2520-\n5010",
                                               "5010-\n11010", "11010-\n32010")))
data_core_edge_area_class

# plot
plot_core_edge_area_bar_forest <- data_core_edge_area_class %>%
  dplyr::filter(vegetation == "forest") %>%
  ggplot(aes(x = class, y = per_total, fill = year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  # geom_text(aes(label = paste0(round(per_total, 2), "%")),
  #           nudge_y = 1, size = 7) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Green-Gold", 8)) +
  annotate(geom = "text", x = 1, y = 30, label = "c", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (not trimmed by roads and rails)",
       x = "Edge distance (m)", y = "Percentage (%)", fill = "Year") +
  theme_bw(base_size = 20) +
  theme(title = element_text(size = 20),
        legend.position = c(.9, .4))
plot_core_edge_area_bar_forest
ggsave(filename = "03_figures/fig04c_forest_core_edge_area_bar.png", plot = plot_core_edge_area_bar_forest,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_core_edge_area_bar_forest_road <- data_core_edge_area_class %>%
  dplyr::filter(vegetation == "forest roads rails") %>%
  ggplot(aes(x = class, y = per_total, fill = year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  # geom_text(aes(label = paste0(round(per_total, 2), "%")),
  #           nudge_y = 1, size = 7) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Green-Gold", 8)) +
  annotate(geom = "text", x = 1, y = 30, label = "c", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (trimmed by roads and rails)",
       x = "Edge distance (m)", y = "Percentage (%)", fill = "Year") +
  theme_bw(base_size = 20) +
  theme(title = element_text(size = 20),
        legend.position = c(.9, .4))
plot_core_edge_area_bar_forest_road
ggsave(filename = "03_figures/fig04c_forest_roads_rails_core_edge_area_bar.png", plot = plot_core_edge_area_bar_forest_road,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_core_edge_area_bar_natural <- data_core_edge_area_class %>%
  dplyr::filter(vegetation == "natural") %>%
  ggplot(aes(x = class, y = per_total, fill = year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  # geom_text(aes(label = paste0(round(per_total, 2), "%")),
  #           nudge_y = 1, size = 7) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Orange-Gold", 8)) +
  annotate(geom = "text", x = 1, y = 30, label = "d", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (not trimmed by roads and rails)",
       x = "Edge distance (m)", y = "Percentage (%)", fill = "Year") +
  ylim(0, 30) +
  theme_bw(base_size = 20) +
  theme(title = element_text(size = 20),
        legend.position = c(.9, .4))
plot_core_edge_area_bar_natural
ggsave(filename = "03_figures/fig04d_natural_core_edge_area_bar.png", plot = plot_core_edge_area_bar_natural,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_core_edge_area_bar_natural_road <- data_core_edge_area_class %>%
  dplyr::filter(vegetation == "natural roads rails") %>%
  ggplot(aes(x = class, y = per_total, fill = year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  # geom_text(aes(label = paste0(round(per_total, 2), "%")),
  #           nudge_y = 1, size = 7) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Orange-Gold", 8)) +
  annotate(geom = "text", x = 1, y = 30, label = "d", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (trimmed by roads and rails)",
       x = "Edge distance (m)", y = "Percentage (%)", fill = "Year") +
  ylim(0, 30) +
  theme_bw(base_size = 20) +
  theme(title = element_text(size = 20),
        legend.position = c(.9, .4))
plot_core_edge_area_bar_natural_road
ggsave(filename = "03_figures/fig04d_natural_roads_rails_core_edge_area_bar.png", plot = plot_core_edge_area_bar_natural_road,
       wi = 25, he = 20, un = "cm", dpi = 300)


# 4 functional connectivity ---------------------------------------------

### data ----

# list files
files_connectivity <- dir(path = "02_results", pattern = "confun", full.names = TRUE)
files_connectivity

# import data
data_connectivity <- NULL

vegetations <- c("forest", "natural", "forest_roads_rails", "natural_roads_rails")
gaps <- c("0060", "0120", "0180", "0240", "0300", "0600", "0900", "1200", "1500")

for(i in c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2021)){

  print(i)

  files_connectivity_i <- stringr::str_subset(files_connectivity, paste0("_", i, "_"))
  data_connectivity_i <- NULL

  for(j in vegetations){

    area_i <- readr::read_csv(paste0("02_results/02_mapbiomas_brazil_af_trinacional_", i, "_af_lim_", sub("_confun", "", j), "_pid_area.csv"),
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
        dplyr::mutate(year = i, vegetation = sub("_confun", "", j), .before = 1)

      data_connectivity_j <- dplyr::bind_rows(data_connectivity_j, data_connectivity_k)

    }

    data_connectivity_i <- dplyr::bind_rows(data_connectivity_i, data_connectivity_j) %>%
      dplyr::bind_rows(tibble::tibble(year = i, vegetation = sub("_confun", "", j), gap_crossing = "0",
                                      area_mean = mean_area_i, high_cluster = max_area_i/total_area_i * 100)) %>%
      dplyr::arrange(gap_crossing)

  }

  data_connectivity <- dplyr::bind_rows(data_connectivity, data_connectivity_i)

}

data_connectivity

# summary
data_connectivity_summary <- data_connectivity %>%
  dplyr::arrange(year, vegetation) %>%
  dplyr::mutate(area_mean = round(area_mean, 2),
                high_cluster = round(high_cluster, 2))
data_connectivity_summary

readr::write_csv(data_connectivity_summary, "02_results/04_data_connectivity_resume.csv")

### plot ----

# plot expected cluster size
plot_connectivity_mean_forest <- data_connectivity %>%
  dplyr::filter(vegetation == "forest") %>%
  ggplot(aes(x = as.factor(as.numeric(gap_crossing)), y = log10(area_mean), fill = as.factor(year))) +
  # geom_line(linewidth = .8) +
  # geom_point(size = 5, color = "white") +
  # geom_point(size = 3, alpha = .5) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Green-Gold", 8)) +
  # scale_x_continuous(breaks = c(0, 60, 120, 180, 240, 300, 600, 900, 1200, 1500),
  #                    labels = as.character(c(0, 60, 120, 180, 240, 300, 600, 900, 1200, 1500)),
  #                    expand = c(.02, .02)) +
  scale_y_continuous(breaks = 1:4, labels = c("10", "100", "1000", "10000")) +
  annotate(geom = "text", x = 1, y = 5, label = "a", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (not trimmed by roads and rails)",
       x = "Gap crossing (m)", y = "Expected cluster size log10(ha)", fill = "Year") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.1, .7),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_connectivity_mean_forest
ggsave(filename = "03_figures/fig05a_connectivity_mean_forest.png", plot = plot_connectivity_mean_forest,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_connectivity_mean_forest_road <- data_connectivity %>%
  dplyr::filter(vegetation == "forest_roads_rails") %>%
  ggplot(aes(x = as.factor(as.numeric(gap_crossing)), y = log10(area_mean), fill = as.factor(year))) +
  # geom_line(linewidth = .8) +
  # geom_point(size = 5, color = "white") +
  # geom_point(size = 3, alpha = .5) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Green-Gold", 8)) +
  # scale_x_continuous(breaks = c(0, 60, 120, 180, 240, 300, 600, 900, 1200, 1500),
  #                    labels = as.character(c(0, 60, 120, 180, 240, 300, 600, 900, 1200, 1500)),
  #                    expand = c(.02, .02)) +
  scale_y_continuous(breaks = 1:4, labels = c("10", "100", "1000", "10000")) +
  annotate(geom = "text", x = 1, y = 5, label = "a", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (trimmed by roads and rails)",
       x = "Gap crossing (m)", y = "Expected cluster size log10(ha)", fill = "Year") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.1, .7),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_connectivity_mean_forest_road
ggsave(filename = "03_figures/fig05a_connectivity_mean_forest_roads_rails.png", plot = plot_connectivity_mean_forest_road,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_connectivity_mean_natural <- data_connectivity %>%
  dplyr::filter(vegetation == "natural") %>%
  ggplot(aes(x = as.factor(as.numeric(gap_crossing)), y = log10(area_mean), fill = as.factor(year))) +
  # geom_line(linewidth = .8) +
  # geom_point(size = 5, color = "white") +
  # geom_point(size = 3, alpha = .5) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Orange-Gold", 8)) +
  # scale_x_continuous(breaks = c(0, 60, 120, 180, 240, 300, 600, 900, 1200, 1500),
  #                    labels = as.character(c(0, 60, 120, 180, 240, 300, 600, 900, 1200, 1500)),
  #                    expand = c(.02, .02)) +
  scale_y_continuous(breaks = 1:4, labels = c("10", "100", "1000", "10000")) +
  annotate(geom = "text", x = 1, y = 5, label = "b", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (not trimmed by roads and rails)",
       x = "Gap crossing (m)", y = "Expected cluster size log10(ha)", fill = "Year") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.1, .7),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_connectivity_mean_natural
ggsave(filename = "03_figures/fig05b_connectivity_mean_natural.png", plot = plot_connectivity_mean_natural,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_connectivity_mean_natural_road <- data_connectivity %>%
  dplyr::filter(vegetation == "natural_roads_rails") %>%
  ggplot(aes(x = as.factor(as.numeric(gap_crossing)), y = log10(area_mean), fill = as.factor(year))) +
  # geom_line(linewidth = .8) +
  # geom_point(size = 5, color = "white") +
  # geom_point(size = 3, alpha = .5) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Orange-Gold", 8)) +
  # scale_x_continuous(breaks = c(0, 60, 120, 180, 240, 300, 600, 900, 1200, 1500),
  #                    labels = as.character(c(0, 60, 120, 180, 240, 300, 600, 900, 1200, 1500)),
  #                    expand = c(.02, .02)) +
  scale_y_continuous(breaks = 1:4, labels = c("10", "100", "1000", "10000")) +
  annotate(geom = "text", x = 1, y = 5, label = "b", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (trimmed by roads and rails)",
       x = "Gap crossing (m)", y = "Expected cluster size log10(ha)", fill = "Year") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.1, .7),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_connectivity_mean_natural_road
ggsave(filename = "03_figures/fig05b_connectivity_mean_natural_roads_rails.png", plot = plot_connectivity_mean_natural_road,
       wi = 25, he = 20, un = "cm", dpi = 300)

# plot
plot_connectivity_high_forest <- data_connectivity %>%
  dplyr::filter(vegetation == "forest") %>%
  ggplot(aes(x = as.numeric(gap_crossing), y = high_cluster, color = as.factor(year))) +
  geom_line(linewidth = .8) +
  geom_point(size = 5, color = "white") +
  geom_point(size = 3, alpha = .5) +
  scale_color_manual(values = paletteer::paletteer_c("ggthemes::Green-Gold", 8)) +
  scale_x_continuous(breaks = c(0, 60, 120, 180, 240, 300, 600, 900, 1200, 1500),
                     labels = as.character(c(0, 60, 120, 180, 240, 300, 600, 900, 1200, 1500)),
                     expand = c(.02, .02)) +
  scale_y_continuous(expand = c(.02, .02)) +
  annotate(geom = "text", x = 50, y = 100, label = "c", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (not trimmed by roads and rails)",
       x = "Gap crossing (m)", color = "Year",
       y = "Highest cluster size \n(% of total remaining vegetation)") +
  ylim(0, 100) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.9, .25),
        axis.text.x = element_text(angle = 40, vjust = .7))
plot_connectivity_high_forest
ggsave(filename = "03_figures/fig05c_connectivity_high_forest.png", plot = plot_connectivity_high_forest,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_connectivity_high_forest_road <- data_connectivity %>%
  dplyr::filter(vegetation == "forest_roads_rails") %>%
  ggplot(aes(x = as.numeric(gap_crossing), y = high_cluster, color = as.factor(year))) +
  geom_line(linewidth = .8) +
  geom_point(size = 5, color = "white") +
  geom_point(size = 3, alpha = .5) +
  scale_color_manual(values = paletteer::paletteer_c("ggthemes::Green-Gold", 8)) +
  scale_x_continuous(breaks = c(0, 60, 120, 180, 240, 300, 600, 900, 1200, 1500),
                     labels = as.character(c(0, 60, 120, 180, 240, 300, 600, 900, 1200, 1500)),
                     expand = c(.02, .02)) +
  scale_y_continuous(expand = c(.02, .02)) +
  annotate(geom = "text", x = 50, y = 100, label = "c", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (trimmed by roads and rails)",
       x = "Gap crossing (m)", color = "Year",
       y = "Highest cluster size \n(% of total remaining vegetation)") +
  ylim(0, 100) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.9, .25),
        axis.text.x = element_text(angle = 40, vjust = .7))
plot_connectivity_high_forest_road
ggsave(filename = "03_figures/fig05c_connectivity_high_forest_roads_rails.png", plot = plot_connectivity_high_forest_road,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_connectivity_high_natural <- data_connectivity %>%
  dplyr::filter(vegetation == "natural") %>%
  ggplot(aes(x = as.numeric(gap_crossing), y = high_cluster, color = as.factor(year))) +
  geom_line(linewidth = .8) +
  geom_point(size = 5, color = "white") +
  geom_point(size = 3, alpha = .5) +
  scale_color_manual(values = paletteer::paletteer_c("ggthemes::Orange-Gold", 8)) +
  scale_x_continuous(breaks = c(0, 60, 120, 180, 240, 300, 600, 900, 1200, 1500),
                     labels = as.character(c(0, 60, 120, 180, 240, 300, 600, 900, 1200, 1500)),
                     expand = c(.02, .02)) +
  scale_y_continuous(expand = c(.02, .02)) +
  annotate(geom = "text", x = 50, y = 100, label = "d", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (not trimmed by roads and rails)",
       x = "Gap crossing (m)", color = "Year",
       y = "Highest cluster size \n(% of total remaining vegetation)") +
  ylim(0, 100) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.9, .25),
        axis.text.x = element_text(angle = 40, vjust = .7))
plot_connectivity_high_natural
ggsave(filename = "03_figures/fig05d_connectivity_high_natural.png", plot = plot_connectivity_high_natural,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_connectivity_high_natural_roads_rails <- data_connectivity %>%
  dplyr::filter(vegetation == "natural_roads_rails") %>%
  ggplot(aes(x = as.numeric(gap_crossing), y = high_cluster, color = as.factor(year))) +
  geom_line(linewidth = .8) +
  geom_point(size = 5, color = "white") +
  geom_point(size = 3, alpha = .5) +
  scale_color_manual(values = paletteer::paletteer_c("ggthemes::Orange-Gold", 8)) +
  scale_x_continuous(breaks = c(0, 60, 120, 180, 240, 300, 600, 900, 1200, 1500),
                     labels = as.character(c(0, 60, 120, 180, 240, 300, 600, 900, 1200, 1500)),
                     expand = c(.02, .02)) +
  scale_y_continuous(expand = c(.02, .02)) +
  annotate(geom = "text", x = 50, y = 100, label = "d", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (trimmed by roads and rails)",
       x = "Gap crossing (m)", color = "Year",
       y = "Highest cluster size \n(% of total remaining vegetation)") +
  ylim(0, 100) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.9, .25),
        axis.text.x = element_text(angle = 40, vjust = .7))
plot_connectivity_high_natural_roads_rails
ggsave(filename = "03_figures/fig05d_connectivity_high_natural_roads_rails.png",
       plot = plot_connectivity_high_natural_roads_rails,
       wi = 25, he = 20, un = "cm", dpi = 300)

# 5 mean isolation --------------------------------------------------------

### data ----

# import mean data
files_isolation <- dir(path = "02_results", pattern = "isolation", full.names = TRUE) %>%
  stringr::str_subset("_quantile", negate = TRUE) %>%
  stringr::str_subset("data_isolation", negate = TRUE) %>%
  stringr::str_subset("larger1000", negate = TRUE) %>%
  sort()
files_isolation

data_isolation_mean <- NULL
for(i in files_isolation){

  names_isolation <- i %>%
    basename() %>%
    stringr::str_split("_", simplify = TRUE)

  data_isolation_mean_i <- readr::read_csv(i) %>%
    dplyr::mutate(vegetation = ifelse(length(names_isolation) == 13,
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

# import quantile data
files_isolation_quantile <- dir(path = "02_results", pattern = "isolation", full.names = TRUE) %>%
  stringr::str_subset("_quantile", negate = FALSE) %>%
  stringr::str_subset("1000_quantile", negate = TRUE)
files_isolation_quantile

data_isolation_quantile <- NULL
for(i in files_isolation_quantile){

  names_isolation <- i %>%
    basename() %>%
    stringr::str_split("_", simplify = TRUE)

  data_isolation_quantile_i <- readr::read_delim(file = i, delim = ":",
                                                 col_names = c("id", "quantile", "value")) %>%
    dplyr::select(2:3) %>%
    dplyr::mutate(vegetation = ifelse(length(names_isolation) == 14,
                                      paste0(names_isolation[, 9], "_", names_isolation[, 10], "_", names_isolation[, 11]),
                                      names_isolation[, 9]),
                  year = names_isolation[, 6],
                  area = ifelse(length(names_isolation) == 14,
                                stringr::str_extract(names_isolation[, 13], pattern = "[0-9]+"),
                                stringr::str_extract(names_isolation[, 11], pattern = "[0-9]+")),
                  .before = 1)
  data_isolation_quantile_i

  data_isolation_quantile <- dplyr::bind_rows(data_isolation_quantile, data_isolation_quantile_i)

}
data_isolation_quantile

# resume
data_isolation_mean_resume <- data_isolation_mean %>%
  dplyr::select(year, vegetation, area, mean, stddev) %>%
  dplyr::arrange(year, vegetation, area)
data_isolation_mean_resume

data_isolation_quantile_resume <- data_isolation_quantile %>%
  dplyr::mutate(quantile = paste0("quantile_", quantile)) %>%
  tidyr::pivot_wider(names_from = quantile, values_from = value) %>%
  dplyr::arrange(year, vegetation, area)
data_isolation_quantile_resume

data_isolation_resume <- data_isolation_mean_resume %>%
  dplyr::left_join(data_isolation_quantile_resume)
data_isolation_resume

readr::write_csv(data_isolation_resume, "data_isolation_resume.csv")

# filter
data_isolation_resume_000 <- data_isolation_resume %>%
  dplyr::filter(area == "0000") %>%
  dplyr::arrange(vegetation, year) %>%
  dplyr::select(1:4, 8)
data_isolation_resume_000

data_isolation_resume_050 <- data_isolation_resume %>%
  dplyr::filter(area == "0050") %>%
  dplyr::arrange(vegetation, year) %>%
  dplyr::select(1:4, 8)
data_isolation_resume_050

data_isolation_resume_100 <- data_isolation_resume %>%
  dplyr::filter(area == "0100") %>%
  dplyr::arrange(vegetation, year)  %>%
  dplyr::select(1:4, 8)
data_isolation_resume_100

data_isolation_resume_150 <- data_isolation_resume %>%
  dplyr::filter(area == "0150") %>%
  dplyr::arrange(vegetation, year) %>%
  dplyr::select(1:4, 8)
data_isolation_resume_150

data_isolation_resume_200 <- data_isolation_resume %>%
  dplyr::filter(area == "0200") %>%
  dplyr::arrange(vegetation, year) %>%
  dplyr::select(1:4, 8)
data_isolation_resume_200

data_isolation_resume_250 <- data_isolation_resume %>%
  dplyr::filter(area == "0250") %>%
  dplyr::arrange(vegetation, year) %>%
  dplyr::select(1:4, 8)
data_isolation_resume_250

data_isolation_resume_350 <- data_isolation_resume %>%
  dplyr::filter(area == "0350") %>%
  dplyr::arrange(vegetation, year) %>%
  dplyr::select(1:4, 8)
data_isolation_resume_350

data_isolation_resume_500 <- data_isolation_resume %>%
  dplyr::filter(area == "0500") %>%
  dplyr::arrange(vegetation, year) %>%
  dplyr::select(1:4, 8)
data_isolation_resume_500

### plot ----

# plot
plot_isolation_forest_mean <- data_isolation_mean %>%
  dplyr::filter(vegetation == "forest") %>%
  ggplot(aes(x = as.factor(as.numeric(area)), y = mean, fill = as.factor(year))) +
  # geom_line(linewidth = .8) +
  # geom_point(size = 7, color = "white") +
  # geom_point(size = 3, alpha = .5) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Green-Gold", 8)) +
  # scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 350, 500),
  #                    labels = as.character(c(0, 50, 100, 150, 200, 350, 500))) +
  labs(title = "Forest vegetation (not trimmed by roads and rails)",
       x = "Smallest patch size (ha)", y = "Mean isolation (m)", fill = "Year") +
  annotate(geom = "text", x = 1, y = 15000, label = "c", size = 10, fontface = "bold") +
  # ylim(0, 25000) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.08, .7),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_isolation_forest_mean
ggsave(filename = "03_figures/fig06c_isolation_forest_mean.png",
       plot = plot_isolation_forest_mean,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_isolation_forest_roads_rails_mean <- data_isolation_mean %>%
  dplyr::filter(vegetation == "forest_roads_rails") %>%
  ggplot(aes(x = as.factor(as.numeric(area)), y = mean, fill = as.factor(year))) +
  # geom_line(linewidth = .8) +
  # geom_point(size = 7, color = "white") +
  # geom_point(size = 3, alpha = .5) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Green-Gold", 8)) +
  # scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 350, 500),
  #                    labels = as.character(c(0, 50, 100, 150, 200, 350, 500))) +
  labs(title = "Forest vegetation (trimmed by roads and rails)",
       x = "Smallest patch size (ha)", y = "Mean isolation (m)", fill = "Year") +
  annotate(geom = "text", x = 1, y = 15000, label = "a", size = 10, fontface = "bold") +
  # ylim(0, 25000) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.08, .7),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_isolation_forest_roads_rails_mean
ggsave(filename = "03_figures/fig06a_isolation_forest_roads_rails_mean.png",
       plot = plot_isolation_forest_roads_rails_mean,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_isolation_forest_median <- data_isolation_quantile %>%
  dplyr::filter(vegetation == "forest", quantile == 50) %>%
  ggplot(aes(x = as.factor(as.numeric(area)), y = value, fill = as.factor(year))) +
  # geom_line(linewidth = .8) +
  # geom_point(size = 7, color = "white") +
  # geom_point(size = 3, alpha = .5) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Green-Gold", 8)) +
  # scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 350, 500),
  #                    labels = as.character(c(0, 50, 100, 150, 200, 350, 500))) +
  labs(title = "Forest vegetation (not trimmed by roads and rails)",
       x = "Smallest patch size (ha)", y = "Median isolation (m)", fill = "Year") +
  annotate(geom = "text", x = 1, y = 8000, label = "e", size = 10, fontface = "bold") +
  # ylim(0, 25000) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.08, .7),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_isolation_forest_median
ggsave(filename = "03_figures/fig06e_isolation_forest_median.png",
       plot = plot_isolation_forest_median,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_isolation_forest_roads_rails_median <- data_isolation_quantile %>%
  dplyr::filter(vegetation == "forest_roads_rails", quantile == 50) %>%
  ggplot(aes(x = as.factor(as.numeric(area)), y = value, fill = as.factor(year))) +
  # geom_line(linewidth = .8) +
  # geom_point(size = 7, color = "white") +
  # geom_point(size = 3, alpha = .5) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Green-Gold", 8)) +
  # scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 350, 500),
  #                    labels = as.character(c(0, 50, 100, 150, 200, 350, 500))) +
  labs(title = "Forest vegetation (trimmed by roads and rails)",
       x = "Smallest patch size (ha)", y = "Median isolation (m)", fill = "Year") +
  annotate(geom = "text", x = 1, y = 8000, label = "a", size = 10, fontface = "bold") +
  # ylim(0, 25000) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.08, .7),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_isolation_forest_roads_rails_median
ggsave(filename = "03_figures/fig06a_isolation_forest_roads_rails_median.png",
       plot = plot_isolation_forest_roads_rails_median,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_isolation_natural_mean <- data_isolation_mean %>%
  dplyr::filter(vegetation == "natural") %>%
  ggplot(aes(x = as.factor(as.numeric(area)), y = mean, fill = as.factor(year))) +
  # geom_line(linewidth = .8) +
  # geom_point(size = 7, color = "white") +
  # geom_point(size = 3, alpha = .5) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Orange-Gold", 8)) +
  # scale_x_continuous(breaks = c(0, 30, 90, 210, 360, 510, 1020),
  #                    labels = as.character(c(0, 30, 90, 210, 360, 510, 1020))) +
  labs(title = "Natural vegetation (not trimmed by roads and rails)",
       x = "Smallest patch size (ha)", y = "Mean isolation (m)", fill = "Year") +
  annotate(geom = "text", x = 1, y = 15000, label = "d", size = 10, fontface = "bold") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.08, .7),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_isolation_natural_mean
ggsave(filename = "03_figures/fig06d_isolation_natural_mean.png",
       plot = plot_isolation_natural_mean,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_isolation_natural_roads_rails_mean <- data_isolation_mean %>%
  dplyr::filter(vegetation == "natural_roads_rails") %>%
  ggplot(aes(x = as.factor(as.numeric(area)), y = mean, fill = as.factor(year))) +
  # geom_line(linewidth = .8) +
  # geom_point(size = 7, color = "white") +
  # geom_point(size = 3, alpha = .5) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Orange-Gold", 8)) +
  # scale_x_continuous(breaks = c(0, 30, 90, 210, 360, 510, 1020),
  #                    labels = as.character(c(0, 30, 90, 210, 360, 510, 1020))) +
  labs(title = "Natural vegetation (trimmed by roads and rails)",
       x = "Smallest patch size (ha)", y = "Mean isolation (m)", fill = "Year") +
  annotate(geom = "text", x = 1, y = 15000, label = "b", size = 10, fontface = "bold") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.08, .7),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_isolation_natural_roads_rails_mean
ggsave(filename = "03_figures/fig06b_isolation_natural_roads_rails_mean.png",
       plot = plot_isolation_natural_roads_rails_mean,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_isolation_natural_median <- data_isolation_quantile %>%
  dplyr::filter(vegetation == "natural", quantile == 50) %>%
  ggplot(aes(x = as.factor(as.numeric(area)), y = value, fill = as.factor(year))) +
  # geom_line(linewidth = .8) +
  # geom_point(size = 7, color = "white") +
  # geom_point(size = 3, alpha = .5) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Orange-Gold", 8)) +
  # scale_x_continuous(breaks = c(0, 30, 90, 210, 360, 510, 1020),
  #                    labels = as.character(c(0, 30, 90, 210, 360, 510, 1020))) +
  labs(title = "Natural vegetation (not trimmed by roads and rails)",
       x = "Smallest patch size (ha)", y = "Median isolation (m)", fill = "Year") +
  annotate(geom = "text", x = 1, y = 8000, label = "f", size = 10, fontface = "bold") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.08, .7),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_isolation_natural_median
ggsave(filename = "03_figures/fig06f_isolation_natural_median.png",
       plot = plot_isolation_natural_median,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_isolation_natural_roads_rails_median <- data_isolation_quantile %>%
  dplyr::filter(vegetation == "natural_roads_rails", quantile == 50) %>%
  ggplot(aes(x = as.factor(as.numeric(area)), y = value, fill = as.factor(year))) +
  # geom_line(linewidth = .8) +
  # geom_point(size = 7, color = "white") +
  # geom_point(size = 3, alpha = .5) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Orange-Gold", 8)) +
  # scale_x_continuous(breaks = c(0, 30, 90, 210, 360, 510, 1020),
  #                    labels = as.character(c(0, 30, 90, 210, 360, 510, 1020))) +
  labs(title = "Natural vegetation (trimmed by roads and rails)",
       x = "Smallest patch size (ha)", y = "Median isolation (m)", fill = "Year") +
  annotate(geom = "text", x = 1, y = 8000, label = "b", size = 10, fontface = "bold") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.08, .7),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_isolation_natural_roads_rails_median
ggsave(filename = "03_figures/fig06b_isolation_natural_roads_rails_median.png",
       plot = plot_isolation_natural_roads_rails_median,
       wi = 25, he = 20, un = "cm", dpi = 300)

# 6 landscape and topographic index ---------------------------------------

## import original data ----

### elevation ----
ele <- readr::read_csv("02_results/06_fabdem_elevation_original.csv",
                       col_names = c("value", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(per = n/sum(n), .after = n) %>%
  dplyr::mutate(class = case_when(
    value <= 100 ~ "≤100",
    value > 100 & value <= 200 ~ ">100-\n200",
    value > 200 & value <= 400 ~ ">200-\n400",
    value > 400 & value <= 800 ~ ">400-\n800",
    value > 800 & value <= 1200 ~ ">800-\n1200",
    value > 1200 & value <= 1600 ~ ">1200-\n1600",
    value > 1600 ~ ">1600")) %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(per_total = sum(per)*100) %>%
  dplyr::mutate(class = forcats::as_factor(class)) %>%
  dplyr::mutate(class = forcats::fct_relevel(class,
                                             c("≤100", ">100-\n200", ">200-\n400",
                                               ">400-\n800", ">800-\n1200",
                                               ">1200-\n1600", ">1600"))) %>%
  dplyr::arrange(class) %>%
  dplyr::mutate(remain = "Original distribution", topography = "elevation", .before = 1)
ele

### slope ----
slo <- readr::read_csv("02_results/06_fabdem_float_slope_original.csv",
                       col_names = c("value", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(per = n/sum(n), .after = n) %>%
  dplyr::mutate(class = case_when(
    value <= 5 ~ "≤5°",
    value > 5 & value <= 10 ~ ">5°-10°",
    value > 10 & value <= 15 ~ ">10°-15°",
    value > 15 & value <= 20 ~ ">15°-20°",
    value > 20 & value <= 25 ~ ">20°-25°",
    value > 25 ~ ">25°")) %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(per_total = sum(per)*100) %>%
  dplyr::mutate(class = forcats::as_factor(class)) %>%
  dplyr::mutate(class = forcats::fct_relevel(class,
                                             c("≤5°", ">5°-10°",
                                               ">10°-15°", ">15°-20°",
                                               ">20°-25°", ">25°"))) %>%
  dplyr::arrange(class) %>%
  dplyr::mutate(remain = "Original distribution", topography = "slope", .before = 1)
slo

### aspect ----
asp_original <- readr::read_csv("02_results/06_fabdem_float_aspect_original.csv",
                                col_names = c("value", "area", "n"), col_types = readr::cols())

asp <- asp_original %>%
  dplyr::filter(value != 0) %>%
  dplyr::mutate(per = n/sum(n), .after = n) %>%
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
  dplyr::mutate(remain = c("1", "2", "Original"), .before = 1)
asp

asp

### pcurvature ----
pcu <- readr::read_csv("02_results/06_fabdem_float_pcurvature_original.csv",
                       col_names = c("value", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(per = n/sum(n), .after = n) %>%
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
  dplyr::summarise(per_total = sum(per)*100) %>%
  dplyr::mutate(class = forcats::as_factor(class)) %>%
  dplyr::mutate(class = forcats::fct_relevel(class,
                                             c("≤-0.005", ">-0.005-\n-0.002",
                                               ">-0.002-\n-0.001", ">-0.001-\n0.000",
                                               ">0.000-\n0.001", ">0.001-\n0.002",
                                               ">0.002-\n0.005", ">0.005"))) %>%
  dplyr::arrange(class) %>%
  dplyr::mutate(remain = "Original distribution", topography = "pcurvature", .before = 1)
pcu

### tcurvature ----
tcu <- readr::read_csv("02_results/06_fabdem_float_tcurvature_original.csv",
                       col_names = c("value", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(per = n/sum(n), .after = n) %>%
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
  dplyr::summarise(per_total = sum(per)*100) %>%
  dplyr::mutate(class = forcats::as_factor(class)) %>%
  dplyr::mutate(class = forcats::fct_relevel(class,
                                             c("≤-0.005", ">-0.005-\n-0.002",
                                               ">-0.002-\n-0.001", ">-0.001-\n0.000",
                                               ">0.000-\n0.001", ">0.001-\n0.002",
                                               ">0.002-\n0.005", ">0.005"))) %>%
  dplyr::arrange(class) %>%
  dplyr::mutate(remain = "Original distribution", topography = "tcurvature", .before = 1)
tcu

### geomorph ----
geomorph <- readr::read_csv("02_results/06_fabdem_float_geomorph_original.csv",
                            col_names = c("value", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(per_total = n/sum(n)*100, .after = n) %>%
  dplyr::bind_cols(geo = c("flat", "peak", "ridge", "shoulder", "spur", "slope",
                           "hollow", "footslope", "valley", "pit")) %>%
  dplyr::mutate(geo = forcats::as_factor(geo)) %>%
  dplyr::mutate(geo = forcats::fct_relevel(geo,
                                           c("flat", "peak", "ridge", "shoulder", "spur", "slope",
                                             "hollow", "footslope", "valley", "pit"))) %>%
  dplyr::mutate(remain = "Original distribution", topography = "geomorph", .before = 1)
geomorph

## import data ----

# list files
files_topography <- dir(path = "02_results", pattern = "06_mapbiomas", full.names = TRUE) %>%
  stringr::str_subset("original", negate = TRUE)
files_topography

# import vegetation data
data_topography_elevation <- NULL
data_topography_slope <- NULL
data_topography_aspect <- NULL
data_topography_pcurvature <- NULL
data_topography_tcurvature <- NULL
data_topography_geomorph <- NULL

for(i in c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2021)){

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
      readr::read_csv(col_names = c("forest", "value", "area", "n")) %>%
      dplyr::mutate(class = case_when(
        value <= 100 ~ "≤100",
        value > 100 & value <= 200 ~ ">100-\n200",
        value > 200 & value <= 400 ~ ">200-\n400",
        value > 400 & value <= 800 ~ ">400-\n800",
        value > 800 & value <= 1200 ~ ">800-\n1200",
        value > 1200 & value <= 1600 ~ ">1200-\n1600",
        value > 1600 ~ ">1600")) %>%
      dplyr::group_by(forest, class) %>%
      dplyr::summarise(n_sum = sum(n)) %>%
      dplyr::mutate(class = forcats::as_factor(class)) %>%
      dplyr::mutate(class = forcats::fct_relevel(class,
                                                 c("≤100", ">100-\n200", ">200-\n400",
                                                   ">400-\n800", ">800-\n1200",
                                                   ">1200-\n1600", ">1600"))) %>%
      dplyr::arrange(class) %>%
      dplyr::mutate(vegetation = j, topography = "elevation", year = i, .before = 1)
    data_topography_elevation_partial

    data_topography_elevation_total <- data_topography_elevation_partial %>%
      dplyr::group_by(class) %>%
      dplyr::summarise(original_distribution = sum(n_sum)) %>%
      dplyr::pull(original_distribution)
    data_topography_elevation_total

    data_topography_elevation_j <- data_topography_elevation_partial %>%
      dplyr::filter(forest == 1) %>%
      dplyr::mutate(n_total = data_topography_elevation_total,
                    per_total = n_sum/data_topography_elevation_total * 100,
                    remain = paste0("Remaining ", j, " (", i, ")"))
    data_topography_elevation_j

    data_topography_elevation_i <- dplyr::bind_rows(data_topography_elevation_i, data_topography_elevation_j)


    ### slope ----
    data_topography_slope_partial <- stringr::str_subset(files_topography_i, paste0(j, "_slope")) %>%
      readr::read_csv(col_names = c("forest", "value", "area", "n")) %>%
      dplyr::mutate(class = case_when(
        value <= 5 ~ "≤5°",
        value > 5 & value <= 10 ~ ">5°-10°",
        value > 10 & value <= 15 ~ ">10°-15°",
        value > 15 & value <= 20 ~ ">15°-20°",
        value > 20 & value <= 25 ~ ">20°-25°",
        value > 25 ~ ">25°")) %>%
      dplyr::group_by(forest, class) %>%
      dplyr::summarise(n_sum = sum(n)) %>%
      dplyr::mutate(class = forcats::as_factor(class)) %>%
      dplyr::mutate(class = forcats::fct_relevel(class,
                                                 c("≤5°", ">5°-10°",
                                                   ">10°-15°", ">15°-20°",
                                                   ">20°-25°", ">25°"))) %>%
      dplyr::arrange(class) %>%
      dplyr::mutate(vegetation = j, topography = "slope", year = i, .before = 1)
    data_topography_slope_partial

    data_topography_slope_total <- data_topography_slope_partial %>%
      dplyr::group_by(class) %>%
      dplyr::summarise(original_distribution = sum(n_sum)) %>%
      dplyr::pull(original_distribution)
    data_topography_slope_total

    data_topography_slope_j <- data_topography_slope_partial %>%
      dplyr::filter(forest == 1) %>%
      dplyr::mutate(n_total = data_topography_slope_total,
                    per_total = n_sum/data_topography_slope_total * 100,
                    remain = paste0("Remaining ", j, " (", i, ")"))
    data_topography_slope_j

    data_topography_slope_i <- dplyr::bind_rows(data_topography_slope_i, data_topography_slope_j)

    ###  aspect ----
    data_topography_aspect_partial <- stringr::str_subset(files_topography_i, paste0(j, "_aspect")) %>%
      readr::read_csv(col_names = c("forest", "value", "area", "n")) %>%
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
      dplyr::group_by(forest, class) %>%
      dplyr::summarise(n_sum = sum(n)) %>%
      dplyr::mutate(class = forcats::as_factor(class)) %>%
      dplyr::mutate(class = forcats::fct_relevel(class,
                                                 c("NE", "N", "NW", "W", "SW", "S", "SE", "E"))) %>%
      dplyr::arrange(class) %>%
      dplyr::mutate(vegetation = j, topography = "aspect", year = i, .before = 1)
    data_topography_aspect_partial

    data_topography_aspect_total <- data_topography_aspect_partial %>%
      dplyr::group_by(class) %>%
      dplyr::summarise(original_distribution = sum(n_sum)) %>%
      dplyr::pull(original_distribution)
    data_topography_aspect_total

    data_topography_aspect_j <- data_topography_aspect_partial %>%
      dplyr::filter(forest == 1) %>%
      dplyr::mutate(n_total = data_topography_aspect_total,
                    per_total = n_sum/sum(n_sum),
                    remain = paste0("Remaining ", j, " (", i, ")"))
    data_topography_aspect_j

    data_topography_aspect_i <- dplyr::bind_rows(data_topography_aspect_i, data_topography_aspect_j)


    ### pcurvature ----
    data_topography_pcurvature_partial <- stringr::str_subset(files_topography_i, paste0(j, "_pcurvature")) %>%
      readr::read_csv(col_names = c("forest", "value", "area", "n")) %>%
      dplyr::mutate(class = case_when(
        value <= -0.005 ~ "≤-0.005",
        value > -0.005 & value <= -0.002 ~ ">-0.005-\n-0.002",
        value > -0.002 & value <= -0.001 ~ ">-0.002-\n-0.001",
        value > -0.001 & value <= 0.000 ~ ">-0.001-\n0.000",
        value > 0.000 & value <= 0.001 ~ ">0.000-\n0.001",
        value > 0.001 & value <= 0.002 ~ ">0.001-\n0.002",
        value > 0.002 & value <= 0.005 ~ ">0.002-\n0.005",
        value > 0.005 ~ ">0.005")) %>%
      dplyr::group_by(forest, class) %>%
      dplyr::summarise(n_sum = sum(n)) %>%
      dplyr::mutate(class = forcats::as_factor(class)) %>%
      dplyr::mutate(class = forcats::fct_relevel(class,
                                                 c("≤-0.005", ">-0.005-\n-0.002",
                                                   ">-0.002-\n-0.001", ">-0.001-\n0.000",
                                                   ">0.000-\n0.001", ">0.001-\n0.002",
                                                   ">0.002-\n0.005", ">0.005"))) %>%
      dplyr::arrange(class) %>%
      dplyr::mutate(vegetation = j, topography = "pcurvature", year = i, .before = 1)
    data_topography_pcurvature_partial

    data_topography_pcurvature_total <- data_topography_pcurvature_partial %>%
      dplyr::group_by(class) %>%
      dplyr::summarise(original_distribution = sum(n_sum)) %>%
      dplyr::pull(original_distribution)
    data_topography_pcurvature_total

    data_topography_pcurvature_j <- data_topography_pcurvature_partial %>%
      dplyr::filter(forest == 1) %>%
      dplyr::mutate(n_total = data_topography_pcurvature_total,
                    per_total = n_sum/data_topography_pcurvature_total * 100,
                    remain = paste0("Remaining ", j, " (", i, ")"))
    data_topography_pcurvature_j

    data_topography_pcurvature_i <- dplyr::bind_rows(data_topography_pcurvature_i, data_topography_pcurvature_j)

    ### tcurvature ----
    data_topography_tcurvature_partial <- stringr::str_subset(files_topography_i, paste0(j, "_tcurvature")) %>%
      readr::read_csv(col_names = c("forest", "value", "area", "n")) %>%
      dplyr::mutate(class = case_when(
        value <= -0.005 ~ "≤-0.005",
        value > -0.005 & value <= -0.002 ~ ">-0.005-\n-0.002",
        value > -0.002 & value <= -0.001 ~ ">-0.002-\n-0.001",
        value > -0.001 & value <= 0.000 ~ ">-0.001-\n0.000",
        value > 0.000 & value <= 0.001 ~ ">0.000-\n0.001",
        value > 0.001 & value <= 0.002 ~ ">0.001-\n0.002",
        value > 0.002 & value <= 0.005 ~ ">0.002-\n0.005",
        value > 0.005 ~ ">0.005")) %>%
      dplyr::group_by(forest, class) %>%
      dplyr::summarise(n_sum = sum(n)) %>%
      dplyr::mutate(class = forcats::as_factor(class)) %>%
      dplyr::mutate(class = forcats::fct_relevel(class,
                                                 c("≤-0.005", ">-0.005-\n-0.002",
                                                   ">-0.002-\n-0.001", ">-0.001-\n0.000",
                                                   ">0.000-\n0.001", ">0.001-\n0.002",
                                                   ">0.002-\n0.005", ">0.005"))) %>%
      dplyr::arrange(class) %>%
      dplyr::mutate(vegetation = j, topography = "tcurvature", year = i, .before = 1)
    data_topography_tcurvature_partial

    data_topography_tcurvature_total <- data_topography_tcurvature_partial %>%
      dplyr::group_by(class) %>%
      dplyr::summarise(original_distribution = sum(n_sum)) %>%
      dplyr::pull(original_distribution)
    data_topography_tcurvature_total

    data_topography_tcurvature_j <- data_topography_tcurvature_partial %>%
      dplyr::filter(forest == 1) %>%
      dplyr::mutate(n_total = data_topography_tcurvature_total,
                    per_total = n_sum/data_topography_tcurvature_total * 100,
                    remain = paste0("Remaining ", j, " (", i, ")"))
    data_topography_tcurvature_j

    data_topography_tcurvature_i <- dplyr::bind_rows(data_topography_tcurvature_i, data_topography_tcurvature_j)


    ### geomorph ----
    data_topography_geomorph_partial <- stringr::str_subset(files_topography_i, paste0(j, "_geomorph")) %>%
      readr::read_csv(col_names = c("forest", "value", "area", "n")) %>%
      dplyr::mutate(vegetation = j, topography = "geomorph", year = i, .before = 1)
    data_topography_geomorph_partial

    data_topography_geomorph_total <- data_topography_geomorph_partial %>%
      dplyr::group_by(value) %>%
      dplyr::summarise(original_distribution = sum(n)) %>%
      dplyr::pull(original_distribution)
    data_topography_geomorph_total

    data_topography_geomorph_j <- data_topography_geomorph_partial %>%
      dplyr::filter(forest == 1) %>%
      dplyr::mutate(n_total = data_topography_geomorph_total,
                    per_total = n/data_topography_geomorph_total * 100,
                    remain = paste0("Remaining ", j, " (", i, ")"))
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
data_topography_pcurvature <- dplyr::bind_rows(pcu, data_topography_pcurvature)
data_topography_tcurvature <- dplyr::bind_rows(tcu, data_topography_tcurvature)
data_topography_geomorph <- data_topography_geomorph %>%
  dplyr::left_join(., geomorph[, c("geo", "value")]) %>%
  dplyr::bind_rows(geomorph, .)

data_topography_aspect
data_topography_aspect$per_total %>% range()
data_topography_aspect$per_total %>% range()

## plot ----

### elevation ----
plot_topography_elevation_forest <- data_topography_elevation %>%
  dplyr::filter(vegetation == "forest" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining forest ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  labs(title = "Forest vegetation (not trimmed by roads and rails)",
       x = "Elevation range (m)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 70, label = "a", size = 10, fontface = "bold") +
  ylim(0, 70) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .27),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_elevation_forest
ggsave(filename = "03_figures/fig08a_topography_elevation_forest.png", plot = plot_topography_elevation_forest,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_elevation_forest_roads_rails <- data_topography_elevation %>%
  dplyr::filter(vegetation == "forest_roads_rails" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining forest_roads_rails ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  labs(title = "Forest vegetation (trimmed by roads and rails)",
       x = "Elevation range (m)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 70, label = "a", size = 10, fontface = "bold") +
  ylim(0, 70) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .27),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_elevation_forest_roads_rails
ggsave(filename = "03_figures/fig08a_topography_elevation_forest_roads_rails.png",
       plot = plot_topography_elevation_forest_roads_rails,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_elevation_natural <- data_topography_elevation %>%
  dplyr::filter(vegetation == "natural" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining natural ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(title = "Natural vegetation (not trimmed by roads and rails)",
       x = "Elevation range (m)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 70, label = "b", size = 10, fontface = "bold") +
  ylim(0, 70) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .27),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_elevation_natural
ggsave(filename = "03_figures/fig08b_topography_elevation_natural.png", plot = plot_topography_elevation_natural,
       wi = 30, he = 20, un = "cm", dpi = 300)


plot_topography_elevation_natural_roads_rails <- data_topography_elevation %>%
  dplyr::filter(vegetation == "natural_roads_rails" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining natural_roads_rails ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(title = "Natural vegetation (trimmed by roads and rails)",
       x = "Elevation range (m)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 70, label = "b", size = 10, fontface = "bold") +
  ylim(0, 70) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .27),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_elevation_natural_roads_rails
ggsave(filename = "03_figures/fig08b_topography_elevation_natural_roads_rails.png",
       plot = plot_topography_elevation_natural_roads_rails,
       wi = 30, he = 20, un = "cm", dpi = 300)

### slope ----
plot_topography_slope_forest <- data_topography_slope %>%
  dplyr::filter(vegetation == "forest" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining forest ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = class, y = per_total, fill = remain, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  # geom_bar(stat = "identity", position = position_dodge()) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  scale_fill_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  labs(title = "Forest vegetation (not trimmed by roads and rails)",
       x = "Slope range (°)", y = "Percentage (%)", color = "", fill = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 63, label = "c", size = 10, fontface = "bold") +
  ylim(0, 65) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .3),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_slope_forest
ggsave(filename = "03_figures/fig08c_topography_slope_forest.png",
       plot = plot_topography_slope_forest,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_slope_forest_roads_rails <- data_topography_slope %>%
  dplyr::filter(vegetation == "forest_roads_rails" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining forest_roads_rails ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = class, y = per_total, fill = remain, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  # geom_bar(stat = "identity", position = position_dodge()) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  scale_fill_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  labs(title = "Forest vegetation (trimmed by roads and rails)",
       x = "Slope range (°)", y = "Percentage (%)", color = "", fill = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 63, label = "d", size = 10, fontface = "bold") +
  ylim(0, 65) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .3),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_slope_forest_roads_rails
ggsave(filename = "03_figures/fig08d_topography_slope_forest_roads_rails.png",
       plot = plot_topography_slope_forest_roads_rails,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_slope_natural <- data_topography_slope %>%
  dplyr::filter(vegetation == "natural" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining natural ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(title = "Natural vegetation (not trimmed by roads and rails)",
       x = "Slope range (°)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 63, label = "e", size = 10, fontface = "bold") +
  ylim(0, 65) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .3),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_slope_natural
ggsave(filename = "03_figures/fig08e_topography_slope_natural.png",
       plot = plot_topography_slope_natural,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_slope_natural_roads_rails <- data_topography_slope %>%
  dplyr::filter(vegetation == "natural_roads_rails" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining natural_roads_rails ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(title = "Natural vegetation (trimmed by roads and rails)",
       x = "Slope range (°)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 63, label = "f", size = 10, fontface = "bold") +
  ylim(0, 65) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .3),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_slope_natural_roads_rails
ggsave(filename = "03_figures/fig08f_topography_slope_natural_roads_rails.png",
       plot = plot_topography_slope_natural_roads_rails,
       wi = 30, he = 20, un = "cm", dpi = 300)


### aspect ----

# # test
# asp_original_degree <- asp_original[asp_original$value != 0, ]
# asp_original_degree <- rep(asp_original_degree$value, asp_original_degree$n)
# asp_original_degree
#
# asp_original_rad <- NISTunits::NISTdegTOradian(asp_original_degree)
# asp_original_rad
#
# set.seed(42)
# asp_original_rad_sample <- sample(asp_original_rad, 1e6)
# asp_original_rad_sample
#
# asp_original_rad_sample_raytest <- CircStats::r.test(asp_original_rad_sample)
# asp_original_rad_sample_raytest
#
# asp_original_rad_sample_circmean <- CircStats::circ.mean(asp_original_rad_sample)
# asp_original_rad_sample_circmean
#
# asp_original_rad_sample_circmean_deg <- NISTunits::NISTradianTOdeg(asp_original_rad_sample_circmean)
# asp_original_rad_sample_circmean_deg
#
# deg <- data.frame(deg = NISTunits::NISTradianTOdeg(asp_original_rad_sample))
# deg
#
# p <- deg %>%
#   ggplot(aes(x = deg, y = (..count..)/sum(..count..))) +
#   geom_histogram(breaks = seq(0, 360, 15), colour = "black", fill = "gray", alpha = .2) +
#   coord_polar(start = 0) +
#   scale_fill_brewer(type = "seq", palette = 3) +
#   scale_x_continuous("", limits = c(0, 360), breaks = seq(0, 360, 45),
#                      labels = c("N", "NW", "W", "SW", "S", "SE", "E", "NE", "N")) +
#   scale_y_continuous(labels = scales::percent) +
#   theme_minimal(base_size = 20) +
#   labs(y = "")
# p
#
# l <- ggplot() +
#   annotate("segment", x = 0, xend = -60,
#            y = c(0:4), yend = c(0:4),
#            linetype = "dashed") +
#   theme_void()
# p + inset_element(l, left = 0, top = 0.88, bottom = 0.48, right = 0.52)

# forest
# asp_forest_degree <- readr::read_csv("02_results/06_mapbiomas_brazil_af_trinacional_2021_af_lim_forest_aspect.csv",
#                                      col_names = c("forest", "aspect", "area", "n")) %>%
#   dplyr::filter(forest == 1, aspect != 0)
# asp_forest_degree
# asp_forest_degree <- rep(asp_forest_degree$aspect, asp_forest_degree$n)
# asp_forest_degree
#
# asp_forest_rad <- NISTunits::NISTdegTOradian(asp_forest_degree)
# asp_forest_rad
#
# set.seed(42)
# asp_forest_rad_sample <- sample(asp_forest_rad, 1e6)
# asp_forest_rad_sample
#
# asp_forest_rad_sample_raytest <- CircStats::r.test(asp_forest_rad_sample)
# asp_forest_rad_sample_raytest
#
# asp_forest_rad_sample_circmean <- CircStats::circ.mean(asp_forest_rad_sample)
# asp_forest_rad_sample_circmean
#
# data <- runif(50, 0, pi)
# mean.dir <- circ.mean(data)
# mean.dir
#
# asp_forest_rad_sample_circmean_deg <- NISTunits::NISTradianTOdeg(asp_forest_rad_sample_circmean)
# asp_forest_rad_sample_circmean_deg
#
#
# deg <- data.frame(deg = NISTunits::NISTradianTOdeg(asp_forest_rad_sample))
# deg
#
# p <- deg %>%
#   ggplot(aes(x = deg, y = (..count..)/sum(..count..))) +
#   geom_histogram(breaks = seq(0, 360, 15), colour = "black", fill = "gray", alpha = .2) +
#   coord_polar(start = 0) +
#   scale_fill_brewer(type = "seq", palette = 3) +
#   scale_x_continuous("", limits = c(0, 360), breaks = seq(0, 360, 45),
#                      labels = c("N", "NW", "W", "SW", "S", "SE", "E", "NE", "N")) +
#   scale_y_continuous(labels = scales::percent) +
#   theme_minimal(base_size = 20) +
#   labs(y = "")
# p
#
# l <- ggplot() +
#   annotate("segment", x = 0, xend = -60,
#            y = c(0:5), yend = c(0:5),
#            linetype = "dashed") +
#   theme_void()
# p + inset_element(l, left = 0, top = 0.90, bottom = 0.48, right = 0.52)

data_topography_aspect %>%
  dplyr::ungroup() %>%
  dplyr::filter(vegetation == "forest") %>%
  dplyr::select(class, per_total) %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(class_mean = mean(per_total))

data_topography_aspect %>%
  dplyr::ungroup() %>%
  dplyr::filter(vegetation == "forest",
                class %in% c("W", "S"),
                remain %in% c("Remaining forest (1985)", "Remaining forest (2021)")) %>%
  arrange(class)

data_topography_aspect_natural <- data_topography_aspect %>%
  dplyr::ungroup() %>%
  dplyr::filter(vegetation == "natural",
                class %in% c("W", "S", "SE")) %>%
  dplyr::select(class, per_total) %>%
  dplyr::arrange(class)
data_topography_aspect_natural

data_topography_aspect %>%
  dplyr::ungroup() %>%
  dplyr::filter(vegetation == "forest",
                class %in% c("W", "S"),
                remain %in% c("Remaining forest (1985)", "Remaining forest (2021)")) %>%
  arrange(class)

png(filename = "03_figures/fig08g_topography_aspect_forest.png", wi = 53, he = 30, units = "cm", res = 300)
par(mar = c(1, 1, 1, 1))
data_topography_aspect %>%
  dplyr::ungroup() %>%
  dplyr::filter(vegetation == "forest") %>%
  dplyr::select(remain, class, per_total) %>%
  tidyr::pivot_wider(names_from = class, values_from = per_total) %>%
  dplyr::select(remain, N, NW, W, SW, S, SE, E, NE) %>%
  dplyr::bind_rows(., asp) %>%
  dplyr::arrange(remain) %>%
  tibble::column_to_rownames(var = "remain") %>%
  fmsb::radarchart(df = .,
                   axistype = 1,
                   seg = 4,
                   pty = NA,
                   pcol = c(NA, paletteer::paletteer_c("ggthemes::Green-Gold", 8)),
                   pfcol = c(adjustcolor("gray70", .3), rep(NA, 8)),
                   plty = c(1, 1),
                   plwd = 3,
                   cglcol = "gray30",
                   cglty = 1,
                   axislabcol = "black",
                   caxislabels = paste0(seq(0, 16, 4), "%"),
                   cglwd = .8,
                   vlcex = 3,
                   calcex = 2,
                   centerzero = TRUE)
text(1.75, .5, "Forest vegetation\n(not trimmed by \nroads and rails)", cex = 3.5)
text(-1, 1.15, substitute(paste(bold("g"))), cex = 4)
legend(x = 1.3, y = .2, col = "gray70", bty = "n", pch = 15,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1,
       legend = "Original aspect")
legend(x = 1.3, y = .1, col = paletteer::paletteer_c("ggthemes::Green-Gold", 8), bty = "n", pch = 22,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1, y.intersp = 1.3,
       legend = data_topography_aspect %>%
         dplyr::ungroup() %>%
         dplyr::filter(vegetation == "forest") %>%
         dplyr::select(remain) %>%
         dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining forest ", "")) %>%
         dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
         dplyr::pull(remain) %>%
         unique())
dev.off()

png(filename = "03_figures/fig08h_topography_aspect_forest_roads_rails.png", wi = 53, he = 30, units = "cm", res = 300)
par(mar = c(1, 1, 1, 1))
data_topography_aspect %>%
  dplyr::ungroup() %>%
  dplyr::filter(vegetation == "forest_roads_rails") %>%
  dplyr::select(remain, class, per_total) %>%
  tidyr::pivot_wider(names_from = class, values_from = per_total) %>%
  dplyr::select(remain, N, NW, W, SW, S, SE, E, NE) %>%
  dplyr::bind_rows(., asp) %>%
  dplyr::arrange(remain) %>%
  tibble::column_to_rownames(var = "remain") %>%
  fmsb::radarchart(df = .,
                   axistype = 1,
                   seg = 4,
                   pty = NA,
                   pcol = c(NA, paletteer::paletteer_c("ggthemes::Green-Gold", 8)),
                   pfcol = c(adjustcolor("gray70", .3), rep(NA, 8)),
                   plty = c(1, 1),
                   plwd = 3,
                   cglcol = "gray30",
                   cglty = 1,
                   axislabcol = "black",
                   caxislabels = paste0(seq(0, 16, 4), "%"),
                   cglwd = .8,
                   vlcex = 3,
                   calcex = 2,
                   centerzero = TRUE)
text(1.75, .5, "Forest vegetation\n(trimmed by \nroads and rails)", cex = 3.5)
text(-1, 1.15, substitute(paste(bold("h"))), cex = 4)
legend(x = 1.3, y = .2, col = "gray70", bty = "n", pch = 15,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1,
       legend = "Original aspect")
legend(x = 1.3, y = .1, col = paletteer::paletteer_c("ggthemes::Green-Gold", 8), bty = "n", pch = 22,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1, y.intersp = 1.3,
       legend = data_topography_aspect %>%
         dplyr::ungroup() %>%
         dplyr::filter(vegetation == "forest") %>%
         dplyr::select(remain) %>%
         dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining forest ", "")) %>%
         dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
         dplyr::pull(remain) %>%
         unique())
dev.off()

png(filename = "03_figures/fig08i_topography_aspect_natural.png", wi = 53, he = 30, units = "cm", res = 300)
par(mar = c(1, 1, 1, 1))
data_topography_aspect %>%
  dplyr::ungroup() %>%
  dplyr::filter(vegetation == "natural") %>%
  dplyr::select(remain, class, per_total) %>%
  tidyr::pivot_wider(names_from = class, values_from = per_total) %>%
  dplyr::select(remain, N, NW, W, SW, S, SE, E, NE) %>%
  dplyr::bind_rows(., asp) %>%
  dplyr::arrange(remain) %>%
  tibble::column_to_rownames(var = "remain") %>%
  fmsb::radarchart(df = .,
                   axistype = 1,
                   seg = 4,
                   pty = NA,
                   pcol = c(NA, paletteer::paletteer_c("ggthemes::Orange-Gold", 8)),
                   pfcol = c(adjustcolor("gray70", .3), rep(NA, 8)),
                   plty = c(1, 1),
                   plwd = 3,
                   cglcol = "gray30",
                   cglty = 1,
                   axislabcol = "black",
                   caxislabels = paste0(seq(0, 16, 4), "%"),
                   cglwd = .8,
                   vlcex = 3,
                   calcex = 2,
                   centerzero = TRUE)
text(1.75, .5, "Natural vegetation\n(not trimmed by \nroads and rails)", cex = 3.5)
text(-1, 1.15, substitute(paste(bold("i"))), cex = 4)
legend(x = 1.3, y = .2, col = "gray70", bty = "n", pch = 15,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1,
       legend = "Original aspect")
legend(x = 1.3, y = .1, col = paletteer::paletteer_c("ggthemes::Orange-Gold", 8), bty = "n", pch = 22,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1, y.intersp = 1.3,
       legend = data_topography_aspect %>%
         dplyr::ungroup() %>%
         dplyr::filter(vegetation == "forest") %>%
         dplyr::select(remain) %>%
         dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining forest ", "")) %>%
         dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
         dplyr::pull(remain) %>%
         unique())
dev.off()

png(filename = "03_figures/fig08j_topography_aspect_natural_roads_rails.png", wi = 53, he = 30, units = "cm", res = 300)
par(mar = c(1, 1, 1, 1))
data_topography_aspect %>%
  dplyr::ungroup() %>%
  dplyr::filter(vegetation == "natural_roads_rails") %>%
  dplyr::select(remain, class, per_total) %>%
  tidyr::pivot_wider(names_from = class, values_from = per_total) %>%
  dplyr::select(remain, N, NW, W, SW, S, SE, E, NE) %>%
  dplyr::bind_rows(., asp) %>%
  dplyr::arrange(remain) %>%
  tibble::column_to_rownames(var = "remain") %>%
  fmsb::radarchart(df = .,
                   axistype = 1,
                   seg = 4,
                   pty = NA,
                   pcol = c(NA, paletteer::paletteer_c("ggthemes::Orange-Gold", 8)),
                   pfcol = c(adjustcolor("gray70", .3), rep(NA, 8)),
                   plty = c(1, 1),
                   plwd = 3,
                   cglcol = "gray30",
                   cglty = 1,
                   axislabcol = "black",
                   caxislabels = paste0(seq(0, 16, 4), "%"),
                   cglwd = .8,
                   vlcex = 3,
                   calcex = 2,
                   centerzero = TRUE)
text(1.75, .5, "Natural vegetation\n(trimmed by \nroads and rails)", cex = 3.5)
text(-1, 1.15, substitute(paste(bold("j"))), cex = 4)
legend(x = 1.3, y = .2, col = "gray70", bty = "n", pch = 15,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1,
       legend = "Original aspect")
legend(x = 1.3, y = .1, col = paletteer::paletteer_c("ggthemes::Orange-Gold", 8), bty = "n", pch = 22,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1, y.intersp = 1.3,
       legend = data_topography_aspect %>%
         dplyr::ungroup() %>%
         dplyr::filter(vegetation == "forest") %>%
         dplyr::select(remain) %>%
         dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining forest ", "")) %>%
         dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
         dplyr::pull(remain) %>%
         unique())
dev.off()

### pcurvature ----
plot_topography_pcurvature_forest <- data_topography_pcurvature %>%
  dplyr::filter(vegetation == "forest" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining forest ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  labs(title = "Forest vegetation (not trimmed by roads and rails)",
       x = "Profile curvature range (m¹)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 50, label = "k", size = 10, fontface = "bold") +
  ylim(0, 55) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .37),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_pcurvature_forest
ggsave(filename = "03_figures/fig08k_topography_pcurvature_forest.png",
       plot = plot_topography_pcurvature_forest,
       wi = 30, he = 20, un = "cm", dpi = 300)


plot_topography_pcurvature_forest_roads_rails <- data_topography_pcurvature %>%
  dplyr::filter(vegetation == "forest_roads_rails" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining forest_roads_rails ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  labs(title = "Forest vegetation (trimmed by roads and rails)",
       x = "Profile curvature range (m¹)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 50, label = "l", size = 10, fontface = "bold") +
  ylim(0, 55) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .37),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_pcurvature_forest_roads_rails
ggsave(filename = "03_figures/fig08l_topography_pcurvature_forest_roads_rails.png",
       plot = plot_topography_pcurvature_forest_roads_rails,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_pcurvature_natural <- data_topography_pcurvature %>%
  dplyr::filter(vegetation == "natural" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining natural ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(title = "Natural vegetation (not trimmed by roads and rails)",
       x = "Profile curvature range (m¹)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 50, label = "m", size = 10, fontface = "bold") +
  ylim(0, 55) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .45),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_pcurvature_natural
ggsave(filename = "03_figures/fig08m_topography_pcurvature_natural.png",
       plot = plot_topography_pcurvature_natural,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_pcurvature_natural_roads_rails <- data_topography_pcurvature %>%
  dplyr::filter(vegetation == "natural_roads_rails" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining natural_roads_rails ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(title = "Natural vegetation (trimmed by roads and rails)",
       x = "Profile curvature range (m¹)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 50, label = "n", size = 10, fontface = "bold") +
  ylim(0, 55) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .45),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_pcurvature_natural_roads_rails
ggsave(filename = "03_figures/fig08n_topography_pcurvature_natural_roads_rails.png",
       plot = plot_topography_pcurvature_natural_roads_rails,
       wi = 30, he = 20, un = "cm", dpi = 300)

### tcurvature ----
plot_topography_tcurvature_forest <- data_topography_tcurvature %>%
  dplyr::filter(vegetation == "forest" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining forest ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  labs(title = "Forest vegetation (not trimmed by roads and rails)",
       x = "Tangential curvature range (m¹)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 50, label = "o", size = 10, fontface = "bold") +
  ylim(0, 50) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .41),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_tcurvature_forest
ggsave(filename = "03_figures/fig08o_topography_tcurvature_forest.png",
       plot = plot_topography_tcurvature_forest,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_tcurvature_forest_roads_rails <- data_topography_tcurvature %>%
  dplyr::filter(vegetation == "forest_roads_rails" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining forest_roads_rails ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  labs(title = "Forest vegetation (trimmed by roads and rails)",
       x = "Tangential curvature range (m¹)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 50, label = "p", size = 10, fontface = "bold") +
  ylim(0, 50) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .41),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_tcurvature_forest_roads_rails
ggsave(filename = "03_figures/fig08p_topography_tcurvature_forest_roads_rails.png",
       plot = plot_topography_tcurvature_forest_roads_rails,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_tcurvature_natural <- data_topography_tcurvature %>%
  dplyr::filter(vegetation == "natural" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining natural ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(title = "Natural vegetation (not trimmed by roads and rails)",
       x = "Tangential curvature range (m)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 60, label = "q", size = 10, fontface = "bold") +
  ylim(0, 60) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .41),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_tcurvature_natural
ggsave(filename = "03_figures/fig08q_topography_tcurvature_natural.png",
       plot = plot_topography_tcurvature_natural,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_tcurvature_natural_roads_rails <- data_topography_tcurvature %>%
  dplyr::filter(vegetation == "natural_roads_rails" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining natural_roads_rails ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(title = "Natural vegetation (trimmed by roads and rails)",
       x = "Tangential curvature range (m)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 60, label = "r", size = 10, fontface = "bold") +
  ylim(0, 60) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .41),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_tcurvature_natural_roads_rails
ggsave(filename = "03_figures/fig08r_topography_tcurvature_natural_roads_rails.png",
       plot = plot_topography_tcurvature_natural_roads_rails,
       wi = 30, he = 20, un = "cm", dpi = 300)

### geomorph ----
plot_topography_geomorph_forest <- data_topography_geomorph %>%
  dplyr::filter(vegetation == "forest" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining forest ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = geo, y = per_total, fill = remain)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("gray30", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  labs(title = "Forest vegetation (not trimmed by roads and rails)",
       x = "Geomorphons", y = "Percentage (%)", fill = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 60, label = "s", size = 12, fontface = "bold") +
  ylim(0, 60) +
  theme_bw(base_size = 30) +
  theme(legend.position = c(.9, .83),
        legend.text = element_text(size = 13),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_geomorph_forest
ggsave(filename = "03_figures/fig08s_topography_geomorphons_forest.png",
       plot = plot_topography_geomorph_forest,
       wi = 40, he = 25, un = "cm", dpi = 300)


plot_topography_geomorph_forest_roads_rails <- data_topography_geomorph %>%
  dplyr::filter(vegetation == "forest_roads_rails" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining forest_roads_rails ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = geo, y = per_total, fill = remain)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("gray30", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  labs(title = "Forest vegetation (trimmed by roads and rails)",
       x = "Geomorphons", y = "Percentage (%)", fill = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 60, label = "a", size = 12, fontface = "bold") +
  ylim(0, 60) +
  theme_bw(base_size = 30) +
  theme(legend.position = c(.9, .83),
        legend.text = element_text(size = 13),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_geomorph_forest_roads_rails
ggsave(filename = "03_figures/fig08a_topography_geomorphons_forest_roads_rails.png",
       plot = plot_topography_geomorph_forest_roads_rails,
       wi = 40, he = 25, un = "cm", dpi = 300)

plot_topography_geomorph_natural <- data_topography_geomorph %>%
  dplyr::filter(vegetation == "natural" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining natural ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = geo, y = per_total, fill = remain)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("gray30", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(x = "Geomorphons", y = "Percentage (%)", fill = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 60, label = "t", size = 12, fontface = "bold") +
  ylim(0, 60) +
  theme_bw(base_size = 30) +
  theme(legend.position = c(.9, .83),
        legend.text = element_text(size = 13),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_geomorph_natural
ggsave(filename = "03_figures/fig08t_topography_geomorph_natural.png",
       plot = plot_topography_geomorph_natural,
       wi = 40, he = 25, un = "cm", dpi = 300)

plot_topography_geomorph_natural_roads_rails <- data_topography_geomorph %>%
  dplyr::filter(vegetation == "natural_roads_rails" | is.na(vegetation)) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "Remaining natural_roads_rails ", "")) %>%
  dplyr::mutate(remain = stringr::str_replace_all(remain, "[()]", "")) %>%
  dplyr::mutate(remain = forcats::fct_relevel(remain, c("Original distribution", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2021"))) %>%
  ggplot(aes(x = geo, y = per_total, fill = remain)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("gray30", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(title = "Natural vegetation (trimmed by roads and rails)",
       x = "Geomorphons", y = "Percentage (%)", fill = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 60, label = "b", size = 12, fontface = "bold") +
  ylim(0, 60) +
  theme_bw(base_size = 30) +
  theme(legend.position = c(.9, .83),
        legend.text = element_text(size = 13),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_geomorph_natural_roads_rails
ggsave(filename = "03_figures/fig08b_topography_geomorph_natural_roads_rails.png",
       plot = plot_topography_geomorph_natural_roads_rails,
       wi = 40, he = 25, un = "cm", dpi = 300)


# 7 protected area cover and proximity ------------------------------------

## forest and vegetation ----

# import data
data_protected_area_forest <- readr::read_csv("02_results/07_mapbiomas_brazil_af_trinacional_2021_af_lim_forest_protected_areas.csv",
                                              col_names = c("forest", "dist_pa", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(area_ha = area/1e4)
data_protected_area_forest

data_protected_area_forest

data_protected_area_forest_sum <- data_protected_area_forest %>%
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
  dplyr::mutate(n_total_per = round(n_total/sum(n_total)*100, 1))
data_protected_area_forest_sum


# plot
plot_protected_area_forest <- ggplot(data = data_protected_area_forest_sum,
                                     aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#129912", rep("#8fba8f", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")),
            nudge_y = 3e5, size = 7) +
  annotate(geom = "text", x = 1, y = 1.05e7, label = "a", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (not trimmed by roads and rails)",
       x = "Protected area distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15))
plot_protected_area_forest
ggsave(filename = "03_figures/fig07a_protected_area_forest.png", plot = plot_protected_area_forest,
       wi = 35, he = 25, un = "cm", dpi = 300)


# import data
data_protected_area_forest_road <- readr::read_csv("02_results/07_mapbiomas_brazil_af_trinacional_2021_af_lim_forest_roads_rails_protected_areas.csv",
                                                   col_names = c("forest", "dist_pa", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(area_ha = area/1e4)
data_protected_area_forest

data_protected_area_forest_road_sum <- data_protected_area_forest_road %>%
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
  dplyr::mutate(n_total_per = round(n_total/sum(n_total)*100, 1))
data_protected_area_forest_road_sum

# plot
plot_protected_area_forest_road <- ggplot(data = data_protected_area_forest_road_sum,
                                          aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#129912", rep("#8fba8f", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")),
            nudge_y = 3e5, size = 7) +
  annotate(geom = "text", x = 1, y = 1.05e7, label = "a", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (trimmed by roads and rails)",
       x = "Protected area distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15))
plot_protected_area_forest_road
ggsave(filename = "03_figures/fig07b_protected_area_forest_road.png", plot = plot_protected_area_forest_road,
       wi = 35, he = 25, un = "cm", dpi = 300)


# import data
data_protected_area_natural <- readr::read_csv("02_results/07_mapbiomas_brazil_af_trinacional_2021_af_lim_natural_protected_areas.csv",
                                               col_names = c("natural", "dist_pa", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(area_ha = area/1e4)
data_protected_area_natural

data_protected_area_natural_sum <- data_protected_area_natural %>%
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
  dplyr::mutate(n_total_per = round(n_total/sum(n_total)*100, 1))
data_protected_area_natural_sum

# plot
plot_protected_area_natural <- ggplot(data = data_protected_area_natural_sum,
                                      aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#ff8800", rep("#fec882", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")), nudge_y = 3e5, size = 7) +
  annotate(geom = "text", x = 1, y = 1.5e7, label = "b", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (not trimmed by roads and rails)",
       x = "Protected area distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15))
plot_protected_area_natural
ggsave(filename = "03_figures/fig07c_protected_area_natural.png", plot = plot_protected_area_natural,
       wi = 35, he = 25, un = "cm", dpi = 300)


# import data
data_protected_area_natural_road <- readr::read_csv("02_results/07_mapbiomas_brazil_af_trinacional_2021_af_lim_natural_roads_rails_protected_areas.csv",
                                                    col_names = c("natural", "dist_pa", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(area_ha = area/1e4)
data_protected_area_natural

data_protected_area_natural_road_sum <- data_protected_area_natural_road %>%
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
  dplyr::mutate(n_total_per = round(n_total/sum(n_total)*100, 1))
data_protected_area_natural_road_sum

# plot
plot_protected_area_natural_road <- ggplot(data = data_protected_area_natural_road_sum,
                                           aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#ff8800", rep("#fec882", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")),
            nudge_y = 3e5, size = 7) +
  annotate(geom = "text", x = 1, y = 1.5e7, label = "b", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (trimmed by roads and rails)",
       x = "Protected area distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15))
plot_protected_area_natural_road
ggsave(filename = "03_figures/fig07d_protected_area_natural_road.png", plot = plot_protected_area_natural_road,
       wi = 35, he = 25, un = "cm", dpi = 300)

# data
data_protected_area_sum <- data_protected_area_forest_sum %>%
  dplyr::select(1, 3, 4) %>%
  dplyr::rename(forest_area_ha = 2, forest_percentage = 3) %>%
  dplyr::bind_cols(data_protected_area_forest_road_sum[, 3:4] %>%
                     dplyr::rename(forest_roads_rails_area_ha = 1, forest_roads_rails_percentage = 2)) %>%
  dplyr::bind_cols(data_protected_area_natural_sum[, 3:4] %>%
                     dplyr::rename(natural_area_ha = 1, natural_percentage = 2)) %>%
  dplyr::bind_cols(data_protected_area_natural_road_sum[, 3:4] %>%
                     dplyr::rename(natural_roads_rails_area_ha = 1, natural_roads_rails_percentage = 2))
data_protected_area_sum

readr::write_csv(data_protected_area_sum, "02_results/07_data_protected_area_sum.csv")

## classes ----

# classes
classes_habitat_cover <- tibble::tibble(
  values = c(0, 3, 4, 5, 11, 12, 13, 32, 49, 50),
  classes = c("Matrix", "Forest formation", "Savanna formation", "Mangroove",
              "Wetland", "Grassland", "Other non forest formations",
              "Salt flat", "Wooded sandbank vegetation",
              "Herbaceous sandbank vegetation"))
classes_habitat_cover

# forest
data_protected_area_forest_classes <- readr::read_csv("02_results/07_mapbiomas_brazil_af_trinacional_2021_af_lim_forest_classes_protected_areas.csv",
                                                      col_names = c("values", "pa", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(area_ha = area/1e4,
                per_total = round(area_ha/sum(area_ha) * 100, 2)) %>%
  dplyr::select(1, 2, 5, 6)
data_protected_area_forest_classes

data_protected_area_forest_classes_per <- data_protected_area_forest_classes %>%
  dplyr::group_by(values) %>%
  dplyr::summarise(area_ha_class_sum = sum(area_ha))
data_protected_area_forest_classes_per

data_protected_area_forest_classes_per_total <- data_protected_area_forest_classes %>%
  dplyr::left_join(data_protected_area_forest_classes_per) %>%
  dplyr::left_join(classes_habitat_cover) %>%
  dplyr::mutate(per_class = round(area_ha/area_ha_class_sum * 100, 2),
                year = 2021, vegetation = "Forest") %>%
  dplyr::filter(values > 0, pa == 1) %>%
  dplyr::select(year, vegetation, classes, values, area_ha, area_ha_class_sum, per_class, per_total)
data_protected_area_forest_classes_per_total


# forest roads rails
data_protected_area_forest_classes_roads <- readr::read_csv("02_results/07_mapbiomas_brazil_af_trinacional_2021_af_lim_forest_classes_roads_rails_protected_areas.csv",
                                                            col_names = c("values", "pa", "area", "n"), col_types = readr::cols()) %>%
  dplyr::mutate(area_ha = area/1e4,
                per_total = round(area_ha/sum(area_ha) * 100, 2)) %>%
  dplyr::select(1, 2, 5, 6)
data_protected_area_forest_classes_roads

data_protected_area_forest_classes_roads_per <- data_protected_area_forest_classes_roads %>%
  dplyr::group_by(values) %>%
  dplyr::summarise(area_ha_class_sum = sum(area_ha))
data_protected_area_forest_classes_roads

data_protected_area_forest_classes_roads_per_total <- data_protected_area_forest_classes_roads %>%
  dplyr::left_join(data_protected_area_forest_classes_roads_per) %>%
  dplyr::left_join(classes_habitat_cover) %>%
  dplyr::mutate(per_class = round(area_ha/area_ha_class_sum * 100, 2),
                year = 2021, vegetation = "Forest Roads Rails") %>%
  dplyr::filter(values > 0, pa == 1) %>%
  dplyr::select(year, vegetation, classes, values, area_ha, area_ha_class_sum, per_class, per_total)
data_protected_area_forest_classes_roads_per_total

# natural
data_protected_area_natural_classes <- readr::read_csv("02_results/07_mapbiomas_brazil_af_trinacional_2021_af_lim_natural_classes_protected_areas.csv",
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
                roads_rails = "without") %>%
  dplyr::filter(values > 0, pa == 1) %>%
  dplyr::select(roads_rails, classes, values, area_ha, area_ha_class_sum, per_protec, per_class, per_total)
data_protected_area_natural_classes_per_total

# natural roads rails
data_protected_area_natural_classes_roads <- readr::read_csv("02_results/07_mapbiomas_brazil_af_trinacional_2021_af_lim_natural_classes_roads_rails_protected_areas.csv",
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
                roads_rails = "with") %>%
  dplyr::filter(values > 0, pa == 1) %>%
  dplyr::select(roads_rails, classes, values, area_ha, area_ha_class_sum, per_protec, per_class, per_total)
data_protected_area_natural_classes_roads_per_total

# bind
data_protected_area_classes <- data_protected_area_natural_classes_per_total %>%
  dplyr::bind_rows(data_protected_area_natural_classes_roads_per_total)
data_protected_area_classes

readr::write_csv(data_protected_area_classes, "02_results/07_data_protected_area_classes.csv")

# plot
plot_protected_area_classes <- data_protected_area_natural_classes_per_total %>%
  dplyr::mutate(classes = forcats::as_factor(classes)) %>%
  dplyr::mutate(classes = reorder(classes, -area_ha_class_sum)) %>%
  ggplot(aes(x = classes, y = log10(area_ha_class_sum), fill = classes)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(per_protec, "%PA / ", per_class, "%CA")),
            nudge_y = max(log10(data_protected_area_natural_classes_roads_per_total$area_ha_class_sum)) * .16,
            size = 6) +
  scale_fill_manual(
    values = c("#006400", "#00ff00", "#b8af4f", "#45c2a5", "#6b9932",
                        "#66ffcc", "#687537", "#bdb76b", "#968c46")
  ) +
  coord_flip() +
  scale_y_continuous(breaks = c(0:10),
                     labels = c("0", "1", "10¹", "10²", "10³", "10⁴", "10⁵",
                                "10⁶", "10⁷", "10⁸", "10⁹"),
                     limits = c(0, 10),
                     expand = c(.01, .01)) +
  labs(x = "", y = "Area log10(ha)", title = "Not trimmed by roads and rails") +
  theme_bw(base_size = 25) +
  theme(title = element_text(size = 25),
        legend.position = "none")
plot_protected_area_classes
ggsave(filename = "03_figures/fig07_plot_protected_area_classes.png",
       plot = plot_protected_area_classes,
       wi = 45, he = 25, un = "cm", dpi = 300)


plot_protected_area_classes_roads <- data_protected_area_natural_classes_roads_per_total %>%
  dplyr::mutate(classes = forcats::as_factor(classes)) %>%
  dplyr::mutate(classes = reorder(classes, -area_ha_class_sum)) %>%
  ggplot(aes(x = classes, y = log10(area_ha_class_sum), fill = classes)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(per_protec, "%PA / ", per_class, "%CA")),
            nudge_y = max(log10(data_protected_area_natural_classes_roads_per_total$area_ha_class_sum)) * .16,
            size = 6) +
  scale_fill_manual(
    values = c("#006400", "#00ff00", "#b8af4f", "#45c2a5", "#6b9932",
                        "#66ffcc", "#687537", "#bdb76b", "#968c46")
  ) +
  coord_flip() +
  scale_y_continuous(breaks = c(0:10),
                     labels = c("0", "1", "10¹", "10²", "10³", "10⁴", "10⁵",
                                "10⁶", "10⁷", "10⁸", "10⁹"),
                     limits = c(0, 10),
                     expand = c(.01, .01)) +
  labs(x = "", y = "Area log10(ha)", title = "Trimmed by roads and rails",) +
  theme_bw(base_size = 25) +
  theme(legend.position = "none")
plot_protected_area_classes_roads
ggsave(filename = "03_figures/fig07_plot_protected_area_classes_roads_rails.png",
       plot = plot_protected_area_classes_roads,
       wi = 45, he = 25, un = "cm", dpi = 300)

## import vegetation data ----
# list files
files_topography_protected <- dir(path = "02_results", pattern = "07", full.names = TRUE) %>%
  stringr::str_subset("areas.csv", negate = TRUE)
files_topography_protected

# import vegetation data
files_topography_protected_elevation <- NULL
files_topography_protected_slope <- NULL
files_topography_protected_aspect <- NULL
files_topography_protected_pcurvature <- NULL
files_topography_protected_tcurvature <- NULL

for(i in c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2021)){}

print(i)

files_topography_i <- stringr::str_subset(files_topography_protected, paste0("_", i, "_"))
data_topography_elevation_i <- NULL
data_topography_slope_i <- NULL
data_topography_aspect_i <- NULL
data_topography_pcurvature_i <- NULL
data_topography_tcurvature_i <- NULL

for(j in c("forest", "natural")){

  # elevation
  data_topography_elevation_partial <- stringr::str_subset(files_topography_i, paste0(j, "_roads_rails_elevation")) %>%
    readr::read_csv(col_names = c("forest", "value", "area", "n")) %>%
    dplyr::mutate(class = case_when(
      value <= 100 ~ "≤100",
      value > 100 & value <= 200 ~ ">100-\n200",
      value > 200 & value <= 400 ~ ">200-\n400",
      value > 400 & value <= 800 ~ ">400-\n800",
      value > 800 & value <= 1200 ~ ">800-\n1200",
      value > 1200 & value <= 1600 ~ ">1200-\n1600",
      value > 1600 ~ ">1600")) %>%
    dplyr::group_by(forest, class) %>%
    dplyr::summarise(n_sum = sum(n)) %>%
    dplyr::mutate(class = forcats::as_factor(class)) %>%
    dplyr::mutate(class = forcats::fct_relevel(class,
                                               c("≤100", ">100-\n200", ">200-\n400",
                                                 ">400-\n800", ">800-\n1200",
                                                 ">1200-\n1600", ">1600"))) %>%
    dplyr::arrange(class) %>%
    dplyr::mutate(vegetation = j, topography = "elevation", year = i, .before = 1)
  data_topography_elevation_partial

  data_topography_elevation_total <- data_topography_elevation_partial %>%
    dplyr::group_by(class) %>%
    dplyr::summarise(original_distribution = sum(n_sum)) %>%
    dplyr::pull(original_distribution)
  data_topography_elevation_total

  data_topography_elevation_j <- data_topography_elevation_partial %>%
    dplyr::filter(forest == 1) %>%
    dplyr::mutate(n_total = data_topography_elevation_total,
                  per_total = n_sum/data_topography_elevation_total * 100,
                  remain = paste0("Remaining ", j, " (", i, ")"))
  data_topography_elevation_j

  data_topography_elevation_i <- dplyr::bind_rows(data_topography_elevation_i, data_topography_elevation_j)


  # slope
  data_topography_slope_partial <- stringr::str_subset(files_topography_i, paste0(j, "_roads_rails_slope")) %>%
    readr::read_csv(col_names = c("forest", "value", "area", "n")) %>%
    dplyr::mutate(class = case_when(
      value <= 5 ~ "≤5°",
      value > 5 & value <= 10 ~ ">5°-10°",
      value > 10 & value <= 15 ~ ">10°-15°",
      value > 15 & value <= 20 ~ ">15°-20°",
      value > 20 & value <= 25 ~ ">20°-25°",
      value > 25 ~ ">25°")) %>%
    dplyr::group_by(forest, class) %>%
    dplyr::summarise(n_sum = sum(n)) %>%
    dplyr::mutate(class = forcats::as_factor(class)) %>%
    dplyr::mutate(class = forcats::fct_relevel(class,
                                               c("≤5°", ">5°-10°",
                                                 ">10°-15°", ">15°-20°",
                                                 ">20°-25°", ">25°"))) %>%
    dplyr::arrange(class) %>%
    dplyr::mutate(vegetation = j, topography = "slope", year = i, .before = 1)
  data_topography_slope_partial

  data_topography_slope_total <- data_topography_slope_partial %>%
    dplyr::group_by(class) %>%
    dplyr::summarise(original_distribution = sum(n_sum)) %>%
    dplyr::pull(original_distribution)
  data_topography_slope_total

  data_topography_slope_j <- data_topography_slope_partial %>%
    dplyr::filter(forest == 1) %>%
    dplyr::mutate(n_total = data_topography_slope_total,
                  per_total = n_sum/data_topography_slope_total * 100,
                  remain = paste0("Remaining ", j, " (", i, ")"))
  data_topography_slope_j

  data_topography_slope_i <- dplyr::bind_rows(data_topography_slope_i, data_topography_slope_j)

  #  aspect
  data_topography_aspect_partial <- stringr::str_subset(files_topography_i, paste0(j, "_roads_rails_aspect")) %>%
    readr::read_csv(col_names = c("forest", "value", "area", "n")) %>%
    dplyr::filter(value != 0) %>%
    dplyr::mutate(class = case_when(
      value >= 1 & value <= 45 ~ "NE",
      value > 45 & value <= 90 ~ "N",
      value > 90 & value <= 135 ~ "NW",
      value > 135 & value <= 180 ~ "W",
      value > 180 & value <= 225 ~ "SW",
      value > 225 & value <= 270 ~ "S",
      value > 270 & value <= 315 ~ "SE",
      value > 315 & value <= 360 ~ "E")) %>%
    dplyr::group_by(forest, class) %>%
    dplyr::summarise(n_sum = sum(n)) %>%
    dplyr::mutate(class = forcats::as_factor(class)) %>%
    dplyr::mutate(class = forcats::fct_relevel(class,
                                               c("NE", "N", "NW", "W", "SW", "S", "SE", "E"))) %>%
    dplyr::arrange(class) %>%
    dplyr::mutate(vegetation = j, topography = "aspect", year = i, .before = 1)
  data_topography_aspect_partial

  data_topography_aspect_total <- data_topography_aspect_partial %>%
    dplyr::group_by(class) %>%
    dplyr::summarise(original_distribution = sum(n_sum)) %>%
    dplyr::pull(original_distribution)
  data_topography_aspect_total

  data_topography_aspect_j <- data_topography_aspect_partial %>%
    dplyr::filter(forest == 1) %>%
    dplyr::mutate(n_total = data_topography_aspect_total,
                  per_total = n_sum/sum(n_sum),
                  remain = paste0("Remaining ", j, " (", i, ")"))
  data_topography_aspect_j

  data_topography_aspect_i <- dplyr::bind_rows(data_topography_aspect_i, data_topography_aspect_j)


  # pcurvature
  data_topography_pcurvature_partial <- stringr::str_subset(files_topography_i, paste0(j, "_roads_rails_pcurvature")) %>%
    readr::read_csv(col_names = c("forest", "value", "area", "n")) %>%
    dplyr::mutate(class = case_when(
      value <= -0.005 ~ "≤-0.005",
      value > -0.005 & value <= -0.002 ~ ">-0.005-\n-0.002",
      value > -0.002 & value <= -0.001 ~ ">-0.002-\n-0.001",
      value > -0.001 & value <= 0.000 ~ ">-0.001-\n0.000",
      value > 0.000 & value <= 0.001 ~ ">0.000-\n0.001",
      value > 0.001 & value <= 0.002 ~ ">0.001-\n0.002",
      value > 0.002 & value <= 0.005 ~ ">0.002-\n0.005",
      value > 0.005 ~ ">0.005")) %>%
    dplyr::group_by(forest, class) %>%
    dplyr::summarise(n_sum = sum(n)) %>%
    dplyr::mutate(class = forcats::as_factor(class)) %>%
    dplyr::mutate(class = forcats::fct_relevel(class,
                                               c("≤-0.005", ">-0.005-\n-0.002",
                                                 ">-0.002-\n-0.001", ">-0.001-\n0.000",
                                                 ">0.000-\n0.001", ">0.001-\n0.002",
                                                 ">0.002-\n0.005", ">0.005"))) %>%
    dplyr::arrange(class) %>%
    dplyr::mutate(vegetation = j, topography = "pcurvature", year = i, .before = 1)
  data_topography_pcurvature_partial

  data_topography_pcurvature_total <- data_topography_pcurvature_partial %>%
    dplyr::group_by(class) %>%
    dplyr::summarise(original_distribution = sum(n_sum)) %>%
    dplyr::pull(original_distribution)
  data_topography_pcurvature_total

  data_topography_pcurvature_j <- data_topography_pcurvature_partial %>%
    dplyr::filter(forest == 1) %>%
    dplyr::mutate(n_total = data_topography_pcurvature_total,
                  per_total = n_sum/data_topography_pcurvature_total * 100,
                  remain = paste0("Remaining ", j, " (", i, ")"))
  data_topography_pcurvature_j

  data_topography_pcurvature_i <- dplyr::bind_rows(data_topography_pcurvature_i, data_topography_pcurvature_j)

  # tcurvature
  data_topography_tcurvature_partial <- stringr::str_subset(files_topography_i, paste0(j, "_roads_rails_tcurvature")) %>%
    readr::read_csv(col_names = c("forest", "value", "area", "n")) %>%
    dplyr::mutate(class = case_when(
      value <= -0.005 ~ "≤-0.005",
      value > -0.005 & value <= -0.002 ~ ">-0.005-\n-0.002",
      value > -0.002 & value <= -0.001 ~ ">-0.002-\n-0.001",
      value > -0.001 & value <= 0.000 ~ ">-0.001-\n0.000",
      value > 0.000 & value <= 0.001 ~ ">0.000-\n0.001",
      value > 0.001 & value <= 0.002 ~ ">0.001-\n0.002",
      value > 0.002 & value <= 0.005 ~ ">0.002-\n0.005",
      value > 0.005 ~ ">0.005")) %>%
    dplyr::group_by(forest, class) %>%
    dplyr::summarise(n_sum = sum(n)) %>%
    dplyr::mutate(class = forcats::as_factor(class)) %>%
    dplyr::mutate(class = forcats::fct_relevel(class,
                                               c("≤-0.005", ">-0.005-\n-0.002",
                                                 ">-0.002-\n-0.001", ">-0.001-\n0.000",
                                                 ">0.000-\n0.001", ">0.001-\n0.002",
                                                 ">0.002-\n0.005", ">0.005"))) %>%
    dplyr::arrange(class) %>%
    dplyr::mutate(vegetation = j, topography = "tcurvature", year = i, .before = 1)
  data_topography_tcurvature_partial

  data_topography_tcurvature_total <- data_topography_tcurvature_partial %>%
    dplyr::group_by(class) %>%
    dplyr::summarise(original_distribution = sum(n_sum)) %>%
    dplyr::pull(original_distribution)
  data_topography_tcurvature_total

  data_topography_tcurvature_j <- data_topography_tcurvature_partial %>%
    dplyr::filter(forest == 1) %>%
    dplyr::mutate(n_total = data_topography_tcurvature_total,
                  per_total = n_sum/data_topography_tcurvature_total * 100,
                  remain = paste0("Remaining ", j, " (", i, ")"))
  data_topography_tcurvature_j

  data_topography_tcurvature_i <- dplyr::bind_rows(data_topography_tcurvature_i, data_topography_tcurvature_j)

}

data_topography_elevation <- dplyr::bind_rows(data_topography_elevation, data_topography_elevation_i)
data_topography_slope <- dplyr::bind_rows(data_topography_slope, data_topography_slope_i)
data_topography_aspect <- dplyr::bind_rows(data_topography_aspect, data_topography_aspect_i)
data_topography_pcurvature <- dplyr::bind_rows(data_topography_pcurvature, data_topography_pcurvature_i)
data_topography_tcurvature <- dplyr::bind_rows(data_topography_tcurvature, data_topography_tcurvature_i)

}

data_topography_elevation <- dplyr::bind_rows(ele, data_topography_elevation)
data_topography_slope <- dplyr::bind_rows(slo, data_topography_slope)
data_topography_pcurvature <- dplyr::bind_rows(pcu, data_topography_pcurvature)
data_topography_tcurvature <- dplyr::bind_rows(tcu, data_topography_tcurvature)

## plot ----

### elevation ----
plot_topography_elevation_forest <- data_topography_elevation %>%
  dplyr::filter(vegetation == "forest" | is.na(vegetation)) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  labs(x = "Elevation range (m)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  ylim(0, 65) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.88, .3),
        legend.text = element_text(size = 12))
plot_topography_elevation_forest
ggsave(filename = "03_figures/fig08a_elevation_forest.png", plot = plot_topography_elevation_forest,
       wi = 35, he = 25, un = "cm", dpi = 300)

plot_topography_elevation_natural <- data_topography_elevation %>%
  dplyr::filter(vegetation == "natural" | is.na(vegetation)) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(x = "Elevation range (m)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  ylim(0, 70) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.88, .3),
        legend.text = element_text(size = 12))
plot_topography_elevation_natural
ggsave(filename = "03_figures/fig08b_slope_forest.png", plot = plot_topography_elevation_natural,
       wi = 35, he = 25, un = "cm", dpi = 300)

### slope ----
plot_topography_slope_forest <- data_topography_slope %>%
  dplyr::filter(vegetation == "forest" | is.na(vegetation)) %>%
  ggplot(aes(x = class, y = per_total, fill = remain, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  # geom_bar(stat = "identity", position = position_dodge()) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  scale_fill_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  labs(x = "Slope range (°)", y = "Percentage (%)", color = "", fill = "", shape = "", linetype = "") +
  ylim(0, 60) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.88, .35),
        legend.text = element_text(size = 12))
plot_topography_slope_forest
ggsave(filename = "03_figures/fig08c_slope_forest.png", plot = plot_topography_slope_forest,
       wi = 35, he = 25, un = "cm", dpi = 300)

plot_topography_slope_natural <- data_topography_slope %>%
  dplyr::filter(vegetation == "natural" | is.na(vegetation)) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(x = "Slope range (°)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  ylim(0, 65) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.88, .35),
        legend.text = element_text(size = 12))
plot_topography_slope_natural
ggsave(filename = "03_figures/fig08d_slope_natural.png", plot = plot_topography_slope_natural,
       wi = 35, he = 25, un = "cm", dpi = 300)

### aspect ----

# test
asp_original_degree <- asp_original[asp_original$value != 0, ]
asp_original_degree <- rep(asp_original_degree$value, asp_original_degree$n)
asp_original_degree

asp_original_rad <- NISTunits::NISTdegTOradian(asp_original_degree)
asp_original_rad

set.seed(42)
asp_original_rad_sample <- sample(asp_original_rad, 1e6)
asp_original_rad_sample

asp_original_rad_sample_raytest <- CircStats::r.test(asp_original_rad_sample)
asp_original_rad_sample_raytest

asp_original_rad_sample_circmean <- CircStats::circ.mean(asp_original_rad_sample)
asp_original_rad_sample_circmean

asp_original_rad_sample_circmean_deg <- NISTunits::NISTradianTOdeg(asp_original_rad_sample_circmean)
asp_original_rad_sample_circmean_deg

deg <- data.frame(deg = NISTunits::NISTradianTOdeg(asp_original_rad_sample))
deg

p <- deg %>%
  ggplot(aes(x = deg, y = (..count..)/sum(..count..))) +
  geom_histogram(breaks = seq(0, 360, 15), colour = "black", fill = "gray", alpha = .2) +
  coord_polar(start = 0) +
  scale_fill_brewer(type = "seq", palette = 3) +
  scale_x_continuous("", limits = c(0, 360), breaks = seq(0, 360, 45),
                     labels = c("N", "NW", "W", "SW", "S", "SE", "E", "NE", "N")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 20) +
  labs(y = "")
p

l <- ggplot() +
  annotate("segment", x = 0, xend = -60,
           y = c(0:4), yend = c(0:4),
           linetype = "dashed") +
  theme_void()
p + inset_element(l, left = 0, top = 0.88, bottom = 0.48, right = 0.52)

# forest
asp_forest_degree <- readr::read_csv("02_results/06_mapbiomas_brazil_af_trinacional_2021_af_lim_forest_aspect.csv",
                                     col_names = c("forest", "aspect", "area", "n")) %>%
  dplyr::filter(forest == 1, aspect != 0)
asp_forest_degree
asp_forest_degree <- rep(asp_forest_degree$aspect, asp_forest_degree$n)
asp_forest_degree

asp_forest_rad <- NISTunits::NISTdegTOradian(asp_forest_degree)
asp_forest_rad

set.seed(42)
asp_forest_rad_sample <- sample(asp_forest_rad, 1e6)
asp_forest_rad_sample

asp_forest_rad_sample_raytest <- CircStats::r.test(asp_forest_rad_sample)
asp_forest_rad_sample_raytest

asp_forest_rad_sample_circmean <- CircStats::circ.mean(asp_forest_rad_sample)
asp_forest_rad_sample_circmean

data <- runif(50, 0, pi)
mean.dir <- circ.mean(data)
mean.dir

asp_forest_rad_sample_circmean_deg <- NISTunits::NISTradianTOdeg(asp_forest_rad_sample_circmean)
asp_forest_rad_sample_circmean_deg


deg <- data.frame(deg = NISTunits::NISTradianTOdeg(asp_forest_rad_sample))
deg

p <- deg %>%
  ggplot(aes(x = deg, y = (..count..)/sum(..count..))) +
  geom_histogram(breaks = seq(0, 360, 15), colour = "black", fill = "gray", alpha = .2) +
  coord_polar(start = 0) +
  scale_fill_brewer(type = "seq", palette = 3) +
  scale_x_continuous("", limits = c(0, 360), breaks = seq(0, 360, 45),
                     labels = c("N", "NW", "W", "SW", "S", "SE", "E", "NE", "N")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 20) +
  labs(y = "")
p

l <- ggplot() +
  annotate("segment", x = 0, xend = -60,
           y = c(0:5), yend = c(0:5),
           linetype = "dashed") +
  theme_void()
p + inset_element(l, left = 0, top = 0.90, bottom = 0.48, right = 0.52)


png(filename = "03_figures/fig08e_aspect_forest.png", wi = 51, he = 30, units = "cm", res = 300)
data_topography_aspect %>%
  dplyr::ungroup() %>%
  dplyr::filter(vegetation == "forest") %>%
  dplyr::select(remain, class, per_total) %>%
  tidyr::pivot_wider(names_from = class, values_from = per_total) %>%
  dplyr::select(remain, N, NW, W, SW, S, SE, E, NE) %>%
  dplyr::bind_rows(., asp) %>%
  dplyr::arrange(remain) %>%
  tibble::column_to_rownames(var = "remain") %>%
  fmsb::radarchart(df = .,
                   axistype = 1,
                   seg = 4,
                   pty = NA,
                   pcol = c(NA, paletteer::paletteer_c("ggthemes::Green-Gold", 8)),
                   pfcol = c(adjustcolor("gray70", .3), rep(NA, 8)),
                   plty = c(1, 1),
                   plwd = 3,
                   cglcol = "gray30",
                   cglty = 1,
                   axislabcol = "black",
                   caxislabels = paste0(seq(8, 16, 2), "%"),
                   cglwd = .8,
                   vlcex = 3,
                   calcex = 2,
                   centerzero = TRUE)
legend(x = 1.3, y = .62, col = "gray70", bty = "n", pch = 15,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1,
       legend = "Original aspect")
legend(x = 1.3, y = .5, col = paletteer::paletteer_c("ggthemes::Green-Gold", 8), bty = "n", pch = 22,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1, y.intersp = 1.3,
       legend = data_topography_aspect %>%
         dplyr::ungroup() %>%
         dplyr::filter(vegetation == "forest") %>%
         dplyr::select(remain, class, per_total) %>%
         dplyr::pull(remain) %>%
         unique())
dev.off()

png(filename = "03_figures/fig08f.png", wi = 51, he = 30, units = "cm", res = 300)
data_topography_aspect %>%
  dplyr::ungroup() %>%
  dplyr::filter(vegetation == "natural") %>%
  dplyr::select(remain, class, per_total) %>%
  tidyr::pivot_wider(names_from = class, values_from = per_total) %>%
  dplyr::select(remain, N, NW, W, SW, S, SE, E, NE) %>%
  dplyr::bind_rows(., asp) %>%
  dplyr::arrange(remain) %>%
  tibble::column_to_rownames(var = "remain") %>%
  fmsb::radarchart(df = .,
                   axistype = 1,
                   seg = 4,
                   pty = NA,
                   pcol = c(NA, pal),
                   pfcol = c(adjustcolor("gray70", .3), rep(NA, 8)),
                   plty = c(1, 1),
                   plwd = 3,
                   cglcol = "gray30",
                   cglty = 1,
                   axislabcol = "black",
                   caxislabels = paste0(seq(8, 16, 2), "%"),
                   cglwd = .8,
                   vlcex = 3,
                   calcex = 2,
                   centerzero = TRUE)
legend(x = 1.3, y = .62, col = "gray70", bty = "n", pch = 15,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1,
       legend = "Original aspect")
legend(x = 1.3, y = .5, col = pal, bty = "n", pch = 22,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1, y.intersp = 1.3,
       legend = data_topography_aspect %>%
         dplyr::ungroup() %>%
         dplyr::filter(vegetation == "natural") %>%
         dplyr::select(remain, class, per_total) %>%
         dplyr::pull(remain) %>%
         unique())
dev.off()

### pcurvature ----
plot_topography_pcurvature_forest <- data_topography_pcurvature %>%
  dplyr::filter(vegetation == "forest" | is.na(vegetation)) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", pal)) +
  labs(x = "Profile curvature range (m¹)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  ylim(0, 45) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.9, .47),
        legend.text = element_text(size = 10))
plot_topography_pcurvature_forest
ggsave(filename = "03_figures/fig08g.png", plot = plot_topography_pcurvature_forest,
       wi = 35, he = 25, un = "cm", dpi = 300)

plot_topography_pcurvature_natural <- data_topography_pcurvature %>%
  dplyr::filter(vegetation == "natural" | is.na(vegetation)) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", pal)) +
  labs(x = "Profile curvature range (m¹)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  ylim(0, 60) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.9, .43),
        legend.text = element_text(size = 10))
plot_topography_pcurvature_natural
ggsave(filename = "03_figures/fig08h.png", plot = plot_topography_pcurvature_natural,
       wi = 35, he = 25, un = "cm", dpi = 300)

### tcurvature ----
plot_topography_tcurvature_forest <- data_topography_tcurvature %>%
  dplyr::filter(vegetation == "forest" | is.na(vegetation)) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", pal)) +
  labs(x = "Tangential curvature range (m¹)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  ylim(0, 50) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.9, .43),
        legend.text = element_text(size = 10))
plot_topography_tcurvature_forest
ggsave(filename = "03_figures/fig08i.png", plot = plot_topography_tcurvature_forest,
       wi = 35, he = 25, un = "cm", dpi = 300)

plot_topography_tcurvature_natural <- data_topography_tcurvature %>%
  dplyr::filter(vegetation == "natural" | is.na(vegetation)) %>%
  ggplot(aes(x = class, y = per_total, color = remain, shape = remain, group = remain)) +
  geom_line(aes(linetype = remain)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", pal)) +
  labs(x = "tcurvature range (m)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  ylim(0, 60) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.9, .43),
        legend.text = element_text(size = 10))
plot_topography_tcurvature_natural
ggsave(filename = "03_figures/fig08j.png", plot = plot_topography_tcurvature_natural,
       wi = 35, he = 25, un = "cm", dpi = 300)
