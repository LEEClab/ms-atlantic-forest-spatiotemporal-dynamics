#' ----
#' title: figures
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

# years
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
years

# 1 number of patch and size distribution ---------------------------------

### data ----
data_area_resume <- readr::read_csv("04_tables/01_data_pid_area_resume.csv") %>%
  dplyr::mutate(year = forcats::as_factor(year))
data_area_resume

### plot ----
plot_data_area_resume_n_patches <- ggplot(data = data_area_resume,
                                          aes(x = year, y = n_patches, color = scenario, fill = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario), linewidth = 2) +
  geom_point(size = 4, shape = 21, stroke = 1.5) +
  geom_vline(xintercept = 5.2, color = "gray66", linewidth = 1.5, lty = 1) +
  scale_color_manual(values = c("#129912", "#129912", "#ff8800", "#ff8800")) +
  scale_fill_manual(values = c("#129912", "gray30", "#ff8800", "gray30")) +
  scale_linetype_manual(values = c("solid", "dotted", "solid", "dotted")) +
  scale_y_continuous(breaks = c(2e6, 2.1e6, 2.2e6, 2.3e6), label = c("2.0", "2.1", "2.2", "2.3")) +
  annotate(geom = "text", x = 5, y = 2200000, label = "Atlantic Forest Law", size = 6, angle = 90) +
  annotate(geom = "text", x = 1, y = 2350000, label = "a", size = 10, fontface = "bold") +
  labs(x = "Year", y = "Number of patches (millions)", color = "Scenarios", fill = "Scenarios", linetype = "Scenarios") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.47, .84),
        legend.title = element_blank(),
        legend.text = element_text(size = 15))
plot_data_area_resume_n_patches
ggsave(paste0("03_figures/fig01a_n_patches.png"),
       plot_data_area_resume_n_patches, wi = 25, he = 20, un = "cm", dpi = 300)

plot_data_area_resume_avg_size <- ggplot(data = data_area_resume,
                                         aes(x = year, y = mean_area_ha, color = scenario, fill = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario), linewidth = 2) +
  geom_point(size = 4, shape = 21, stroke = 1.5) +
  geom_vline(xintercept = 5.2, color = "gray66", linewidth = 1.5, lty = 1) +
  scale_color_manual(values = c("#129912", "#129912", "#ff8800", "#ff8800")) +
  scale_fill_manual(values = c("#129912", "gray30", "#ff8800", "gray30")) +
  scale_linetype_manual(values = c("solid", "dotted", "solid", "dotted")) +
  annotate(geom = "text", x = 5, y = 23, label = "Atlantic Forest Law", size = 6, angle = 90) +
  annotate(geom = "text", x = 1, y = 35, label = "b", size = 10, fontface = "bold") +
  labs(x = "Year", y = "Average size of patches (ha)",
       color = "Scenarios", fill = "Scenarios", linetype = "Scenarios") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.55, .84),
        legend.title = element_blank(),
        legend.text = element_text(size = 15))
plot_data_area_resume_avg_size
ggsave(paste0("03_figures/fig01b_avg_size_patches.png"),
       plot_data_area_resume_avg_size, wi = 25, he = 20, un = "cm", dpi = 300)

# data
data_area_resume_plot <- NULL
for(i in files_area){

  print(i)

  name_area <- basename(i) %>%
    stringr::str_replace("01_mapbiomas_brazil_af_trinacional_", "") %>%
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

  data_area_i <- readr::read_csv(i, col_names = c("values", "area", "n"), col_types = cols()) %>%
    dplyr::mutate(area_ha = area/1e4) %>%
    dplyr::mutate(class = case_when(area_ha < 1 ~ "<1",
                                    area_ha >= 1 & area_ha < 5 ~ "1-5",
                                    area_ha >= 5 & area_ha < 10 ~ "5-10",
                                    area_ha >= 10 & area_ha < 50 ~ "10-50",
                                    area_ha >= 50 & area_ha < 100 ~ "50-100",
                                    area_ha >= 100 & area_ha < 250 ~ "100-250",
                                    area_ha >= 250 & area_ha < 500 ~ "250-500",
                                    area_ha >= 500 & area_ha < 1000 ~ "500-1000",
                                    area_ha >= 1000 & area_ha < 2500 ~ "1000-2500",
                                    area_ha >= 2500 & area_ha < 5000 ~ "2500-5000",
                                    area_ha >= 5000 & area_ha < 10000 ~ "5000-10000",
                                    area_ha >= 10000 & area_ha < 25000 ~ "10000-25000",
                                    area_ha >= 25000 & area_ha < 50000 ~ "25000-50000",
                                    area_ha >= 50000 & area_ha < 100000 ~ "50000-100000",
                                    area_ha >= 100000 & area_ha < 250000 ~ "100000-250000",
                                    area_ha >= 250000 & area_ha < 500000 ~ "250000-500000",
                                    area_ha >= 500000 & area_ha < 1000000 ~ "500000-1000000",
                                    area_ha >= 1000000 ~ ">1000000")) %>%
    dplyr::bind_rows(tibble::tibble(values = 0, area = 0, n = 0, area_ha = 0, class = ">1000000")) %>%
    dplyr::mutate(class = forcats::as_factor(class)) %>%
    dplyr::mutate(class = forcats::fct_relevel(class,
                                               c("<1", "1-5", "5-10", "10-50",
                                                 "50-100", "100-250",
                                                 "250-500", "500-1000",
                                                 "1000-2500", "2500-5000",
                                                 "5000-10000", "10000-25000",
                                                 "25000-50000", "50000-100000",
                                                 "100000-250000", "250000-500000",
                                                 "500000-1000000", ">1000000")))

  # resume data
  data_area_resume_plot_i <- data_area_i %>%
    dplyr::group_by(class) %>%
    dplyr::summarise(class_area = sum(area_ha),
                     class_n = n()) %>%
    dplyr::mutate(class_area_per = round(class_area/sum(class_area)*100, 2),
                  class_n_per = round(class_n/sum(class_n)*100, 4)) %>%
    dplyr::mutate(year = title_area_numbers,
                  scenario = title_area_letters)
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

  ggsave(filename = paste0("03_figures/fig01_", name_area, ".png"),
         plot = plot_number_patch_size_dist,
         wi = 25, he = 20, un = "cm", dpi = 300)
}

# export
readr::write_csv(data_area_resume_plot, "04_tables/01_data_area_resume_plot.csv")

# 2 habitat cover ---------------------------------------------------------

### data total ----
data_habitat_cover_resume_total <- readr::read_csv("04_tables/02_data_habitat_cover_resume_total.csv") %>%
  dplyr::mutate(year = forcats::as_factor(year))
data_habitat_cover_resume_total$scenario

### plot total ----
plot_habitat_cover_resume_total_forest <- data_habitat_cover_resume_total %>%
  dplyr::filter(classes != 0,
                scenario == "Forest vegetation (not trimmed by roads and rails)") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Forest vegetation", "Forest formation",
  #                                                         "Mangroove", "Wooded sandbank vegetation"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(class_abbreviation, "FV", "FF", "MG", "WS"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .9, label = paste0(p, "%")), size = 5, color = "black") +
  scale_fill_manual(values = c("#129912", "#006400", "#687537", "#6b9932")) +
  # facet_grid(vars(forcats::fct_relevel(classes,
  #                                    "Forest vegetation", "Forest formation",
  #                                    "Mangroove", "Wooded sandbank vegetation")), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(class_abbreviation, "FV", "FF", "MG", "WS")), scales = "free") +
  labs(title = "Forest vegetation (not trimmed by roads and rails)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(size = 10))
plot_habitat_cover_resume_total_forest
ggsave(paste0("03_figures/fig02a_habitat_cover_resume_total_forest.png"),
       plot_habitat_cover_resume_total_forest, wi = 25, he = 20, un = "cm", dpi = 300)

plot_habitat_cover_resume_total_forest_roads_rails <- data_habitat_cover_resume_total %>%
  dplyr::filter(classes != 0,
                scenario == "Forest vegetation (trimmed by roads and rails)") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Forest vegetation", "Forest formation",
  #                                                         "Mangroove", "Wooded sandbank vegetation"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(class_abbreviation, "FV", "FF", "MG", "WS"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .9, label = paste0(p, "%")), size = 5, color = "black") +
  scale_fill_manual(values = c("#129912", "#006400", "#687537", "#6b9932")) +
  # facet_grid(vars(forcats::fct_relevel(classes,
  #                                    "Forest vegetation", "Forest formation",
  #                                    "Mangroove", "Wooded sandbank vegetation")), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(class_abbreviation, "FV", "FF", "MG", "WS")), scales = "free") +
  labs(title = "Forest vegetation (trimmed by roads and rails)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(size = 10))
plot_habitat_cover_resume_total_forest_roads_rails
ggsave(paste0("03_figures/fig02b_habitat_cover_resume_total_forest_roads_rails.png"),
       plot_habitat_cover_resume_total_forest_roads_rails, wi = 25, he = 20, un = "cm", dpi = 300)

plot_habitat_cover_resume_total_natural <- data_habitat_cover_resume_total %>%
  dplyr::filter(classes != 0,
                scenario == "Natural vegetation (not trimmed by roads and rails)") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Natural vegetation", "Forest formation",
  #                                                         "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                                                         "Wetland", "Grassland", "Salt flat",
  #                                                         "Herbaceous sandbank vegetation",
  #                                                         "Other non forest formations"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(class_abbreviation, "NV", "FF",
                                                          "SF", "MG", "WS", "WT",
                                                          "GL", "ST", "HS", "OF"))) +

  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .7, label = paste0(p, "%")), size = 5, color = "black") +
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
  facet_grid(vars(forcats::fct_relevel(class_abbreviation,
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
ggsave(paste0("03_figures/fig02c_habitat_cover_resume_total_natural.png"),
       plot_habitat_cover_resume_total_natural, wi = 25, he = 20, un = "cm", dpi = 300)

plot_habitat_cover_resume_total_natural_roads_rails <- data_habitat_cover_resume_total %>%
  dplyr::filter(classes != 0,
                scenario == "Natural vegetation (trimmed by roads and rails)") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Natural vegetation", "Forest formation",
  #                                                         "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                                                         "Wetland", "Grassland", "Salt flat",
  #                                                         "Herbaceous sandbank vegetation",
  #                                                         "Other non forest formations"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(class_abbreviation, "NV", "FF",
                                                          "SF", "MG", "WS", "WT",
                                                          "GL", "ST", "HS", "OF"))) +

  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .7, label = paste0(p, "%")), size = 5, color = "black") +
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
  facet_grid(vars(forcats::fct_relevel(class_abbreviation,
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
ggsave(paste0("03_figures/fig02d_habitat_cover_resume_total_natural_roads_rails.png"),
       plot_habitat_cover_resume_total_natural_roads_rails, wi = 25, he = 20, un = "cm", dpi = 300)


### data br ----
data_habitat_cover_br_resume_total <- readr::read_csv("04_tables/02_data_habitat_cover_br_resume.csv") %>%
  dplyr::mutate(year = forcats::as_factor(year))
data_habitat_cover_br_resume_total

### plot br ----
plot_habitat_cover_br_resume_total_forest <- data_habitat_cover_br_resume_total %>%
  dplyr::filter(classes != 0,
                scenario == "Forest vegetation (not trimmed by roads and rails)") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Forest vegetation", "Forest formation",
  #                                                         "Mangroove", "Wooded sandbank vegetation"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(class_abbreviation, "FV", "FF", "MG", "WS"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .9, label = paste0(p, "%")), size = 5, color = "black") +
  scale_fill_manual(values = c("#129912", "#006400", "#687537", "#6b9932")) +
  # facet_grid(vars(forcats::fct_relevel(classes,
  #                                    "Forest vegetation", "Forest formation",
  #                                    "Mangroove", "Wooded sandbank vegetation")), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(class_abbreviation, "FV", "FF", "MG", "WS")), scales = "free") +
  labs(title = "Forest vegetation (not trimmed by roads and rails) (Brazil)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(size = 10))
plot_habitat_cover_br_resume_total_forest
ggsave(paste0("03_figures/fig02e_habitat_cover_br_resume_total_forest.png"),
       plot_habitat_cover_br_resume_total_forest, wi = 25, he = 20, un = "cm", dpi = 300)

plot_habitat_cover_br_resume_total_forest_roads_rails <- data_habitat_cover_br_resume_total %>%
  dplyr::filter(classes != 0,
                scenario == "Forest vegetation (trimmed by roads and rails)") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Forest vegetation", "Forest formation",
  #                                                         "Mangroove", "Wooded sandbank vegetation"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(class_abbreviation, "FV", "FF", "MG", "WS"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .9, label = paste0(p, "%")), size = 5, color = "black") +
  scale_fill_manual(values = c("#129912", "#006400", "#687537", "#6b9932")) +
  # facet_grid(vars(forcats::fct_relevel(classes,
  #                                    "Forest vegetation", "Forest formation",
  #                                    "Mangroove", "Wooded sandbank vegetation")), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(class_abbreviation, "FV", "FF", "MG", "WS")), scales = "free") +
  labs(title = "Forest vegetation (trimmed by roads and rails) (Brazil)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(size = 10))
plot_habitat_cover_br_resume_total_forest_roads_rails
ggsave(paste0("03_figures/fig02f_habitat_cover_br_resume_total_forest_roads_rails.png"),
       plot_habitat_cover_br_resume_total_forest_roads_rails, wi = 25, he = 20, un = "cm", dpi = 300)


plot_habitat_cover_br_resume_total_natural <- data_habitat_cover_br_resume_total %>%
  dplyr::filter(classes != 0,
                scenario == "Natural vegetation (not trimmed by roads and rails)") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Natural vegetation", "Forest formation",
  #                                                         "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                                                         "Wetland", "Grassland", "Salt flat",
  #                                                         "Herbaceous sandbank vegetation",
  #                                                         "Other non forest formations"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(class_abbreviation, "NV", "FF",
                                                          "SF", "MG", "WS", "WT",
                                                          "GL", "ST", "HS", "OF"))) +

  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .7, label = paste0(p, "%")), size = 5, color = "black") +
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
  facet_grid(vars(forcats::fct_relevel(class_abbreviation,
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
ggsave(paste0("03_figures/fig02g_habitat_cover_br_resume_total_natural.png"),
       plot_habitat_cover_br_resume_total_natural, wi = 25, he = 20, un = "cm", dpi = 300)

plot_habitat_cover_br_resume_total_natural_roads_rails <- data_habitat_cover_br_resume_total %>%
  dplyr::filter(classes != 0,
                scenario == "Natural vegetation (trimmed by roads and rails)") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Natural vegetation", "Forest formation",
  #                                                         "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                                                         "Wetland", "Grassland", "Salt flat",
  #                                                         "Herbaceous sandbank vegetation",
  #                                                         "Other non forest formations"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(class_abbreviation, "NV", "FF",
                                                          "SF", "MG", "WS", "WT",
                                                          "GL", "ST", "HS", "OF"))) +

  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .7, label = paste0(p, "%")), size = 5, color = "black") +
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
  facet_grid(vars(forcats::fct_relevel(class_abbreviation,
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
ggsave(paste0("03_figures/fig02h_habitat_cover_br_resume_total_natural_roads_rails.png"),
       plot_habitat_cover_br_resume_total_natural_roads_rails, wi = 25, he = 20, un = "cm", dpi = 300)


### data ar ----
data_habitat_cover_ar_resume_total <- readr::read_csv("04_tables/02_data_habitat_cover_ar_resume.csv") %>%
  dplyr::mutate(year = forcats::as_factor(year))
data_habitat_cover_ar_resume_total

### plot ar ----
plot_habitat_cover_ar_resume_total_forest <- data_habitat_cover_ar_resume_total %>%
  dplyr::filter(classes != 0,
                scenario == "Forest vegetation (not trimmed by roads and rails)") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Forest vegetation", "Forest formation",
  #                                                         "Mangroove", "Wooded sandbank vegetation"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(class_abbreviation, "FV", "FF", "MG", "WS"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .9, label = paste0(p, "%")), size = 5, color = "black") +
  scale_fill_manual(values = c("#129912", "#006400", "#687537", "#6b9932")) +
  # facet_grid(vars(forcats::fct_relevel(classes,
  #                                    "Forest vegetation", "Forest formation",
  #                                    "Mangroove", "Wooded sandbank vegetation")), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(class_abbreviation, "FV", "FF", "MG", "WS")), scales = "free") +
  labs(title = "Forest vegetation (not trimmed by roads and rails) (Argentina)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(size = 10))
plot_habitat_cover_ar_resume_total_forest
ggsave(paste0("03_figures/fig02i_habitat_cover_ar_resume_total_forest.png"),
       plot_habitat_cover_ar_resume_total_forest, wi = 25, he = 20, un = "cm", dpi = 300)


plot_habitat_cover_ar_resume_total_forest_roads_rails <- data_habitat_cover_ar_resume_total %>%
  dplyr::filter(classes != 0,
                scenario == "Forest vegetation (trimmed by roads and rails)") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Forest vegetation", "Forest formation",
  #                                                         "Mangroove", "Wooded sandbank vegetation"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(class_abbreviation, "FV", "FF", "MG", "WS"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .9, label = paste0(p, "%")), size = 5, color = "black") +
  scale_fill_manual(values = c("#129912", "#006400", "#687537", "#6b9932")) +
  # facet_grid(vars(forcats::fct_relevel(classes,
  #                                    "Forest vegetation", "Forest formation",
  #                                    "Mangroove", "Wooded sandbank vegetation")), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(class_abbreviation, "FV", "FF", "MG", "WS")), scales = "free") +
  labs(title = "Forest vegetation (trimmed by roads and rails) (Argentina)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(size = 10))
plot_habitat_cover_ar_resume_total_forest_roads_rails
ggsave(paste0("03_figures/fig02i_habitat_cover_ar_resume_total_forest_roads_rails.png"),
       plot_habitat_cover_ar_resume_total_forest_roads_rails, wi = 25, he = 20, un = "cm", dpi = 300)

plot_habitat_cover_ar_resume_total_natural <- data_habitat_cover_ar_resume_total %>%
  dplyr::filter(classes != 0,
                scenario == "Natural vegetation (not trimmed by roads and rails)") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Natural vegetation", "Forest formation",
  #                                                         "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                                                         "Wetland", "Grassland", "Salt flat",
  #                                                         "Herbaceous sandbank vegetation",
  #                                                         "Other non forest formations"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(class_abbreviation, "NV", "FF",
                                                          "SF", "MG", "WS", "WT",
                                                          "GL", "ST", "HS", "OF"))) +

  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .7, label = paste0(p, "%")), size = 5, color = "black") +
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
  facet_grid(vars(forcats::fct_relevel(class_abbreviation,
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
ggsave(paste0("03_figures/fig02j_habitat_cover_ar_resume_total_natural.png"),
       plot_habitat_cover_ar_resume_total_natural, wi = 25, he = 20, un = "cm", dpi = 300)


plot_habitat_cover_ar_resume_total_natural_roads_rails <- data_habitat_cover_ar_resume_total %>%
  dplyr::filter(classes != 0,
                scenario == "Natural vegetation (trimmed by roads and rails)") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Natural vegetation", "Forest formation",
  #                                                         "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                                                         "Wetland", "Grassland", "Salt flat",
  #                                                         "Herbaceous sandbank vegetation",
  #                                                         "Other non forest formations"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(class_abbreviation, "NV", "FF",
                                                          "SF", "MG", "WS", "WT",
                                                          "GL", "ST", "HS", "OF"))) +

  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .7, label = paste0(p, "%")), size = 5, color = "black") +
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
  facet_grid(vars(forcats::fct_relevel(class_abbreviation,
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
ggsave(paste0("03_figures/fig02k_habitat_cover_ar_resume_total_natural_roads_rails.png"),
       plot_habitat_cover_ar_resume_total_natural_roads_rails, wi = 25, he = 20, un = "cm", dpi = 300)

### data py ----
data_habitat_cover_py_resume_total <- readr::read_csv("04_tables/02_data_habitat_cover_py_resume.csv") %>%
  dplyr::mutate(year = forcats::as_factor(year))
data_habitat_cover_py_resume_total

### plot py ----
plot_habitat_cover_py_resume_total_forest <- data_habitat_cover_py_resume_total %>%
  dplyr::filter(classes != 0,
                scenario == "Forest vegetation (not trimmed by roads and rails)") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Forest vegetation", "Forest formation",
  #                                                         "Mangroove", "Wooded sandbank vegetation"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(class_abbreviation, "FV", "FF", "MG", "WS"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .9, label = paste0(p, "%")), size = 5, color = "black") +
  scale_fill_manual(values = c("#129912", "#006400", "#687537", "#6b9932")) +
  # facet_grid(vars(forcats::fct_relevel(classes,
  #                                    "Forest vegetation", "Forest formation",
  #                                    "Mangroove", "Wooded sandbank vegetation")), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(class_abbreviation, "FV", "FF", "MG", "WS")), scales = "free") +
  labs(title = "Forest vegetation (not trimmed by roads and rails) (Paraguay)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(size = 10))
plot_habitat_cover_py_resume_total_forest
ggsave(paste0("03_figures/fig02l_habitat_cover_py_resume_total_forest.png"),
       plot_habitat_cover_py_resume_total_forest, wi = 25, he = 20, un = "cm", dpi = 300)


plot_habitat_cover_py_resume_total_forest_roads_rails <- data_habitat_cover_py_resume_total %>%
  dplyr::filter(classes != 0,
                scenario == "Forest vegetation (trimmed by roads and rails)") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Forest vegetation", "Forest formation",
  #                                                         "Mangroove", "Wooded sandbank vegetation"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(class_abbreviation, "FV", "FF", "MG", "WS"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .9, label = paste0(p, "%")), size = 5, color = "black") +
  scale_fill_manual(values = c("#129912", "#006400", "#687537", "#6b9932")) +
  # facet_grid(vars(forcats::fct_relevel(classes,
  #                                    "Forest vegetation", "Forest formation",
  #                                    "Mangroove", "Wooded sandbank vegetation")), scales = "free") +
  facet_grid(vars(forcats::fct_relevel(class_abbreviation, "FV", "FF", "MG", "WS")), scales = "free") +
  labs(title = "Forest vegetation (trimmed by roads and rails) (Paraguay)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(size = 10))
plot_habitat_cover_py_resume_total_forest_roads_rails
ggsave(paste0("03_figures/fig02m_habitat_cover_py_resume_total_forest_roads_rails.png"),
       plot_habitat_cover_py_resume_total_forest_roads_rails, wi = 25, he = 20, un = "cm", dpi = 300)


plot_habitat_cover_py_resume_total_natural <- data_habitat_cover_py_resume_total %>%
  dplyr::filter(classes != 0,
                scenario == "Natural vegetation (not trimmed by roads and rails)") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Natural vegetation", "Forest formation",
  #                                                         "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                                                         "Wetland", "Grassland", "Salt flat",
  #                                                         "Herbaceous sandbank vegetation",
  #                                                         "Other non forest formations"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(class_abbreviation, "NV", "FF",
                                                          "SF", "MG", "WS", "WT",
                                                          "GL", "ST", "HS", "OF"))) +

  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .7, label = paste0(p, "%")), size = 5, color = "black") +
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
  facet_grid(vars(forcats::fct_relevel(class_abbreviation,
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
ggsave(paste0("03_figures/fig02n_habitat_cover_py_resume_total_natural.png"),
       plot_habitat_cover_py_resume_total_natural, wi = 25, he = 20, un = "cm", dpi = 300)

plot_habitat_cover_py_resume_total_natural_roads_rails <- data_habitat_cover_py_resume_total %>%
  dplyr::filter(classes != 0,
                scenario == "Natural vegetation (trimmed by roads and rails)") %>%
  # ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(classes,
  #                                                         "Natural vegetation", "Forest formation",
  #                                                         "Savanna formation", "Mangroove", "Wooded sandbank vegetation",
  #                                                         "Wetland", "Grassland", "Salt flat",
  #                                                         "Herbaceous sandbank vegetation",
  #                                                         "Other non forest formations"))) +
  ggplot(aes(x = year, y = p, fill = forcats::fct_relevel(class_abbreviation, "NV", "FF",
                                                          "SF", "MG", "WS", "WT",
                                                          "GL", "ST", "HS", "OF"))) +

  geom_bar(stat = "identity") +
  geom_text(aes(y = p * .7, label = paste0(p, "%")), size = 5, color = "black") +
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
  facet_grid(vars(forcats::fct_relevel(class_abbreviation,
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
ggsave(paste0("03_figures/fig02o_habitat_cover_py_resume_total_natural_roads_rails.png"),
       plot_habitat_cover_py_resume_total_natural_roads_rails, wi = 25, he = 20, un = "cm", dpi = 300)

# 3  core and edge area ---------------------------------------------------

### data ----
data_core_edge_area <- readr::read_csv("04_tables/03_data_core_edge_area.csv") %>%
  dplyr::mutate(year = forcats::as_factor(year))
data_core_edge_area

### plot ----
plot_core_edge_area_forest <- data_core_edge_area %>%
  dplyr::filter(scenario == "Forest vegetation (not trimmed by roads and rails)") %>%
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
ggsave(filename = "03_figures/fig03a_forest_core_edge_area.png", plot = plot_core_edge_area_forest,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_core_edge_area_forest_road <- data_core_edge_area %>%
  dplyr::filter(scenario == "Forest vegetation (trimmed by roads and rails)") %>%
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
ggsave(filename = "03_figures/fig03a_forest_roads_rails_core_edge_area.png",
       plot = plot_core_edge_area_forest_road,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_core_edge_area_natural <- data_core_edge_area %>%
  dplyr::filter(scenario == "Natural vegetation (not trimmed by roads and rails)") %>%
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
ggsave(filename = "03_figures/fig03b_forest_core_edge_area.png",
       plot = plot_core_edge_area_natural,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_core_edge_area_natural_road <- data_core_edge_area %>%
  dplyr::filter(scenario == "Forest vegetation (trimmed by roads and rails)") %>%
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
ggsave(filename = "03_figures/fig03b_natural_roads_rails_core_edge_area.png",
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
  dplyr::group_by(scenario, year, class) %>%
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
  dplyr::filter(scenario == "Forest vegetation (not trimmed by roads and rails)") %>%
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
ggsave(filename = "03_figures/fig03c_forest_core_edge_area_bar.png", plot = plot_core_edge_area_bar_forest,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_core_edge_area_bar_forest_road <- data_core_edge_area_class %>%
  dplyr::filter(scenario == "Forest vegetation (trimmed by roads and rails)") %>%
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
ggsave(filename = "03_figures/fig03c_forest_roads_rails_core_edge_area_bar.png", plot = plot_core_edge_area_bar_forest_road,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_core_edge_area_bar_natural <- data_core_edge_area_class %>%
  dplyr::filter(scenario == "Natural vegetation (not trimmed by roads and rails)") %>%
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
ggsave(filename = "03_figures/fig03d_natural_core_edge_area_bar.png", plot = plot_core_edge_area_bar_natural,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_core_edge_area_bar_natural_road <- data_core_edge_area_class %>%
  dplyr::filter(scenario == "Natural vegetation (trimmed by roads and rails)") %>%
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
ggsave(filename = "03_figures/fig03d_natural_roads_rails_core_edge_area_bar.png", plot = plot_core_edge_area_bar_natural_road,
       wi = 25, he = 20, un = "cm", dpi = 300)


# 4 functional connectivity ---------------------------------------------

### data ----
data_connectivity <- readr::read_csv("04_tables/04_data_connectivity_resume.csv")
data_connectivity

### plot ----

# plot expected cluster size
plot_connectivity_mean_forest <- data_connectivity %>%
  dplyr::filter(scenario == "Forest vegetation (not trimmed by roads and rails)") %>%
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
ggsave(filename = "03_figures/fig04a_connectivity_mean_forest.png", plot = plot_connectivity_mean_forest,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_connectivity_mean_forest_road <- data_connectivity %>%
  dplyr::filter(scenario == "Forest vegetation (trimmed by roads and rails)") %>%
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
ggsave(filename = "03_figures/fig04a_connectivity_mean_forest_roads_rails.png", plot = plot_connectivity_mean_forest_road,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_connectivity_mean_natural <- data_connectivity %>%
  dplyr::filter(scenario == "Natural vegetation (not trimmed by roads and rails)") %>%
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
ggsave(filename = "03_figures/fig04b_connectivity_mean_natural.png", plot = plot_connectivity_mean_natural,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_connectivity_mean_natural_road <- data_connectivity %>%
  dplyr::filter(scenario == "Natural vegetation (trimmed by roads and rails)") %>%
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
ggsave(filename = "03_figures/fig04b_connectivity_mean_natural_roads_rails.png", plot = plot_connectivity_mean_natural_road,
       wi = 25, he = 20, un = "cm", dpi = 300)

# plot
plot_connectivity_high_forest <- data_connectivity %>%
  dplyr::filter(scenario == "Forest vegetation (not trimmed by roads and rails)") %>%
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
ggsave(filename = "03_figures/fig04c_connectivity_high_forest.png", plot = plot_connectivity_high_forest,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_connectivity_high_forest_road <- data_connectivity %>%
  dplyr::filter(scenario == "Forest vegetation (trimmed by roads and rails)") %>%
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
ggsave(filename = "03_figures/fig04c_connectivity_high_forest_roads_rails.png", plot = plot_connectivity_high_forest_road,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_connectivity_high_natural <- data_connectivity %>%
  dplyr::filter(scenario == "Natural vegetation (not trimmed by roads and rails)") %>%
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
ggsave(filename = "03_figures/fig04d_connectivity_high_natural.png", plot = plot_connectivity_high_natural,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_connectivity_high_natural_roads_rails <- data_connectivity %>%
  dplyr::filter(scenario == "Natural vegetation (trimmed by roads and rails)") %>%
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
ggsave(filename = "03_figures/fig04d_connectivity_high_natural_roads_rails.png",
       plot = plot_connectivity_high_natural_roads_rails,
       wi = 25, he = 20, un = "cm", dpi = 300)

# 5 mean isolation --------------------------------------------------------

### data ----
# import mean data
data_isolation_mean <- readr::read_csv("04_tables/05_data_isolation_resume.csv")
data_isolation_mean

### plot ----

# plot
plot_isolation_forest_mean <- data_isolation_mean %>%
  dplyr::filter(scenario == "Forest vegetation (not trimmed by roads and rails)") %>%
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
  annotate(geom = "text", x = 1, y = 22500, label = "a", size = 10, fontface = "bold") +
  ylim(0, 23000) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.09, .6),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_isolation_forest_mean
ggsave(filename = "03_figures/fig05a_isolation_forest_mean.png",
       plot = plot_isolation_forest_mean,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_isolation_forest_roads_rails_mean <- data_isolation_mean %>%
  dplyr::filter(scenario == "Forest vegetation (trimmed by roads and rails)") %>%
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
  annotate(geom = "text", x = 1, y = 22500, label = "a", size = 10, fontface = "bold") +
  ylim(0, 23000) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.09, .6),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_isolation_forest_roads_rails_mean
ggsave(filename = "03_figures/fig05a_isolation_forest_roads_rails_mean.png",
       plot = plot_isolation_forest_roads_rails_mean,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_isolation_natural_mean <- data_isolation_mean %>%
  dplyr::filter(scenario == "Natural vegetation (not trimmed by roads and rails)") %>%
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
  annotate(geom = "text", x = 1, y = 22500, label = "b", size = 10, fontface = "bold") +
  ylim(0, 23000) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.09, .6),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_isolation_natural_mean
ggsave(filename = "03_figures/fig05b_isolation_natural_mean.png",
       plot = plot_isolation_natural_mean,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_isolation_natural_roads_rails_mean <- data_isolation_mean %>%
  dplyr::filter(scenario == "Natural vegetation (trimmed by roads and rails)") %>%
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
  annotate(geom = "text", x = 1, y = 22500, label = "b", size = 10, fontface = "bold") +
  ylim(0, 23000) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.09, .6),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_isolation_natural_roads_rails_mean
ggsave(filename = "03_figures/fig05b_isolation_natural_roads_rails_mean.png",
       plot = plot_isolation_natural_roads_rails_mean,
       wi = 25, he = 20, un = "cm", dpi = 300)

# 6 protected area and indigenous territories ------------------------------

## protected area ----

### vegetation ----
data_protected_area <- readr::read_csv("04_tables/06_data_protected_area.csv") %>%
  dplyr::mutate(class = forcats::as_factor(class))
data_protected_area

# plot
plot_protected_area_forest <- data_protected_area %>%
  dplyr::filter(scenario == "Forest vegetation (not trimmed by roads and rails)") %>%
  ggplot(aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#129912", rep("#8fba8f", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")),
            nudge_y = 3e5, size = 7) +
  annotate(geom = "text", x = 1, y = 1.05e7, label = "a", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (not trimmed by roads and rails)",
       x = "Protected area distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_protected_area_forest
ggsave(filename = "03_figures/fig06a_protected_area_forest_2020.png", plot = plot_protected_area_forest,
       wi = 35, he = 25, un = "cm", dpi = 300)


# plot
plot_protected_area_forest_road <- data_protected_area %>%
  dplyr::filter(scenario == "Forest vegetation (trimmed by roads and rails)") %>%
  ggplot(aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#129912", rep("#8fba8f", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")),
            nudge_y = 3e5, size = 7) +
  annotate(geom = "text", x = 1, y = 1.05e7, label = "a", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (trimmed by roads and rails)",
       x = "Protected area distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_protected_area_forest_road
ggsave(filename = "03_figures/fig06b_protected_area_forest_road_2020.png", plot = plot_protected_area_forest_road,
       wi = 35, he = 25, un = "cm", dpi = 300)

plot_protected_area_natural <- data_protected_area %>%
  dplyr::filter(scenario == "Natural vegetation (not trimmed by roads and rails)") %>%
  ggplot(aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#ff8800", rep("#fec882", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")), nudge_y = 3e5, size = 7) +
  annotate(geom = "text", x = 1, y = 1.5e7, label = "b", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (not trimmed by roads and rails)",
       x = "Protected area distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_protected_area_natural
ggsave(filename = "03_figures/fig06c_protected_area_natural_2020.png", plot = plot_protected_area_natural,
       wi = 35, he = 25, un = "cm", dpi = 300)


plot_protected_area_natural_road <- data_protected_area %>%
  dplyr::filter(scenario == "Natural vegetation (trimmed by roads and rails)") %>%
  ggplot(aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#ff8800", rep("#fec882", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")),
            nudge_y = 3e5, size = 7) +
  annotate(geom = "text", x = 1, y = 1.5e7, label = "b", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (trimmed by roads and rails)",
       x = "Protected area distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_protected_area_natural_road
ggsave(filename = "03_figures/fig06d_protected_area_natural_road_2020.png", plot = plot_protected_area_natural_road,
       wi = 35, he = 25, un = "cm", dpi = 300)


### classes ----

# data
data_protected_area_classes <- readr::read_csv("04_tables/06_data_protected_area_classes.csv")
data_protected_area_classes

# plot
plot_protected_area_classes <- data_protected_area_classes %>%
  dplyr::filter(scenario == "not_trimmed") %>%
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
  annotate(geom = "text", x = 9, y = 9.5, label = "a", size = 10, fontface = "bold") +
  labs(x = "", y = "Area log10(ha)", title = "Protected area (not trimmed by roads and rails)") +
  theme_bw(base_size = 25) +
  theme(title = element_text(size = 25),
        legend.position = "none")
plot_protected_area_classes
ggsave(filename = "03_figures/fig06e_plot_protected_area_classes_2020.png",
       plot = plot_protected_area_classes,
       wi = 45, he = 25, un = "cm", dpi = 300)


plot_protected_area_classes_roads <- data_protected_area_classes %>%
  dplyr::filter(scenario == "trimmed") %>%
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
  annotate(geom = "text", x = 9, y = 9.5, label = "b", size = 10, fontface = "bold") +
  labs(x = "", y = "Area log10(ha)", title = "Protected area (trimmed by roads and rails)") +
  theme_bw(base_size = 25) +
  theme(legend.position = "none")
plot_protected_area_classes_roads
ggsave(filename = "03_figures/fig06f_plot_protected_area_classes_roads_rails_2020.png",
       plot = plot_protected_area_classes_roads,
       wi = 45, he = 25, un = "cm", dpi = 300)

## indigenous territory ----

### vegetation ----

# import data
data_indigenous_territory <- readr::read_csv("04_tables/06_data_indigenous_territory.csv", col_types = readr::cols()) %>%
  dplyr::mutate(class = forcats::as_factor(class))
data_indigenous_territory

# plot
plot_indigenous_territory_forest <- data_indigenous_territory %>%
  dplyr::filter(scenario == "Forest vegetation (not trimmed by roads and rails)") %>%
  ggplot(aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#129912", rep("#8fba8f", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")), nudge_y = 5e5, size = 7) +
  annotate(geom = "text", x = 1, y = 2e7, label = "c", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (not trimmed by roads and rails)",
       x = "Indigenous territory distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_indigenous_territory_forest
ggsave(filename = "03_figures/fig06a_indigenous_territory_forest_2020.png", plot = plot_indigenous_territory_forest,
       wi = 35, he = 25, un = "cm", dpi = 300)


plot_indigenous_territory_forest_road <- data_indigenous_territory %>%
  dplyr::filter(scenario == "Forest vegetation (trimmed by roads and rails)") %>%
  ggplot(aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#129912", rep("#8fba8f", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")), nudge_y = 5e5, size = 7) +
  annotate(geom = "text", x = 1, y = 2e7, label = "c", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (trimmed by roads and rails)",
       x = "Indigenous territory distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_indigenous_territory_forest_road
ggsave(filename = "03_figures/fig06b_indigenous_territory_forest_road_2020.png", plot = plot_indigenous_territory_forest_road,
       wi = 35, he = 25, un = "cm", dpi = 300)


plot_indigenous_territory_natural <- data_indigenous_territory %>%
  dplyr::filter(scenario == "Natural vegetation (not trimmed by roads and rails)") %>%
  ggplot(aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#ff8800", rep("#fec882", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")), nudge_y = 1e6, size = 7) +
  annotate(geom = "text", x = 1, y = 3.5e7, label = "d", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (not trimmed by roads and rails)",
       x = "Indigenous territory distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_indigenous_territory_natural
ggsave(filename = "03_figures/fig06c_indigenous_territory_natural_2020.png", plot = plot_indigenous_territory_natural,
       wi = 35, he = 25, un = "cm", dpi = 300)


plot_indigenous_territory_natural_road <- ggplot(data = data_indigenous_territory_natural_road_sum,
                                                 aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#ff8800", rep("#fec882", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")), nudge_y = 1e6, size = 7) +
  annotate(geom = "text", x = 1, y = 3.5e7, label = "d", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (trimmed by roads and rails)",
       x = "Indigenous territory distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_indigenous_territory_natural_road
ggsave(filename = "03_figures/fig06d_indigenous_territory_natural_road_2020.png", plot = plot_indigenous_territory_natural_road,
       wi = 35, he = 25, un = "cm", dpi = 300)

### classes ----

# data
data_indigenous_territory_classes <- readr::read_csv("04_tables/06_data_indigenous_territory_classes.csv")
data_indigenous_territory_classes

# plot
plot_indigenous_territory_classes <- data_indigenous_territory_classes %>%
  dplyr::filter(scenario == "not_trimmed") %>%
  dplyr::mutate(classes = forcats::as_factor(classes)) %>%
  dplyr::mutate(classes = reorder(classes, -area_ha_class_sum)) %>%
  ggplot(aes(x = classes, y = log10(area_ha_class_sum), fill = classes)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(per_protec, "%IT / ", per_class, "%CA")),
            nudge_y = max(log10(data_indigenous_territory_natural_classes_roads_per_total$area_ha_class_sum)) * .16,
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
  annotate(geom = "text", x = 9, y = 9.5, label = "c", size = 10, fontface = "bold") +
  labs(x = "", y = "Area log10(ha)", title = "Indigenous territory (not trimmed by roads and rails)") +
  theme_bw(base_size = 25) +
  theme(title = element_text(size = 25),
        legend.position = "none")
plot_indigenous_territory_classes
ggsave(filename = "03_figures/fig06e_plot_indigenous_territory_classes.png",
       plot = plot_indigenous_territory_classes,
       wi = 45, he = 25, un = "cm", dpi = 300)


plot_indigenous_territory_classes_roads <- data_indigenous_territory_classes %>%
  dplyr::filter(scenario == "trimmed") %>%
  dplyr::mutate(classes = forcats::as_factor(classes)) %>%
  dplyr::mutate(classes = reorder(classes, -area_ha_class_sum)) %>%
  ggplot(aes(x = classes, y = log10(area_ha_class_sum), fill = classes)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(per_protec, "%IT / ", per_class, "%CA")),
            nudge_y = max(log10(data_indigenous_territory_natural_classes_roads_per_total$area_ha_class_sum)) * .16,
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
  annotate(geom = "text", x = 9, y = 9.5, label = "d", size = 10, fontface = "bold") +
  labs(x = "", y = "Area log10(ha)", title = "Indigenous territory (trimmed by roads and rails)") +
  theme_bw(base_size = 25) +
  theme(legend.position = "none")
plot_indigenous_territory_classes_roads
ggsave(filename = "03_figures/fig06f_plot_indigenous_territory_classes_roads_rails.png",
       plot = plot_indigenous_territory_classes_roads,
       wi = 45, he = 25, un = "cm", dpi = 300)

# 7 landscape and topographic index ---------------------------------------

## import data ----
data_topography_elevation <- readr::read_csv("04_tables/07_data_topography_elevation.csv") %>%
  dplyr::mutate(class = forcats::as_factor(class))
data_topography_elevation

data_topography_slope <- readr::read_csv("04_tables/07_data_topography_slope.csv") %>%
  dplyr::mutate(class = forcats::as_factor(class))
data_topography_slope

data_topography_aspect <- readr::read_csv("04_tables/07_data_topography_aspect.csv") %>%
  dplyr::mutate(class = forcats::as_factor(class))
data_topography_aspect

data_topography_pcurvature <- readr::read_csv("04_tables/07_data_topography_pcurvature.csv") %>%
  dplyr::mutate(class = forcats::as_factor(class))
data_topography_pcurvature

data_topography_tcurvature <- readr::read_csv("04_tables/07_data_topography_tcurvature.csv") %>%
  dplyr::mutate(class = forcats::as_factor(class))
data_topography_tcurvature

data_topography_geomorph <- readr::read_csv("04_tables/07_data_topography_geomorph.csv") %>%
  dplyr::mutate(class = forcats::as_factor(class))
data_topography_geomorph

## plot ----

### elevation ----
plot_topography_elevation_forest <- data_topography_elevation %>%
  dplyr::filter(scenario %in% c("Forest vegetation (not trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  ggplot(aes(x = class, y = per_class, color = scenario, shape = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario)) +
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
ggsave(filename = "03_figures/fig07a_topography_elevation_forest.png", plot = plot_topography_elevation_forest,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_elevation_forest_roads_rails <- data_topography_elevation %>%
  dplyr::filter(scenario %in% c("Forest vegetation (trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  ggplot(aes(x = class, y = per_class, color = scenario, shape = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario)) +
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
ggsave(filename = "03_figures/fig07a_topography_elevation_forest_roads_rails.png",
       plot = plot_topography_elevation_forest_roads_rails,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_elevation_natural <- data_topography_elevation %>%
  dplyr::filter(scenario %in% c("Natural vegetation (not trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  ggplot(aes(x = class, y = per_class, color = scenario, shape = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario)) +
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
ggsave(filename = "03_figures/fig07b_topography_elevation_natural.png", plot = plot_topography_elevation_natural,
       wi = 30, he = 20, un = "cm", dpi = 300)


plot_topography_elevation_natural_roads_rails <- data_topography_elevation %>%
  dplyr::filter(scenario %in% c("Forest vegetation (trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  ggplot(aes(x = class, y = per_class, color = scenario, shape = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario)) +
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
ggsave(filename = "03_figures/fig07b_topography_elevation_natural_roads_rails.png",
       plot = plot_topography_elevation_natural_roads_rails,
       wi = 30, he = 20, un = "cm", dpi = 300)

### slope ----
plot_topography_slope_forest <- data_topography_slope %>%
  dplyr::filter(scenario %in% c("Forest vegetation (not trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  ggplot(aes(x = class, y = per_class, fill = scenario, color = scenario, shape = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario)) +
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
ggsave(filename = "03_figures/fig07c_topography_slope_forest.png",
       plot = plot_topography_slope_forest,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_slope_forest_roads_rails <- data_topography_slope %>%
  dplyr::filter(scenario %in% c("Forest vegetation (trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  ggplot(aes(x = class, y = per_class, fill = scenario, color = scenario, shape = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  # geom_bar(stat = "identity", position = position_dodge()) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  scale_fill_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  labs(title = "Forest vegetation (trimmed by roads and rails)",
       x = "Slope range (°)", y = "Percentage (%)", color = "", fill = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 63, label = "c", size = 10, fontface = "bold") +
  ylim(0, 65) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .3),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_slope_forest_roads_rails
ggsave(filename = "03_figures/fig07c_topography_slope_forest_roads_rails.png",
       plot = plot_topography_slope_forest_roads_rails,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_slope_natural <- data_topography_slope %>%
  dplyr::filter(scenario %in% c("Natural vegetation (not trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  ggplot(aes(x = class, y = per_class, color = scenario, shape = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(title = "Natural vegetation (not trimmed by roads and rails)",
       x = "Slope range (°)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 63, label = "d", size = 10, fontface = "bold") +
  ylim(0, 65) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .3),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_slope_natural
ggsave(filename = "03_figures/fig07d_topography_slope_natural.png",
       plot = plot_topography_slope_natural,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_slope_natural_roads_rails <- data_topography_slope %>%
  dplyr::filter(scenario %in% c("Natural vegetation (trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  ggplot(aes(x = class, y = per_class, color = scenario, shape = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(title = "Natural vegetation (trimmed by roads and rails)",
       x = "Slope range (°)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 63, label = "d", size = 10, fontface = "bold") +
  ylim(0, 65) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .3),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_slope_natural_roads_rails
ggsave(filename = "03_figures/fig07d_topography_slope_natural_roads_rails.png",
       plot = plot_topography_slope_natural_roads_rails,
       wi = 30, he = 20, un = "cm", dpi = 300)


### aspect ----
png(filename = "03_figures/fig07e_topography_aspect_forest.png", wi = 53, he = 30, units = "cm", res = 300)
par(mar = c(1, 1, 1, 1))
data_topography_aspect %>%
  dplyr::ungroup() %>%
  dplyr::filter(scenario == "Forest vegetation (not trimmed by roads and rails)") %>%
  dplyr::select(year, class, per_class) %>%
  dplyr::mutate(per_class = per_class/100) %>%
  tidyr::pivot_wider(names_from = class, values_from = per_class) %>%
  dplyr::rename(scenario = year) %>%
  dplyr::mutate(scenario = as.factor(scenario)) %>%
  dplyr::select(scenario, N, NW, W, SW, S, SE, E, NE) %>%
  dplyr::bind_rows(asp_figure, .) %>%
  tibble::column_to_rownames(var = "scenario") %>%
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
text(-1, 1.15, substitute(paste(bold("e"))), cex = 4)
legend(x = 1.3, y = .2, col = "gray70", bty = "n", pch = 15,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1,
       legend = "Original aspect")
legend(x = 1.3, y = .1, col = paletteer::paletteer_c("ggthemes::Green-Gold", 8), bty = "n", pch = 22,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1, y.intersp = 1.3,
       legend = c("1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))
dev.off()

png(filename = "03_figures/fig07f_topography_aspect_forest_roads_rails.png", wi = 53, he = 30, units = "cm", res = 300)
par(mar = c(1, 1, 1, 1))
data_topography_aspect %>%
  dplyr::ungroup() %>%
  dplyr::filter(scenario == "Forest vegetation (trimmed by roads and rails)") %>%
  dplyr::select(year, class, per_class) %>%
  dplyr::mutate(per_class = per_class/100) %>%
  tidyr::pivot_wider(names_from = class, values_from = per_class) %>%
  dplyr::rename(scenario = year) %>%
  dplyr::mutate(scenario = as.factor(scenario)) %>%
  dplyr::select(scenario, N, NW, W, SW, S, SE, E, NE) %>%
  dplyr::bind_rows(asp_figure, .) %>%
  tibble::column_to_rownames(var = "scenario") %>%
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
text(-1, 1.15, substitute(paste(bold("f"))), cex = 4)
legend(x = 1.3, y = .2, col = "gray70", bty = "n", pch = 15,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1,
       legend = "Original aspect")
legend(x = 1.3, y = .1, col = paletteer::paletteer_c("ggthemes::Green-Gold", 8), bty = "n", pch = 22,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1, y.intersp = 1.3,
       legend = c("1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))
dev.off()

png(filename = "03_figures/fig07g_topography_aspect_natural.png", wi = 53, he = 30, units = "cm", res = 300)
par(mar = c(1, 1, 1, 1))
data_topography_aspect %>%
  dplyr::ungroup() %>%
  dplyr::filter(scenario == "Natural vegetation (not trimmed by roads and rails)") %>%
  dplyr::select(year, class, per_class) %>%
  dplyr::mutate(per_class = per_class/100) %>%
  tidyr::pivot_wider(names_from = class, values_from = per_class) %>%
  dplyr::rename(scenario = year) %>%
  dplyr::mutate(scenario = as.factor(scenario)) %>%
  dplyr::select(scenario, N, NW, W, SW, S, SE, E, NE) %>%
  dplyr::bind_rows(asp_figure, .) %>%
  tibble::column_to_rownames(var = "scenario") %>%
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
text(-1, 1.15, substitute(paste(bold("g"))), cex = 4)
legend(x = 1.3, y = .2, col = "gray70", bty = "n", pch = 15,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1,
       legend = "Original aspect")
legend(x = 1.3, y = .1, col = paletteer::paletteer_c("ggthemes::Orange-Gold", 8), bty = "n", pch = 22,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1, y.intersp = 1.3,
       legend = c("1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))
dev.off()

png(filename = "03_figures/fig07h_topography_aspect_natural_roads_rails.png", wi = 53, he = 30, units = "cm", res = 300)
par(mar = c(1, 1, 1, 1))
data_topography_aspect %>%
  dplyr::ungroup() %>%
  dplyr::filter(scenario == "Natural vegetation (trimmed by roads and rails)") %>%
  dplyr::select(year, class, per_class) %>%
  dplyr::mutate(per_class = per_class/100) %>%
  tidyr::pivot_wider(names_from = class, values_from = per_class) %>%
  dplyr::rename(scenario = year) %>%
  dplyr::mutate(scenario = as.factor(scenario)) %>%
  dplyr::select(scenario, N, NW, W, SW, S, SE, E, NE) %>%
  dplyr::bind_rows(asp_figure, .) %>%
  tibble::column_to_rownames(var = "scenario") %>%
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
text(-1, 1.15, substitute(paste(bold("h"))), cex = 4)
legend(x = 1.3, y = .2, col = "gray70", bty = "n", pch = 15,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1,
       legend = "Original aspect")
legend(x = 1.3, y = .1, col = paletteer::paletteer_c("ggthemes::Orange-Gold", 8), bty = "n", pch = 22,
       pt.cex = 5, pt.lwd = 4, cex = 2, x.intersp = 1, y.intersp = 1.3,
       legend = c("1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))
dev.off()

### pcurvature ----
plot_topography_pcurvature_forest <- data_topography_pcurvature %>%
  dplyr::filter(scenario %in% c("Forest vegetation (not trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  ggplot(aes(x = class, y = per_class, color = scenario, shape = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  labs(title = "Forest vegetation (not trimmed by roads and rails)",
       x = "Profile curvature range (m¹)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 50, label = "i", size = 10, fontface = "bold") +
  ylim(0, 55) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .35),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_pcurvature_forest
ggsave(filename = "03_figures/fig07i_topography_pcurvature_forest.png",
       plot = plot_topography_pcurvature_forest,
       wi = 30, he = 20, un = "cm", dpi = 300)


plot_topography_pcurvature_forest_roads_rails <- data_topography_pcurvature %>%
  dplyr::filter(scenario %in% c("Forest vegetation (trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  ggplot(aes(x = class, y = per_class, color = scenario, shape = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  labs(title = "Forest vegetation (trimmed by roads and rails)",
       x = "Profile curvature range (m¹)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 50, label = "j", size = 10, fontface = "bold") +
  ylim(0, 55) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .35),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_pcurvature_forest_roads_rails
ggsave(filename = "03_figures/fig07j_topography_pcurvature_forest_roads_rails.png",
       plot = plot_topography_pcurvature_forest_roads_rails,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_pcurvature_natural <- data_topography_pcurvature %>%
  dplyr::filter(scenario %in% c("Natural vegetation (not trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  ggplot(aes(x = class, y = per_class, color = scenario, shape = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(title = "Natural vegetation (not trimmed by roads and rails)",
       x = "Profile curvature range (m¹)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 50, label = "k", size = 10, fontface = "bold") +
  ylim(0, 55) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .45),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_pcurvature_natural
ggsave(filename = "03_figures/fig07k_topography_pcurvature_natural.png",
       plot = plot_topography_pcurvature_natural,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_pcurvature_natural_roads_rails <- data_topography_pcurvature %>%
  dplyr::filter(scenario %in% c("Natural vegetation (trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  ggplot(aes(x = class, y = per_class, color = scenario, shape = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(title = "Natural vegetation (trimmed by roads and rails)",
       x = "Profile curvature range (m¹)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 50, label = "l", size = 10, fontface = "bold") +
  ylim(0, 55) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .45),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_pcurvature_natural_roads_rails
ggsave(filename = "03_figures/fig07l_topography_pcurvature_natural_roads_rails.png",
       plot = plot_topography_pcurvature_natural_roads_rails,
       wi = 30, he = 20, un = "cm", dpi = 300)

### tcurvature ----
plot_topography_tcurvature_forest <- data_topography_tcurvature %>%
  dplyr::filter(scenario %in% c("Forest vegetation (not trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  ggplot(aes(x = class, y = per_class, color = scenario, shape = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  labs(title = "Forest vegetation (not trimmed by roads and rails)",
       x = "Tangential curvature range (m¹)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 50, label = "m", size = 10, fontface = "bold") +
  ylim(0, 50) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .41),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_tcurvature_forest
ggsave(filename = "03_figures/fig07m_topography_tcurvature_forest.png",
       plot = plot_topography_tcurvature_forest,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_tcurvature_forest_roads_rails <- data_topography_tcurvature %>%
  dplyr::filter(scenario %in% c("Forest vegetation (trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  ggplot(aes(x = class, y = per_class, color = scenario, shape = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  labs(title = "Forest vegetation (trimmed by roads and rails)",
       x = "Tangential curvature range (m¹)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 50, label = "n", size = 10, fontface = "bold") +
  ylim(0, 50) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .41),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_tcurvature_forest_roads_rails
ggsave(filename = "03_figures/fig07n_topography_tcurvature_forest_roads_rails.png",
       plot = plot_topography_tcurvature_forest_roads_rails,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_tcurvature_natural <- data_topography_tcurvature %>%
  dplyr::filter(scenario %in% c("Natural vegetation (not trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  ggplot(aes(x = class, y = per_class, color = scenario, shape = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(title = "Natural vegetation (not trimmed by roads and rails)",
       x = "Tangential curvature range (m)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 60, label = "o", size = 10, fontface = "bold") +
  ylim(0, 60) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .41),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_tcurvature_natural
ggsave(filename = "03_figures/fig07o_topography_tcurvature_natural.png",
       plot = plot_topography_tcurvature_natural,
       wi = 30, he = 20, un = "cm", dpi = 300)

plot_topography_tcurvature_natural_roads_rails <- data_topography_tcurvature %>%
  dplyr::filter(scenario %in% c("Natural vegetation (trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  ggplot(aes(x = class, y = per_class, color = scenario, shape = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario)) +
  geom_point(size = 7, color = "white") +
  geom_point(size = 5, alpha = .5) +
  scale_shape_manual(values = c(19, rep(15, 8))) +
  scale_linetype_manual(values = c(1, rep(2, 8))) +
  scale_color_manual(values = c("black", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(title = "Natural vegetation (trimmed by roads and rails)",
       x = "Tangential curvature range (m)", y = "Percentage (%)", color = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 60, label = "p", size = 10, fontface = "bold") +
  ylim(0, 60) +
  theme_bw(base_size = 24) +
  theme(legend.position = c(.9, .41),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_tcurvature_natural_roads_rails
ggsave(filename = "03_figures/fig07p_topography_tcurvature_natural_roads_rails.png",
       plot = plot_topography_tcurvature_natural_roads_rails,
       wi = 30, he = 20, un = "cm", dpi = 300)

### geomorph ----
plot_topography_geomorph_forest <- data_topography_geomorph %>%
  dplyr::filter(scenario %in% c("Forest vegetation (not trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  dplyr::mutate(geo = forcats::fct_relevel(geo, c("flat", "peak", "ridge", "shoulder", "spur", "slope", "hollow", "footslope", "valley", "pit"))) %>%
  ggplot(aes(x = geo, y = per_class, fill = scenario)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("gray30", paletteer::paletteer_c("ggthemes::Green-Gold", 8))) +
  labs(title = "Forest vegetation (not trimmed by roads and rails)",
       x = "Geomorphons", y = "Percentage (%)", fill = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 60, label = "q", size = 12, fontface = "bold") +
  ylim(0, 60) +
  theme_bw(base_size = 30) +
  theme(legend.position = c(.9, .83),
        legend.text = element_text(size = 13),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_geomorph_forest
ggsave(filename = "03_figures/fig07q_topography_geomorphons_forest.png",
       plot = plot_topography_geomorph_forest,
       wi = 40, he = 25, un = "cm", dpi = 300)


plot_topography_geomorph_forest_roads_rails <- data_topography_geomorph %>%
  dplyr::filter(scenario %in% c("Forest vegetation (trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  dplyr::mutate(geo = forcats::fct_relevel(geo, c("flat", "peak", "ridge", "shoulder", "spur", "slope", "hollow", "footslope", "valley", "pit"))) %>%
  ggplot(aes(x = geo, y = per_class, fill = scenario)) +
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
ggsave(filename = "03_figures/fig07a_topography_geomorphons_forest_roads_rails.png",
       plot = plot_topography_geomorph_forest_roads_rails,
       wi = 40, he = 25, un = "cm", dpi = 300)

plot_topography_geomorph_natural <- data_topography_geomorph %>%
  dplyr::filter(scenario %in% c("Natural vegetation (not trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  dplyr::mutate(geo = forcats::fct_relevel(geo, c("flat", "peak", "ridge", "shoulder", "spur", "slope", "hollow", "footslope", "valley", "pit"))) %>%
  ggplot(aes(x = geo, y = per_class, fill = scenario)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("gray30", paletteer::paletteer_c("ggthemes::Orange-Gold", 8))) +
  labs(title = "Natural vegetation (not trimmed by roads and rails)",
       x = "Geomorphons", y = "Percentage (%)", fill = "", shape = "", linetype = "") +
  annotate(geom = "text", x = 1, y = 60, label = "r", size = 12, fontface = "bold") +
  ylim(0, 60) +
  theme_bw(base_size = 30) +
  theme(legend.position = c(.9, .83),
        legend.text = element_text(size = 13),
        legend.title = element_blank(),
        legend.margin = margin(t = -25))
plot_topography_geomorph_natural
ggsave(filename = "03_figures/fig07r_topography_geomorph_natural.png",
       plot = plot_topography_geomorph_natural,
       wi = 40, he = 25, un = "cm", dpi = 300)

plot_topography_geomorph_natural_roads_rails <- data_topography_geomorph %>%
  dplyr::filter(scenario %in% c("Natural vegetation (trimmed by roads and rails)", "Original distribution")) %>%
  dplyr::mutate(scenario = ifelse(is.na(year), scenario, year)) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, c("Original distribution", "1986", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))) %>%
  dplyr::mutate(geo = forcats::fct_relevel(geo, c("flat", "peak", "ridge", "shoulder", "spur", "slope", "hollow", "footslope", "valley", "pit"))) %>%
  ggplot(aes(x = geo, y = per_class, fill = scenario)) +
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
ggsave(filename = "03_figures/fig07b_topography_geomorph_natural_roads_rails.png",
       plot = plot_topography_geomorph_natural_roads_rails,
       wi = 40, he = 25, un = "cm", dpi = 300)
