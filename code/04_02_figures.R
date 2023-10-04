#' ----
#' title: atlantic forest spatiotemporal dynamic - figures
#' author: mauricio vancine
#' date: 2022-12-09
#' operational system: gnu/linux - ubuntu - pop_os
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(paletteer)

# options
options(scipen = 1000)

# fig 01 ------------------------------------------------------------------

## data ----
data_fig01 <- readr::read_csv("03_tables/data_fig01.csv") %>%
  dplyr::mutate(class_n_per = ifelse(class_n_per < 0.01, "<0.01", class_n_per)) %>%
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
data_fig01

## figure ----
for(i in unique(data_fig01$year)){

  # filter
  data_fig01_i <- data_fig01 %>%
    dplyr::filter(year == i)

  for(j in unique(data_fig01_i$scenario)){

    # filter
    data_fig01_i_j <- data_fig01_i %>%
      dplyr::filter(scenario == j)

    if(stringr::str_detect(j, "Forest")){

      plot_fig01 <- ggplot(data = data_fig01_i_j,
                           aes(x = class, y = class_area/1e6)) +
        geom_bar(stat = "identity", fill = ifelse(str_detect(j, "Forest"), "#129912", "#ff8800")) +
        geom_text(aes(label = paste0(class_area_per, "%A / ", class_n_per, "%NF")),
                  nudge_y = 15 * .2, size = 5) +
        # scale_fill_manual(values = c("#d7191c", "#e24430", "#ed6e43", "#f89957", "#fdba6f",
        #                              "#fed18a", "#fee8a5", "#ffffc0", "#e6f4a7", "#cce98f",
        #                              "#b3de76", "#92cf64", "#6abc58", "#42a94d", "#1a9641")) +
        coord_flip() +
        labs(title = paste0(j, "(", i, ")"), x = "Class of fragment size (ha)", y = "Area (ha) (millions)") +
        # scale_y_continuous(breaks = c(0, 5e6, 1e7, 1.5e7, 2e7),
        #                    label = c("0", "5", "10", "15", "20")) +
        ylim(c(0, 17.5)) +
        theme_bw(base_size = 20) +
        theme(legend.position = "none",
              title = element_text(size = 20),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18))

    } else{

      plot_fig01 <- ggplot(data = data_fig01_i_j,
                           aes(x = class, y = class_area/1e6)) +
        geom_bar(stat = "identity", fill = ifelse(str_detect(j, "Forest"), "#129912", "#ff8800")) +
        geom_text(aes(label = paste0(class_area_per, "%A / ", class_n_per, "%NF")),
                  nudge_y = 43 * .2, size = 5) +
        # scale_fill_manual(values = c("#d7191c", "#e24430", "#ed6e43", "#f89957", "#fdba6f",
        #                              "#fed18a", "#fee8a5", "#ffffc0", "#e6f4a7", "#cce98f",
        #                              "#b3de76", "#92cf64", "#6abc58", "#42a94d", "#1a9641")) +
        coord_flip() +
        labs(title = paste0(j, "(", i, ")"), x = "Class of fragment size (ha)", y = "Area (ha) (millions)") +
        ylim(c(0, 45)) +
        theme_bw(base_size = 20) +
        theme(legend.position = "none",
              title = element_text(size = 20),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18))

    }

    # export
    ggsave(filename = paste0("04_figures/fig01/fig01_", i, "_", gsub("[()]", "", gsub(" ", "_", tolower(j))), ".png"),
           plot = plot_fig01, wi = 25, he = 20, un = "cm", dpi = 300)

  }

}

# fig 03 ------------------------------------------------------------------

## data ----
data_fig03 <- readr::read_csv("03_tables/data_fig03.csv") %>%
  dplyr::mutate(year = forcats::as_factor(year))
data_fig03

### plot ----
plot_fig03a <- ggplot(data = data_fig03,
                     aes(x = year, y = n_patches, color = scenario,
                         fill = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario), linewidth = 2) +
  geom_point(size = 4, shape = 21, stroke = 1.5) +
  geom_vline(xintercept = 5.2, color = "gray66", linewidth = 1.5, lty = 1) +
  geom_vline(xintercept = 5.8, color = "gray66", linewidth = 1.5, lty = 1) +
  geom_vline(xintercept = 6.4, color = "gray66", linewidth = 1.5, lty = 1) +
  scale_color_manual(values = c("#129912", "#129912", "#ff8800", "#ff8800")) +
  scale_fill_manual(values = c("#129912", "gray30", "#ff8800", "gray30")) +
  scale_linetype_manual(values = c("solid", "dotted", "solid", "dotted")) +
  scale_y_continuous(breaks = c(2e6, 2.1e6, 2.2e6, 2.3e6), label = c("2.0", "2.1", "2.2", "2.3")) +
  annotate(geom = "text", x = 5, y = 2215000, label = "Atlantic Forest Laws", size = 6, angle = 90) +
  annotate(geom = "text", x = 5.6, y = 2225000, label = "Pact for the Restoration", size = 6, angle = 90) +
  annotate(geom = "text", x = 6.2, y = 2250000, label = "Native Veg. Protection Law", size = 6, angle = 90) +
  annotate(geom = "text", x = 1, y = 2350000, label = "a", size = 10, fontface = "bold") +
  labs(x = "Year", y = "Number of fragments (millions)", color = "Scenarios", fill = "Scenarios", linetype = "Scenarios") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.33, .87),
        legend.title = element_blank(),
        legend.text = element_text(size = 15))
plot_fig03a
ggsave("04_figures/fig03a.png",
       plot_fig03a, wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig03b <- ggplot(data = data_fig03,
                      aes(x = year, y = mean_area_ha, color = scenario,
                          fill = scenario, group = scenario)) +
  geom_line(aes(linetype = scenario), linewidth = 2) +
  geom_point(size = 4, shape = 21, stroke = 1.5) +
  geom_vline(xintercept = 5.2, color = "gray66", linewidth = 1.5, lty = 1) +
  geom_vline(xintercept = 5.8, color = "gray66", linewidth = 1.5, lty = 1) +
  geom_vline(xintercept = 6.4, color = "gray66", linewidth = 1.5, lty = 1) +
  scale_color_manual(values = c("#129912", "#129912", "#ff8800", "#ff8800")) +
  scale_fill_manual(values = c("#129912", "gray30", "#ff8800", "gray30")) +
  scale_linetype_manual(values = c("solid", "dotted", "solid", "dotted")) +
  annotate(geom = "text", x = 5, y = 22.5, label = "Atlantic Forest Laws", size = 6, angle = 90) +
  annotate(geom = "text", x = 5.6, y = 23, label = "Pact for the Restoration", size = 6, angle = 90) +
  annotate(geom = "text", x = 6.2, y = 23, label = "Native Veg. Protection Law", size = 5.5, angle = 90) +
  annotate(geom = "text", x = 1, y = 35, label = "b", size = 10, fontface = "bold") +
  labs(x = "Year", y = "Average size of fragments (ha)",
       color = "Scenarios", fill = "Scenarios", linetype = "Scenarios") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.34, .87),
        legend.title = element_blank(),
        legend.text = element_text(size = 15))
plot_fig03b
ggsave("04_figures/fig03b.png",
       plot_fig03b, wi = 25, he = 20, un = "cm", dpi = 300)

# 2 habitat cover ---------------------------------------------------------

### data total ----
data_fig04 <- readr::read_csv("03_tables/data_fig04.csv") %>%
  dplyr::mutate(year = forcats::as_factor(year))
data_fig04

### plot total ----
plot_fig04_forest_not_trimmed <- data_fig04 %>%
  dplyr::filter(classes != 0,
                scenario == "Forest vegetation (not trimmed)") %>%
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
  labs(title = "Forest vegetation (not trimmed)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 10))
plot_fig04_forest_not_trimmed
ggsave(paste0("04_figures/fig04_forest_not_trimmed.png"),
       plot_fig04_forest_not_trimmed, wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig04_forest_trimmed <- data_fig04 %>%
  dplyr::filter(classes != 0,
                scenario == "Forest vegetation (trimmed)") %>%
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
  labs(title = "Forest vegetation (trimmed)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 10))
plot_fig04_forest_trimmed
ggsave(paste0("04_figures/fig04_forest_trimmed.png"),
       plot_fig04_forest_trimmed, wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig04_natural_not_trimmed <- data_fig04 %>%
  dplyr::filter(classes != 0,
                scenario == "Natural vegetation (not trimmed)") %>%
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
  labs(title = "Natural vegetation (not trimmed)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10))
plot_fig04_natural_not_trimmed
ggsave(paste0("04_figures/fig04_natural_not_trimmed.png"),
       plot_fig04_natural_not_trimmed, wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig04_natural_trimmed <- data_fig04 %>%
  dplyr::filter(classes != 0,
                scenario == "Natural vegetation (trimmed)") %>%
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
  labs(title = "Natural vegetation (trimmed)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10))
plot_fig04_natural_trimmed
ggsave(paste0("04_figures/fig04_natural_trimmed.png"),
       plot_fig04_natural_trimmed, wi = 25, he = 20, un = "cm", dpi = 300)

### data brazil ----
data_fig04_br <- readr::read_csv("03_tables/data_fig04_br.csv") %>%
  dplyr::mutate(year = forcats::as_factor(year))
data_fig04_br

### plot brazil ----
plot_fig04_forest_not_trimmed_br <- data_fig04_br %>%
  dplyr::filter(classes != 0,
                scenario == "Forest vegetation (not trimmed)") %>%
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
  labs(title = "Forest vegetation (not trimmed) (Brazil)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 10))
plot_fig04_forest_not_trimmed_br
ggsave(paste0("04_figures/fig04_forest_not_trimmed_br.png"),
       plot_fig04_forest_not_trimmed_br, wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig04_forest_trimmed_br <- data_fig04_br %>%
  dplyr::filter(classes != 0,
                scenario == "Forest vegetation (trimmed)") %>%
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
  labs(title = "Forest vegetation (trimmed) (Brazil)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 10))
plot_fig04_forest_trimmed_br
ggsave(paste0("04_figures/fig04_forest_trimmed_br.png"),
       plot_fig04_forest_trimmed_br, wi = 25, he = 20, un = "cm", dpi = 300)


plot_fig04_natural_not_trimmed_br <- data_fig04_br %>%
  dplyr::filter(classes != 0,
                scenario == "Natural vegetation (not trimmed)") %>%
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
  labs(title = "Natural vegetation (not trimmed) (Brazil)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10))
plot_fig04_natural_not_trimmed_br
ggsave(paste0("04_figures/fig04_natural_not_trimmed_br.png"),
       plot_fig04_natural_not_trimmed_br, wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig04_natural_not_trimmed_br <- data_fig04 %>%
  dplyr::filter(classes != 0,
                scenario == "Natural vegetation (trimmed)") %>%
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
  labs(title = "Natural vegetation (trimmed) (Brazil)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10))
plot_fig04_natural_not_trimmed_br
ggsave(paste0("04_figures/fig04_natural_not_trimmed_br.png"),
       plot_fig04_natural_not_trimmed_br, wi = 25, he = 20, un = "cm", dpi = 300)


### data argentina ----
data_fig04_ar <- readr::read_csv("03_tables/data_fig04_ar.csv") %>%
  dplyr::mutate(year = forcats::as_factor(year))
data_fig04_ar

### plot argentina ----
plot_fig04_forest_not_trimmed_ar <- data_fig04_ar %>%
  dplyr::filter(classes != 0,
                scenario == "Forest vegetation (not trimmed)") %>%
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
  labs(title = "Forest vegetation (not trimmed) (Argentina)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 10))
plot_fig04_forest_not_trimmed_ar
ggsave(paste0("04_figures/fig04_forest_not_trimmed_ar.png"),
       plot_fig04_forest_not_trimmed_ar, wi = 25, he = 20, un = "cm", dpi = 300)


plot_fig04_forest_trimmed_ar <- data_fig04_ar %>%
  dplyr::filter(classes != 0,
                scenario == "Forest vegetation (trimmed)") %>%
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
  labs(title = "Forest vegetation (trimmed) (Argentina)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 10))
plot_fig04_forest_not_trimmed_ar
ggsave(paste0("04_figures/fig04_forest_not_trimmed_ar.png"),
       plot_fig04_forest_not_trimmed_ar, wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig04_natural_not_trimmed_ar <- data_fig04_ar %>%
  dplyr::filter(classes != 0,
                scenario == "Natural vegetation (not trimmed)") %>%
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
  labs(title = "Natural vegetation (not trimmed) (Argentina)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10))
plot_fig04_natural_not_trimmed_ar
ggsave(paste0("04_figures/fig04_natural_not_trimmed_ar.png"),
       plot_fig04_natural_not_trimmed_ar, wi = 25, he = 20, un = "cm", dpi = 300)


plot_fig04_natural_trimmed_ar <- data_fig04_ar %>%
  dplyr::filter(classes != 0,
                scenario == "Natural vegetation (trimmed)") %>%
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
  labs(title = "Natural vegetation (trimmed) (Argentina)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10))
plot_fig04_natural_trimmed_ar
ggsave(paste0("04_figures/fig04_natural_trimmed_ar.png"),
       plot_fig04_natural_trimmed_ar, wi = 25, he = 20, un = "cm", dpi = 300)

### data paraguay ----
data_fig04_py <- readr::read_csv("03_tables/data_fig04_py.csv") %>%
  dplyr::mutate(year = forcats::as_factor(year))
data_fig04_py

### plot paraguay ----
plot_fig04_forest_not_trimmed_py <- data_fig04_py %>%
  dplyr::filter(classes != 0,
                scenario == "Forest vegetation (not trimmed)") %>%
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
  labs(title = "Forest vegetation (not trimmed) (Paraguay)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 10))
plot_fig04_forest_not_trimmed_py
ggsave(paste0("04_figures/fig04_forest_not_trimmed_py.png"),
       plot_fig04_forest_not_trimmed_py, wi = 25, he = 20, un = "cm", dpi = 300)


plot_fig04_forest_trimmed_py <- data_fig04_py %>%
  dplyr::filter(classes != 0,
                scenario == "Forest vegetation (trimmed)") %>%
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
  labs(title = "Forest vegetation (trimmed) (Paraguay)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 10))
plot_fig04_forest_trimmed_py
ggsave(paste0("04_figures/fig04_forest_trimmed_py.png"),
       plot_fig04_forest_trimmed_py, wi = 25, he = 20, un = "cm", dpi = 300)


plot_fig04_natural_not_trimmed_py <- data_fig04_py %>%
  dplyr::filter(classes != 0,
                scenario == "Natural vegetation (not trimmed)") %>%
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
  labs(title = "Natural vegetation (not trimmed) (Paraguay)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10))
plot_fig04_natural_not_trimmed_py
ggsave(paste0("04_figures/fig04_natural_not_trimmed_py.png"),
       plot_fig04_natural_not_trimmed_py, wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig04_natural_trimmed_py <- data_fig04_py %>%
  dplyr::filter(classes != 0,
                scenario == "Natural vegetation (trimmed)") %>%
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
  labs(title = "Natural vegetation (trimmed) (Paraguay)", x = "Year", y = "Percentage (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10))
plot_fig04_natural_trimmed_py
ggsave(paste0("04_figures/fig04_natural_trimmed_py.png"),
       plot_fig04_natural_trimmed_py,
       wi = 25, he = 20, un = "cm", dpi = 300)

# 3  core and edge area ---------------------------------------------------

### data ----
data_fig05 <- readr::read_csv("03_tables/data_fig05.csv") %>%
  dplyr::mutate(year = forcats::as_factor(year))
data_fig05

data_fig05_class <- data_fig05 %>%
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
data_fig05_class

### plot ----
plot_fig05_forest_not_trimmed <- data_fig05 %>%
  dplyr::filter(scenario == "Forest vegetation (not trimmed)") %>%
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
  labs(title = "Forest vegetation (not trimmed)",
       x = "Edge distance (m) log10", y = "Cumulative area (%)", color = "Year") +
  ylim(15, 100) +
  theme_bw(base_size = 20) +
  theme(title = element_text(size = 20),
        legend.position = c(.9, .25))
ggsave(filename = "04_figures/fig05_forest_not_trimmed.png",
       plot = plot_fig05_forest_not_trimmed,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig05_forest_trimmed <- data_fig05 %>%
  dplyr::filter(scenario == "Forest vegetation (trimmed)") %>%
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
  labs(title = "Forest vegetation (trimmed)",
       x = "Edge distance (m) log10", y = "Cumulative area (%)", color = "Year") +
  ylim(15, 100) +
  theme_bw(base_size = 20) +
  theme(title = element_text(size = 20),
        legend.position = c(.9, .25))
ggsave(filename = "04_figures/fig05_forest_trimmed.png",
       plot = plot_fig05_forest_trimmed,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig05_natural_not_trimmed <- data_fig05 %>%
  dplyr::filter(scenario == "Natural vegetation (not trimmed)") %>%
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
  labs(title = "Natural vegetation (not trimmed)",
       x = "Edge distance (m) log10", y = "Cumulative area (%)", color = "Year") +
  theme_bw(base_size = 20) +
  theme(title = element_text(size = 20),
        legend.position = c(.9, .25))
ggsave(filename = "04_figures/fig05_natural_not_trimmed.png",
       plot = plot_fig05_natural_not_trimmed,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig05_natural_trimmed <- data_fig05 %>%
  dplyr::filter(scenario == "Natural vegetation (trimmed)") %>%
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
  labs(title = "Natural vegetation (trimmed)",
       x = "Edge distance (m) log10", y = "Cumulative area (%)", color = "Year") +
  theme_bw(base_size = 20) +
  theme(title = element_text(size = 20),
        legend.position = c(.9, .25))
ggsave(filename = "04_figures/fig05_natural_trimmed.png",
       plot = plot_fig05_natural_trimmed,
       wi = 25, he = 20, un = "cm", dpi = 300)


plot_fig05_forest_not_trimmed_bar <- data_fig05_class %>%
  dplyr::filter(scenario == "Forest vegetation (not trimmed)") %>%
  ggplot(aes(x = class, y = per_total, fill = year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  # geom_text(aes(label = paste0(round(per_total, 2), "%")),
  #           nudge_y = 1, size = 7) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Green-Gold", 8)) +
  annotate(geom = "text", x = 1, y = 30, label = "c", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (not trimmed)",
       x = "Edge distance (m)", y = "Percentage (%)", fill = "Year") +
  theme_bw(base_size = 20) +
  theme(title = element_text(size = 20),
        legend.position = c(.9, .4))
plot_fig05_forest_not_trimmed_bar
ggsave(filename = "04_figures/fig05_forest_not_trimmed_bar.png",
       plot = plot_fig05_forest_not_trimmed_bar,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig05_forest_trimmed_bar <- data_fig05_class %>%
  dplyr::filter(scenario == "Forest vegetation (trimmed)") %>%
  ggplot(aes(x = class, y = per_total, fill = year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  # geom_text(aes(label = paste0(round(per_total, 2), "%")),
  #           nudge_y = 1, size = 7) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Green-Gold", 8)) +
  annotate(geom = "text", x = 1, y = 30, label = "c", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (trimmed)",
       x = "Edge distance (m)", y = "Percentage (%)", fill = "Year") +
  theme_bw(base_size = 20) +
  theme(title = element_text(size = 20),
        legend.position = c(.9, .4))
plot_fig05_forest_trimmed_bar
ggsave(filename = "04_figures/fig05_forest_trimmed_bar.png",
       plot = plot_fig05_forest_trimmed_bar,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig05_natural_not_trimmed_bar <- data_fig05_class %>%
  dplyr::filter(scenario == "Natural vegetation (not trimmed)") %>%
  ggplot(aes(x = class, y = per_total, fill = year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  # geom_text(aes(label = paste0(round(per_total, 2), "%")),
  #           nudge_y = 1, size = 7) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Orange-Gold", 8)) +
  annotate(geom = "text", x = 1, y = 30, label = "d", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (not trimmed)",
       x = "Edge distance (m)", y = "Percentage (%)", fill = "Year") +
  ylim(0, 30) +
  theme_bw(base_size = 20) +
  theme(title = element_text(size = 20),
        legend.position = c(.9, .4))
plot_fig05_natural_not_trimmed_bar
ggsave(filename = "04_figures/fig05_natural_not_trimmed_bar.png",
       plot = plot_fig05_natural_not_trimmed_bar,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig05_natural_trimmed_bar <- data_fig05_class %>%
  dplyr::filter(scenario == "Natural vegetation (trimmed)") %>%
  ggplot(aes(x = class, y = per_total, fill = year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  # geom_text(aes(label = paste0(round(per_total, 2), "%")),
  #           nudge_y = 1, size = 7) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Orange-Gold", 8)) +
  annotate(geom = "text", x = 1, y = 30, label = "d", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (trimmed)",
       x = "Edge distance (m)", y = "Percentage (%)", fill = "Year") +
  ylim(0, 30) +
  theme_bw(base_size = 20) +
  theme(title = element_text(size = 20),
        legend.position = c(.9, .4))
plot_fig05_natural_trimmed_bar
ggsave(filename = "04_figures/fig05_natural_trimmed_bar.png",
       plot = plot_fig05_natural_trimmed_bar,
       wi = 25, he = 20, un = "cm", dpi = 300)

# 4 functional connectivity ---------------------------------------------

### data ----
data_fig06 <- readr::read_csv("03_tables/data_fig06.csv")
data_fig06

### plot ----

# plot expected cluster size
plot_fig06_forest_not_trimmed_mean <- data_fig06 %>%
  dplyr::filter(scenario == "Forest vegetation (not trimmed)") %>%
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
  labs(title = "Forest vegetation (not trimmed)",
       x = "Gap-crossing (m)", y = "Expected cluster size log10(ha)", fill = "Year") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.1, .7),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_fig06_forest_not_trimmed_mean
ggsave(filename = "04_figures/fig06_forest_not_trimmed_mean.png",
       plot = plot_fig06_forest_not_trimmed_mean,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig06_forest_trimmed_mean <- data_fig06 %>%
  dplyr::filter(scenario == "Forest vegetation (trimmed)") %>%
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
  labs(title = "Forest vegetation (trimmed)",
       x = "Gap-crossing (m)", y = "Expected cluster size log10(ha)", fill = "Year") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.1, .7),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_fig06_forest_not_trimmed_mean
ggsave(filename = "04_figures/fig06_forest_not_trimmed_mean.png",
       plot = plot_fig06_forest_not_trimmed_mean,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig06_natural_not_trimmed_mean <- data_fig06 %>%
  dplyr::filter(scenario == "Natural vegetation (not trimmed)") %>%
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
  labs(title = "Natural vegetation (not trimmed)",
       x = "Gap-crossing (m)", y = "Expected cluster size log10(ha)", fill = "Year") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.1, .7),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_fig06_natural_not_trimmed_mean
ggsave(filename = "04_figures/fig06_natural_not_trimmed_mean.png",
       plot = plot_fig06_natural_not_trimmed_mean,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig06_natural_trimmed_mean <- data_fig06 %>%
  dplyr::filter(scenario == "Natural vegetation (trimmed)") %>%
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
  labs(title = "Natural vegetation (trimmed)",
       x = "Gap-crossing (m)", y = "Expected cluster size log10(ha)", fill = "Year") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.1, .7),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_fig06_natural_trimmed_mean
ggsave(filename = "04_figures/fig06_natural_trimmed_mean.png",
       plot = plot_fig06_natural_trimmed_mean,
       wi = 25, he = 20, un = "cm", dpi = 300)


plot_fig06_forest_not_trimmed_high <- data_fig06 %>%
  dplyr::filter(scenario == "Forest vegetation (not trimmed)") %>%
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
  labs(title = "Forest vegetation (not trimmed)",
       x = "Gap-crossing (m)", color = "Year",
       y = "Highest cluster size \n(% of total remaining vegetation)") +
  ylim(0, 100) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.9, .25),
        axis.text.x = element_text(angle = 40, vjust = .7))
plot_fig06_forest_not_trimmed_high
ggsave(filename = "04_figures/fig06_forest_not_trimmed_high.png",
       plot = plot_fig06_forest_not_trimmed_high,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig06_forest_trimmed_high <- data_fig06 %>%
  dplyr::filter(scenario == "Forest vegetation (trimmed)") %>%
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
  labs(title = "Forest vegetation (trimmed)",
       x = "Gap-crossing (m)", color = "Year",
       y = "Highest cluster size \n(% of total remaining vegetation)") +
  ylim(0, 100) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.9, .25),
        axis.text.x = element_text(angle = 40, vjust = .7))
plot_fig06_forest_trimmed_high
ggsave(filename = "04_figures/fig06_forest_trimmed_high.png",
       plot = plot_fig06_forest_trimmed_high,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig06_natural_not_trimmed_high <- data_fig06 %>%
  dplyr::filter(scenario == "Natural vegetation (not trimmed)") %>%
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
  labs(title = "Natural vegetation (not trimmed)",
       x = "Gap-crossing (m)", color = "Year",
       y = "Highest cluster size \n(% of total remaining vegetation)") +
  ylim(0, 100) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.9, .25),
        axis.text.x = element_text(angle = 40, vjust = .7))
plot_fig06_natural_not_trimmed_high
ggsave(filename = "04_figures/fig06_natural_not_trimmed_high.png",
       plot = plot_fig06_natural_not_trimmed_high,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig06_natural_trimmed_high <- data_fig06 %>%
  dplyr::filter(scenario == "Natural vegetation (trimmed)") %>%
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
  labs(title = "Natural vegetation (trimmed)",
       x = "Gap-crossing (m)", color = "Year",
       y = "Highest cluster size \n(% of total remaining vegetation)") +
  ylim(0, 100) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.9, .25),
        axis.text.x = element_text(angle = 40, vjust = .7))
plot_fig06_natural_trimmed_high
ggsave(filename = "04_figures/fig06_natural_trimmed_high.png",
       plot = plot_fig06_natural_trimmed_high,
       wi = 25, he = 20, un = "cm", dpi = 300)

# 5 mean isolation --------------------------------------------------------

### data ----
# import mean data
data_fig07 <- readr::read_csv("03_tables/data_fig07.csv")
data_fig07

### plot ----

# plot
plot_fig07_forest_not_trimmed_mean <- data_fig07 %>%
  dplyr::filter(scenario == "Forest vegetation (not trimmed)") %>%
  ggplot(aes(x = as.factor(as.numeric(area)), y = mean, fill = as.factor(year))) +
  # geom_line(linewidth = .8) +
  # geom_point(size = 7, color = "white") +
  # geom_point(size = 3, alpha = .5) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Green-Gold", 8)) +
  # scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 350, 500),
  #                    labels = as.character(c(0, 50, 100, 150, 200, 350, 500))) +
  labs(title = "Forest vegetation (not trimmed)",
       x = "Smallest fragment size (ha)", y = "Mean isolation (m)", fill = "Year") +
  annotate(geom = "text", x = 1, y = 22500, label = "a", size = 10, fontface = "bold") +
  ylim(0, 23000) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.09, .6),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_fig07_forest_not_trimmed_mean
ggsave(filename = "04_figures/fig07_forest_not_trimmed_mean.png",
       plot = plot_fig07_forest_not_trimmed_mean,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig07_forest_trimmed_mean <- data_fig07 %>%
  dplyr::filter(scenario == "Forest vegetation (trimmed)") %>%
  ggplot(aes(x = as.factor(as.numeric(area)), y = mean, fill = as.factor(year))) +
  # geom_line(linewidth = .8) +
  # geom_point(size = 7, color = "white") +
  # geom_point(size = 3, alpha = .5) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Green-Gold", 8)) +
  # scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 350, 500),
  #                    labels = as.character(c(0, 50, 100, 150, 200, 350, 500))) +
  labs(title = "Forest vegetation (trimmed)",
       x = "Smallest fragment size (ha)", y = "Mean isolation (m)", fill = "Year") +
  annotate(geom = "text", x = 1, y = 22500, label = "a", size = 10, fontface = "bold") +
  ylim(0, 23000) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.09, .6),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_fig07_forest_trimmed_mean
ggsave(filename = "04_figures/fig07_forest_trimmed_mea.png",
       plot = plot_fig07_forest_trimmed_mean,
       wi = 25, he = 20, un = "cm", dpi = 300)


plot_fig07_natural_not_trimmed_mean <- data_fig07 %>%
  dplyr::filter(scenario == "Natural vegetation (not trimmed)") %>%
  ggplot(aes(x = as.factor(as.numeric(area)), y = mean, fill = as.factor(year))) +
  # geom_line(linewidth = .8) +
  # geom_point(size = 7, color = "white") +
  # geom_point(size = 3, alpha = .5) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Orange-Gold", 8)) +
  # scale_x_continuous(breaks = c(0, 30, 90, 210, 360, 510, 1020),
  #                    labels = as.character(c(0, 30, 90, 210, 360, 510, 1020))) +
  labs(title = "Natural vegetation (not trimmed)",
       x = "Smallest fragment size (ha)", y = "Mean isolation (m)", fill = "Year") +
  annotate(geom = "text", x = 1, y = 22500, label = "b", size = 10, fontface = "bold") +
  ylim(0, 23000) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.09, .6),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_fig07_natural_not_trimmed_mean
ggsave(filename = "04_figures/fig07_natural_not_trimmed_mean.png",
       plot = plot_fig07_natural_not_trimmed_mean,
       wi = 25, he = 20, un = "cm", dpi = 300)

plot_fig07_natural_trimmed_mean <- data_fig07 %>%
  dplyr::filter(scenario == "Natural vegetation (trimmed)") %>%
  ggplot(aes(x = as.factor(as.numeric(area)), y = mean, fill = as.factor(year))) +
  # geom_line(linewidth = .8) +
  # geom_point(size = 7, color = "white") +
  # geom_point(size = 3, alpha = .5) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = paletteer::paletteer_c("ggthemes::Orange-Gold", 8)) +
  # scale_x_continuous(breaks = c(0, 30, 90, 210, 360, 510, 1020),
  #                    labels = as.character(c(0, 30, 90, 210, 360, 510, 1020))) +
  labs(title = "Natural vegetation (trimmed)",
       x = "Smallest fragment size (ha)", y = "Mean isolation (m)", fill = "Year") +
  annotate(geom = "text", x = 1, y = 22500, label = "b", size = 10, fontface = "bold") +
  ylim(0, 23000) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.09, .6),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_fig07_natural_trimmed_mean
ggsave(filename = "04_figures/fig07_natural_trimmed_mean.png",
       plot = plot_fig07_natural_trimmed_mean,
       wi = 25, he = 20, un = "cm", dpi = 300)

# 6 protected area and indigenous territories ------------------------------

## protected area ----

### vegetation ----
data_fig08_pa <- readr::read_csv("03_tables/data_fig08_pa.csv") %>%
  dplyr::mutate(class = forcats::as_factor(class))
data_fig08_pa

# plot
plot_fig08_forest_not_trimmed_pa <- data_fig08_pa %>%
  dplyr::filter(scenario == "Forest vegetation (not trimmed)") %>%
  ggplot(aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#129912", rep("#8fba8f", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")),
            nudge_y = 3e5, size = 7) +
  annotate(geom = "text", x = 1, y = 1.05e7, label = "a", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (not trimmed)",
       x = "Protected area distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_fig08_forest_not_trimmed_pa
ggsave("04_figures/fig08_forest_not_trimmed_pa.png",
       plot_fig08_forest_not_trimmed_pa,
       wi = 35, he = 25, un = "cm", dpi = 300)

plot_fig08_forest_trimmed_pa <- data_fig08_pa %>%
  dplyr::filter(scenario == "Forest vegetation (trimmed)") %>%
  ggplot(aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#129912", rep("#8fba8f", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")),
            nudge_y = 3e5, size = 7) +
  annotate(geom = "text", x = 1, y = 1.05e7, label = "a", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (trimmed)",
       x = "Protected area distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_fig08_forest_trimmed_pa
ggsave(filename = "04_figures/fig08_forest_trimmed_pa.png",
       plot = plot_fig08_forest_trimmed_pa,
       wi = 35, he = 25, un = "cm", dpi = 300)

plot_fig08_natural_not_trimmed_pa <- data_fig08_pa %>%
  dplyr::filter(scenario == "Natural vegetation (not trimmed)") %>%
  ggplot(aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#ff8800", rep("#fec882", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")), nudge_y = 3e5, size = 7) +
  annotate(geom = "text", x = 1, y = 1.5e7, label = "b", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (not trimmed)",
       x = "Protected area distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_fig08_natural_not_trimmed_pa
ggsave(filename = "04_figures/fig08_natural_not_trimmed_pa.png",
       plot = plot_fig08_natural_not_trimmed_pa,
       wi = 35, he = 25, un = "cm", dpi = 300)

plot_fig08_natural_trimmed_pa <- data_fig08_pa %>%
  dplyr::filter(scenario == "Natural vegetation (trimmed)") %>%
  ggplot(aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#ff8800", rep("#fec882", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")),
            nudge_y = 3e5, size = 7) +
  annotate(geom = "text", x = 1, y = 1.5e7, label = "b", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (trimmed)",
       x = "Protected area distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_fig08_natural_trimmed_pa
ggsave(filename = "04_figures/fig08_natural_trimmed_pa.png",
       plot = plot_fig08_natural_trimmed_pa,
       wi = 35, he = 25, un = "cm", dpi = 300)

### classes ----

# data
data_fig08_pa_classes <- readr::read_csv("03_tables/data_fig08_classes_pa.csv")
data_fig08_pa_classes

# plot
plot_fig08_not_trimmed_pa_classes <- data_fig08_pa_classes %>%
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
  labs(x = "", y = "Area log10(ha)", title = "Protected area (not trimmed)") +
  theme_bw(base_size = 25) +
  theme(title = element_text(size = 25),
        legend.position = "none")
plot_fig08_not_trimmed_pa_classes
ggsave("04_figures/fig08_not_trimmed_pa_classes.png",
       plot_fig08_not_trimmed_pa_classes,
       wi = 45, he = 25, un = "cm", dpi = 300)

plot_fig08_trimmed_pa_classes <- data_fig08_pa_classes %>%
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
  labs(x = "", y = "Area log10(ha)", title = "Protected area (trimmed)") +
  theme_bw(base_size = 25) +
  theme(legend.position = "none")
plot_fig08_trimmed_pa_classes
ggsave("04_figures/fig08_trimmed_pa_classes.png",
       plot_fig08_trimmed_pa_classes,
       wi = 45, he = 25, un = "cm", dpi = 300)

## indigenous territories ----

### vegetation ----

# import data
data_fig08_it <- readr::read_csv("03_tables/data_fig08_it.csv", col_types = readr::cols()) %>%
  dplyr::mutate(class = forcats::as_factor(class))
data_fig08_it

# plot
plot_fig08_forest_not_trimmed_it <- data_fig08_it %>%
  dplyr::filter(scenario == "Forest vegetation (not trimmed)") %>%
  ggplot(aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#129912", rep("#8fba8f", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")), nudge_y = 5e5, size = 7) +
  annotate(geom = "text", x = 1, y = 2e7, label = "c", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (not trimmed)",
       x = "Indigenous territories distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_fig08_forest_not_trimmed_it
ggsave(filename = "04_figures/fig08_forest_not_trimmed_it.png",
       plot = plot_fig08_forest_not_trimmed_it,
       wi = 35, he = 25, un = "cm", dpi = 300)


plot_fig08_forest_trimmed_it <- data_fig08_it %>%
  dplyr::filter(scenario == "Forest vegetation (trimmed)") %>%
  ggplot(aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#129912", rep("#8fba8f", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")), nudge_y = 5e5, size = 7) +
  annotate(geom = "text", x = 1, y = 2e7, label = "c", size = 10, fontface = "bold") +
  labs(title = "Forest vegetation (trimmed)",
       x = "Indigenous territories distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_fig08_forest_trimmed_it
ggsave(filename = "04_figures/fig08_forest_trimmed_it.png",
       plot = plot_fig08_forest_trimmed_it,
       wi = 35, he = 25, un = "cm", dpi = 300)

plot_fig08_natural_trimmed_it <- data_fig08_it %>%
  dplyr::filter(scenario == "Natural vegetation (not trimmed)") %>%
  ggplot(aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#ff8800", rep("#fec882", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")), nudge_y = 1e6, size = 7) +
  annotate(geom = "text", x = 1, y = 3.5e7, label = "d", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (not trimmed)",
       x = "Indigenous territories distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_fig08_natural_trimmed_it
ggsave(filename = "04_figures/fig08_natural_trimmed_it.png",
       plot = plot_fig08_natural_trimmed_it,
       wi = 35, he = 25, un = "cm", dpi = 300)

plot_fig08_natural_trimmed_it <- data_fig08_it %>%
  dplyr::filter(scenario == "Natural vegetation (trimmed)") %>%
  ggplot(aes(x = class, y = area_ha_total)) +
  geom_bar(stat = "identity", fill = c("#ff8800", rep("#fec882", 10))) +
  geom_text(aes(label = paste0(n_total_per, "%")), nudge_y = 1e6, size = 7) +
  annotate(geom = "text", x = 1, y = 3.5e7, label = "d", size = 10, fontface = "bold") +
  labs(title = "Natural vegetation (trimmed)",
       x = "Indigenous territories distance (m)", y = "Area (ha)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
plot_fig08_natural_trimmed_it
ggsave(filename = "04_figures/fig08_natural_trimmed_it.png",
       plot = plot_fig08_natural_trimmed_it,
       wi = 35, he = 25, un = "cm", dpi = 300)

### classes ----

# data
data_fig08_it_classes <- readr::read_csv("03_tables/data_fig08_classes_it.csv")
data_fig08_it_classes

# plot
plot_indigenous_territories_classes <- data_fig08_it_classes %>%
  dplyr::filter(scenario == "not_trimmed") %>%
  dplyr::mutate(classes = forcats::as_factor(classes)) %>%
  dplyr::mutate(classes = reorder(classes, -area_ha_class_sum)) %>%
  ggplot(aes(x = classes, y = log10(area_ha_class_sum), fill = classes)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(per_protec, "%IT / ", per_class, "%CA")),
            nudge_y = max(log10(data_indigenous_territories_natural_classes_roads_per_total$area_ha_class_sum)) * .16,
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
  labs(x = "", y = "Area log10(ha)", title = "Indigenous territories (not trimmed)") +
  theme_bw(base_size = 25) +
  theme(title = element_text(size = 25),
        legend.position = "none")
plot_indigenous_territories_classes
ggsave(filename = "04_figures/fig06e_plot_indigenous_territories_classes.png",
       plot = plot_indigenous_territories_classes,
       wi = 45, he = 25, un = "cm", dpi = 300)


plot_indigenous_territories_classes_roads <- data_indigenous_territories_classes %>%
  dplyr::filter(scenario == "trimmed") %>%
  dplyr::mutate(classes = forcats::as_factor(classes)) %>%
  dplyr::mutate(classes = reorder(classes, -area_ha_class_sum)) %>%
  ggplot(aes(x = classes, y = log10(area_ha_class_sum), fill = classes)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(per_protec, "%IT / ", per_class, "%CA")),
            nudge_y = max(log10(data_indigenous_territories_natural_classes_roads_per_total$area_ha_class_sum)) * .16,
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
  labs(x = "", y = "Area log10(ha)", title = "Indigenous territories (trimmed)") +
  theme_bw(base_size = 25) +
  theme(legend.position = "none")
plot_indigenous_territories_classes_roads
ggsave(filename = "04_figures/fig06f_plot_indigenous_territories_classes_roads_railways.png",
       plot = plot_indigenous_territories_classes_roads,
       wi = 45, he = 25, un = "cm", dpi = 300)

# end ---------------------------------------------------------------------
