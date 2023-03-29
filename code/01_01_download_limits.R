#' ----
#' title: download and prepare limits
#' author: mauricio vancine
#' date: 2022-11-24
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(rnaturalearth)
library(sf)
library(tmap)

# options
sf::sf_use_s2(FALSE)
options(timeout = 1e5)

# download data -----------------------------------------------------------

# atlantic forest limits
# download.file(url = "https://github.com/LEEClab/ATLANTIC-limits/blob/master/limites_integradores_wgs84_v1_1_0.rar",
#               destfile = "01_data/01_limits/01_af_limit/limites_integradores_wgs84_v1_1_0.rar", mode = "wb")
#
# system("unrar e 01_data/01_limits/01_af_limit/limites_integradores_wgs84_v1_1_0.rar 01_data/01_limits/01_af_limit/")

# ibge 2019
# download.file(url = "http://geoftp.ibge.gov.br/informacoes_ambientais/estudos_ambientais/biomas/vetores/Biomas_250mil.zip",
#               destfile = "01_data/01_limits/01_af_limit/Biomas_250mil.zip", mode = "wb")
#
# unzip(zipfile = "01_data/01_limits/01_af_limit/Biomas_250mil.zip",
#       exdir = "01_data/01_limits/01_af_limit")

# ecoregions
# download.file(url = "https://storage.googleapis.com/teow2016/Ecoregions2017.zip",
#               destfile = "01_data/01_limits/02_ecoregions/Ecoregions2017.zip", mode = "wb")
#
# unzip(zipfile = "01_data/01_limits/02_ecoregions/Ecoregions2017.zip",
#       exdir = "01_data/01_limits/02_ecoregions")

# countries
countries <- rnaturalearth::ne_countries(scale = 10, country = c("Argentina", "Brazil", "Paraguay"), returnclass = "sf")
countries

countries_sa <- rnaturalearth::ne_countries(scale = 10, continent = "South America", returnclass = "sf") %>%
  sf::st_union(rnaturalearth::ne_countries(scale = 10, country = "France", returnclass = "sf")) %>%
  sf::st_crop(rnaturalearth::ne_countries(continent = "South America", returnclass = "sf")) %>%
  sf::st_as_sf()
countries_sa

# prepare limits ----------------------------------------------------------

# import af limit
af_lim <- sf::st_read("01_data/01_limits/01_af_limit/ma_limite_integrador_muylaert_et_al_2018_wgs84_geodesic_v1_2_0.shp")
af_lim

# import ecoregions
eco <- sf::st_read("01_data/01_limits/03_ecoregions/Ecoregions2017.shp") %>%
  sf::st_make_valid()
eco

# import states
states <- sf::st_read("01_data/01_limits/02_states/states_ibge_2021_wgs84_geodesic.gpkg") %>%
  sf::st_make_valid() %>%
  sf::st_transform(crs = 4326)
states

# import municipalities
mun <- sf::st_read("01_data/01_limits/02_states/municipalities_ibge_2021_wgs84_geodesic.gpkg") %>%
  sf::st_make_valid() %>%
  sf::st_transform(crs = 4326)
mun

# crop limits -------------------------------------------------------------

# countries
countries_af <- sf::st_intersection(countries, af_lim)
countries_af

tm_shape(countries_af) +
  tm_polygons()

# ecoregions af
eco_af <- sf::st_intersection(eco, af_lim) %>%
  sf::st_cast("MULTIPOLYGON") %>%
  dplyr::select(-id)
eco_af

tm_shape(eco_af) +
  tm_polygons(col = "COLOR")

tm_shape(eco_af) +
  tm_polygons(col = "COLOR_BIO")

# states af
states_af <- sf::st_intersection(states, af_lim)
states_af

states_af %>%
  sf::st_drop_geometry() %>%
  dplyr::pull(nome) %>%
  unique()

# municipalities af
mun_af <- sf::st_intersection(mun, af_lim) %>%
  sf::st_cast("MULTIPOLYGON")
mun_af

# area --------------------------------------------------------------------

# total area ha
area_af <- as.numeric(st_area(af_lim)/1e4)
area_af

# countries area ha
area_af_ar_py <- as.numeric(st_area(countries_af)/1e4)[c(1, 3)]
area_af_ar_py

area_af_br <- area_af - sum(area_af_ar_py)
area_af_br

area_af_br_ar_py <- c(area_af_br, area_af_ar_py)
area_af_br_ar_py

area_per_af_br_ar_py <- area_af_br_ar_py/area_af*100
area_per_af_br_ar_py
sum(area_per_af_br_ar_py)

# brazil municipalities
mun_af %>%
  sf::st_drop_geometry() %>%
  dplyr::pull(geocodigo) %>%
  unique() %>%
  length()

# export ------------------------------------------------------------------

# export
sf::st_write(countries, "01_data/01_limits/02_states/countries_arg_bra_pay_natural_earth_wgs84_geodesic.gpkg", delete_dsn = TRUE)
sf::st_write(countries_sa, "01_data/01_limits/02_states/countries_sa_natural_earth_wgs84_geodesic.gpkg", delete_dsn = TRUE)
sf::st_write(countries_af, "01_data/01_limits/02_states/countries_arg_bra_pay_natural_earth_af_wgs84_geodesic.gpkg", delete_dsn = TRUE)
sf::st_write(mun_af, "01_data/01_limits/02_states/municipalities_ibge_2021_af_wgs84_geodesic.gpkg", delete_dsn = TRUE)
sf::st_write(eco_af, "01_data/01_limits/03_ecoregions/ecoregions_2017_af_wgs84_geodesic.gpkg", delete_dsn = TRUE)

# end ---------------------------------------------------------------------
