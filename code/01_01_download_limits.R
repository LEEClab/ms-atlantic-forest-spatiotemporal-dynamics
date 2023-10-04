#' ----
#' title: atlantic forest spatiotemporal dynamic - download and prepare limits
#' author: mauricio vancine
#' date: 2022-11-24
#' operational system: gnu/linux - ubuntu - pop_os
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(rnaturalearth)
library(sf)
library(geobr)
library(tmap)

# options
sf::sf_use_s2(FALSE)
options(timeout = 1e5)

# download data -----------------------------------------------------------

# atlantic forest limits
download.file(url = "https://github.com/LEEClab/ATLANTIC-limits/blob/master/limites_integradores_wgs84_v1_1_0.rar",
              destfile = "01_data/01_limits/01_af_limit/limites_integradores_wgs84_v1_1_0.rar", mode = "wb")

system("unrar e 01_data/01_limits/01_af_limit/limites_integradores_wgs84_v1_1_0.rar 01_data/01_limits/01_af_limit/")

geobr::read_biomes(year = 2004) %>%
  dplyr::filter(name_biome == "Mata Atlântica") %>%
  sf::st_transform(4326) %>%
  sf::st_write("01_data/01_limits/01_af_limit/limit_af_ibge_2004_wgs84_geo.shp")

geobr::read_biomes(year = 2019) %>%
  dplyr::filter(name_biome == "Mata Atlântica") %>%
  sf::st_transform(4326) %>%
  sf::st_write("01_data/01_limits/01_af_limit/limit_af_ibge_2019_wgs84_geo.shp")

download.file(url = "https://storage.googleapis.com/teow2016/Ecoregions2017.zip",
              destfile = "01_data/01_limits/00_raw/ecoregions2017.zip", mode = "wb")

unzip(zipfile = "01_data/01_limits/00_raw/ecoregions2017.zip", exdir = "01_data/01_limits/00_raw/")

sf::st_read("01_data/01_limits/00_raw/Ecoregions2017.shp") %>%
  dplyr::filter(BIOME_NAME %in% c("Tropical & Subtropical Moist Broadleaf Forests",
                                  "Mangroves",
                                  "Tropical & Subtropical Dry Broadleaf Forests"),
                REALM == "Neotropic",
                ECO_NAME %in% c("Alto Paraná Atlantic forests",
                                "Araucaria moist forests",
                                "Bahia coastal forests",
                                "Bahia interior forests",
                                "Serra do Mar coastal forests",
                                "Southern Atlantic Brazilian mangroves",
                                "Atlantic Coast restingas",
                                "Brazilian Atlantic dry forests",
                                "Pernambuco interior forests",
                                "Pernambuco coastal forests",
                                "Caatinga Enclaves moist forests")) %>%
  sf::st_write("01_data/01_limits/01_af_limit/limit_af_wwf_terr_ecos_biorregions_2017_gcs_wgs84.shp")

download.file(url = "http://antigo.mma.gov.br/estruturas/202/_arquivos/shape_mata_atlantica_ibge_5milhoes_policonica_sirgas2000shp_202.zip",
              destfile = "01_data/01_limits/00_raw/shape_mata_atlantica_ibge_5milhoes_policonica_sirgas2000shp_202.zip", mode = "wb")

unzip(zipfile = "01_data/01_limits/00_raw/shape_mata_atlantica_ibge_5milhoes_policonica_sirgas2000shp_202.zip",
      exdir = "01_data/01_limits/00_raw/")

sf::st_read("01_data/01_limits/00_raw/shape_mata_atlantica_IBGE_5milhoes_policonica_sirgas2000.shp") %>%
  sf::st_transform(4326) %>%
  sf::st_write("01_data/01_limits/01_af_limit/limit_af_lawaf2006_gcs_wgs84.shp")

# brazil ibge 1:250,000
download.file(url = "https://geoftp.ibge.gov.br/cartas_e_mapas/bases_cartograficas_continuas/bc250/versao2021/geopackage/bc250_2021_11_18.zip",
              destfile = "01_data/01_limits/00_raw/2021_bc_250.zip", mode = "wb")

unzip(zipfile = "01_data/01_limits/00_raw/2021_bc_250.zip", exdir = "01_data/01_limits")

# countries
countries <- rnaturalearth::ne_countries(scale = 10, country = c("Argentina", "Brazil", "Paraguay"), returnclass = "sf")
countries

countries_sa <- rnaturalearth::ne_countries(scale = 10, continent = "South America", returnclass = "sf") %>%
  sf::st_union(rnaturalearth::ne_countries(scale = 10, country = "France", returnclass = "sf")) %>%
  sf::st_crop(rnaturalearth::ne_countries(continent = "South America", returnclass = "sf")) %>%
  sf::st_as_sf()
countries_sa

# prepare limits ----------------------------------------------------------

# import af limit - after manual edition with brazil ibge 1:250,000
af_lim <- sf::st_read("01_data/01_limits/01_af_limit/ma_limite_integrador_muylaert_et_al_2018_wgs84_geodesic_v1_2_0.shp")
af_lim

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

# end ---------------------------------------------------------------------
