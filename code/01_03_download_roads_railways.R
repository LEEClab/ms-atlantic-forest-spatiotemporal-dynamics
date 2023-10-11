#' ----
#' title: atlantic forest spatiotemporal dynamics - download roads and railways
#' author: mauricio vancine
#' date: 2022-11-19
#' operational system: gnu/linux - ubuntu - pop_os
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)

# options
options(timeout = 1e5)

# download data -----------------------------------------------------------

# brazil
download.file(url = "https://geoftp.ibge.gov.br/cartas_e_mapas/bases_cartograficas_continuas/bc250/versao2021/geopackage/bc250_2021_11_18.zip",
              destfile = "01_data/01_limits/00_raw/2021_bc_250.zip", mode = "wb")

# argentina
download.file(url = "https://dnsg.ign.gob.ar/apps/api/v1/capas-sig/Transporte/Vial/vial_nacional/shp",
              destfile = "01_data/03_roads_railways/00_raw/roads_argentina_national.zip", mode = "wb")

download.file(url = "https://dnsg.ign.gob.ar/apps/api/v1/capas-sig/Transporte/Vial/vial_provincial/shp",
              destfile = "01_data/03_roads_railways/00_raw/roads_argentina_provincial.zip", mode = "wb")

download.file(url = "https://dnsg.ign.gob.ar/apps/api/v1/capas-sig/Transporte/Ferroviario/lineas_de_transporte_ferroviario_AN010/shp",
              destfile = "01_data/03_roads_railways/00_raw/railways_argentina.zip", mode = "wb")

# paraguay
download.file(url = "https://www.ine.gov.py/microdatos/register/CARTOGRAFIA%20DIGITAL%202012%20ZIP/SHAPE/PAIS.ZIP",
              destfile = "01_data/03_roads_railways/00_raw/atlas_paraguay.zip", mode = "wb")

# unzip data --------------------------------------------------------------

# brazil
unzip(zipfile = "01_data/01_limits/00_raw/2021_bc_250.zip", exdir = "01_data/01_limits")

# argentina
unzip(zipfile = "01_data/03_roads_railways/00_raw/roads_argentina_national.zip", exdir = "01_data/03_roads_railways/00_raw/")
file.rename(from = dir(path = "01_data/03_roads_railways/00_raw", pattern = "vial_nacional", full.names = TRUE),
            to = sub("vial_nacional", "roads_argentina_national",
                     dir(path = "01_data/03_roads_railways/00_raw", pattern = "vial_nacional", full.names = TRUE)))

unzip(zipfile = "01_data/03_roads_railways/00_raw/roads_argentina_provincial.zip", exdir = "01_data/03_roads_railways/00_raw")
file.rename(from = dir(path = "01_data/03_roads_railways/00_raw", pattern = "vial_provincial", full.names = TRUE),
            to = sub("vial_provincial", "roads_argentina_provincial",
                     dir(path = "01_data/03_roads_railways/00_raw", pattern = "vial_provincial", full.names = TRUE)))

unzip(zipfile = "01_data/03_roads_railways/00_raw/railways_argentina.zip", exdir = "01_data/03_roads_railways/00_raw/")
file.rename(from = dir(path = "01_data/03_roads_railways/00_raw", pattern = "lineas_de_transporte_ferroviario_AN010", full.names = TRUE),
            to = sub("lineas_de_transporte_ferroviario_AN010", "railways_argentina",
                     dir(path = "01_data/03_roads_railways/00_raw", pattern = "lineas_de_transporte_ferroviario_AN010", full.names = TRUE)))

# paraguay
unzip(zipfile = "01_data/03_roads_railways/00_raw/atlas_paraguay.zip", exdir = "01_data/03_roads_railways/00_raw/")
file.copy(from = dir(path = "01_data/03_roads_railways/00_raw/atlas_paraguay/00_raw/", pattern = "Vias principales_Paraguay", full.names = TRUE),
            to = "01_data/03_roads_railways/00_raw")
file.rename(from = dir(path = "01_data/03_roads_railways/00_raw", pattern = "Vias principales_Paraguay", full.names = TRUE),
            to = sub("Vias principales_Paraguay", "roads_paraguay_national",
                     dir(path = "01_data/03_roads_railways/00_raw", pattern = "Vias principales_Paraguay", full.names = TRUE)))

# end ---------------------------------------------------------------------
