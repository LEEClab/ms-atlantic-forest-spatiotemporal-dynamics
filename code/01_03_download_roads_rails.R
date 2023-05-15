#' ----
#' title: download roads and rails
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
download.file(url = "https://servicos.dnit.gov.br/dnitgeo/geoserver/ows?SERVICE=WFS&VERSION=2.0.0&REQUEST=GetFeature&TYPENAMES=vgeo:vw_snv_rod&SRSNAME=EPSG:4674&OUTPUTFORMAT=shape-zip&CQL_FILTER=dt_ini%3C=%272022-11-18T16:39:22.996Z%27%20AND%20dt_fim%3E=%272022-11-18T16:39:22.996Z%27",
              destfile = "01_data/03_roads_rails/00_raw/roads_brazil_national.zip", mode = "wb")

download.file(url = "https://servicos.dnit.gov.br/dnitgeo/geoserver/ows?SERVICE=WFS&VERSION=2.0.0&REQUEST=GetFeature&TYPENAMES=vgeo:vw_cide_rod_2021&SRSNAME=EPSG:4674&OUTPUTFORMAT=shape-zip&CQL_FILTER=INCLUDE",
              destfile = "01_data/03_roads_rails/00_raw/roads_brazil_state.zip", mode = "wb")

download.file(url = "https://www.gov.br/infraestrutura/pt-br/assuntos/dados-de-transportes/bit/arquivos-bit/Ferrovias.zip",
              destfile = "01_data/03_roads_rails/00_raw/rails_brazil.zip", mode = "wb")

# argentina
download.file(url = "https://dnsg.ign.gob.ar/apps/api/v1/capas-sig/Transporte/Vial/vial_nacional/shp",
              destfile = "01_data/03_roads_rails/00_raw/roads_argentina_national.zip", mode = "wb")

download.file(url = "https://dnsg.ign.gob.ar/apps/api/v1/capas-sig/Transporte/Vial/vial_provincial/shp",
              destfile = "01_data/03_roads_rails/00_raw/roads_argentina_provincial.zip", mode = "wb")

download.file(url = "https://dnsg.ign.gob.ar/apps/api/v1/capas-sig/Transporte/Ferroviario/lineas_de_transporte_ferroviario_AN010/shp",
              destfile = "01_data/03_roads_rails/00_raw/rails_argentina.zip", mode = "wb")

# paraguay
download.file(url = "https://www.ine.gov.py/microdatos/register/CARTOGRAFIA%20DIGITAL%202012%20ZIP/SHAPE/PAIS.ZIP",
              destfile = "01_data/03_roads_rails/00_raw/atlas_paraguay.zip", mode = "wb")

# unzip data --------------------------------------------------------------

# brazil
unzip(zipfile = "01_data/03_roads_rails/00_raw/roads_brazil_national.zip", exdir = "01_data/03_roads_rails/00_raw/")
file.rename(from = dir(path = "01_data/03_roads_rails/00_raw", pattern = "vw_snv_rod", full.names = TRUE),
            to = sub("vw_snv_rod", "roads_brazil_national",
                     dir(path = "01_data/03_roads_rails/00_raw", pattern = "vw_snv_rod", full.names = TRUE)))

unzip(zipfile = "01_data/03_roads_rails/00_raw/roads_brazil_state.zip", exdir = "01_data/03_roads_rails/00_raw/")
file.rename(from = dir(path = "01_data/00_raw/03_roads_rails", pattern = "vw_cide_rod_2021", full.names = TRUE),
            to = sub("vw_cide_rod_2021", "roads_brazil_state",
                     dir(path = "01_data/03_roads_rails/00_raw", pattern = "vw_cide_rod_2021", full.names = TRUE)))

unzip(zipfile = "01_data/03_roads_rails/00_raw/rails_brazil.zip", exdir = "01_data/03_roads_rails/00_raw/")
file.copy(from = dir(path = "01_data/03_roads_rails/00_raw/Ferrovias", full.names = TRUE,
                     pattern = paste0("Ferrovias", c(".dbf", ".prj", ".shp$", ".shx"), collapse = "|")),
          to = "01_data/03_roads_rails/00_raw")
file.rename(from = dir(path = "01_data/03_roads_rails/00_raw", pattern = "Ferrovias", full.names = TRUE),
            to = sub("Ferrovias", "rails_brazil",
                     dir(path = "01_data/03_roads_rails/00_raw", pattern = "Ferrovias", full.names = TRUE)))

# argentina
unzip(zipfile = "01_data/03_roads_rails/00_raw/roads_argentina_national.zip", exdir = "01_data/03_roads_rails/00_raw/")
file.rename(from = dir(path = "01_data/03_roads_rails/00_raw", pattern = "vial_nacional", full.names = TRUE),
            to = sub("vial_nacional", "roads_argentina_national",
                     dir(path = "01_data/03_roads_rails/00_raw", pattern = "vial_nacional", full.names = TRUE)))

unzip(zipfile = "01_data/03_roads_rails/00_raw/roads_argentina_provincial.zip", exdir = "01_data/03_roads_rails/00_raw")
file.rename(from = dir(path = "01_data/03_roads_rails/00_raw", pattern = "vial_provincial", full.names = TRUE),
            to = sub("vial_provincial", "roads_argentina_provincial",
                     dir(path = "01_data/03_roads_rails/00_raw", pattern = "vial_provincial", full.names = TRUE)))

unzip(zipfile = "01_data/03_roads_rails/00_raw/rails_argentina.zip", exdir = "01_data/03_roads_rails/00_raw/")
file.rename(from = dir(path = "01_data/03_roads_rails/00_raw", pattern = "lineas_de_transporte_ferroviario_AN010", full.names = TRUE),
            to = sub("lineas_de_transporte_ferroviario_AN010", "rails_argentina",
                     dir(path = "01_data/03_roads_rails/00_raw", pattern = "lineas_de_transporte_ferroviario_AN010", full.names = TRUE)))

# paraguay
unzip(zipfile = "01_data/03_roads_rails/00_raw/atlas_paraguay.zip", exdir = "01_data/03_roads_rails/00_raw/")
file.copy(from = dir(path = "01_data/03_roads_rails/00_raw/atlas_paraguay/00_raw/", pattern = "Vias principales_Paraguay", full.names = TRUE),
            to = "01_data/03_roads_rails/00_raw")
file.rename(from = dir(path = "01_data/03_roads_rails/00_raw", pattern = "Vias principales_Paraguay", full.names = TRUE),
            to = sub("Vias principales_Paraguay", "roads_paraguay_national",
                     dir(path = "01_data/03_roads_rails/00_raw", pattern = "Vias principales_Paraguay", full.names = TRUE)))

# end ---------------------------------------------------------------------
