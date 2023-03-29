#' ----
#' title: download urban areas
#' author: mauricio vancine
#' date: 2022-11-19
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)

# options
options(timeout = 1e5)

# download data -----------------------------------------------------------

# argentina
download.file(url = "https://dnsg.ign.gob.ar/apps/api/v1/capas-sig/H%C3%A1bitat+e+infraestructura+social/Asentamientos+y+edificios/areas_de_asentamientos_y_edificios_020105/shp",
              destfile = "01_data/04_urban/00_raw/urban_argentina.zip", mode = "wb")

# paraguay
download.file(url = "https://www.ine.gov.py/microdatos/register/CARTOGRAFIA%20DIGITAL%202012%20ZIP/SHAPE/PAIS.ZIP",
              destfile = "01_data/03_roads_rails/00_raw/atlas_paraguay.zip", mode = "wb")

# unzip -------------------------------------------------------------------

# argentina
unzip(zipfile = "01_data/04_urban/00_raw/urban_argentina.zip", exdir = "01_data/04_urban/00_raw")

# paraguay
unzip(zipfile = "01_data/03_roads_rails/00_raw/atlas_paraguay.zip", exdir = "01_data/03_roads_rails/00_raw/")

file.copy(from = paste0("01_data/03_roads_rails/00_raw/atlas_paraguay/Manzanas_Paraguay", c(".dbf", ".prj", ".shp", ".shx")),
          to = paste0("01_data/04_urban/00_raw/Manzanas_Paraguay", c(".dbf", ".prj", ".shp", ".shx")))
