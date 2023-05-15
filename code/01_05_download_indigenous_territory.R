#' ----
#' title: download indigenous territory
#' author: mauricio vancine
#' date: 2023-04-10
#' operational system: gnu/linux - ubuntu - pop_os
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)

# download data -----------------------------------------------------------

# download
download.file(url = "https://geoserver.funai.gov.br/geoserver/Funai/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Funai:tis_poligonais&CQL_FILTER=dominio_uniao=%27t%27&outputFormat=SHAPE-ZIP",
              destfile = "01_data/06_indigenous_territory/indigenous_territory_brazil_homologada.zip", mode = "wb")

download.file(url = "https://geoserver.funai.gov.br/geoserver/Funai/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Funai:tis_poligonais&CQL_FILTER=dominio_uniao=%27f%27&outputFormat=SHAPE-ZIP",
              destfile = "01_data/06_indigenous_territory/indigenous_territory_brazil_naohomologada.shp", mode = "wb")

download.file(url = "https://opendata.arcgis.com/api/v3/datasets/11048eb48ad44f278af577be33624012_0/downloads/data?format=shp&spatialRefId=4326&where=1%3D1",
              destfile = "01_data/06_indigenous_territory/indigenous_territory_paraguay.zip", mode = "wb")


# unzip
unzip(zipfile = "01_data/04_protected_areas/00_raw/WDPA_WDOECM_Nov2022_Public_SA_shp.zip",
      exdir = "01_data/04_protected_areas/00_raw")

unzip(zipfile = "01_data/04_protected_areas/00_raw/WDPA_WDOECM_Nov2022_Public_SA_shp_0.zip",
      exdir = "01_data/04_protected_areas/00_raw/WDPA_WDOECM_Nov2022_Public_SA_shp_0")

unzip(zipfile = "01_data/06_indigenous_territory/indigenous_territory_paraguay.zip",
      exdir = "01_data/06_indigenous_territory")

# rename
file.rename(from = "01_data/06_indigenous_territory/Comunidades_1.dbf",
            to = "01_data/06_indigenous_territory/indigenous_territory_paraguay_wgs84_geo.dbf")

file.rename(from = "01_data/06_indigenous_territory/Comunidades_1.prj",
            to = "01_data/06_indigenous_territory/indigenous_territory_paraguay_wgs84_geo.prj")

file.rename(from = "01_data/06_indigenous_territory/Comunidades_1.shx",
            to = "01_data/06_indigenous_territory/indigenous_territory_paraguay_wgs84_geo.shx")

file.rename(from = "01_data/06_indigenous_territory/Comunidades_1.shp",
            to = "01_data/06_indigenous_territory/indigenous_territory_paraguay_wgs84_geo.shp")

# delete
unlink(x = "01_data/06_indigenous_territory/Comunidades_1.cpg")
unlink(x = "01_data/06_indigenous_territory/Comunidades_1.xml")

# end ---------------------------------------------------------------------
