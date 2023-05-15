#' ----
#' title: download protected areas
#' author: mauricio vancine
#' date: 2022-11-19
#' operational system: gnu/linux - ubuntu - pop_os
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)

# download data -----------------------------------------------------------

# download
download.file(url = "https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_Nov2022_Public_SA_shp.zip",
              destfile = "01_data/05_protected_areas/00_raw/WDPA_WDOECM_Nov2022_Public_SA_shp.zip", mode = "wb")

# unzip
unzip(zipfile = "01_data/05_protected_areas/00_raw/WDPA_WDOECM_Nov2022_Public_SA_shp.zip",
      exdir = "01_data/05_protected_areas/00_raw")

unzip(zipfile = "01_data/05_protected_areas/00_raw/WDPA_WDOECM_Nov2022_Public_SA_shp_0.zip",
      exdir = "01_data/05_protected_areas/00_raw/WDPA_WDOECM_Nov2022_Public_SA_shp_0")

unzip(zipfile = "01_data/05_protected_areas/00_raw/WDPA_WDOECM_Nov2022_Public_SA_shp_1.zip",
      exdir = "01_data/05_protected_areas/00_raw/WDPA_WDOECM_Nov2022_Public_SA_shp_1")

unzip(zipfile = "01_data/05_protected_areas/00_raw/WDPA_WDOECM_Nov2022_Public_SA_shp_2.zip",
      exdir = "01_data/05_protected_areas/00_raw/WDPA_WDOECM_Nov2022_Public_SA_shp_2")

# end ---------------------------------------------------------------------
