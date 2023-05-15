#' ----
#' title: download mapbiomas
#' author: mauricio vancine
#' date: 2022-11-19
#' operational system: gnu/linux - ubuntu - pop_os
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(rgee)

# options
sf::sf_use_s2(FALSE)
options(timeout = 1e5)

# prepare rgee ------------------------------------------------------------

# install
# rgee::ee_install(py_env = "rgee")
# rgee::ee_clean_pyenv()

# check
rgee::ee_check()

# initialize
rgee::ee_users()
rgee::ee_Initialize(drive = TRUE)

# mapbiomas brazil v7 -----------------------------------------------------

# years
years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
years

# mapbiomas brazil
url <- paste0("https://storage.googleapis.com/mapbiomas-public/brasil/collection-6/lclu/coverage/brasil_coverage_", years, ".tif")
url

destfiles <- paste0("01_data/02_mapbiomas/00_raw/00_mapbiomas_brazil/mapbiomas_v7_", basename(url))
destfiles

# download
purrr::map2(url, destfiles, download.file, mode = "wb")

# mapbiomas trinacional ---------------------------------------------------

# for
for(i in years){

  # import collection
  mapbiomas_trinacional_v2 <- rgee::ee$Image("projects/mapbiomas_af_trinacional/public/collection2/mapbiomas_atlantic_forest_collection20_integration_v1") %>%
    ee$Image$select(paste0("classification_", i))
  mapbiomas_trinacional_v2

  # class
  class(mapbiomas_trinacional_v2)

  # information
  mapbiomas_trinacional_v2$bandNames()$getInfo()

  # export to drive
  task_image <- rgee::ee_image_to_drive(
    image = mapbiomas_trinacional_v2,
    fileNamePrefix = "mapbiomas_trinacional_v2",
    fileFormat = "GEO_TIFF",
    scale = 30,
    maxPixels = 1e13)
  task_image

  task_image$start()
  ee_monitoring(task_image)

  # download
  ee_drive_to_local(task = task_img,
                    dsn = paste0("01_data/02_mapbiomas/00_raw/01_mapbiomas_trinacional/mapbiomas_trinacional_v2_", i, ".tif"))

}

# end ---------------------------------------------------------------------
