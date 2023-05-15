#' ---
#' title: download fabdem
#' author: mauricio vancine
#' date: 2022-03-21
#' operational system: gnu/linux - ubuntu - pop_os
#' ---

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)

# options
options(timeout = 1e6)

# files
files <- c("S10W040-N00W030_FABDEM_V1-0.zip",
           "S10W050-N00W040_FABDEM_V1-0.zip",
           "S10W060-N00W050_FABDEM_V1-0.zip",
           "S10W070-N00W060_FABDEM_V1-0.zip",
           "S10W080-N00W070_FABDEM_V1-0.zip",

           "S20W040-S10W030_FABDEM_V1-0.zip",
           "S20W050-S10W040_FABDEM_V1-0.zip",
           "S20W060-S10W050_FABDEM_V1-0.zip",
           "S20W070-S10W060_FABDEM_V1-0.zip",
           "S20W080-S10W070_FABDEM_V1-0.zip",

           "S30W040-S20W030_FABDEM_V1-0.zip",
           "S30W050-S20W040_FABDEM_V1-0.zip",
           "S30W060-S20W050_FABDEM_V1-0.zip",
           "S30W070-S20W060_FABDEM_V1-0.zip",
           "S30W080-S20W070_FABDEM_V1-0.zip",

           "S40W040-S30W030_FABDEM_V1-0.zip",
           "S40W050-S30W040_FABDEM_V1-0.zip",
           "S40W060-S30W050_FABDEM_V1-0.zip",
           "S40W070-S30W060_FABDEM_V1-0.zip",
           "S40W080-S30W070_FABDEM_V1-0.zip")
files

# download
script_download <- NULL

for(i in files){

  script_download <- rbind(script_download,
                           paste0("wget -c ", paste0("https://data.bris.ac.uk/datasets/25wfy0f9ukoge2gs7a5mqpq2j7/", i)))

}

script_download

write.table(script_download, "script_download.sh", quote = FALSE, row.names = FALSE, col.names = FALSE)
