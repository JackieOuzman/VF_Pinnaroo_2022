library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
install.packages("sf")
library(sf)



### bring in animal logs for VF all

path <- "W:/VF/Pinnaroo 2022/animal_log/jax_working/all_data_R/"
path_spatial <- "W:/VF/Pinnaroo 2022/Spatial/VF/VF_display_modified/"
  "
  
VF_all_animal_logs <- read.csv(paste0(path, "trial_csiro_pinnaroo_mob_273_angus_heifers_filtered_fenceID_local_time.csv"))

str(VF_all_animal_logs)

VF_bounary <- st_read(paste0(path_spatial, "pinnaroo_VF_boundary_dGPS_proj.shp"))
