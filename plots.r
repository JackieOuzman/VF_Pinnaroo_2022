
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


path_output_files <- "W:/VF/Pinnaroo 2022/animal_log/jax_working_outputs/"


pinnaroo_paddock_area <- st_read(paste0(path_output_files, "pinnaroo_paddock_area.shp"))
pinnaroo_Vf_area <- st_read(paste0(path_output_files, "pinnaroo_Vf_area.shp"))                                

VF_animals_logs <- read_csv(paste0(path_output_files,"animal_GPS_data_sf_trans_clip.csv"))
Control_animals_logs <- read_csv(paste0(path_output_files,"control_animal_GPS_data_sf_trans_clip.csv"))

animal_logs <- rbind(VF_animals_logs,Control_animals_logs )

names(animal_logs)
animal_logs %>%  distinct(Name)


#turn into spatial data
animal_logs_sf <-
  st_as_sf(animal_logs,
           coords = c("X", "Y"),
           crs = 28354,
           agr = "constant")

#fix up names of clms
pinnaroo_Vf_area <- pinnaroo_Vf_area %>% 
  rename("Jax_fence_ID"  = "Jx_f_ID")


#water_pts <- read_csv("W:/VF/Pinnaroo 2022/animal_log/raw_data_supplied/trial_csiro_pinnaroo_mob_203_mobile_water_tank_filtered.csv")
water_pts <- read_csv("W:/VF/Pinnaroo 2022/animal_log/mobile_water_pts_av_proj.csv")
names(water_pts)

#turn into spatial data
water_pts_sf <-
  st_as_sf(water_pts,
           coords = c("POINT_X", "POINT_Y"),
           crs = 28354,
           agr = "constant")



ggplot() +
  geom_sf(data = pinnaroo_paddock_area, color = "black", fill = NA) +
  geom_sf(data = pinnaroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = animal_logs_sf ,alpha = 0.01) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  facet_wrap(. ~ Jax_fence_ID)



### also need to find out where the north water point is.


#### I would like to display the audio and pulse records as another colored dots

# audio_records_only <- VF_animals_logs_sf %>% 
#   filter()