library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)



### bring in animal logs for VF all

path <- "W:/VF/Pinnaroo 2022/animal_log/jax_working/all_data_R/"
path_spatial <- "W:/VF/Pinnaroo 2022/Spatial/VF/VF_display_modified/"
  



  




animal_GPS_data <- read_csv(paste0(path, "trial_csiro_pinnaroo_mob_273_angus_heifers_filtered_fenceID_local_time.csv"), 
                           col_types = 
                             cols(
                               timeOfEvent                = col_datetime(format = "%d/%m/%Y %H:%M"),
                               local_time               = col_datetime(format = "%d/%m/%Y %H:%M"),
                               EST                      = col_datetime(format = "%d/%m/%Y %H:%M")
                               ))
 

animal_GPS_data <- animal_GPS_data %>% 
  rename ("local_time_excel" = "local_time",
          "EST_excel" = "EST")
str(animal_GPS_data)


animal_GPS_data <- animal_GPS_data %>% 
  mutate(GMT = ymd_hms(timeOfEvent, tz = "GMT"))

animal_GPS_data <- animal_GPS_data %>% 
  mutate(local_time = with_tz(GMT, tz = "Australia/Adelaide"))


names(animal_GPS_data)
animal_GPS_data <- animal_GPS_data %>% 
  dplyr::select(ID_jaxs:Jax_fence_ID, GMT:local_time)

### filter data between two dates start and end of trial

animal_GPS_data <- animal_GPS_data %>%  filter(local_time >= ymd_hms("2022-07-20 15:21:00", tz= "Australia/Adelaide"), #yyy-mm-dd hh:mm:ss
                                               local_time <=  ymd_hms("2022-07-29 06:30:00", tz= "Australia/Adelaide"))

animal_GPS_data <- animal_GPS_data %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))


start_of_trial  <-  yday(ymd_hms("2022-07-20 15:21:00", tz= "Australia/Adelaide")) 
vf1_area_start  <-  yday(ymd_hms("2022-07-20 15:21:00", tz= "Australia/Adelaide")) 
vf2_area_start  <-  yday(ymd_hms("2022-07-22 11:47:00", tz= "Australia/Adelaide"))
vf3_area_start  <-  yday(ymd_hms("2022-07-22 13:43:00", tz= "Australia/Adelaide"))
vf4_area_start  <-  yday(ymd_hms("2022-07-25 10:02:00", tz= "Australia/Adelaide"))
vf5_area_start  <-  yday(ymd_hms("2022-07-26 10:57:00", tz= "Australia/Adelaide"))
vf6_area_start  <-  yday(ymd_hms("2022-07-27 11:58:00", tz= "Australia/Adelaide"))
deactivation    <-  yday(ymd_hms("2022-07-29 06:30:00", tz= "Australia/Adelaide"))

#time the fence were activated and deactivated

Fence_activation_time <- animal_GPS_data %>% 
  distinct(Jax_fence_ID)

Fence_activation_time <-Fence_activation_time %>% 
  mutate(start_fence = c(
    as.POSIXct(ymd_hms("2022-07-20 15:21:00", tz= "Australia/Adelaide")), 
    as.POSIXct(ymd_hms("2022-07-22 11:47:00", tz= "Australia/Adelaide")),
    as.POSIXct(ymd_hms("2022-07-22 13:43:00", tz= "Australia/Adelaide")),
    as.POSIXct(ymd_hms("2022-07-25 10:02:00", tz= "Australia/Adelaide")),
    as.POSIXct(ymd_hms("2022-07-26 10:57:00", tz= "Australia/Adelaide")),
    as.POSIXct(ymd_hms("2022-07-27 11:58:00", tz= "Australia/Adelaide"))
  ))
Fence_activation_time <-Fence_activation_time %>% 
  mutate(end_fence = c(
    as.POSIXct(ymd_hms("2022-07-22 11:47:00", tz= "Australia/Adelaide")),
    as.POSIXct(ymd_hms("2022-07-22 13:43:00", tz= "Australia/Adelaide")),
    as.POSIXct(ymd_hms("2022-07-25 10:02:00", tz= "Australia/Adelaide")),
    as.POSIXct(ymd_hms("2022-07-26 10:57:00", tz= "Australia/Adelaide")),
    as.POSIXct(ymd_hms("2022-07-27 11:58:00", tz= "Australia/Adelaide")),
    as.POSIXct(ymd_hms("2022-07-29 06:30:00", tz= "Australia/Adelaide"))
  ))   
  
Fence_activation_time <-Fence_activation_time %>% 
  mutate(grazing_time_hours = difftime(end_fence, start_fence , units="hours"))

str(Fence_activation_time)



### append the activation - grazing time data to the animal log

animal_GPS_data <- 
  left_join(animal_GPS_data, Fence_activation_time)
  
rm(Fence_activation_time)

############################################################################################
############                  Turn into spatial data          ##############################
############################################################################################
#str(animal_GPS_data)

## remove null values in coodinates
animal_GPS_data <- animal_GPS_data %>% 
  filter(!is.na(gpsData.lng))

#turn into spatial data
animal_GPS_data_sf <-
  st_as_sf(animal_GPS_data,
           coords = c("gpsData.lng", "gpsData.lat"),
           crs = 4326,
           agr = "constant")

animal_GPS_data_sf_trans <-
  st_transform(animal_GPS_data_sf, crs = 28354)


rm(animal_GPS_data,animal_GPS_data_sf )


############################################################################################
############                  bring in boundaries             ##############################
############################################################################################



pinnaroo_Vf_area_hard_fence_bound <- st_read(paste0(path_spatial, "pinnaroo_VF_boundary_dGPS_proj.shp"))  # this is the hard fences

pinnaroo_paddock_area <-             st_read("W:/VF/Pinnaroo 2022/Spatial/pinnaroo_boundary_dGPS_proj.shp")
pinnaroo_Vf_area <-                  st_read("W:/VF/Pinnaroo 2022/Spatial/VF/VF_display_modified/all_VF.shp")

str(pinnaroo_Vf_area_hard_fence_bound)
pinnaroo_Vf_area_hard_fence_bound %>%  distinct(Name)
pinnaroo_Vf_area_hard_fence_bound<- 
  pinnaroo_Vf_area_hard_fence_bound %>%
  filter(Name== "VF")


plot(pinnaroo_Vf_area_hard_fence_bound)
plot(pinnaroo_Vf_area)
plot(pinnaroo_paddock_area)

pinnaroo_Vf_area_hard_fence_bound <-
  st_transform(pinnaroo_Vf_area_hard_fence_bound, crs = 28354)

pinnaroo_paddock_area <-
  st_transform(pinnaroo_paddock_area, crs = 28354)
pinnaroo_Vf_area <-
  st_transform(pinnaroo_Vf_area, crs = 28354)




pinnaroo_Vf_area <- pinnaroo_Vf_area %>% 
  dplyr::arrange(Jax_Fence_)

pinnaroo_Vf_area <-pinnaroo_Vf_area %>% 
  mutate(start_fence = c(
    as.POSIXct(ymd_hms("2022-07-20 15:21:00", tz= "Australia/Adelaide")), 
    as.POSIXct(ymd_hms("2022-07-22 11:47:00", tz= "Australia/Adelaide")),
    as.POSIXct(ymd_hms("2022-07-22 13:43:00", tz= "Australia/Adelaide")),
    as.POSIXct(ymd_hms("2022-07-25 10:02:00", tz= "Australia/Adelaide")),
    as.POSIXct(ymd_hms("2022-07-26 10:57:00", tz= "Australia/Adelaide")),
    as.POSIXct(ymd_hms("2022-07-27 11:58:00", tz= "Australia/Adelaide"))
  ))
pinnaroo_Vf_area <-pinnaroo_Vf_area %>% 
  mutate(end_fence = c(
    as.POSIXct(ymd_hms("2022-07-22 11:47:00", tz= "Australia/Adelaide")),
    as.POSIXct(ymd_hms("2022-07-22 13:43:00", tz= "Australia/Adelaide")),
    as.POSIXct(ymd_hms("2022-07-25 10:02:00", tz= "Australia/Adelaide")),
    as.POSIXct(ymd_hms("2022-07-26 10:57:00", tz= "Australia/Adelaide")),
    as.POSIXct(ymd_hms("2022-07-27 11:58:00", tz= "Australia/Adelaide")),
    as.POSIXct(ymd_hms("2022-07-29 06:30:00", tz= "Australia/Adelaide"))
  ))   


str(pinnaroo_Vf_area)


################################################################
###               Clip to the VF hard fences           #########
################################################################

plot(pinnaroo_Vf_area_hard_fence_bound)

#To the large block boundary
animal_GPS_data_sf_trans_clip <-
  st_intersection(animal_GPS_data_sf_trans, pinnaroo_Vf_area_hard_fence_bound)


## remove all the rows that don't have fence ID
#unique(animal_GPS_data_sf_trans_clip$fencesID)
animal_GPS_data_sf_trans_clip <-animal_GPS_data_sf_trans_clip %>% 
  filter(!is.na(fencesID) ) %>% 
  filter(fencesID !=  "NULL")
str(animal_GPS_data_sf_trans_clip)


rm(animal_GPS_data, animal_GPS_data_sf, animal_GPS_data_sf_trans)


########################################################################################################