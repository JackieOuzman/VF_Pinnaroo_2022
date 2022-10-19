library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)



### bring in animal logs for VF all


path_spatial <- "W:/VF/Pinnaroo 2022/Spatial/VF/VF_display_modified/"




animal_GPS_data <- read_csv("W:/VF/Pinnaroo 2022/animal_log/raw_data_supplied/trial_csiro_pinnaroo_mob_273_angus_heifers_filtered.csv")


#format time and date clm from character to time
animal_GPS_data <-
  animal_GPS_data %>%
  mutate(timeOfEvent = as.POSIXct(timeOfEvent, tz = "GMT", format = "%d/%m/%Y %H:%M"))






animal_GPS_data <- animal_GPS_data %>% 
  mutate(GMT = ymd_hms(timeOfEvent, tz = "GMT"))

animal_GPS_data <- animal_GPS_data %>% 
  mutate(local_time = with_tz(GMT, tz = "Australia/Adelaide"))

str(animal_GPS_data)

names(animal_GPS_data)

## Add a clm for ID_jaxs
animal_GPS_data <- animal_GPS_data %>% 
  dplyr::mutate( ID_jaxs = row_number())

## Add a clm for Jax_fence_ID

animal_GPS_data <- animal_GPS_data %>% 
  dplyr::mutate(
    Jax_fence_ID = case_when(
      fencesID == "1a459" ~ "1",
      fencesID == "1a2b9" ~ "2",
      fencesID == "14143" ~ "3",
      fencesID == "1b424" ~ "4",
      fencesID == "18711" ~ "5",
      fencesID == "1cdd8" ~ "6",
      fencesID == "NULL" ~ "no fence activite",
      TRUE                      ~ "check"))


## reorder the clms
animal_GPS_data <- animal_GPS_data %>% 
  dplyr::select(ID_jaxs,Jax_fence_ID, deviceUIDHex:local_time)




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

Fence_activation_time <- Fence_activation_time %>% 
  filter(Jax_fence_ID != "no fence activite")

Fence_activation_time <- Fence_activation_time %>% 
  arrange(Jax_fence_ID)
## _______________________________--------------------------------------________________________###

#### having trouble here ###########



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
  mutate(grazing_time_hours_for_VF = difftime(end_fence, start_fence , units="hours"))

str(Fence_activation_time)

### add clm for the start of the trial
Fence_activation_time <-Fence_activation_time %>% 
  mutate(start_trial =  as.POSIXct(ymd_hms("2022-07-20 15:21:00", tz= "Australia/Adelaide"))) 

str(Fence_activation_time)


### append the activation - grazing time data to the animal log

animal_GPS_data <- 
  left_join(animal_GPS_data, Fence_activation_time)
 
animal_GPS_data <- animal_GPS_data %>% 
  mutate(graz_hours_frm_start_trial_to_log = difftime(local_time, start_trial , units="hours"))


 
rm(Fence_activation_time)
str(animal_GPS_data)





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



pinnaroo_Vf_area <- pinnaroo_Vf_area %>% 
  rename("Jax_fence_ID"  = "Jax_Fence_")

#Write out files "W:/VF/Pinnaroo 2022/animal_log/jax_working_outputs"

output_path <- "W:/VF/Pinnaroo 2022/animal_log/jax_working_outputs"
str(pinnaroo_paddock_area)
str(pinnaroo_Vf_area)
str(animal_GPS_data_sf_trans_clip)


#write_sf(pinnaroo_paddock_area)
st_write(pinnaroo_paddock_area, 
         dsn = output_path, 
         layer = "pinnaroo_paddock_area.shp", 
         driver = "ESRI Shapefile")

st_write(pinnaroo_Vf_area, 
         dsn = output_path, 
         layer = "pinnaroo_Vf_area.shp", 
         driver = "ESRI Shapefile")


############################################################################################################################
### format the aniaml log data so I output the clm with local time and keep time difference cals and have clm for x and y

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(animal_GPS_data_sf_trans_clip))
animal_GPS_data_sf_trans_clip <- as.data.frame(animal_GPS_data_sf_trans_clip)

animal_GPS_data_sf_trans_clip <- animal_GPS_data_sf_trans_clip %>% 
  dplyr::select(-"geometry")


animal_GPS_data_sf_trans_clip <-   cbind(animal_GPS_data_sf_trans_clip,coordinates )
## ensure the date and time clms are outputting and outputting in the correct format.


animal_GPS_data_sf_trans_clip$local_time <-   format(animal_GPS_data_sf_trans_clip$local_time, usetz=TRUE)
animal_GPS_data_sf_trans_clip$GMT        <-   format(animal_GPS_data_sf_trans_clip$GMT, usetz=TRUE)
animal_GPS_data_sf_trans_clip$start_fence <-  format(animal_GPS_data_sf_trans_clip$start_fence, usetz=TRUE)
animal_GPS_data_sf_trans_clip$end_fence    <- format(animal_GPS_data_sf_trans_clip$end_fence, usetz=TRUE)
animal_GPS_data_sf_trans_clip$start_trial    <- format(animal_GPS_data_sf_trans_clip$start_trial, usetz=TRUE)

write.csv(animal_GPS_data_sf_trans_clip, 
          paste0(output_path,"/animal_GPS_data_sf_trans_clip.csv"), 
          row.names=FALSE)




