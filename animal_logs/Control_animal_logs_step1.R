library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)



### bring in animal logs for VF all

#path <- "W:/VF/Pinnaroo 2022/animal_log/jax_working/all_data_R/"
path_spatial <- "W:/VF/Pinnaroo 2022/Spatial/VF/VF_display_modified/"
  


control_animal_GPS_data <- read_csv("W:/VF/Pinnaroo 2022/animal_log/raw_data_supplied/trial_csiro_pinnaroo_mob_274_control_mob_filtered.csv")


#format time and date clm from character to time
control_animal_GPS_data <-
  control_animal_GPS_data %>%
  mutate(timeOfEvent = as.POSIXct(timeOfEvent, tz = "GMT", format = "%d/%m/%Y %H:%M"))
  




# control_animal_GPS_data <- read_csv("W:/VF/Pinnaroo 2022/animal_log/raw_data_supplied/trial_csiro_pinnaroo_mob_274_control_mob_filtered.csv", 
#                            col_types = 
#                              cols(
#                                timeOfEvent                = col_datetime(format = "%d/%m/%Y %H:%M")#,
#                                #local_time               = col_datetime(format = "%d/%m/%Y %H:%M"),
#                                #EST                      = col_datetime(format = "%d/%m/%Y %H:%M")
#                                ))
#  




control_animal_GPS_data <- control_animal_GPS_data %>% 
  mutate(GMT = ymd_hms(timeOfEvent, tz = "GMT"))

control_animal_GPS_data <- control_animal_GPS_data %>% 
  mutate(local_time = with_tz(GMT, tz = "Australia/Adelaide"))


names(control_animal_GPS_data)
## Add a clm for ID_jaxs
control_animal_GPS_data <- control_animal_GPS_data %>% 
  dplyr::mutate( ID_jaxs = row_number())
## select some clms
control_animal_GPS_data <- control_animal_GPS_data %>% 
  dplyr::select(ID_jaxs, deviceUIDHex: local_time)

### filter data between two dates start and end of trial

control_animal_GPS_data <- control_animal_GPS_data %>%  filter(
  local_time >= ymd_hms("2022-07-20 15:30:00", tz = "Australia/Adelaide"),#yyy-mm-dd hh:mm:ss
  local_time <=  ymd_hms("2022-07-28 07:30:00", tz = "Australia/Adelaide")
)

control_animal_GPS_data <- control_animal_GPS_data %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))


start_of_trial  <-  yday(ymd_hms("2022-07-20 15:30:00", tz= "Australia/Adelaide")) 

end_of_trial    <-  yday(ymd_hms("2022-07-28 07:30:00", tz= "Australia/Adelaide"))



### Add some clm so it matches the VF animal logs 

control_animal_GPS_data <- control_animal_GPS_data %>% 
  mutate(start_fence = "NA",
         end_fence = "NA",
         grazing_time_hours_for_VF = "NA",
         start_trial =  as.POSIXct(ymd_hms("2022-07-20 15:30:00", tz= "Australia/Adelaide")), 
         Jax_fence_ID = "Control"
         )
str(control_animal_GPS_data)
 
control_animal_GPS_data <- control_animal_GPS_data %>% 
  mutate(graz_hours_frm_start_trial_to_log = difftime(local_time, start_trial , units="hours"))


 





############################################################################################
############                  Turn into spatial data          ##############################
############################################################################################
#str(animal_GPS_data)

## remove null values in coodinates
control_animal_GPS_data <- control_animal_GPS_data %>% 
  filter(!is.na(gpsData.lng))

#turn into spatial data
control_animal_GPS_data_sf <-
  st_as_sf(control_animal_GPS_data,
           coords = c("gpsData.lng", "gpsData.lat"),
           crs = 4326,
           agr = "constant")

control_animal_GPS_data_sf_trans <-
  st_transform(control_animal_GPS_data_sf, crs = 28354)


rm(control_animal_GPS_data,control_animal_GPS_data_sf )


############################################################################################
############                  bring in boundaries             ##############################
############################################################################################

### stuck here

# pinnaroo_control_area_hard_fence_bound <- st_read("W:/VF/Pinnaroo 2022/Spatial/VF/VF_display_modified/pinnaroo_VF_boundary_dGPS_proj.shp")  # this is the hard fences

pinnaroo_paddock_area <-             st_read("W:/VF/Pinnaroo 2022/Spatial/pinnaroo_boundary_dGPS_proj.shp")
pinnaroo_control_area <-             st_read("W:/VF/Pinnaroo 2022/Spatial/pinnaroo_boundary_dGPS_proj.shp")


plot(pinnaroo_paddock_area)
plot(pinnaroo_control_area)

pinnaroo_control_area %>%  distinct(Name)

pinnaroo_control_area<- 
  pinnaroo_control_area %>%
  filter(Name == "Control")






pinnaroo_paddock_area <-
  st_transform(pinnaroo_paddock_area, crs = 28354)
pinnaroo_control_area <-
  st_transform(pinnaroo_control_area, crs = 28354)






################################################################
###               Clip to the VF hard fences           #########
################################################################

plot(pinnaroo_control_area)

#To the large block boundary
control_animal_GPS_data_sf_trans_clip <-
  st_intersection(control_animal_GPS_data_sf_trans, pinnaroo_control_area)




rm(control_animal_GPS_data, control_animal_GPS_data_sf, control_animal_GPS_data_sf_trans)


########################################################################################################


output_path <- "W:/VF/Pinnaroo 2022/animal_log/jax_working_outputs"
str(control_animal_GPS_data_sf_trans_clip)

############################################################################################################################
### format the aniaml log data so I output the clm with local time and keep time difference cals and have clm for x and y

## 1.convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(control_animal_GPS_data_sf_trans_clip))
control_animal_GPS_data_sf_trans_clip <- as.data.frame(control_animal_GPS_data_sf_trans_clip)

control_animal_GPS_data_sf_trans_clip <- control_animal_GPS_data_sf_trans_clip %>% 
  dplyr::select(-"geometry")


control_animal_GPS_data_sf_trans_clip <-   cbind(control_animal_GPS_data_sf_trans_clip,coordinates )
## ensure the date and time clms are outputting and outputting in the correct format.


control_animal_GPS_data_sf_trans_clip$local_time <-   format(control_animal_GPS_data_sf_trans_clip$local_time, usetz=TRUE)
control_animal_GPS_data_sf_trans_clip$GMT        <-   format(control_animal_GPS_data_sf_trans_clip$GMT, usetz=TRUE)
control_animal_GPS_data_sf_trans_clip$start_fence <-  format(control_animal_GPS_data_sf_trans_clip$start_fence, usetz=TRUE)
control_animal_GPS_data_sf_trans_clip$end_fence    <- format(control_animal_GPS_data_sf_trans_clip$end_fence, usetz=TRUE)
control_animal_GPS_data_sf_trans_clip$start_trial    <- format(control_animal_GPS_data_sf_trans_clip$start_trial, usetz=TRUE)

write.csv(control_animal_GPS_data_sf_trans_clip, 
          paste0(output_path,"/control_animal_GPS_data_sf_trans_clip.csv"), 
          row.names=FALSE)








