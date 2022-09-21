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
  



  




control_animal_GPS_data <- read_csv("W:/VF/Pinnaroo 2022/animal_log/raw_data_supplied/trial_csiro_pinnaroo_mob_274_control_mob_filtered.csv", 
                           col_types = 
                             cols(
                               timeOfEvent                = col_datetime(format = "%d/%m/%Y %H:%M")#,
                               #local_time               = col_datetime(format = "%d/%m/%Y %H:%M"),
                               #EST                      = col_datetime(format = "%d/%m/%Y %H:%M")
                               ))
 




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
         start_trial =  as.POSIXct(ymd_hms("2022-07-20 15:30:00", tz= "Australia/Adelaide")), 
         Name = "control",
         POLY_AREA = "NA"
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



# pinnaroo_Vf_area <-pinnaroo_Vf_area %>% 
#   mutate(start_fence = c(
#     as.POSIXct(ymd_hms("2022-07-20 15:21:00", tz= "Australia/Adelaide")), 
#     as.POSIXct(ymd_hms("2022-07-22 11:47:00", tz= "Australia/Adelaide")),
#     as.POSIXct(ymd_hms("2022-07-22 13:43:00", tz= "Australia/Adelaide")),
#     as.POSIXct(ymd_hms("2022-07-25 10:02:00", tz= "Australia/Adelaide")),
#     as.POSIXct(ymd_hms("2022-07-26 10:57:00", tz= "Australia/Adelaide")),
#     as.POSIXct(ymd_hms("2022-07-27 11:58:00", tz= "Australia/Adelaide"))
#   ))
# pinnaroo_Vf_area <-pinnaroo_Vf_area %>% 
#   mutate(end_fence = c(
#     as.POSIXct(ymd_hms("2022-07-22 11:47:00", tz= "Australia/Adelaide")),
#     as.POSIXct(ymd_hms("2022-07-22 13:43:00", tz= "Australia/Adelaide")),
#     as.POSIXct(ymd_hms("2022-07-25 10:02:00", tz= "Australia/Adelaide")),
#     as.POSIXct(ymd_hms("2022-07-26 10:57:00", tz= "Australia/Adelaide")),
#     as.POSIXct(ymd_hms("2022-07-27 11:58:00", tz= "Australia/Adelaide")),
#     as.POSIXct(ymd_hms("2022-07-29 06:30:00", tz= "Australia/Adelaide"))
#   ))   
# 
# 
# str(pinnaroo_Vf_area)


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

st_write(control_animal_GPS_data_sf_trans_clip, 
         paste0(output_path,"/control_animal_GPS_data_sf_trans_clip.csv"), 
         layer_options = "GEOMETRY=AS_XY")






ggplot() +
  geom_sf(data = pinnaroo_paddock_area, color = "black", fill = NA) +
  #geom_sf(data = pinnaroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = control_animal_GPS_data_sf_trans_clip ,alpha = 0.01) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
  


