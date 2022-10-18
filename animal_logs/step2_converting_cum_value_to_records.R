library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


path_output_files <- "W:/VF/Pinnaroo 2022/animal_log/jax_working_outputs/"
VF_animals_logs <- read_csv(paste0(path_output_files,"animal_GPS_data_sf_trans_clip.csv"))


# str(VF_animals_logs)
# str(test)
# test <- VF_animals_logs %>% 
#   arrange(Jax_fence_ID,deviceName , local_time )
# 
# test <- head(test, 20)
# test <- select(test, ID_jaxs, Jax_fence_ID,deviceName , local_time, cumulativeAudioCount)
# 
# 
# #now make a new clm that uses lag
# #lag()
# 
# test <- test %>% 
#   mutate(Audio_values = cumulativeAudioCount - lag(cumulativeAudioCount))


#### I think this works but I need to do it for each fence and animal

# so i will split the data file and join it back together perhaps run as function? loop

#make a list###
str(VF_animals_logs)
list_of_animals_days <- VF_animals_logs %>% 
  group_by(Jax_fence_ID, deviceName) %>% 
  summarise(local_time_max = max(local_time, na.rm = TRUE),
            local_time_min = min(local_time, na.rm = TRUE),
            cumulativeAudioCount_min = min(cumulativeAudioCount, na.rm = TRUE),
            cumulativeAudioCount_max = max(cumulativeAudioCount, na.rm = TRUE)
            ) %>% 
  select(Jax_fence_ID, deviceName, local_time_min, local_time_max , 
         cumulativeAudioCount_min, cumulativeAudioCount_max)
list_of_animals_days <-ungroup(list_of_animals_days)

# ggplot(list_of_animals_days, aes(x = Jax_fence_ID, y = cumulativeAudioCount_max))+
#   geom_col()+
#   facet_wrap(.~ deviceName)+ 
#   labs(
#     x = "collar",
#     y = "max audio",
#     title = paste("Max count of audio of animals per fence")
#   )

list_animals <- VF_animals_logs %>% 
  group_by( deviceName)
     
list_animals <-ungroup(list_animals)


#list_animals <- "1_1390038"
list_animals <- list_of_animals_days %>% 
  select(deviceName) %>% 
  distinct(deviceName)
  #mutate(list_animals = paste0(Jax_fence_ID, "_",deviceName )) %>% 
  #select(list_animals)

list_animals <- list_animals %>% 
  filter(deviceName != "1390310")

#list_animals <- head(list_animals,20)

for (list_animals in list_animals){
  
  
  ##################################################################################################################
  
  #fence_x <- sub("_.*", "", list_animals)
  fence_x <- 1
  deviceName_x <- sub("*._", "", list_animals)
  
  df <- VF_animals_logs %>% 
    filter(Jax_fence_ID == fence_x) %>% 
    filter(deviceName == deviceName_x)
  
  df <- df %>% arrange(local_time)
  
  df <- df %>% 
    mutate(Audio_values = cumulativeAudioCount - lag(cumulativeAudioCount))
  
  df <- df %>% 
    mutate(Shock_values = cumulativeShockCount - lag(cumulativeShockCount))
  
  df <- df %>%  select(ID_jaxs:fencesID ,Audio_values, Shock_values, resting_percentage:Y   )
  
  name <- paste0(fence_x,"_", deviceName_x)
  assign(name,df)
}
print(list_animals)  
