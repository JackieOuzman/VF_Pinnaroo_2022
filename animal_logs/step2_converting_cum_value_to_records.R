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


str(VF_animals_logs)
str(test)
test <- VF_animals_logs %>% 
  arrange(Jax_fence_ID,deviceName , local_time )

test <- head(test, 20)
test <- select(test, ID_jaxs, Jax_fence_ID,deviceName , local_time, cumulativeAudioCount)


#now make a new clm that uses lag
#lag()

test <- test %>% 
  mutate(Audio_values = cumulativeAudioCount - lag(cumulativeAudioCount))


#### I think this works but I need to do it for each fence and animal

# so i will split the data file and join it back togther perhaps run as function? loop

#make a list###

for (site_yrs_list in site_yrs_list){
  
  
  ##################################################################################################################
  
  
  df <- VF_animals_logs %>% 
    filter(Jax_fence_ID = fence_x) %>% 
    filter(deviceName = deviceName_x)
  
  df <- df %>% 
    mutate(Audio_values = cumulativeAudioCount - lag(cumulativeAudioCount))
  
  df <- df %>% 
    mutate(Shock_values = cumulativeShockCount - lag(cumulativeShockCount))
  
  name <- paste0(fence_x,"_", deviceName_x)
  assign(name,df)
}
  