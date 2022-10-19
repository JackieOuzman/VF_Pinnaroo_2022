library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


## this code takes the cleaned data and splits it into animals and converts the cumm value into counts

path_output_files <- "W:/VF/Pinnaroo 2022/animal_log/jax_working_outputs/"
VF_animals_logs <- read_csv(paste0(path_output_files,"animal_GPS_data_sf_trans_clip.csv"))


### check on records
check_records <- VF_animals_logs %>% 
  group_by( deviceName,Jax_fence_ID ) %>% 
  summarise(n_AudioCount = sum(cumulativeAudioCount),
            n_ShockCount = sum(cumulativeShockCount)
  )

check_records <- check_records %>% 
  arrange(Jax_fence_ID,deviceName, n_AudioCount )

### check on values
check_min_max <- VF_animals_logs %>% 
  group_by( deviceName,Jax_fence_ID ) %>% 
  summarise(min_AudioCount = min(cumulativeAudioCount,na.rm = TRUE),
            max_AudioCount = max(cumulativeAudioCount,na.rm = TRUE),
            min_ShockCount = sum(cumulativeShockCount,na.rm = TRUE),
            max_ShockCount = max(cumulativeShockCount,na.rm = TRUE)
  )

check_min_max <- check_min_max %>% 
  arrange(Jax_fence_ID,deviceName, min_AudioCount )

### for fence 1 there are 35 cows all with records more than 1 and max value more than 0


#make a list###
str(VF_animals_logs)

#list_animals <- "1_1390038"
list_animals <- VF_animals_logs %>% 
  select(deviceName) %>% 
  distinct(deviceName)
list_animals <-ungroup(list_animals)


list_animals <- list_animals %>% 
  filter(deviceName != "1390310")

test <- list_animals
#list_animals <- head(list_animals,20)

# I can't understand why it falls over?? - it wasn't a list??

list_animals$deviceName[1:34]
list_animals <- c(
1390038, 
1390063, 
1390068, 
1390103, 
1390139, 
1390171, 
1390196, 
1390221, 
1390305,
1390416, 
1390456, 
1390495, 
1390560, 
1390577, 
1390581, 
1390743, 
1390749,
1390775, 
1390826, 
1390832, 
1390858, 
1390889, 
1391048, 
1391200, 
1391209, 
1391211, 
1391234, 
1391339, 
1391387, 
1391502, 
1391505, 
1391517, 
1391842, 
1391796)



for (list_animals in list_animals){
  
  
  ##################################################################################################################
  
  #fence_x <- sub("_.*", "", list_animals)
  fence_x <- 1
  deviceName_x <- sub("*._", "", list_animals)
  
  df <- VF_animals_logs %>% 
    #filter(Jax_fence_ID == fence_x) %>% 
    filter(deviceName == deviceName_x)
  
  df <- df %>% arrange(local_time)
  
  df <- df %>% 
    mutate(Audio_values = cumulativeAudioCount - lag(cumulativeAudioCount))
  
  df <- df %>% 
    mutate(Shock_values = cumulativeShockCount - lag(cumulativeShockCount))
  
  df <- df %>%  select(ID_jaxs:fencesID ,Audio_values, Shock_values, resting_percentage:Y   )
  
  name <- paste0("temp_",deviceName_x)
  assign(name,df)
}

## merge into one file and remove the renaming records



Fence_all <- rbind(
  temp_1390038, 
  temp_1390063, 
  temp_1390068, 
  temp_1390103, 
  temp_1390139, 
  temp_1390171, 
  temp_1390196, 
  temp_1390221, 
  temp_1390305,
  temp_1390416, 
  temp_1390456, 
  temp_1390495, 
  temp_1390560, 
  temp_1390577, 
  temp_1390581, 
  temp_1390743, 
  temp_1390749,
  temp_1390775, 
  temp_1390826, 
  temp_1390832, 
  temp_1390858, 
  temp_1390889, 
  temp_1391048, 
  temp_1391200, 
  temp_1391209, 
  temp_1391211, 
  temp_1391234, 
  temp_1391339, 
  temp_1391387, 
  temp_1391502, 
  temp_1391505, 
  temp_1391517, 
  temp_1391842, 
  temp_1391796)
rm(
  "temp_1390038", 
  "temp_1390063", 
  "temp_1390068", 
  "temp_1390103", 
  "temp_1390139", 
  "temp_1390171", 
  "temp_1390196", 
  "temp_1390221", 
  "temp_1390305",
  "temp_1390416", 
  "temp_1390456", 
  "temp_1390495", 
  "temp_1390560", 
  "temp_1390577", 
  "temp_1390581", 
  "temp_1390743", 
  "temp_1390749",
  "temp_1390775", 
  "temp_1390826", 
  "temp_1390832", 
  "temp_1390858", 
  "temp_1390889", 
  "temp_1391048", 
  "temp_1391200", 
  "temp_1391209", 
  "temp_1391211", 
  "temp_1391234", 
  "temp_1391339", 
  "temp_1391387", 
  "temp_1391502", 
  "temp_1391505", 
  "temp_1391517", 
  "temp_1391842", 
  "temp_1391796")

output_path <- "W:/VF/Pinnaroo 2022/animal_log/jax_working_outputs"

write.csv(Fence_all, 
          paste0(output_path,"/animal_GPS_data_clip_uncum_step2.csv"), 
          row.names=FALSE)
