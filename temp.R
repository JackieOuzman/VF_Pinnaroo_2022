library(readxl)
library(tidyverse)
library(dplyr)
library(FSA) 
library(agricolae)
library(multcomp)
library(multcomp)
library(lsmeans)
library(multcompView)
library(Rmisc)

library(ggplot2)
library(car)
library(DescTools)


### Bring in the weeds data ###


weeds_all <- read_excel("W:/VF/Pinnaroo 2022/weeds/weed_counts_all/all weed counts.xlsx", sheet = "all")

weeds_all <- weeds_all %>% 
  filter(is.na (comment)) 


### Get the data ready for analysis and plots ###
names(weeds_all)
# Tidy up names of colms for plots
weeds_all <- weeds_all %>% 
  dplyr::rename("Total count weeds" = "total_coun")
# make cluster a factor
weeds_all$cluster2Pr <- as.factor(weeds_all$cluster2Pr )

weeds_all_long <- weeds_all %>% 
  pivot_longer(
    cols = "Rye Plants": "Total count weeds",
    names_to = "weed type",
    values_to = "value",
    values_drop_na = TRUE
  )

names(weeds_all_long)
dplyr::distinct(weeds_all_long, event)


####   Plots pre trial#### 



weeds_all_long %>%  
  filter(event ==  "pre trial") %>% 
    ggplot(aes(x = as.factor(cluster2Pr), y = value,
               color = as.factor(cluster2Pr))) +
    geom_point(position = position_dodge(width=0.75)) +
    geom_boxplot(alpha = 0.1, width=0.75, 
                 aes(fill = as.factor(cluster2Pr)))+
    theme_bw()+
    facet_wrap( .~ `weed type`, scales = "free")+
    labs(x = "cluster solution", y = "count")+
    theme(legend.position = "none")




##### loop FOR ANOVA AND SUMMARY TABLE ####
names(weeds_all_long)
distinct(weeds_all_long, `weed type`)
distinct(weeds_all_long, `event`)

df <- weeds_all_long

#### FOR THE CLUSTER GROUPING #####
timing_varaible_list <- c(
  "pre trialXRye Plants",
  "pre trialXTotal count weeds"#,
  
  #"pre trialXRye Tillers"#,
  #"pre trialXBROME Plants",
  #"pre trialXBroadLeaf",
  #"pre trialXOther",
  #"pre trialXBROME Tillers"
  
  )




timing_varaible_list




#make a loop

for (timing_varaible_list in timing_varaible_list){

  
  timing_varaible <- as.data.frame(str_split(timing_varaible_list, "X"),
                               col.names = "for_analysis" )
  
  
### PULL out the info for the analysis to filter the data
timing_varaible$for_analysis <- as.character(timing_varaible$for_analysis)

timing <- timing_varaible[1,1]
var <-   timing_varaible[2,1]
  
## filter the long data frame on timing and the weed measure  
  data <- df %>% filter(`weed type` == var) %>% 
    filter(event == "pre trial")
  
## summaries the filted data and create new clms   
  
  summary <- Summarize(value ~ cluster2Pr,
                       data=data,
                       digits=3)
  summary
  summary <- summary %>%  dplyr::mutate(grouping = "cluster") 
  summary <- summary %>%  dplyr::mutate(weed_type = var)
  summary <- summary %>%  dplyr::mutate(event = timing)

  
## ANOVA   

model = lm(value ~ cluster2Pr,
                         data=data)
model_anova <- Anova(model, type="II") # Can use type="III"
              
p_value_ANOVA <- model_anova[1,4]
F_value_ANOVA <- model_anova[1,3]

### add these values into the summary data

summary <- summary %>%
  mutate(p_value_ANOVA  = p_value_ANOVA,
         F_value_ANOVA  = F_value_ANOVA)

#Add in the significance ***
summary <- summary %>%
  mutate(
    ANOVA_sign = case_when(
      p_value_ANOVA < 0.001 ~ "***",
      p_value_ANOVA <= 0.01 ~  "**",
      p_value_ANOVA <= 0.05 ~  "*",
      p_value_ANOVA >  0.05 ~  "ns",
      TRUE ~ "check"
      
    )
  )

summary <-summary %>%  dplyr::select(
  "event" ,
  "grouping",
  "cluster2Pr"  ,
  "weed_type" ,
  "mean"   ,
  "ANOVA_sign" ,
  "p_value_ANOVA",
  "F_value_ANOVA",
  "n" ,
  "sd",
  "min",
  "Q1",
  "median",
  "Q3",
  "max",
  "percZero"
)

name <- paste0("summary_",timing, "_",var )
assign(name,summary)
  

 
}

summary_pre_trial_Cluster <- rbind(`summary_pre trial_Rye Plants`, `summary_pre trial_Total count weeds`)

summary_pre_trial_Cluster

DT::datatable(summary_pre_trial_Cluster ,
              rownames = FALSE,  
              options = list(columnDefs =
                               list(list(className = 'dt-center',
                                         targets = "_all")))) %>%
  formatRound(c(5), 2) %>%
  formatRound(c(7:14), 2) #this round clm number  to 2 decimal places



################################################################################################################################################

weeds_all_long %>%  
  filter(event ==  "pre trial") %>% 
  ggplot(aes(x = as.factor(Paddock_ar), y = value,
             color = (Paddock_ar))) +
  geom_point(position = position_dodge(width=0.75)) +
  geom_boxplot(alpha = 0.1, width=0.75, 
               aes(fill = Paddock_ar))+
                 theme_bw()+
                 facet_wrap( .~ `weed type`, scales = "free")+
                 labs(x = "paddock area", y = "count", title =  "pre trial")+
                 theme(legend.position = "none")
               





##### loop FOR ANOVA AND SUMMARY TABLE ####
names(weeds_all_long)
distinct(weeds_all_long, `weed type`)
distinct(weeds_all_long, `event`)

df <- weeds_all_long

#### FOR THE VF GROUPING #####   JAXS THIS IS WHEN YOU LOOK AT THE GRAPHS AND WORK OUT WHAT TO ANALYSES
timing_varaible_list <- c(
  "pre trialXRye Plants",
  "pre trialXTotal count weeds"#,
  
  #"pre trialXRye Tillers"#,
  #"pre trialXBROME Plants",
  #"pre trialXBroadLeaf",
  #"pre trialXOther",
  #"pre trialXBROME Tillers"
  
)




timing_varaible_list


##before running this remove summary_df
rm(`summary_pre trial_Rye Plants`, `summary_pre trial_Total count weeds`)

#make a loop

for (timing_varaible_list in timing_varaible_list){
  
  
  timing_varaible <- as.data.frame(str_split(timing_varaible_list, "X"),
                                   col.names = "for_analysis" )
  
  
  ### PULL out the info for the analysis to filter the data
  timing_varaible$for_analysis <- as.character(timing_varaible$for_analysis)
  
  timing <- timing_varaible[1,1]
  var <-   timing_varaible[2,1]
  
  ## filter the long data frame on timing and the weed measure  
  data <- df %>% filter(`weed type` == var) %>% 
    filter(event == "pre trial")
  
  ## summaries the filted data and create new clms   
  
  summary <- Summarize(value ~ Paddock_ar,  ##   JAXS can change the grouping here cluster2Pr / Paddock_ar
                       data=data,
                       digits=3)
  summary
  summary <- summary %>%  dplyr::mutate(grouping = "VF")  ##   JAXS can change the grouping here cluster2Pr / Paddock_ar
  summary <- summary %>%  dplyr::mutate(weed_type = var) 
  summary <- summary %>%  dplyr::mutate(event = timing)
  
  
  ## ANOVA   
  
  model = lm(value ~ Paddock_ar, ##   JAXS can change the grouping here cluster2Pr / Paddock_ar
             data=data)
  model_anova <- Anova(model, type="II") # Can use type="III"
  
  p_value_ANOVA <- model_anova[1,4]
  F_value_ANOVA <- model_anova[1,3]
  
  ### add these values into the summary data
  
  summary <- summary %>%
    mutate(p_value_ANOVA  = p_value_ANOVA,
           F_value_ANOVA  = F_value_ANOVA)
  
  #Add in the significance ***
  summary <- summary %>%
    mutate(
      ANOVA_sign = case_when(
        p_value_ANOVA < 0.001 ~ "***",
        p_value_ANOVA <= 0.01 ~  "**",
        p_value_ANOVA <= 0.05 ~  "*",
        p_value_ANOVA >  0.05 ~  "ns",
        TRUE ~ "check"
        
      )
    )
  
  summary <-summary %>%  dplyr::select(
    "event" ,
    "grouping",
    "Paddock_ar"  , ### Paddock_ar / cluster2Pr
    "weed_type" ,
    "mean"   ,
    "ANOVA_sign" ,
    "p_value_ANOVA",
    "F_value_ANOVA",
    "n" ,
    "sd",
    "min",
    "Q1",
    "median",
    "Q3",
    "max"
  )
  
  name <- paste0("summary_",timing, "_",var )
  assign(name,summary)
  

  
}

summary_pre_trial_VF <- rbind(`summary_pre trial_Rye Plants`, `summary_pre trial_Total count weeds`) 

summary_pre_trial_VF


####################################################################################################################################

weeds_all_long %>% distinct(event)
weeds_all_long %>%  
  filter(event ==  "post trial 1") %>% 
  ggplot(aes(x = as.factor(cluster2Pr), y = value,
             color = as.factor(cluster2Pr))) +
  geom_point(position = position_dodge(width=0.75)) +
  geom_boxplot(alpha = 0.1, width=0.75, 
               aes(fill = as.factor(cluster2Pr)))+
  theme_bw()+
  facet_wrap( .~ `weed type`, scales = "free")+
  labs(x = "cluster solution", y = "count", title =  "Post trial")+
  theme(legend.position = "none")


##### loop FOR ANOVA AND SUMMARY TABLE ####
names(weeds_all_long)
distinct(weeds_all_long, `weed type`)
distinct(weeds_all_long, `event`)

df <- weeds_all_long

#### FOR THE CLUSTER GROUPING #####   JAXS THIS IS WHEN YOU LOOK AT THE GRAPHS AND WORK OUT WHAT TO ANALYSES
timing_varaible_list <- c(
  "post trialXRye Plants",
  "post trialXTotal count weeds",
  "post trialXRye Tillers"#,
  #"post trial 1XBROME Plants",
  #"post trial 1XBroadLeaf",
  #"post trial 1XOther",
  #"post trial 1XBROME Tillers"
  
)




timing_varaible_list




#make a loop

for (timing_varaible_list in timing_varaible_list){
  
  
  timing_varaible <- as.data.frame(str_split(timing_varaible_list, "X"),
                                   col.names = "for_analysis" )
  
  
  ### PULL out the info for the analysis to filter the data
  timing_varaible$for_analysis <- as.character(timing_varaible$for_analysis)
  
  timing <- timing_varaible[1,1]
  var <-   timing_varaible[2,1]
  
  ## filter the long data frame on timing and the weed measure  
  data <- df %>% filter(`weed type` == var) %>% 
    filter(event == "post trial 1")
  
  ## summaries the filted data and create new clms   
  
  summary <- Summarize(value ~ cluster2Pr,  ##   JAXS can change the grouping here cluster2Pr
                       data=data,
                       digits=3)
  summary
  summary <- summary %>%  dplyr::mutate(grouping = "cluster")  ##   JAXS can change the grouping here cluster2Pr
  summary <- summary %>%  dplyr::mutate(weed_type = var)
  summary <- summary %>%  dplyr::mutate(event = timing)
  
  
  ## ANOVA   
  
  model = lm(value ~ cluster2Pr, ##   JAXS can change the grouping here cluster2Pr
             data=data)
  model_anova <- Anova(model, type="II") # Can use type="III"
  
  p_value_ANOVA <- model_anova[1,4]
  F_value_ANOVA <- model_anova[1,3]
  
  ### add these values into the summary data
  
  summary <- summary %>%
    mutate(p_value_ANOVA  = p_value_ANOVA,
           F_value_ANOVA  = F_value_ANOVA)
  
  #Add in the significance ***
  summary <- summary %>%
    mutate(
      ANOVA_sign = case_when(
        p_value_ANOVA < 0.001 ~ "***",
        p_value_ANOVA <= 0.01 ~  "**",
        p_value_ANOVA <= 0.05 ~  "*",
        p_value_ANOVA >  0.05 ~  "ns",
        TRUE ~ "check"
        
      )
    )
  
  summary <-summary %>%  dplyr::select(
    "event" ,
    "grouping",
    "cluster2Pr"  ,
    "weed_type" ,
    "mean"   ,
    "ANOVA_sign" ,
    "p_value_ANOVA",
    "F_value_ANOVA",
    "n" ,
    "sd",
    "min",
    "Q1",
    "median",
    "Q3",
    "max"
  )
  
  name <- paste0("summary_",timing, "_",var )
  assign(name,summary)
  
  
  
}

summary_post_trial_Cluster <- rbind(`summary_post trial_Rye Plants`, `summary_post trial_Total count weeds`, `summary_post trial_Rye Tillers`)

summary_post_trial_Cluster

##before running this remove summary_df
rm(`summary_post trial_Rye Plants`, `summary_post trial_Total count weeds`)

##########################################################################################################################################

## grouping by Assessor for post trial weed counts post trial
names(weeds_all_long)
weeds_all_long %>% distinct(Assessor)


weeds_all_long %>%  
  filter(event ==  "post trial 1") %>% 
  ggplot(aes(x = as.factor(Assessor), y = value,
             color = (Assessor))) +
  geom_point(position = position_dodge(width=0.75)) +
  geom_boxplot(alpha = 0.1, width=0.75, 
               aes(fill = Assessor))+
  theme_bw()+
  facet_wrap( .~ `weed type`, scales = "free")+
  labs(x = "Assessor", y = "count", title =  "post trial")+
  theme(legend.position = "none")









  