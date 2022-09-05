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


names(weeds_pre_trial)

#names(weeds_pre_trial)
weeds_pre_trial_summary <- weeds_pre_trial %>% 
  group_by(cluster2Pr) %>% 
  summarise(
    n = n(),
    mean = mean(total_coun,  na.rm = FALSE),
    sd = sd(total_coun,   na.rm = FALSE),
    se = sd/sqrt(n)) %>% 
  mutate(grouping = "cluster2") %>% 
  rename(name=cluster2Pr )


weeds_pre_trial_summary

weeds_pre_trial_summary_total_count


#### ANOVA #####

Summarize(total_coun ~ cluster2Pr,
          data=weeds_pre_trial,
          digits=3)
model = lm(total_coun ~ cluster2Pr,
           data=weeds_pre_trial)
Anova(model, type="II") # Can use type="III"

#plots of model
hist(residuals(model),
     col="darkgray")

par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
