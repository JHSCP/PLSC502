##### 6 #####
data("gay", package = "qss")
library(dplyr)
library(tidyverse)

unique(gay$treatment)


gay %>% 
  filter(study == 1) %>% 
  filter(wave == 1) %>% 
  filter(treatment %in% c("Same-Sex Marriage Script by Gay Canvasser",
                          "Same-Sex Marriage Script by Straight Canvasser",
                          "No Contact")) %>% 
  group_by(treatment) %>% 
  summarise(freq= n())



##### 7 #####

followup <- gay %>%
  filter(wave <= 2) %>% 
  filter(treatment %in% c("Same-Sex Marriage Script by Gay Canvasser",
                          "Same-Sex Marriage Script by Straight Canvasser",
                          "No Contact")) 

str(gay)

###### DiD approach ######
# within group difference (after (=wave2) - before(=wave1))
estimatons <- followup %>% 
  group_by(wave, treatment) %>% 
  summarise(estimations = mean(ssm)) %>% 
  pivot_wider(names_from = wave, 
              values_from = estimations)  %>%
  rename(wave1 = "1",
         wave2 = "2") %>%  
  mutate(change = wave2 - wave1)


change_control  <- estimatons$change[estimatons$treatment == "No Contact"]
change_gay      <- estimatons$change[estimatons$treatment == "Same-Sex Marriage Script by Gay Canvasser"]
change_straight <- estimatons$change[estimatons$treatment == "Same-Sex Marriage Script by Straight Canvasser"]

# between units difference (treated - control)
DiD_gay      <- change_gay - change_control
DiD_straight <- change_straight - change_control

DiD_gay
DiD_straight
