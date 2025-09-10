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
  summarise(N= n()) %>% 
  print()



##### 7 #####

followup_effect <- gay %>%
  filter(wave <= 2) %>% 
  filter(study == 1) %>% 
  filter(treatment %in% c("Same-Sex Marriage Script by Gay Canvasser",
                          "Same-Sex Marriage Script by Straight Canvasser",
                          "No Contact")) %>% 
  group_by(treatment, wave) %>% 
  summarise(mean = mean(ssm)) %>% 
  pivot_wider(names_from = wave, 
              values_from = mean,
              names_prefix = "wave") %>% 
  mutate(t_effect = wave2 - wave1) %>% 
  print()
