##### 6 #####
data("gay", package = "qss")
library(dplyr)
library(tidyverse)

unique(gay$treatment)

baseline <- gay %>%
  filter(wave == 1) %>%
  filter(treatment %in% c("Same-Sex Marriage Script by Gay Canvasser",
                          "Same-Sex Marriage Script by Straight Canvasser",
                          "No Contact"))

tapply(baseline$ssm, baseline$treatment, mean, na.rm = TRUE)



#The "No Contact" group, the control, has a mean of approximately 3.250, while the two treatment groups have means of 3.256 and 3.193.
#Therefore, it is possible to conclude that the mean attitude scores for the three groups are very similar at the baseline in wave 1.


##### 7 #####
followup <- gay %>%
  filter(treatment %in% c("Same-Sex Marriage Script by Gay Canvasser",
                          "Same-Sex Marriage Script by Straight Canvasser",
                          "No Contact")) %>% 
  arrange(study, wave) 


###### DiD approach ######
1
2

treat
wo treat


