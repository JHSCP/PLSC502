library(qss)

library(tidyverse)
library(dplyr)

data("congress", package = "qss")

#### 5 ####


congress %>% select(dwnom1,dwnom2,congress) %>% 
  mutate(mean_dwnom1 = mean(dwnom1, na.rm=T),
         mean_dwnom2 = mean(dwnom2, na.rm=T),
         mean_congress = mean(congress, na.rm= T),
         median_dwnom1 = median(dwnom1, na.rm=T),
         median_dwnom2 = median(dwnom2, na.rm=T),
         median_congress = median(congress, na.rm= T),
         sd_dwnom1 = sd(dwnom1, na.rm=T),
         sd_dwnom2 = sd(dwnom2, na.rm=T),
         sd_congress = sd(congress, na.rm= T),
         IQR_dwnom1 = IQR(dwnom1, na.rm=T),
         IQR_dwnom2 = IQR(dwnom2, na.rm=T),
         IQR_congress = IQR(congress, na.rm= T)) %>% 
  select(-c(dwnom1,dwnom2,congress)) %>%
  data.frame() %>%  head(1)


congress %>%
  summarise(across(c(dwnom1, dwnom2, congress), ~ mean(is.na(.))))

sum(is.na(congress$congress))

#### 6 ####
#DW-NOMINATE first dimension score represents economic liberalism/conservatism,
#second dimension score represents racial liberalism/conservatism.

# scatt
congress %>% 
  ggplot(aes(x= dwnom1, y = party)) +
  geom_point() +
  labs(x = "",
       y = "Party") +
  theme_minimal()


congress %>% 
  ggplot(aes(x= dwnom2, y = party)) +
  geom_point() +
  theme_minimal()


congress %>% 
  ggplot(aes(x=dwnom1,y =dwnom2, color = party, alpha = I(1/6))) +
  geom_point() +
  labs(x= "DW-NOMINATE first dimension score",
       y= "DW-NOMINATE second dimension score",
       title= "") +
  theme_minimal() 



# density
congress %>% 
  ggplot(aes(x = dwnom1)) +
  geom_density() +
  theme_minimal()


congress %>% 
  ggplot() +
  geom_density(aes(x = dwnom1, color = party)) +
  theme_minimal()


congress %>%
  ggplot(aes(x = dwnom1, fill = party, color = party)) +
  geom_density(alpha = 0.4, na.rm = TRUE) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "DW-NOMINATE 1st Dimension",
       y = "Density", 
       fill = "Party", 
       color = "Party") +
  theme_minimal()


congress %>%
  ggplot(aes(x = dwnom2, fill = party, color = party)) +
  geom_density(alpha = 0.4, na.rm = TRUE) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "DW-NOMINATE 2nd Dimension", 
       y = "Density", 
       fill = "Party", 
       color = "Party") +
  theme_minimal()

# boxplot
congress %>% 
  mutate(party = factor(party, levels = c("Democrat", "Republican", "Other"))) %>% 
  ggplot(aes(x=party, y= dwnom1)) +
  geom_boxplot() +
  labs(x= "Party Ideology",
       y = "DW-NOMINATE Score (1st Dimension)") +
  theme_minimal()

congress %>% 
  mutate(party = factor(party, levels = c("Democrat", "Republican", "Other"))) %>% 
  ggplot(aes(x=party, y= dwnom2)) +
  geom_boxplot() +
  labs(x= "Party Ideology",
       y = "DW-NOMINATE Score (2nd Dimension)") +
  theme_minimal()


congress %>% 
  mutate(party = factor(party, levels = c("Democrat", "Republican", "Other"))) %>% 
  ggplot(aes(x=dwnom1, y= dwnom2, color = party)) +
  geom_boxplot() +
  theme_minimal()




#### 7 ####

congress %>% group_by(congress, party) %>% 
  summarise(median_dwnom1 = median(dwnom1,na.rm=T)) %>% 
  filter(party != "Other") %>% 
  ggplot(aes(x=congress, y = median_dwnom1)) +
  geom_line(aes(color = party)) +
  scale_color_manual(values = c(Democrat= "blue",Republican ="red"))+
  labs(x= "Congress",
       y= "DW-NOMINATE Score (1st Dimension)",
       titles = "DW-NOMINATE Score Over Congress by Party") +
  theme_minimal()


congress %>% filter(party != "Other") %>% 
  ggplot(aes(x=dwnom1, y =dwnom2)) +
  geom_point(aes(color = party), alpha = I(1/2)) +
  scale_color_manual(values = c(Democrat= "blue",Republican ="red"))+
  labs(x= "DW-NOMINATE Score (1st Dimension)", 
       y = "DW-NOMINATE Score (2nd Dimension)",
       titles= "DW-NOMINATE Score by Party") +
  facet_wrap(~congress) +
  theme_minimal()




