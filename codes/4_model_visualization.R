#model visualization


#load packages
library(ggplot2)
library(ggeffects)
library(ggridges)
library(lubridate)
library(tidyverse)
library(plyr)

#Visualize best model prediction

plot(ggpredict(fit_zipoisson_b, terms = "tide_height [all]")) +
  labs(
    x = "Tide Height (m)", 
    y = "Number of Juvenile Salmon",
    title = "Predicted CPUE at Tide Height"
  )

#  ggplot to visualize curve of data
ggplot(salmon, aes(x= tide_height, y = salmonid_sp))+
  geom_point()+
  stat_smooth(method = loess)+
  xlim(0,4)+
  ylim(0,20)

#ggplot to visualize curve of data by individual site (excluding weak sites sal1 and sal 2)
salmon_ggplot <- salmon %>%
  subset(site!="sal1" & site!="sal2")

# salmon_ggplot$site <- revalue(salmon_ggplot$site, c("sal3"="A - 1.90m", "sal4"="B - 2.04m", "sal5"="C - 2.76m", "sal6" = "E - 3.32m", "sal7" = "D - 3.20m"))

salmon_ggplot$site <- revalue(salmon_ggplot$site, c("sal3"="A", "sal4"="B", "sal5"="C", "sal7" = "D", "sal6" = "E"))

ggplot(salmon_ggplot, aes(x= tide_height, y = salmonid_sp, colour = site)) +
  geom_point(aes(colour = NA))+
  stat_smooth(aes(x = tide_height, y = salmonid_sp), method = loess)+
  xlab("Tide Height (m)")+
  ylab("Number of Juvenile Salmon")+
  labs(color = "Camera")+
  annotate("rect", xmin=0, xmax=1.85, ymin=-1, ymax=11.0, alpha=0.15, fill="chartreuse") +
  annotate("rect", xmin=1.85, xmax=4.4, ymin=-1, ymax=11.0, alpha=0.15, fill="blue")+
  xlim(0,5) +
  ylim(-1,11)

#rideline plot

salmon_ggplot %>% 
  filter(tide_height>="0.5") %>% 
  filter(salmonid_sp!="0") %>%
  ggplot(aes(x= tide_height, y = salmonid_sp, colour = site)) +
  geom_density_ridges2(aes(x= tide_height, y = site, fill=salmonid_sp),alpha=0.5) +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
  theme(legend.position = "none")+
  scale_x_continuous(limits = c(1,4.5)) +
  scale_y_discrete(limits=rev) +
  xlab("Tide Height (m)") +
  ylab("Count")

#ridgeline plot bootleg

salmon_ggplot %>% filter(tide_height>="1.5") %>% ggplot(aes(x= tide_height, y= salmonid_sp, colour = site))+
  geom_point(aes(colour=site))+
  stat_smooth(aes(x = tide_height, y = salmonid_sp, group=site), method = loess)+
  scale_y_continuous(limits = c(0,10), breaks = c(0,5, 10), position = "right") +
  scale_x_continuous(limits = c(1,4.5)) +
  xlab("Tide Height (m)") +
  ylab("Count") +
  theme_minimal() +
  facet_wrap(~site, ncol = 1, strip.position = "left") 


#Plot Presence absence data for fun

salmon_ggplot %>% filter(tide_height>="1.5") %>% ggplot(aes(x = tide_height, y = salmonid_pa))+
  geom_point()+
  geom_smooth(mapping=aes(x = tide_height, y = salmonid_pa), method = loess, color = "blue", fill = "lightblue")+
  annotate("rect", xmin=0, xmax=1.85, ymin=-0.05, ymax=1.05, alpha=0.2, fill="red") +
  annotate("rect", xmin=1.85, xmax=4.5, ymin=-0.05, ymax=1.05, alpha=0.2, fill="green")+
  xlab("Tide (m)")+
  ylab("Presence-Absence")

  
  

 
  
  
  