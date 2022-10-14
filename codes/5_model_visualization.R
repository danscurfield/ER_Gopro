#model visualization

#load packages
library(ggplot2)
library(lubridate)

# df_zipoission_b <- fit_zipoisson_b$frame %>%
#   subset(select=-c(waypoint_name))


#include predicted data
zipoisson_predict <- expand.grid(std_tide_poisson = seq(0, 4, 0.0353))



# #   add_column(waypoint_name = NA)
zipoisson_predict <- expand.grid(std_tide_poisson = seq(0, 4, 0.25),
                     waypoint_name = c("sal_gp01-2", "sal_gp03-2", "sal_gp03-new", "sal_gp03-3", "sal_gp04-2", "sal_gp05-2"))




zipoisson_predict$phat <- predict(fit_zipoisson_b, newdata = zipoisson_predict, interval='confidence')


  
  
## plot prediction values
ggplot(zipoisson_predict, aes(x = std_tide_poisson, y = phat))+
  geom_line(color = 'blue')+
  labs(x = "tide height", y = "count")+
  geom_smooth(salmon, mapping=aes(x = std_tide_poisson, y = salmonid_sp), method = "glm")+
  geom_point(salmon, mapping = aes(x = std_tide_poisson, y = salmonid_sp))+
  xlim(0,5) +
  ylim(0,20)



 
ggplot(zipoisson_predict, aes(x = std_tide_poisson, y = phat, color = waypoint_name)) +
  geom_point()+
  geom_line() + 
  labs(x = "tide", y = "count")+
  geom_point(salmon, mapping=aes(x = std_tide_poisson, y = salmonid_sp), color = "blue")+
  xlim(0,4) +
  ylim(0,20)


#plot with stat_smooth not predicted values
ggplot(zipoisson_predict, aes(x = std_tide_poisson, y = phat, color = waypoint_name)) +
  geom_point()+
  geom_line() + 
  labs(x = "tide", y = "count")
  
  ggplot(salmon, mapping=aes(x = tide_height, y = salmonid_sp))+
  geom_point()+
  geom_smooth(mapping=aes(x = tide_height, y = salmonid_sp), method = glm, color = "blue", fill = "lightblue")+
  labs(x = "tide", y = "count")+
  xlim(0,6) +
  ylim(0,20)


#Plot Presence absence data for fun
ggplot(salmon_pa, aes(x = tide_height, y = flatfish_pa))+
  geom_point()+
  geom_smooth(mapping=aes(x = tide_height, y = flatfish_pa), method = loess, color = "blue", fill = "lightblue")+
  annotate("rect", xmin=0, xmax=1.85, ymin=-0.05, ymax=1.05, alpha=0.2, fill="red") +
  annotate("rect", xmin=1.85, xmax=4.5, ymin=-0.05, ymax=1.05, alpha=0.2, fill="green")+
  xlab("Tide (m)")+
  ylab("Presence-Absence")

  
  

 
  
  
  