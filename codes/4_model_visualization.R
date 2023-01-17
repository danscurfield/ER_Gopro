#model visualization


#load packages
library(ggplot2)
library(ggeffects)
library(ggridges)
library(lubridate)
library(tidyverse)
library(plyr)
library(patchwork)

#Visualize best model prediction

p1 <- ggpredict(fit_poisson_b, terms = "camera_depth [all]")
p1$species <- "salmon"

plot(p1)+
  ylim(0,6)+
  labs(
    x = "Camera Depth (m)", 
    y = "Number of Juvenile Salmon",
    title = "Predicted CPUE at Tide Height"
  )


p2 <- ggpredict(sculfit_poisson_b, terms = "camera_depth [all]")
p2$species <- "sculpin"

plot(p2)+
  ylim(0,6)+
  labs(
    x = "Camera Depth (m)", 
    y = "Number of Sculpin",
    title = "Predicted CPUE at Tide Height"
  )
p2

# p3 <- ggpredict(flatfit_nbinom2, terms = "camera_depth [all]")
# p3$species <- "flatfish"
# 
# plot(p3)+
#   ylim(0,0.1)+
#   labs(
#     x = "Camera Depth (m)", 
#     y = "Number of Flatfish",
#     title = "Predicted CPUE at Tide Height"
#   )


p4 <- ggpredict(stickleback_poisson_b, terms = "camera_depth [all]")
p4$species <- "stickleback"

plot(p4)+
  ylim(0,6)+
  labs(
    x = "Camera Depth (m)", 
    y = "Number of stickleback",
    title = "Predicted CPUE at Tide Height"
  )
p4
  
p <- full_join(p1, p2, by=NULL, all=TRUE)%>%
  full_join(., p4, by=NULL, all=TRUE)
  #%>%
  # mutate(x = x+1.90)

psal1 <- p
psal1$x <- p$x+1.90

ggplot(psal1, aes(x =x, y=predicted, colour=species))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill=species), alpha=0.1, color="grey")

psal5 <- p
psal5$x <- p$x+3.32
psal5$predicted <- ifelse(psal5$x<4.52, psal5$predicted, NA)
psal5$conf.low <- ifelse(psal5$x<4.52, psal5$conf.low, NA)
psal5$conf.high <- ifelse(psal5$x<4.52, psal5$conf.high, NA)

ggplot(psal5, aes(x =x, y=predicted, colour=species))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill=species), alpha=0.1, color="grey")+
  xlim(3.5, 4.5)



p1|p2|p3

plot(p, facet = TRUE, colors="species")

ggplot(p, aes(x =x, y=predicted, colour=species))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill=species), alpha=0.1, color="grey")

#species frequency at 1m
#max tide is 4.52m
p1m <- p
p1m$x <- p$x+1
p1m$predicted <- ifelse(p1m$x<4.52, p1m$predicted, NA)
p1m$conf.low <- ifelse(p1m$x<4.52, p1m$conf.low, NA)
p1m$conf.high <- ifelse(p1m$x<4.52, p1m$conf.high, NA)

ggplot(p1m, aes(x =x, y=predicted, colour=species))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill=species), alpha=0.1, color="grey")

#species frequency at 2m
#max tide is 4.52m
p2m <- p
p2m$x <- p$x+2
p2m$predicted <- ifelse(p2m$x<4.52, p1m$predicted, NA)
p2m$conf.low <- ifelse(p2m$x<4.52, p1m$conf.low, NA)
p2m$conf.high <- ifelse(p2m$x<4.52, p1m$conf.high, NA)

ggplot(p2m, aes(x =x, y=predicted, colour=species))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill=species), alpha=0.1, color="grey")
       
#species frequency at 3m
#max tide is 4.52m
p3m <- p
p3m$x <- p$x+3
p3m$predicted <- ifelse(p3m$x<4.52, p3m$predicted, NA)
p3m$conf.low <- ifelse(p3m$x<4.52, p3m$conf.low, NA)
p3m$conf.high <- ifelse(p3m$x<4.52, p3m$conf.high, NA)

ggplot(p3m, aes(x =x, y=predicted, colour=species))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill=species), alpha=0.1, color="grey")

#species frequency at 3m
#max tide is 4.52m

p4m <- p
p4m$x <- p$x+4
p4m$predicted <- ifelse(p4m$x<4.52, p4m$predicted, NA)
p4m$conf.low <- ifelse(p4m$x<4.52, p4m$conf.low, NA)
p4m$conf.high <- ifelse(p4m$x<4.52, p4m$conf.high, NA)

ggplot(p4m, aes(x =x, y=predicted, colour=species))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill=species), alpha=0.1, color="grey")

#############trying to make JOn plot#####
#add sites to p df
ptest <- p 
ptest$sal1 <- 3.15
ptest$sal2 <- 3.47
ptest$sal3 <- 1.90
ptest$sal4 <- 2.04
ptest$sal5 <- 2.76
ptest$sal6 <- 3.32
ptest $sal7 <- 3.20

#create site column and shift to long format
ptest <- ptest %>%
  pivot_longer(cols = sal1:sal7, names_to = "site", values_to = "cam_elev")

#create column for tide height
ptest$tide_height <- ptest$x+ptest$cam_elev-1.90

#create ggplot visualization of sites an curves

ggplot(ptest, aes(x=tide_height, y=predicted, colour=species))+
  geom_point()#+
  #facet_grid(vars(site))

######marsh and meadow figure##################
#combine data frames to get species salmon, stickleback, sculpin
sal_mar <- ggpredict(sal_mar_nbinom1, terms = "camera_depth [all]")
sal_mar$species <- "salmon"

sti_mar <- ggpredict(sti_mar_nbinom1, terms = "camera_depth [all]")
sti_mar$species <- "stickleback"

scu_mar <- ggpredict(scu_mar_nbinom1, terms = "camera_depth [all]")
scu_mar$species <- "sculpin"

marsh_plot <- full_join(sal_mar, sti_mar, by=NULL, all=TRUE)%>%
  full_join(., scu_mar, by=NULL, all=TRUE)

marsh_plot$x <- marsh_plot$x+1.97
  

#Plot marsh number distributions

ggplot(marsh_plot, aes(x =x, y=predicted, colour=species))+
  geom_line()+
  xlim(1,4.5)+
  ylim(0,0.5)+
  labs(x="Tide Height (m)", y="Probability of Presence")+
  theme_classic()+
  theme(legend.position = "none")
  

#combine data frames to get species salmon, stickleback, sculpin
sal_mea <- ggpredict(sal_mea_nbinom2, terms = "camera_depth [all]")
sal_mea$species <- "salmon"

sti_mea <- ggpredict(sti_mea_nbinom2, terms = "camera_depth [all]")
sti_mea$species <- "stickleback"

scu_mea <- ggpredict(scu_mea_nbinom2, terms = "camera_depth [all]")
scu_mea$species <- "sculpin"

meadow_plot <- full_join(sal_mea, sti_mea, by=NULL, all=TRUE)%>%
  full_join(., scu_mea, by=NULL, all=TRUE)

meadow_plot$x <- meadow_plot$x+3.07

#Plot marsh number distributions

ggplot(meadow_plot, aes(x =x, y=predicted, colour=species))+
  geom_line()+
  xlim(1,4.5)+
  ylim(0,0.52)+
  labs(x="Tide Height (m)", y="Probability of Presence")+
  theme_classic()+
  theme(legend.position = "none")


############## classic plot ################
#change df for better visualizations.
#ggplot to visualize curve of data by individual site (excluding weak sites sal1 and sal 2)
salmon_ggplot <- salmon %>%
  subset(site!="sal1" & site!="sal2")

# salmon_ggplot$site <- revalue(salmon_ggplot$site, c("sal3"="A - 1.90m", "sal4"="B - 2.04m", "sal5"="C - 2.76m", "sal6" = "E - 3.32m", "sal7" = "D - 3.20m"))

salmon_ggplot$site <- revalue(salmon_ggplot$site, c("sal3"="Camera A", "sal4"=" Camera B", "sal5"="Camera C", "sal7" = "Camera D", "sal6" = "Camera E"))

#Final plot format
predict_final <- plot(ggpredict(fit_nbinom2, terms = "camera_depth [all]")) +
  labs(
    x = "Camera Depth (m)", 
    y = "Number of Juvenile Salmon",
    title = "Predicted CPUE at Tide Height")


predict_final <- ggpredict(fit_nbinom2, terms = "camera_depth [all]") %>%
  dplyr::rename(salmonid_sp = predicted, camera_depth = x)

plot(predict_final)


ggplot(salmon_ggplot, aes(x = camera_depth, y = salmonid_sp))+
  geom_point(aes(color = site))+
  geom_ribbon(data = predict_final, aes(y = salmonid_sp, x = camera_depth, ymin = conf.low, ymax = conf.high), alpha = 0.3) +
  geom_line(data = predict_final, aes(y = salmonid_sp)) +
  theme_classic() +
  xlim(-2, 3)+
  ylim(0, 20)

#ggplot showing the proportion of salmon, sculpin and flatfish at depths


#  ggplot to visualize curve of data
ggplot(salmon, aes(x= camera_depth, y = salmonid_sp))+
  geom_point()+
  stat_smooth(method = loess)+
  xlim(0,4)+
  ylim(0,20)


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
  labs(x = "Tide Height (m)", title = "Density of presence-only counts")+
  theme(axis.title.y=element_blank())

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

#predicted model data added to back to tide height (not camera depth)

predict_final_tide <- predict_final
  predict_final_tide$sal1 <- predict_final$camera_depth +1.90
  predict_final_tide$sal2 <- predict_final$camera_depth +2.04
  predict_final_tide$sal3 <- predict_final$camera_depth +2.76
  predict_final_tide$sal4 <- predict_final$camera_depth +3.32
  predict_final_tide$sal5 <- predict_final$camera_depth +3.20

predict_final_tide <- pivot_longer(predict_final_tide, cols = sal1:sal5, names_to = "site") %>%
  dplyr::rename(tide_height=value)

    
    
    
    salmon$cam_elev<- ifelse(salmon$waypoint_name=="sal_gp01-2", 3.15,
                             ifelse(salmon$waypoint_name == "sal_gp02-2", 3.47, 
                                    ifelse(salmon$waypoint_name=="sal_gp03-2", 1.90,
                                           ifelse(salmon$waypoint_name=="sal_gp03-3", 2.04,
                                                  ifelse(salmon$waypoint_name=="sal_gp03-new", 2.76,
                                                         ifelse(salmon$waypoint_name=="sal_gp04-2", 3.32,
                                                                ifelse(salmon$waypoint_name=="sal_gp05-2", 3.20, NA
                                                                )))))))
  
# salmon$cam_elev<- ifelse(salmon$waypoint_name=="sal_gp01-2", 3.15,
#                   ifelse(salmon$waypoint_name == "sal_gp02-2", 3.47, 
#                   ifelse(salmon$waypoint_name=="sal_gp03-2", 1.90,
#                   ifelse(salmon$waypoint_name=="sal_gp03-3", 2.04,
#                   ifelse(salmon$waypoint_name=="sal_gp03-new", 2.76,
#                   ifelse(salmon$waypoint_name=="sal_gp04-2", 3.32,
#                   ifelse(salmon$waypoint_name=="sal_gp05-2", 3.20, NA
#                                                             )))))))
 
#plotpredictions at tide height not camera height

fit_salmon <- fit_nbinom$frame

fit_salmon$tide_height<-ifelse(fit_salmon$site=="sal1",
                               fit_salmon$camera_depth+3.15-0.31,
                        ifelse(salmon$site=="sal2", 
                               fit_salmon$camera_depth+3.47-0.31,
                        ifelse(salmon$site=="sal3", 
                               fit_salmon$camera_depth+1.90-0.31,
                        ifelse(salmon$site=="sal4", 
                               fit_salmon$camera_depth+2.04-0.31,
                        ifelse(salmon$site=="sal5", 
                               fit_salmon$camera_depth+2.76-0.31,
                        ifelse(salmon$site=="sal6", 
                               fit_salmon$camera_depth+3.32-0.31,
                        ifelse(salmon$site=="sal7", 
                               fit_salmon$camera_depth+3.20-0.31, NA)))))))

ggplot(fit_salmon, aes(x= tide_height, y = salmonid_sp)) +
  geom_point()+
  stat_smooth(aes(x= tide_height, y= salmonid_sp), method = loess)

                                
                              
ggplot(salmon_ggplot, aes(x= tide_height, y = salmonid_sp)) +
  geom_point(aes(colour = NA))+
  stat_smooth(aes(x = tide_height, y = salmonid_sp), method = loess)+
  xlab("Tide Height (m)")+
  ylab("Number of Juvenile Salmon")+
  labs(color = "Camera")+
  annotate("rect", xmin=0, xmax=1.85, ymin=-1, ymax=11.0, alpha=0.15, fill="chartreuse") +
  annotate("rect", xmin=1.85, xmax=4.4, ymin=-1, ymax=11.0, alpha=0.15, fill="blue")+
  xlim(0,5) +
  ylim(-1,11)

  
  