#**INPUT**#
#"sal_tides.csv" - salmon estuary tide data for study period
#"salmon.csv" - salmon estuary master spreadsheet
#"salmon_zero.csv" - salmon estuary master spreadsheet with 


#**OUTPUT**#
#


#**PSEUDO-CODE**#
#Step 1: read in salmon estuary with 0's master spreadsheet. Manipulate data to get columns for modelling. 

#Step 2: add presence absence data to main dataframe

#**Objectives**#
#import estuary data and combine them into a master spreadsheet



# Initial Setup --
#library(ggplot2)
#library(ggpubr)
library(tidyverse)
library(dplyr)
library(plyr)
library(lubridate)

#Step 1: read in salmon estuary with 0's master spreadsheet. Manipulate data to get columns for modelling. 

#Read in data
salmon <- read_csv("data/salmon.csv") 
View(salmon)

#standardize tide height at each site by zero-ing tide height at first wetted.
## waypoint_name and their respect elevation from sea level (0 tide)
## sal_gp01-2 - 3.15 - correct    - sal1
## sal_gp02-2 - 3.47 - correct    - sal2
## sal_gp03-2 - 3.47 - 1.90       - sal3
## sal_gp03-3 - 3.71 - 2.04       - sal4
## sal_gp03-new - 2.76 - correct  - sal5
## sal_gp04-2 - 3.32 - correct    - sal6
## sal_gp05-2 - 3.47 -3.20        - sal7

salmon$cam_elev<- ifelse(salmon$waypoint_name=="sal_gp01-2", 3.15,
                  ifelse(salmon$waypoint_name == "sal_gp02-2", 3.47, 
                  ifelse(salmon$waypoint_name=="sal_gp03-2", 1.90,
                  ifelse(salmon$waypoint_name=="sal_gp03-3", 2.04,
                  ifelse(salmon$waypoint_name=="sal_gp03-new", 2.76,
                  ifelse(salmon$waypoint_name=="sal_gp04-2", 3.32,
                  ifelse(salmon$waypoint_name=="sal_gp05-2", 3.20, NA
                         )))))))
#to adequately zero the camera heights but keep zero data substract the difference bt lowest camera elevation from all camera elevations and exclude tide below same threshold (highest low tide). 
## the highest low-tide of our study period is 1.59m which occured on 2022-06-07.(don't need this simce counts only go to daily low tide).

#create differece in height between lowest cameral elevation 
## sal_gp03-new - 2.76
## Do I need to add 1.59 to begin zeros at that point?
## BEAUTIFUL!
## 
## ... Did this calculation wrong. Need to subtract each site by the height at wetted then add 1.59 or whatever to include necesary 0 data.

#salmon$tide_height is the height of the tide
#salmon$tide_correction = tide height - (camera elevation - elevation of lowest camera). To have each site start counting close to a corrected 0m point.
#salmon$std_tide is good! just need to make is so Salmon_sp etc. is NA when std_tide is negative! yeeehooo.

#May want to delete below - tide correction and standardizations are off

# salmon$tide_correction <- salmon$tide_height-(salmon$cam_elev-2.76)
# 
# salmon$std_tide <- salmon$tide_height-(salmon$cam_elev-2.76)-(salmon$cam_elev-1.59)+1.59 

salmon$std_tide <- (salmon$tide_height-salmon$cam_elev)+1.90
salmon$camera_depth <- salmon$std_tide - 1.59 #this is probably redundant


#convert waypoint_name strings to site names
## sal_gp01-2 - sal1
## sal_gp02-2 - sal2
## sal_gp03-2 - sal3
## sal_gp03-3 - sal4
## sal_gp03-new - sal5
## sal_gp04-2 - sal6
## sal_gp05-2 - sal7

salmon$waypoint_name <- as.factor(salmon$waypoint_name)
levels(salmon$waypoint_name)
salmon$waypoint_name <- revalue(salmon$waypoint_name, c("sal_gp01-2"="sal1", "sal_gp02-2"="sal2", "sal_gp03-2"="sal3", "sal_gp03-3"="sal4", "sal_gp03-new"="sal5", "sal_gp04-2"="sal6", "sal_gp05-2"="sal7" ))
levels(salmon$waypoint_name)

#exclude columns unnecessary for modelling and remove unnecesary rows
## unneccesary rows = other estuary data, std tide <0, and filter trial launches. 

salmon <- salmon %>%
  subset(select=-c(2,4:10,14:15,17,20:25, 27:33,35:40)) %>%
  subset(estuary!="cluxewe" & estuary!="englishman" & estuary!="nanaimo" & estuary!="cowichan" | is.na(estuary)) %>%
  #filter(zero_tide>0 | is.na(salmonid_sp)) %>%
  dplyr::rename(site=waypoint_name) %>%
  subset(launch!="gp001"&launch!="gp002"&launch!="gp003" | is.na(launch))

# exclude counts between 0 and 1.59 tide as it is the highest low tide over survey period.

salmon$salmonid_sp <- ifelse(salmon$std_tide>0, salmon$salmonid_sp, NA)
salmon$sculpin_sp <- ifelse(salmon$std_tide>0, salmon$sculpin_sp, NA)
salmon$flatfish_sp <- ifelse(salmon$std_tide>0, salmon$flatfish_sp, NA)
salmon$stickleback_sp <- ifelse(salmon$std_tide>0, salmon$stickleback_sp, NA)

#mutate df to get presence absence data

salmon$salmonid_pa <- ifelse(salmon$salmonid_sp=="0", 0, 1)
salmon$sculpin_pa <- ifelse(salmon$sculpin_sp=="0", 0, 1)
salmon$flatfish_pa <- ifelse(salmon$flatfish_sp=="0", 0, 1)
salmon$stickleback_pa <- ifelse(salmon$stickleback_sp=="0", 0, 1)

#bin tide heights to the nearest 0.5 m. 
salmon$tide_binned <- round_any(salmon$std_tide, 0.5)

#exclude any NA's
salmon <- salmon %>%
  filter(!is.na(std_tide))

#can probably delete below

#########visualise data for normality########
hist(salmon$salmonid_sp, density = NULL)
hist(salmon$nozerosalmonid, density = NULL)


boxplot(salmon$salmonid_sp)
boxplot(salmon$nozerosalmonid)

## histogram with x axis in log10 scale
ggplot(salmon, aes(salmonid_sp)) + geom_histogram() + scale_x_log10()

#Test for normality
#Density plot
ggdensity(salmon$salmonid_sp, 
          main = "Density plot of salmon counts",
          xlab = "Salmon Counts")
ggdensity(salmon$nozerosalmonid, 
          main = "Density plot of salmon counts",
          xlab = "Salmon Counts")

#Checking the variance against the mean
summary(salmon$salmonid_sp)
var(salmon$salmonid_sp, na.rm = TRUE)#variance (16.2) higher than mean(1.3)

#Q-Q plot
ggqqplot(salmon$salmonid_sp)
ggqqplot(salmon$nozerosalmonid)

#Shapiro-Wilks Method
shapiro.test(salmon$salmonid_sp) #From the output, the p-value < 0.05 implying that the distribution of the data are significantly different from normal distribution. In other words, we can assume the data is non-normally distributed.

shapiro.test(salmon$nozerosalmonid)#From the output, the p-value < 0.05 implying that the distribution of the data are significantly different from normal distribution. In other words, we can assume the data is non-normally distributed.

