# Read in and tidying of gopro deployment, footage counts and site information

# Initial Setup --
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plyr)
library(lubridate)

# Read in Data
## Camera deployment data
deploy <- read_csv("data/gp_deployment.csv")  #gopro deployment information
  
##Footage review count data
sal_counts <- read_csv("data/sal_video_review.csv") #salmon estuary video counts
sal_counts$datetime <- with(sal_counts, as.POSIXct(paste(date, time), format="%m/%d/%Y %H:%M:%S"))

##Camera Site information
sal_info <- read_csv("data/sal_site_info.csv") #salmon estuary gopro site information

## Tide data
kelsey_bay1 <- read_csv("data/kelsey_bay1.csv")
kelsey_bay2 <- read_csv("data/kelsey_bay2.csv")
kelsey_bay3 <- read_csv("data/kelsey_bay3.csv")
kelsey_bay4 <- read_csv("data/kelsey_bay4.csv")
kelsey_bay5 <- read_csv("data/kelsey_bay5.csv")
kelsey_bay6 <- read_csv("data/kelsey_bay6.csv")
kelsey_bay7 <- read_csv("data/kelsey_bay7.csv")
kelsey_bay8 <- read_csv("data/kelsey_bay8.csv")

sal_tides <- join_all(list(kelsey_bay1,kelsey_bay2, kelsey_bay3, kelsey_bay4, kelsey_bay5, kelsey_bay6, kelsey_bay7, kelsey_bay8), by= c("Date", "predictions(m)"), type='full')

sal_tides$datetime <- as.POSIXct(sal_tides$Date) #rename to datetime
sal_tides$tide_height <- sal_tides$`predictions(m)`#rename to tide_height
  
sal_tides <- select(sal_tides, -Date, -`predictions(m)`) #drop redundant columns
  


#combine and clean data
View(deploy)
View(sal_counts)
View(sal_info)
View(sal_tides)

## need to join count data and tide height but time is conflicting
sal_counts_delete <- left_join(deploy, sal_counts, by = c("date", "launch")) %>%
  left_join(., sal_info, by="waypoint_name") %>%
  left_join(., sal_tides, by= c("date", "time")) %>%
  filter(estuary %in% c("salmon"))

sal_counts_delete2 <- full_join(sal_tides, sal_counts, by = c("datetime"))


View(sal_counts_delete)

View(sal_counts_delete2)


#visualize data distribution (normal, linear?) use histogram?
hist(sal_counts$salmonid_sp, density = NULL)


boxplot(sal_counts$salmonid_sp)

