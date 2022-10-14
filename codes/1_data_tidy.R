# Read in and tidying of gopro deployment, footage counts and site information

# Initial Setup --
library(tidyverse)
library(dplyr)
library(plyr)
library(lubridate)

# Read in Dataframes
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


write_csv(sal_tides, file.path("data/sal_tides.csv"))

sal_tides <- read_csv("data/sal_tides.csv") 

sal_tides$datetime <- with(sal_tides, as.POSIXct(Date), format = "%m/%d/%Y %H:%M:%S")

sal_tides$tide_height <- sal_tides$`predictions(m)`#rename to tide_height
  
sal_tides <- sal_tides %>% #drop redundant columns
  subset(select=c("datetime", 'tide_height'))

#convert datetime to 10 min using nearest tide height

sal_tides_fd <- sal_tides %>%
  mutate(sal_tides, floor_date(datetime, unit = '10 min')) %>%
  subset(select = -datetime) %>%
  dplyr::rename(datetime = `floor_date(datetime, unit = "10 min")`)

sal_tides_cd <- sal_tides %>%
  mutate(sal_tides, ceiling_date(datetime, unit = '10 min')) %>%
  subset(select = -datetime) %>%
  dplyr::rename(datetime = `ceiling_date(datetime, unit = "10 min")`)

#Merge ceiling and floor datetime to get tide height on 10 min interval  
sal_tides_10min <- merge(sal_tides_fd, sal_tides_cd, all = TRUE) 
  
#combine and clean data

#make Salmon Estuary CPUE dataframe
##Join count, tide, site, and deployment dataframes

salmon <- sal_counts %>% merge(sal_tides_10min, by = "datetime", copy = FALSE, suffix = c(".x", ".y"), all = TRUE)%>%
  full_join(., deploy, by= c("launch", "date"))%>%
  left_join(., sal_info, by="waypoint_name") %>%
  unite("notes", c("notes.x","notes.y"), remove = FALSE, sep = " , ", na.rm = TRUE) %>%
  subset(select=-c(date, time, notes.x, notes.y, camera.x)) %>%
  dplyr::rename(camera=camera.y)%>%
  dplyr::rename(tide_type=tide)

write_csv(salmon, file.path("data/salmon.csv"))

#read in Salmon Estuary data frame with 0's for dewatered for Presence/Absemce
sal_pa <- read_csv("data/sal_video_review_pa.csv")
sal_pa$datetime <- with(sal_pa, as.POSIXct(paste(date, time), format="%m/%d/%Y %H:%M:%S"))

#make Salmon Estuary Presence-Absense dataframe
##Join count, tide, site, and deployment dataframes

salmon_pa <- sal_pa %>% merge(sal_tides_10min, by = "datetime", copy = FALSE, suffix = c(".x", ".y"), all = TRUE)%>%
  full_join(., deploy, by= c("launch", "date"))%>%
  left_join(., sal_info, by="waypoint_name") %>%
  unite("notes", c("notes.x","notes.y"), remove = FALSE, sep = " , ", na.rm = TRUE) %>%
  subset(select=-c(date, time, notes.x, notes.y, camera.x)) %>%
  dplyr::rename(camera=camera.y)%>%
  dplyr::rename(tide_type=tide)

write_csv(salmon_pa, file.path("data/salmon_pa.csv"))





#visualize data distribution (normal, linear?) use histogram?

hist(salmon$salmonid_sp, density = NULL)


boxplot(sal_counts$salmonid_sp)

