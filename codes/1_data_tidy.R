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


write_csv(sal_tides, file.path("data/sal_tides.csv"))

sal_tides <- read_csv("data/sal_tides.csv") 

sal_tides$datetime <- with(sal_tides, as.POSIXct(Date), format = "%m/%d/%Y %H:%M:%S")

sal_tides$tide_height <- sal_tides$`predictions(m)`#rename to tide_height
  
sal_tides <- select(sal_tides, -Date, -`predictions(m)`) #drop redundant columns
  
#combine and clean data

## Join count, tide, site, and deployment dataframes

salmon <- sal_tides %>% full_join(sal_counts, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = FALSE) %>%
  left_join(., deploy, by= c("launch", "camera", "date")) %>%
  left_join(., sal_info, by="waypoint_name") %>%
  unite("notes", c("notes.x","notes.y"), remove = FALSE, sep = " , ", na.rm = TRUE) %>%
  select(-date, -time, -notes.x, -notes.y)

write_csv(salmon, file.path("data/salmon.csv"))

#visualize data distribution (normal, linear?) use histogram?

hist(salmon$salmonid_sp, density = NULL)


boxplot(sal_counts$salmonid_sp)

