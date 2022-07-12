# Read in and tidying of gopro deployment, footage counts and site information

# Initial Setup --
library(tidyverse)

# Read in Data
read_csv("data/gp_deployment.csv") #gopro deployment information

read_csv("data/sal_video_review.csv") #salmon estuary video counts
read_csv("data/sal_site_info.csv") #salmon estuary gopro site information
