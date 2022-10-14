
# Initial Setup --
library(ggplot2)
library(ggpubr)

#Read in data
salmon <- read_csv("data/salmon.csv") #%>%
  drop_na(salmonid_sp)
View(salmon)

salmon$std_tide <- salmon$bed_elevation-salmon$tide_height
salmon$std_tide_poisson <- salmon$std_tide + 1.51

#should exclude launch "GP040" as it was a site used once and was not tidally dewetted as it was supposed to. 
#Gp038 too?

# salmon$nozerosalmonid <- salmon$salmonid_sp %>%
#   lapply(na_if, y = 0) %>%
#   as.numeric()

## mutate data to include all tide heights beyond height at lowest site and convert CPUE to a presence absence binomial

#create a dataframe to add zeros to all of the NA for tides too low for salmon presence
#convert to zero for NA's below site wetted elevation - should be on 10 min intervals just like camera.
#Make giant if statement (good luck)

salmon_pa <- read_csv("data/salmon_pa.csv")

salmon_pa$salmonid_pa <- ifelse(salmon_pa$salmonid_sp=="0", 0, 1)
salmon_pa$sculpin_pa <- ifelse(salmon_pa$sculpin_sp=="0", 0, 1)
salmon_pa$flatfish_pa <- ifelse(salmon_pa$flatfish_sp=="0", 0, 1)

salmon_pa <- salmon_pa %>%
  subset(select=-c(5:17,20:33))

  
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

