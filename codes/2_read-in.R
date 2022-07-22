
# Initial Setup --
library(ggplot2)
library(ggpubr)

#Read in data
salmon <- read_csv("data/salmon.csv")
View(salmon)

salmon$std_tide <- salmon$bed_elevation-salmon$tide_height
salmon$std_tide_poisson <- salmon$std_tide + 1.65

salmon$nozerosalmonid <- salmon$salmonid_sp %>%
  lapply(na_if, y = 0) %>%
  as.numeric()
  
  
#visualise data for normality
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

