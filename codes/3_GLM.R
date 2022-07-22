# Initial Setup --
library(tidyverse)
library(devtools)
library(dplyr)
library(MASS)
require(pscl)
library(boot)


ggplot(salmon, aes(x = std_tide_poisson, y = salmonid_sp)) +
  geom_point() +
  stat_smooth() +
  xlim(0,5) +
  ylim(0,20)


## histogram with x axis in log10 scale
ggplot(salmon, aes(salmonid_sp)) + geom_histogram() + scale_x_log10()

#Test for normality
#Density plot
ggdensity(salmon$salmonid_sp, 
          main = "Density plot of salmon counts",
          xlab = "Salmon Counts")

#Checking the variance against the mean
summary(salmon$salmonid_sp)
var(salmon$salmonid_sp, na.rm = TRUE)#variance (16.2) higher than mean(1.3)

#Q-Q plot
ggqqplot(salmon$salmonid_sp)

#Shapiro-Wilks Method
shapiro.test(salmon$salmonid_sp) #From the output, the p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.



#run GLM of GAM
#Negative binomial model
M1 <- glm.nb(salmonid_sp ~ 
               std_tide_poisson, 
             data = salmon)

summary(M1)

### Poisson GLM
S1 <- glm(salmonid_sp ~ std_tide_poisson + habitat,
          family = "poisson",
          data = salmon)

summary(S1)

## Check for over/underdispersion in the model
E2 <- resid(S1, type = "pearson")
N <- nrow(salmon)-sum(is.na(salmon$salmonid_sp)) # #of footage
p  <- length(coef(S1))   
sum(E2^2) / (N - p) #some underdispersion


### Negative Binomial GLM

S2 <- glm.nb(salmonid_sp ~ std_tide_poisson,
             data = salmon)

summary(S2)

# Dispersion statistic
E2 <- resid(S2, type = "pearson")
N  <- nrow(salmon)-sum(is.na(salmon$salmonid_sp)) # #of footage
p  <- length(coef(S2)) + 1  # '+1' is for variance parameter in NB
sum(E2^2) / (N - p) #some underdispersion

#Zero-Inflated Poisson GLM

S3 <- zeroinfl(salmonid_sp ~ std_tide_poisson | ## Predictor for the Poisson process
                 std_tide_poisson, ## Predictor for the Bernoulli process;
               dist = 'poisson',
               data = salmon)

summary(S3)
View(S3)

chisq <- chisq.test(x = S3$fitted.values, y = S3$y)
chisq$observed

plot(residuals(S3) ~ fitted(S3))
coef(S3)
coef(S3, model = "count")
logLik(S3)

rootogram(S3, main = "S3", ylim = c(-5, 15), max = 50)

#confidence intervals for parameters with bootstrapping
dput(coef(S3, "count"))
dput(coef(S3, "zero"))

f <- function(data, i) {
  require(pscl)
  m <- zeroinfl(salmonid_sp ~ std_tide_poisson | std_tide_poisson, data = salmon[i, ],
                start = list(count = c(1.0864, 0.4202), zero = c(1.1231, 0.0216)))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}


set.seed(10)
res <- boot(S3, f, R = 1200, parallel = "snow", ncpus = 4)

#Zero-Inflated Negative Binomial GLM
S4 <- zeroinfl(salmonid_sp ~ std_tide_poisson,
               link = "logit",
               dist = 'negbin',
               data = salmon)
summary(S4)

rootogram(S4, main = "S4", ylim = c(-5, 15), max = 50)

AIC(M1, S4)

#hurdle model

S5 <- hurdle(salmonid_sp ~ std_tide_poisson |
               std_tide_poisson, 
             dist = "negbin",
             offset = logoffset,
             data = salmon)

summary(S5)

rootogram(S5, main = "S5", ylim = c(-5, 15), max = 50)


#sketch fitted and predicted values
#

S3plot <- ggplot(salmon, aes(tide_height, salmonid_sp)) +
  geom_point() +
  geom_abline(S3, aes())

# Dispersion statistic
E3 <- resid(S3, type = "pearson")
N  <- nrow(salmon)-sum(is.na(salmon$salmonid_sp)) # #of footage
p  <- length(coef(S3)) + 1  # '+1' is for variance parameter in NB
sum(E2^2) / (N - p) #some underdispersion

