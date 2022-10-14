# Initial Setup --
library(tidyverse)
library(devtools)
library(dplyr)
library(MASS)
require(pscl)
library(boot)
library(ggfan)

##might not need this - factor conflicts with ggplot 'continuous scale'
# salmon <- within(salmon, {
#   salmonid_sp <- factor(salmonid_sp)
#   std_tide_poisson <- factor(std_tide_poisson)
# })

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



#run GLM or GAM
#Negative binomial model

# M1 <- glm.nb(salmonid_sp ~ 
#                std_tide_poisson, 
#              data = salmon)
# 
# summary(M1)

### Poisson GLM

S1 <- glm(salmonid_sp ~ std_tide_poisson + habitat,
          family = "poisson",
          data = salmon)

summary(S1)

## Check for over/underdispersion in the model

# E2 <- resid(S1, type = "pearson")
# N <- nrow(salmon)-sum(is.na(salmon$salmonid_sp)) # #of footage
# p  <- length(coef(S1))   
# sum(E2^2) / (N - p) #some underdispersion


### Negative Binomial GLM

# S2 <- glm.nb(salmonid_sp ~ std_tide_poisson,
#              data = salmon)
# 
# summary(S2)

# Dispersion statistic

# E2 <- resid(S2, type = "pearson")
# N  <- nrow(salmon)-sum(is.na(salmon$salmonid_sp)) # #of footage
# p  <- length(coef(S2)) + 1  # '+1' is for variance parameter in NB
# sum(E2^2) / (N - p) #some underdispersion

#Zero-Inflated Poisson GLM
#For no reggressors for zero component use | 1, in the model

S3 <- zeroinfl(salmonid_sp ~ std_tide_poisson+(1|habitat) | 1, dist = 'poisson', data = salmon)
summary(S3 <- zeroinfl(salmonid_sp ~ std_tide_poisson | 1, dist = 'poisson', data = salmon))

summary(S3)

mnull <- update(S3, . ~ 1)

pchisq(2 * (logLik(S3) - logLik(mnull)), df = 3, lower.tail = FALSE)
#'log Lik.' 8.862471e-174 (df=3)

## Running a standard poisson regression to see if of zero inflated is an improvement by running a Vuong test of the two models. 

summary(S1 <- glm(salmonid_sp ~ std_tide_poisson, family = 'poisson', data = salmon))

summary(S3 <- zeroinfl(salmonid_sp ~ std_tide_poisson | 1, dist = 'poisson', data = salmon))

vuong(S1, S3)
#test statistic is significant indicating our zero-inflated model is superior.

#get confidence intervals for the parameters 
##poisson model - incident risk ratio
##zero-inflated - odds ratio

dput(coef(S3, "count"))
#c(`(Intercept)` = 1.08688300456533, std_tide_poisson = 0.420046915885622

dput(coef(S3, "zero"))
#c(`(Intercept)` = 1.1612141189021)

f <- function(data, i) {
  require(pscl)
  m <- zeroinfl(salmonid_sp ~ std_tide_poisson | 1, data = data[i, ],
                start = list(count = c(1.0869, 0.4200), zero = 1.1612))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}

set.seed(10)
res <- boot(salmon, f, R = 1200, parallel = "snow", ncpus = 4)

## print results
res


## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1, 3, 5), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaLL = bca[5]))
}))

## add row names
row.names(parms) <- names(coef(S3))
## print results
parms

#                             Est       pLL       pUL     bcaLL
#count_(Intercept)      1.1456896 0.4197654 1.7093621 0.4210242
#count_std_tide_poisson 0.4200469 0.1743689 0.6898399 0.1792748
#zero_(Intercept)       1.1612141 0.7712766 1.6637538 0.7297258
#                           bcaLL
#count_(Intercept)      1.7096299
#count_std_tide_poisson 0.6914382
#zero_(Intercept)       1.5852704

## compare with normal based approximation
confint(S3)

#                           2.5 %    97.5 %
#count_(Intercept)      0.7640837 1.5272954
#count_std_tide_poisson 0.2336672 0.6064267
#zero_(Intercept)       0.7284286 1.5939997

##now can estimate incidence risk ration (IRR) for Poisson model and odds ration (OR) for logistic zero-inflation model. 

expparms <- t(sapply(c(1, 3, 5), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaLL = bca[5]))
}))

## add row names
row.names(expparms) <- names(coef(S3))
## print results
expparms

#                            Est      pLL      pUL    bcaLL    bcaLL
#count_(Intercept)      3.144609 1.521605 5.525438 1.523521 5.526916
#count_std_tide_poisson 1.522033 1.190495 1.993396 1.196349 1.996585
#zero_(Intercept)       3.193809 2.162525 5.279091 2.074512 4.880611

# use expand.grid function to help predict. (maybe later)

newdata1 <- expand.grid(std_tide_poisson = seq(0, 4, 0.2))
newdata1$phat <- predict(S3, newdata = newdata1, interval='confidence')
newdata1

# ci <- predict(S3, newdata = newdata1, interval="confidence", level = 0.95)
# predict(S1, interval = "confidence")
# 
# ci
# 
# new_df <- cbind(newdata1, ci)
# 
# new_df <- cbind(data1, temp_var)
# 
# ggplot(newdata1, aes(std_tide_poisson, phat))+
#   geom_point() +
#   geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
#   geom_line(aes(y=upr), color = "red", linetype = "dashed")+
#   geom_smooth(method=lm, se=TRUE)
# 
# ggplot(salmon, aes(std_tide_poisson, salmonid_sp))+
#   geom_point()+
#   geom_smooth(method = lm, se = TRUE)
# 
# count_var <- predict(S3, interval="prediction")
# 
# new_df <- cbind(salmon, count_var)
# 
# ggplot(S3, aes(mo))
# 
# dat <- S3$model


ggplot(newdata1, aes(x = std_tide_poisson, y = phat)) +
  geom_point(color = 'blue')+
  geom_line(color = 'blue') + 
  labs(x = "tide", y = "count")+
  geom_point(salmon, mapping=aes(x = std_tide_poisson, y = salmonid_sp))+
  xlim(0,4) +
  ylim(0,20)
  #geom_ribbon(data = newdata1, aes(x = std_tide_poisson, ymin = low, ymax = high), alpha = 0.1) +
  geom_line() +
 # facet_wrap(~camper) + -> can use this for estuary "salmon",  "englishman"
  labs(x = "tide", y = "count")+
  geom_point(salmon, mapping=aes(x = std_tide_poisson, y = salmonid_sp))+
  xlim(0,4) +
  ylim(0,20)

#plot results with confidence intervals etc. 

S3.predict <- predict(S3)
S3.predict <- as_tibble(S3$fitted.values)


ggplot(salmon, aes(x = std_tide_poisson, y = salmonid_sp)) +
  geom_point() +
  #(data = newdata1, aes(x = std_tide_poisson, y = phat))+
 geom_ribbon(data = newdata1, aes(x = std_tide_poisson, ymin = low, ymax = high), alpha = 0.1) 
  xlim(0,4) +
  ylim(0,20)
  



ggplot(salmon, aes(x = std_tide_poisson, y = salmonid_sp)) +
  geom_point() +
  stat_smooth() +
  xlim(0,5) +
  ylim(0,20)












######Below is old modelling efforts########
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
  m <- zeroinfl(salmonid_sp ~ std_tide_poisson | std_tide_poisson, data = data[i, ],
                start = list(count = c(1.0864, 0.4202), zero = 1.1612))
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

