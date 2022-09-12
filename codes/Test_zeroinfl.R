#Testing zeroinfl() function with data 
 
require(ggplot2)
require(pscl)
require(boot)

zinb <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
zinb <- within(zinb, {
  nofish <- factor(nofish)
  livebait <- factor(livebait)
  camper <- factor(camper)
})

summary(zinb)

## histogram with x axis in log10 scale
ggplot(zinb, aes(count)) + geom_histogram() + scale_x_log10()

## zero inflated poisson regression
summary(m1 <- zeroinfl(count ~ child + camper | persons, data = zinb))

mnull <- update(m1, . ~ 1)

pchisq(2 * (logLik(m1) - logLik(mnull)), df = 3, lower.tail = FALSE)

## 'log Lik.' 4.041242e-41 (df=5)


## Running a standard poisson regression to see if of zero inflated is an imporvemny by running a Vuong test of the two models. 

summary(p1 <- glm(count ~ child + camper, family = poisson, data = zinb))

vuong(p1, m1)
#test statistic is significant indicating our zero-inflated model is superior. 

#get confidence intervals for the parameters 
##poisson model - incident risk ratio
##zero-inflated - odds ratio

dput(coef(m1, "count"))
## c(`(Intercept)` = 1.59788890535741, child = -1.04283980037201, camper = 0.834022175460643)

dput(coef(m1, "zero"))
## c(`(Intercept)` = 1.29743873378692, persons = -0.564347227953224)

f <- function(data, i) {
  require(pscl)
  m <- zeroinfl(count ~ child + camper | persons, data = data[i, ],
                start = list(count = c(1.598, -1.0428, 0.834), zero = c(1.297, -0.564)))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}

set.seed(10)
res <- boot(zinb, f, R = 1200, parallel = "snow", ncpus = 4)

## print results
res

## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaLL = bca[5]))
}))

## add row names
row.names(parms) <- names(coef(m1))
## print results
parms

#Est          pLL        pUL        bcaLL       bcaLL
#count_(Intercept)  1.5978885  0.929548809  2.0891957  1.052591464  2.21402500
#count_child       -1.0428385 -1.778733547 -0.1686651 -1.727480382 -0.09541710
#count_camper       0.8340222  0.008795793  1.5978058  0.001069699  1.58913696
#zero_(Intercept)   1.2974392  0.458982712  2.2596422  0.441060605  2.21684371
#zero_persons      -0.5643472 -1.100952918 -0.1167136 -1.061884331 -0.03569355

## compare with normal based approximation
confint(m1)

#                     2.5 %     97.5 %
#count_(Intercept)  1.4302372  1.7655406
#count_child       -1.2388133 -0.8468663
#count_camper       0.6505171  1.0175273
#zero_(Intercept)   0.5647018  2.0301756
#zero_persons      -0.8837504 -0.2449440

##now can estimate incidence risk ration (IRR) for Poisson model and odds ration (OR) for logistic zero-inflation model. 

expparms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaLL = bca[5]))
}))

## add row names
row.names(expparms) <- names(coef(m1))
## print results
expparms

#                        Est       pLL       pUL     bcaLL     bcaLL
#count_(Intercept) 4.9425853 2.5333659 8.0784160 2.8650662 9.1524811
#count_child       0.3524528 0.1688519 0.8447918 0.1777317 0.9089937
#count_camper      2.3025614 1.0088346 4.9421772 1.0010703 4.8995186
#zero_(Intercept)  3.6599122 1.5824634 9.5796696 1.5543549 9.1783157
#zero_persons      0.5687313 0.3325540 0.8898401 0.3458036 0.9649360

# use expand.grid function to help predict. 

newdata1 <- expand.grid(0:3, factor(0:1), 1:4)
colnames(newdata1) <- c("child", "camper", "persons")
newdata1 <- subset(newdata1, subset=(child<=persons))
newdata1$phat <- predict(m1, newdata1)

ggplot(newdata1, aes(x = child, y = phat, colour = factor(persons))) +
  geom_point() +
  geom_line() +
  facet_wrap(~camper) +
  labs(x = "Number of Children", y = "Predicted Fish Caught")









