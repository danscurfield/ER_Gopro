#Poisson mixed-effects model

#**INPUT**#
#Salmon Master dataframe


#**OUTPUT**#
#fit_nbinom1: general linear mixed effect model with random effect for site, fit with negative binomial distribution (family=nbinom1). 
#Include model interpretation and stats


#**PSEUDO-CODE**#
#Step 1: run GLMM for random effects for site and fixed effect for habitat.
#Step 2: run GLMM for random effects for site only.
#Step 3: run GLMM for random effects for habitat only.
#Step 4: run GLMM with no random or fixed effects for site/habitat.
#Step 5: fit best fit model (AIC) with binomial distributions and evaluate model best fit.
 
#**Objectives**#
#Model Salmon estuary count data

#install packages
library(glmmTMB)
library(bbmle)

# run models - GLMM with random effects for site and fixed effect for habitat
# zero-inflated model with poisson distribution (* or + for habitat?)
fit_poisson_a = glmmTMB(salmonid_sp ~ std_tide + habitat + (1|site),
                          data = salmon,
                          family = poisson)

summary(fit_poisson_a)

#without fixed effect for habitat
fit_poisson_b = glmmTMB(salmonid_sp ~ std_tide + (1|site), 
                          data = salmon,
                          family = poisson)

summary(fit_poisson_b)

predict(fit_poisson_b, salmon)
nd<- salmon[1,]
nd$subject <- "new"
predict(fit_poisson_b, newdata=nd, allow.new.levels=TRUE)
nd_pop<- data.frame(tide_height=unique(salmon$tide_height), site=NA)
plot(predict(fit_poisson_b, newdata=nd_pop))


#with random effect for habitat
fit_poisson_c = glmmTMB(salmonid_sp ~ std_tide + (1|habitat),
                          data = salmon,
                          family = poisson)

#with no random effect or fixed effect for habitat or site
fit_poisson_d = glmmTMB(salmonid_sp ~ std_tide, 
                          data = salmon, 
                          family = poisson)

#summary(fit_zipoisson_d)

#zero-inflated glmm with random effect for site and w/o fixed effect for habitat is the best model - fit_zipoission_b
AICtab(fit_poisson_a, fit_poisson_b, fit_poisson_c, fit_poisson_d)


# zero-inflated model with poisson distribution (* or + for habitat?)
fit_poisson_a = glmmTMB(salmonid_sp ~ std_tide + habitat + (1|site),
                        data = salmon,
                        family = poisson)

summary(fit_poisson_a)

#without fixed effect for habitat
fit_poisson_b = glmmTMB(salmonid_sp ~ std_tide + (1|site), 
                        data = salmon,
                        family = poisson)

summary(fit_poisson_b)

predict(fit_poisson_b, salmon)
nd<- salmon[1,]
nd$subject <- "new"
predict(fit_poisson_b, newdata=nd, allow.new.levels=TRUE)
nd_pop<- data.frame(tide_height=unique(salmon$tide_height), site=NA)
plot(predict(fit_poisson_b, newdata=nd_pop))


#with random effect for habitat
fit_poisson_c = glmmTMB(salmonid_sp ~ std_tide + (1|habitat),
                        data = salmon,
                        family = poisson)

#with no random effect or fixed effect for habitat or site
fit_poisson_d = glmmTMB(salmonid_sp ~ std_tide, 
                        data = salmon, 
                        family = poisson)



#fit model with negative bimomial distribution
fit_nbinom <- update(fit_poisson_b,family=nbinom2)
summary(fit_nbinom)

#fit model with negative binomial distribution w “NB1” fit (variance = φμ)
fit_nbinom1 <- update(fit_poisson_b,family=nbinom1)
summary(fit_zinbinom1)

#evaluate model fits
AICtab(fit_poisson_b, fit_nbinom, fit_nbinom1)

#evaluate model fit with hurdle model
AICtab(fit_poisson_b, fit_nbinom, fit_nbinom1)
