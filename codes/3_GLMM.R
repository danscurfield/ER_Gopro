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


#test - delete
#Consider tweedie model

gam_mod <- gam(salmonid_sp ~ std_tide, data = salmon)
plot(gam_mod, all.terms = TRUE)
plot(ggpredict(gam_mod, terms = "std_tide"))
termplot(gam_mod, terms = "std_tide")

############ run models - GLMM with random effects for site and fixed effect for habitat###############
fit_poisson_a = glmmTMB(salmonid_sp ~ std_tide + habitat + (1|site),
                          data = salmon,
                          family = poisson)

summary(fit_poisson_a)

#without fixed effect for habitat
salmon2 <- salmon %>%
  filter(!is.na(std_tide))

# salmon$std_tide2 <- salmon$std_tide^2

fit_poisson_b = glmmTMB(salmonid_sp ~ tide_height + poly(tide_height,2) + (1|site), 
                          data = salmon2,
                          family = poisson)

summary(fit_poisson_b)


#with random effect for habitat
fit_poisson_c = glmmTMB(salmonid_sp ~ std_tide + (1|habitat),
                          data = salmon,
                          family = poisson)

#with no random effect or fixed effect for habitat or site
fit_poisson_d = glmmTMB(salmonid_sp ~ std_tide, 
                          data = salmon, 
                          family = poisson)





#summary(fit_zipoisson_d)

#glmm with random effect for site and w/o fixed effect for habitat is the best model - fit_poission_b
AICtab(fit_poisson_a, fit_poisson_b, fit_poisson_c, fit_poisson_d)


####Zer0-inflated glmm#############



# zero-inflated model with poisson distribution (* or + for habitat?)
fit_zipoisson_a = glmmTMB(salmonid_sp ~ std_tide + habitat + (1|site),
                        ziformula = ~ 1, 
                        data = salmon,
                        family = poisson)

summary(fit_poisson_a)

#without fixed effect for habitat
fit_zipoisson_b = glmmTMB(salmonid_sp ~ tide_height + poly(tide_height,2) + (1|site), 
                        ziformula = ~ 1, 
                        data = salmon2,
                        family = poisson)

summary(fit_poisson_b)


#with random effect for habitat
fit_zipoisson_c = glmmTMB(salmonid_sp ~ std_tide + (1|habitat),
                        ziformula = ~ 1, 
                        data = salmon,
                        family = poisson)

#with no random effect or fixed effect for habitat or site
fit_zipoisson_d = glmmTMB(salmonid_sp ~ std_tide, 
                        ziformula = ~ 1, 
                        data = salmon, 
                        family = poisson)

AICtab(fit_zipoisson_a, fit_zipoisson_b, fit_zipoisson_c, fit_zipoisson_d)

AICtab(fit_poisson_a, fit_poisson_b, fit_poisson_c, fit_poisson_d, fit_zipoisson_a, fit_zipoisson_b, fit_zipoisson_c, fit_zipoisson_d)


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
