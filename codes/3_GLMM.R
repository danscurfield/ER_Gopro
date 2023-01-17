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
# library(bbmle)
library(DHARMa)


############ run models - GLMM with random effects for site and fixed effect for habitat###############
fit_poisson_a = glmmTMB(salmonid_sp ~ camera_depth + habitat + poly(camera_depth,2) + (1|site),
                          data = salmon,
                          family = poisson)

summary(fit_poisson_a)

#without fixed effect for habitat

# salmon2 <- salmon %>%
#   filter(!is.na(std_tide))

fit_poisson_b = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2) + (1|site), 
                          data = salmon,
                          family = poisson)

summary(fit_poisson_b)


#with random effect for habitat
fit_poisson_c = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2) + (1|habitat),
                          data = salmon,
                          family = poisson)

#with no random effect or fixed effect for habitat or site
fit_poisson_d = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2),
                          data = salmon, 
                          family = poisson)





#summary(fit_zipoisson_d)

#glmm with random effect for site and w/o fixed effect for habitat is the best model - fit_poission_b

# AICtab(fit_poisson_a, fit_poisson_b, fit_poisson_c, fit_poisson_d)
AIC(fit_poisson_a, fit_poisson_b, fit_poisson_c, fit_poisson_d)


####Zer0-inflated glmm#############

# zero-inflated model with poisson distribution (* or + for habitat?)
fit_zipoisson_a = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2) + habitat + (1|site),
                        ziformula = ~ 1, 
                        data = salmon,
                        family = poisson)

summary(fit_poisson_a)

#without fixed effect for habitat
fit_zipoisson_b = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2) + (1|site), 
                        ziformula = ~ 1, 
                        data = salmon,
                        family = poisson)

summary(fit_poisson_b)


#with random effect for habitat
fit_zipoisson_c = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2) + (1|habitat),
                        ziformula = ~ 1, 
                        data = salmon,
                        family = poisson)

#with no random effect or fixed effect for habitat or site
fit_zipoisson_d = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2),
                        ziformula = ~ 1, 
                        data = salmon, 
                        family = poisson)

AICtab(fit_zipoisson_a, fit_zipoisson_b, fit_zipoisson_c, fit_zipoisson_d)

AICtab(fit_poisson_a, fit_poisson_b, fit_poisson_c, fit_poisson_d, fit_zipoisson_a, fit_zipoisson_b, fit_zipoisson_c, fit_zipoisson_d)


#fit model with negative bimomial distribution
fit_nbinom2 <- update(fit_poisson_b,family=nbinom2)
summary(fit_nbinom)

#fit model with negative binomial distribution w “NB1” fit (variance = φμ)
fit_nbinom1 <- update(fit_poisson_b,family=nbinom1)
summary(fit_zinbinom1)

#evaluate model fits
AICtab(fit_poisson_b, fit_nbinom2, fit_nbinom1)

#evaluate model fit with hurdle model
AICtab(fit_poisson_b, fit_nbinom, fit_nbinom1)

######fit model to sculpin, flatfish and stickle back#################

#sculpin
sculfit_poisson_a = glmmTMB(sculpin_sp ~ camera_depth + habitat + poly(camera_depth,2) + (1|site),
                        data = salmon,
                        family = poisson)

sculfit_poisson_b = glmmTMB(sculpin_pa ~ camera_depth + poly(camera_depth,2) + (1|site), 
                        data = salmon,
                        family = poisson)

sculfit_poisson_c = glmmTMB(sculpin_sp ~ camera_depth + poly(camera_depth,2) + (1|habitat),
                        data = salmon,
                        family = poisson)

sculfit_poisson_d = glmmTMB(sculpin_sp ~ camera_depth + poly(camera_depth,2),
                        data = salmon, 
                        family = poisson)

sculfit_nbinom2 <- update(sculfit_poisson_b,family=nbinom2)

# AICtab(sculfit_poisson_a, culfit_poisson_b, culfit_poisson_c, culfit_poisson_d, sculfit_nbinom2)
AIC(sculfit_poisson_a, sculfit_poisson_b, sculfit_poisson_c, sculfit_poisson_d, sculfit_nbinom2)

summary(sculfit_poisson_a)

#flatfish
flatfit_poisson_b = glmmTMB(flatfish_sp ~ camera_depth + poly(camera_depth,2) + (1|site), 
                            data = salmon,
                            family = poisson)

flatfit_nbinom2 <- update(flatfit_poisson_b,family=nbinom2)

#stickleback
stickleback_poisson_b = glmmTMB(stickleback_pa ~ camera_depth + poly(camera_depth,2) + (1|site), 
                            data = salmon,
                            family = poisson)

stickleback_nbinom2 <- update(stickleback_poisson_b,family=nbinom2)

#######MODEL TESTING#################

# norm_resid <- simulateResiduals(mod_norm_full)
# plot(norm_resid)

pois_resid <- simulateResiduals(fit_poisson_b)
plot(pois_resid)

zinb_resid <- simulateResiduals(fit_zipoisson_b)
plot(zinb_resid)

nb2_resid <- simulateResiduals(fit_nbinom2)
plot(nb2_resid)

nb1_resid <- simulateResiduals(fit_nbinom1)
plot(nb1_resid)

#The best fit model is negative binomial 

###########modelling by site############
#model data for site marsh sites (sal1 = 1.90m, sal2 = 2.04m, sal3, sal4)

##exclude all other site data
salmon_marsh <- salmon %>%
  select(c("stickleback_sp", "salmonid_sp", "flatfish_sp", "sculpin_sp", "tide_height", "std_tide", "site", "stickleback_pa", "salmonid_pa", "flatfish_pa", "sculpin_pa", "camera_depth", "habitat")) %>%
  filter(site == "sal1"| site == "sal2" | site == "sal3"| site == "sal4")

#fit models based on salmon data

sal_mar_glmm_a = glmmTMB(salmonid_sp ~ camera_depth + habitat + poly(camera_depth,2) + (1|site),
                         data = salmon_marsh,
                         family = poisson)

sal_mar_glmm_b = glmmTMB(salmonid_pa ~ camera_depth + poly(camera_depth,2) + (1|site), 
                         data = salmon_marsh,
                         family = poisson)

sal_mar_glmm_c = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2) + (1|habitat),
                         data = salmon_marsh,
                         family = poisson)

sal_mar_glmm_d = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2),
                         data = salmon_marsh, 
                         family = poisson)

sal_mar_ziglmm_a = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2) + habitat + (1|site),
                           ziformula = ~ 1, 
                           data = salmon_marsh,
                           family = poisson)

sal_mar_ziglmm_b = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2) + (1|site), 
                           ziformula = ~ 1, 
                           data = salmon_marsh,
                           family = poisson)

sal_mar_ziglmm_c = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2) + (1|habitat),
                           ziformula = ~ 1, 
                           data = salmon_marsh,
                           family = poisson)

sal_mar_ziglmm_d = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2),
                           ziformula = ~ 1, 
                           data = salmon_marsh, 
                           family = poisson)

sal_mar_nbinom2 <- update(sal_mar_glmm_b,family=nbinom2)

sal_mar_nbinom1 <- update(sal_mar_glmm_b,family=nbinom1)


AIC(sal_mar_glmm_a, sal_mar_glmm_b, sal_mar_glmm_c, sal_mar_glmm_d, sal_mar_ziglmm_a, sal_mar_ziglmm_b, sal_mar_ziglmm_c, sal_mar_ziglmm_d, sal_mar_nbinom2, sal_mar_nbinom1)


sal_mar_resid <- simulateResiduals(sal_mar_nbinom2)
plot(sal_mar_resid)

plot(ggpredict(sal_mar_nbinom2, terms = "camera_depth [all]"))+
  labs(
    x = "Camera Depth (m)", 
    y = "Number of Juvenile Salmon",
    title = "Predicted CPUE at Tide Height"
  )

#model sculpin data

scu_mar_glmm_b = glmmTMB(sculpin_pa ~ camera_depth + poly(camera_depth,2) + (1|site), 
                         data = salmon,
                         family = poisson)

scu_mar_nbinom2 <- update(scu_mar_glmm_b,family=nbinom2)

scu_mar_nbinom1 <- update(scu_mar_glmm_b,family=nbinom1)

plot(ggpredict(scu_mar_nbinom2, terms = "camera_depth [all]"))+
  ylim(0,10)+
  labs(
    x = "Camera Depth (m)", 
    y = "Number of Juvenile Salmon",
    title = "Predicted CPUE at Tide Height"
  )

#model stickleback data

sti_mar_glmm_b = glmmTMB(stickleback_pa ~ camera_depth + poly(camera_depth,2) + (1|site), 
                         data = salmon_marsh,
                         family = poisson)

sti_mar_nbinom2 <- update(scu_mar_glmm_b,family=nbinom2)
sti_mar_nbinom1 <- update(scu_mar_glmm_b,family=nbinom1)

plot(ggpredict(sti_mar_nbinom2, terms = "camera_depth [all]"))+
  ylim(0,10)+
  labs(
    x = "Camera Depth (m)", 
    y = "Number of Juvenile Salmon",
    title = "Predicted CPUE at Tide Height"
  )

#model flatfish

fla_mar_glmm_b = glmmTMB(flatfish_sp ~ camera_depth + poly(camera_depth,2) + (1|site), 
                         data = salmon_marsh,
                         family = poisson)

fla_mar_nbinom2 <- update(fla_mar_glmm_b,family=nbinom2)
fla_mar_nbinom1 <- update(fla_mar_glmm_b,family=nbinom1)

plot(ggpredict(fla_mar_nbinom2, terms = "camera_depth [all]"))+
  ylim(0,10)+
  labs(
    x = "Camera Depth (m)", 
    y = "Number of Juvenile Salmon",
    title = "Predicted CPUE at Tide Height"
  )



#model data for site meadow sites (sal5 = m, sal6 = m, sal7)

##exclude all other site data
salmon_meadow <- salmon %>%
  select(c("stickleback_sp", "salmonid_sp", "flatfish_sp", "sculpin_sp", "tide_height", "std_tide", "site", "stickleback_pa", "salmonid_pa", "flatfish_pa", "sculpin_pa", "camera_depth", "habitat")) %>%
  filter(site == "sal5"| site == "sal6" | site == "sal7")

#fit models based on salmon data

sal_mea_glmm_a = glmmTMB(salmonid_sp ~ camera_depth + habitat + poly(camera_depth,2) + (1|site),
                        data = salmon_meadow,
                        family = poisson)

sal_mea_glmm_b = glmmTMB(salmonid_pa ~ camera_depth + poly(camera_depth,2) + (1|site), 
                                  data = salmon_meadow,
                                  family = poisson)
sal_mea_glmm_c = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2) + (1|habitat),
                        data = salmon_meadow,
                        family = poisson)

sal_mea_glmm_d = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2),
                        data = salmon_meadow, 
                        family = poisson)

sal_mea_ziglmm_a = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2) + habitat + (1|site),
                          ziformula = ~ 1, 
                          data = salmon_meadow,
                          family = poisson)

sal_mea_ziglmm_b = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2) + (1|site), 
                          ziformula = ~ 1, 
                          data = salmon_meadow,
                          family = poisson)

sal_mea_ziglmm_c = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2) + (1|habitat),
                          ziformula = ~ 1, 
                          data = salmon_meadow,
                          family = poisson)

sal_mea_ziglmm_d = glmmTMB(salmonid_sp ~ camera_depth + poly(camera_depth,2),
                          ziformula = ~ 1, 
                          data = salmon_meadow, 
                          family = poisson)

sal_mea_nbinom2 <- update(sal_mea_glmm_b,family=nbinom2)

sal_mea_nbinom1 <- update(sal_mea_glmm_b,family=nbinom1)


AIC(sal_mea_glmm_a, sal_mea_glmm_b, sal_mea_glmm_c, sal_mea_glmm_d, sal_mea_ziglmm_a, sal_mea_ziglmm_b, sal_mea_ziglmm_c, sal_mea_ziglmm_d, sal_mea_nbinom2, sal_mea_nbinom1)


sal_mea_resid <- simulateResiduals(sal_mea_nbinom2)
plot(sal_mea_resid)

plot(ggpredict(meadow_nbinom2, terms = "camera_depth [all]"))+
  labs(
    x = "Camera Depth (m)", 
    y = "Number of Juvenile Salmon",
    title = "Predicted CPUE at Tide Height"
  )

#model sculpin data

scu_mea_glmm_b = glmmTMB(sculpin_pa ~ camera_depth + poly(camera_depth,2) + (1|site), 
                         data = salmon_meadow,
                         family = poisson)

scu_mea_nbinom2 <- update(scu_mea_glmm_b,family=nbinom2)

plot(ggpredict(scu_mea_nbinom2, terms = "camera_depth [all]"))+
  ylim(0,10)+
  labs(
    x = "Camera Depth (m)", 
    y = "Number of Juvenile Salmon",
    title = "Predicted CPUE at Tide Height"
  )

#model stickleback data

sti_mea_glmm_b = glmmTMB(stickleback_pa ~ camera_depth + poly(camera_depth,2) + (1|site), 
                         data = salmon_meadow,
                         family = poisson)

sti_mea_nbinom2 <- update(sti_mea_glmm_b,family=nbinom2)

plot(ggpredict(sti_mea_nbinom2, terms = "camera_depth [all]"))+
  ylim(0,10)+
  labs(
    x = "Camera Depth (m)", 
    y = "Number of Juvenile Salmon",
    title = "Predicted CPUE at Tide Height"
  )

#model flatfish

fla_mea_glmm_b = glmmTMB(flatfish_sp ~ camera_depth + poly(camera_depth,2) + (1|site), 
                         data = salmon_meadow,
                         family = poisson)

fla_mea_nbinom2 <- update(fla_mea_glmm_b,family=nbinom2)

plot(ggpredict(fla_mea_nbinom2, terms = "camera_depth [all]"))+
  ylim(0,10)+
  labs(
    x = "Camera Depth (m)", 
    y = "Number of Juvenile Salmon",
    title = "Predicted CPUE at Tide Height"
  )

