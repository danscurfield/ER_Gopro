#Zero-inflated poisson mixed-effects model

# #install packages
# library(GLMMadaptive)
# 
# #run model 1
# fm1 <- mixed_model(salmonid_sp ~ std_tide_poisson * habitat, random = ~ 1 | launch, data = salmon, family = zi.poisson(), zi_fixed = ~ habitat)
# 
# fm1

#install packages
library(glmmTMB)
library(bbmle)

# run models - ZIGLMM with random effects for site and fixed effect for habitat
# zero-inflated model with poisson distribution (* or + for habitat?)
fit_zipoisson_a = glmmTMB(salmonid_sp ~ std_tide + habitat + (1|site),
  ziformula = ~ 1,
  data = salmon,
  family = poisson)

 summary(fit_zipoisson_a)

#without fixed effect for habitat
fit_zipoisson_b = glmmTMB(salmonid_sp ~ std_tide + (1|site),
                        ziformula = ~ 1,
                        data = salmon,
                        family = poisson)

summary(fit_zipoisson_b)

#with random effect for habitat
fit_zipoisson_c = glmmTMB(salmonid_sp ~ std_tide + (1|habitat),
                          ziformula = ~ 1,
                          data = salmon,
                          family = poisson)
 
# summary(fit_zipoisson_c)

#zero-inflated glmm with random effect for site and w/o fixed effect for habitat is the best model - fit_zipoission_b
AICtab(fit_zipoisson_a, fit_zipoisson_b, fit_zipoisson_c)

#fit model with negative bimomial distribution
fit_zinbinom <- update(fit_zipoisson_b,family=nbinom2)
summary(fit_zinbinom)

#fit model with negative binomial distribution w “NB1” fit (variance = φμ)
fit_zinbinom1 <- update(fit_zipoisson_b,family=nbinom1)
summary(fit_zinbinom1)

#evaluate model fits
AICtab(fit_zipoisson_b, fit_zinbinom, fit_zinbinom1)

#run models - HURDLE
fit_hnbinom1 <- update(fit_zinbinom1,
                       ziformula=~.,
                       data=salmon,
                       family=truncated_nbinom1)
summary(fit_hnbinom1)

#evaluate model fit with hurdle model
AICtab(fit_zipoisson_a, fit_zinbinom, fit_zinbinom1, fit_hnbinom1)
  
