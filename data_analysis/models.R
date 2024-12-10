## Script Name: Models

## Script Description: Run final model & diagnostics

## Script Notes: 


# Set up ####
## read in data
source("data_cleaning/QAQC.R")

# Model ####
## check normality of data
hist(all.bkgrd$total.biomass.rounded.percap) ## skewed distrib
hist(log(all.bkgrd$total.biomass.rounded.percap)) ## taking log of data helps normalize the distribution

## run the model 
## random effect of both phyto and block
m1_bio <-(lmer(log(total.biomass.rounded.percap)~bkgrd * treatment + treatment * origin + bkgrd * origin + (1|phyto) + (1|block), data=all.bkgrd))

## random effect of phyto only
m1_bio_alt <-(lmer(log(total.biomass.rounded.percap)~bkgrd * treatment + treatment * origin + bkgrd * origin + (1|phyto), data=all.bkgrd))

anova(m1_bio, m1_bio_alt)
## use anova to compare the two models
## AIC is lowest on m1_bio model, so keep random effect of block in the model

## model diagnostics
## Model Checks ####
plot(fitted(m1_bio), resid(m1_bio))
qqnorm(resid(m1_bio))
qqline(resid(m1_bio))

## Model Summary ####
anova(m1_bio)
summary(m1_bio)


pairs(emmeans(m1_bio,~bkgrd),adjust="BH")
pairs(emmeans(m1_bio,~treatment),adjust="BH")
pairs(emmeans(m1_bio,~treatment+bkgrd),adjust="BH")

