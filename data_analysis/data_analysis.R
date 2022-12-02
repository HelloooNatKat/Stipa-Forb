#STEPS 
#1. Look at histogram of the response variable. Does it look normal? If not, is there a transformation to make it (the response) look normal? If not, explore transformations that will. You want the response to look normal because this will ultimately make the residuals normal.
#2. Build your model, include an interaction in the fixed effects if you know there might be one. Including the transformation, if needed. 
#3. Build a model without the interaction. 
#4. Look at the residuals, make sure they're normally distributed. If not, go back to Step 1. 
#5. Look if there is a significant difference (AIC value) between models. If there isn't a significant differ difference, choose the simpler model. 
#6. Compare random effects structures using the anova. 
#7. Look at summary; in Fixed effects section, look at Estimates and Pr(>t)
#8. If you have any variables with more than two levels (ex.intercept (control), treatmentD), and they are categorical, you will need to do contrasts. 

 

library(lmerTest)
rm(list=ls())
source("data_cleaning/QAQC.R")
#source = this is how you connect scripts together 
hist(all.bkgrd.treatment$total.biomass.rounded.percap)
hist(log(all.bkgrd.treatment$total.biomass.rounded.percap))

model.biomass.1 <-(lmer(log(total.biomass.rounded.percap)~treatment + (1|phyto), data=all.bkgrd.treatment))
model.biomass.2 <-(lmer(log(total.biomass.rounded.percap)~treatment + (1|phyto) + (1|block), data=all.bkgrd.treatment))

plot(fitted(model.biomass.1), resid(model.biomass.1))
qqnorm(resid(model.biomass.1))
#quantile-quanitle plot; plots predicted q against actual q's; another way to assess residuals by looking at tails
qqline(resid(model.biomass.1))
anova(model.biomass.1, model.biomass.2)
#anova in this instance, is used to compare two models; gives you stuff like AIC/BIC (tells you if your model is decent, best to minimize AIC)
  #loglik = the higher the value, the better the model; it is the best way to compare random effects between models (the models have to be exactly the same, only difference can be in the random effects)
#in this case model 2 is the much better model

#typical ANOVA definition: "ANOVA, which stands for Analysis of Variance, is a statistical test used to analyze the difference between the means of more than two groups. A one-way ANOVA uses one independent variable, while a two-way ANOVA uses two independent variables" 

summary(model.biomass.2)
#look at Fixed effects

model.biomass.3 <-(lmer(log(total.biomass.rounded.percap)~treatment + (1|phyto) + (1|block), data=all.bkgrd.treatment))

#linear model assumptions: residuals are normally distributed; so it's good to check if this is true or not
  #when you look at the graph, you want residuals to be randomly distributed 

#TO DO
#new model: treatment*fungroupphyto*fungroupofbackground = this will give a three way interaction, so you can simplify first
