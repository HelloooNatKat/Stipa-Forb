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
hist(all.bkgrd$total.biomass.rounded.percap)
hist(log(all.bkgrd$total.biomass.rounded.percap))
#taking a log of a number less than 1 gives you a negative value 

model.biomass.1 <-(lmer(log(total.biomass.rounded.percap)~treatment + (1|phyto), data=all.bkgrd))
# + (1|phyto) is a random effect; accounting for variation across species (random effect captures some of this variability)
model.biomass.2 <-(lmer(log(total.biomass.rounded.percap)~treatment + (1|phyto) + (1|block), data=all.bkgrd))

plot(fitted(model.biomass.1), resid(model.biomass.1))
#model diagnostics: is the data fitting assumptions the model makes (this is called a residual plot)
plot(fitted(model.biomass.2), resid(model.biomass.2))
qqnorm(resid(model.biomass.1))
qqline(resid(model.biomass.1))
qqnorm(resid(model.biomass.2))
qqline(resid(model.biomass.2))
#quantile-quanitle plot; plots predicted q against actual q's; another way to assess residuals by looking at tails; checking the normality of the residuals 
anova(model.biomass.1, model.biomass.2)
#anova in this instance, is used to compare two models; gives you stuff like AIC/BIC (tells you if your model is decent, best to minimize AIC)
  #loglik = the higher the value, the better the model; it is the best way to compare random effects between models (the models have to be exactly the same, only difference can be in the random effects)
#a difference of 2 (between models) is significant, like a p-value 
  #in this case model 2 is the much better model

#typical ANOVA definition: "ANOVA, which stands for Analysis of Variance, is a statistical test used to analyze the difference between the means of more than two groups. A one-way ANOVA uses one independent variable, while a two-way ANOVA uses two independent variables" 

summary(model.biomass.2)
#look at Fixed effects

#linear model assumptions: residuals are normally distributed; so it's good to check if this is true or not
  #when you look at the graph, you want residuals to be randomly distributed 

###HYPOTHESIS

## hypothesis (1a): native forb species will perform better in Stipa pulchra backgrounds than in Bromus hordeaceus backgrounds because of historical plant community relations
##in discussion mention the "Control" was still subject to competition (unweeded)

#predictor:bkgrd, functional group, origin 
#response:total.biomass.rounded.percap

model.biomass.3 <-(lmer(log(total.biomass.rounded.percap)~bkgrd + origin + (1|phyto) + (1|block), data=all.bkgrd))
# "+" means you think predictor variables would have an additive effect 
# "*" means you think there's an interactive effect between variables 
# keeping the random effects because some species might respond more similarly to themselves than others 

plot(fitted(model.biomass.3), resid(model.biomass.3))
qqnorm(resid(model.biomass.3))
qqline(resid(model.biomass.3))
summary(model.biomass.3)
# "Estimate"; estimated effect of the model
  #example: negative effect of Bromus, positive effect of Control
# "Random effects"; "Residual" explains variance not explained by Groups, block or phyto 

model.biomass.3.B <-(lmer(log(total.biomass.rounded.percap)~bkgrd * origin + (1|phyto) + (1|block), data=all.bkgrd))
# "+" means you think predictor variables would have an additive effect 
# "*" means you think there's an interactive effect between variables 
# keeping the random effects because some species might respond more similarly to themselves than others 

plot(fitted(model.biomass.3.B), resid(model.biomass.3.B))
qqnorm(resid(model.biomass.3.B))
qqline(resid(model.biomass.3.B))
summary(model.biomass.3.B)

## hypothesis (1b): non-native species will perform equally well in native and non-native backgrounds due to high seed output and survival rate

###QUESTION: should we model non-native and native separately? 

## hypothesis (2a): the difference between stipa background and bromus will be greater under the drought condition than ambient condition
#our results seem to indicate the opposite...maybe not as a facilitative role as we thought (but not necessarily competitive); OR direct effects of the environment more important than immediate facilitative effects 

model.biomass.3.B <-(lmer(log(total.biomass.rounded.percap)~bkgrd * treatment + (1|phyto) + (1|block), data=all.bkgrd))

## hypothesis (2b): native forbs may be more affected by drought conditions than non-native grasses due to local adaptations (workshop this question)

#reduce hypotheses; focus on "who's your neighbor" question (background), rather than the identity of the phytometer; check if there is interactive effect with precip.
#Stipa serves as a facilitator at the beginning, but once a species has invaded the system is unhappy
#do a trait comparison? put a pin in this until June (add as another hypothesis)
#focal species do best in stipa not bromus, in both drought and ammbient background (add this in my abstract)

#TO DO
#new model: treatment*fungroupphyto*fungroupofbackground = this will give a three way interaction, so you can simplify first
#use density as covariate, see if it impacts results, if it does, then perhaps only using H density and potentially drop species 
  #for example, include things like MAEL and TWIL grouping for functional group but no individual anaylsis 
