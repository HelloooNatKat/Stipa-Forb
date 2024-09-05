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
library(dplyr)
library(lme4)
library(MuMIn)
rm(list=ls())
library(emmeans)
library(ggplot2)
library(car)
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

###MAY COMMENT
#Liklihood Ratio Test (LRT) loglik = the higher the value, the better the model; it is the best way to compare random effects between models (the models have to be exactly the same, only difference can be in the random effects)
#"we structured the model based on how it was designed (block and phytometer); incorporated factors that would be responsible for variation 

#a difference of 2 (between models) is significant, like a p-value 

  #in this case model 2 is the much better model

#typical ANOVA definition: "ANOVA, which stands for Analysis of Variance, is a statistical test used to analyze the difference between the means of more than two groups. A one-way ANOVA uses one independent variable, while a two-way ANOVA uses two independent variables" 

summary(model.biomass.2)
#look at Fixed effects

#linear model assumptions: residuals are normally distributed; so it's good to check if this is true or not
  #when you look at the graph, you want residuals to be randomly distributed 
###MAY COMMENT
#treatmentD is the difference between (estimated biomass(log) of treatmentC)
#the estimated biomass for treatmentD is -0.7 + (-0.67)
#here we are only looking at precip treatment effects 

###HYPOTHESIS

## hypothesis (1a): native forb species will perform better in Stipa pulchra backgrounds than in Bromus hordeaceus backgrounds because of historical plant community relations
##in discussion mention the "Control" was still subject to competition (unweeded)

#START JULY

hypothesis_1a<-(lmer(log(total.biomass.rounded.percap)~bkgrd*functional_group + bkgrd*origin + (1|phyto) + (1|block), data=all.bkgrd))
###[all.bkgrd$origin == "native"&all.bkgrd$functional_group == "forb",]))

summary(hypothesis_1a)
#QUESTION: why does summary only show "functional_groupggrass"? 

plot(fitted(hypothesis_1a), resid(hypothesis_1a))
qqnorm(resid(hypothesis_1a))
qqline(resid(hypothesis_1a))
anova(hypothesis_1a)
pairs(emmeans(hypothesis_1a,~bkgrd),adjust="BH")
#all species are doing better in Stipa than BRHO, less so in Control but stil better than in BRHO
#QUESTION: because I have bkgrd*functional_group + bkgrd*origin, is using emmeans this way okay? 

#Reminder Example: if the estimate is negative, then you know the BRHO is smaller 
#Reminder Example: BRHO (10) - Control (15) = -5, meaning Control had higher biomass (meaning native phytometer species had more biomass growth under Control)

#predictor:bkgrd, functional group, origin 
#response:total.biomass.rounded.percap

hypothesis_1a_graph<-all.bkgrd %>%
  group_by(phyto, bkgrd, origin) %>% 
  summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
  mutate(bkgrd=ifelse(bkgrd=="BRHO", "Bromus", ifelse(bkgrd=="Stipa", "Nassella", bkgrd)))

ggplot(hypothesis_1a_graph,aes(x=bkgrd, y=mean_biomass, color=origin))+
  geom_point(size=3)+ #size can be adjusted here!
  geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  facet_wrap(vars(phyto), scales="free")+
  theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Origin")+
  scale_color_manual(values = c("#008080", "#CA562C"), labels = c("Native", "Non-Native"))+
  theme(legend.position = "bottom") 

#END JULY

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

#START JULY

non_native_plot<-all.bkgrd %>%
  filter(origin!="native") %>%
  group_by(phyto, bkgrd, origin) %>% 
  summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
  mutate(bkgrd=ifelse(bkgrd=="BRHO", "Bromus", ifelse(bkgrd=="Stipa", "Nassella", bkgrd)))

ggplot(non_native_plot,aes(x=bkgrd, y=mean_biomass))+
  geom_point(size=3)+ #size can be adjusted here!
  geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  facet_wrap(vars(phyto), scales="free")+
  theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")

#END JULY

###QUESTION: should we model non-native and native separately? 

## hypothesis (2a): the difference between stipa background and bromus will be greater under the drought condition than ambient condition

#START JULY
hypothesis_2a_graph<-all.bkgrd %>%
  group_by(phyto, bkgrd, origin, treatment) %>% 
  summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
  mutate(bkgrd=ifelse(bkgrd=="BRHO", "Bromus", ifelse(bkgrd=="Stipa", "Nassella", bkgrd)))

hypothesis_2a_graph$phyto <- as.factor(hypothesis_2a_graph$phyto)
hypothesis_2a_graph <- hypothesis_2a_graph %>%
  mutate(phyto = fct_relevel(phyto, "ACAM", "GITR", "LENI", "MAEL", "PLER", "TWIL", "ANAR", "BRHO", "LOMU", "THIR"))

ggplot(hypothesis_2a_graph,aes(x=bkgrd, y=mean_biomass, color=treatment))+
  geom_point(size=3)+ #size can be adjusted here!
  geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  facet_wrap(vars(phyto), scales="free",ncol=6,nrow=2)+
  theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Precipitation Treatment")+
  scale_color_manual(values = c("#008080", "#CA562C"), labels = c("Control", "Drought"))+
  theme(legend.position = "bottom") 

#END JULY

#our results seem to indicate the opposite...maybe not as a facilitative role as we thought (but not necessarily competitive); OR direct effects of the environment more important than immediate facilitative effects 

model.biomass.native <-(lmer(log(total.biomass.rounded.percap)~bkgrd + (1|phyto) + (1|block), data=all.bkgrd[all.bkgrd$treatment == "C" & all.bkgrd$origin == "native",]))
# "," means take all the rows where treatment = C (anything after the comma, it would look for just columns)

###MAY COMMENT
#here we are only looking at the effect of background on native phytometers under control precip treatment
#could include both native and non-natives in this model (create a separate model, depending on the question)
#^^^model.biomass.native <-(lmer(log(total.biomass.rounded.percap)~bkgrd * origin + (1|phyto) + (1|block), data=all.bkgrd[all.bkgrd$treatment == "C"))
#@Carmen - why did we choose control treatment and not drought? (maybe we wanted to see generally the interaction between species without the effect of rainfall/less rainfall)

plot(fitted(model.biomass.native), resid(model.biomass.native))
qqnorm(resid(model.biomass.native))
qqline(resid(model.biomass.native))
summary(model.biomass.native)

pairs(emmeans(model.biomass.native,~bkgrd),adjust="BH")
#natives are doing better in Stipa than BRHO, and mid in Control

###MAY COMMENT
#if the estimate is negative, then you know the BRHO is smaller 
# Example: BRHO (10) - Control (15) = -5, meaning Control had higher biomass (meaning native phytometer species had more biomass growth under Control)

model.biomass.native.treatment <-(lmer(log(total.biomass.rounded.percap)~bkgrd + treatment + (1|phyto) + (1|block), data=all.bkgrd[all.bkgrd$origin == "non_native",]))
# "," means take all the rows where treatment = C (anything after the comma, it would look for just columns)
#change "non_native" to "native" to check 
plot(fitted(model.biomass.native.treatment), resid(model.biomass.native.treatment))
qqnorm(resid(model.biomass.native.treatment))
qqline(resid(model.biomass.native.treatment))
summary(model.biomass.native.treatment)
anova(model.biomass.native.treatment)
pairs(emmeans(model.biomass.native.treatment,~bkgrd),adjust="BH")

model.biomass.3.C <-(lmer(log(total.biomass.rounded.percap)~bkgrd * treatment + (1|phyto) + (1|block), data=all.bkgrd))
# "," means take all the rows where treatment = C (anything after the comma, it would look for just columns)
plot(fitted(model.biomass.3.C), resid(model.biomass.3.C))
qqnorm(resid(model.biomass.3.C))
qqline(resid(model.biomass.3.C))
summary(model.biomass.3.C)

model.biomass.3.D <-(lmer(log(total.biomass.rounded.percap)~bkgrd * treatment + treatment * origin + (1|phyto) + (1|block), data=all.bkgrd))
plot(fitted(model.biomass.3.D), resid(model.biomass.3.D))
qqnorm(resid(model.biomass.3.D))
qqline(resid(model.biomass.3.D))
anova(model.biomass.3.D)
summary(model.biomass.3.D)
#Intercept here: bkgrdBRHO

###MAY COMMENT 
#if I want all two way interactions, and not a three way interaction, bkgrd*treatment*origin - bkgrd:treatment:origin - bkgrd:origin
#^this is the same as model.biomass.3.D, just different way of writing 
#the ":" just means interaction, "*" compare all the main effects, AND the interactions 

anova(model.biomass.3.D)
#no effect of origin 

###MAY COMMENT
#run emmmeans on background and treatment separately and background+treatment


#Chhaya notes: drought just means lower biomass; no interaction between treatment and origin; but who your neighbors are does matter (native species=doing well)...we thought interactions might matter (that's why we included them), even though they don't matter...even though there's no overall significant effect of origin, we are interested in how backgrounds and treatments affect native and non-native species (motivation for the post-hoc ones...these came after model 3.D because we had three different backgrounds (increases complexity of analysis), and compared this to origin). 

#TO-DO
#Create two figures (use means and se) for native and non-native using background and treatment (use summary data frames from CNGA code...groupby (native/non-natives, treatment)). 

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

native_plot<-all.bkgrd %>%
  filter(origin!="non_native") %>%
  group_by(phyto, bkgrd, origin, treatment) %>% 
  summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
  mutate(bkgrd=ifelse(bkgrd=="BRHO", "Bromus", ifelse(bkgrd=="Stipa", "Nassella", bkgrd)))

non_native_plot<-all.bkgrd %>%
  filter(origin!="native") %>%
  group_by(phyto, bkgrd, origin, treatment) %>% 
  summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
  mutate(bkgrd=ifelse(bkgrd=="BRHO", "Bromus", ifelse(bkgrd=="Stipa", "Nassella", bkgrd)))

ggplot(native_plot,aes(x=bkgrd, y=mean_biomass, color=treatment))+
  geom_point(size=3)+ #size can be adjusted here!
  geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  facet_wrap(vars(phyto), scales="free")+
  theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Precipitation Treatment")+
  scale_color_manual(values = c("#008080", "#CA562C"), labels = c("Control", "Drought"))+
  theme(legend.position = "bottom") 

ggplot(non_native_plot,aes(x=bkgrd, y=mean_biomass, color=treatment))+
  geom_point(size=3)+ #size can be adjusted here!
  geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  facet_wrap(vars(phyto), scales="free")+
  theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Precipitation Treatment")+
  scale_color_manual(values = c("#008080", "#CA562C"), labels = c("Control", "Drought"))

anova(model.biomass.3.C,model.biomass.3.D)
#the effect of background does not affect the effect of treatment 


library(emmeans) 
pairs(emmeans(model.biomass.3.D,~bkgrd),adjust="BH")
pairs(emmeans(model.biomass.3.D,~treatment),adjust="BH")
#"estimate"=estimated biomass difference, the "-" means Control has more biomass than BRHO
pairs(emmeans(model.biomass.3.D,~treatment+bkgrd),adjust="BH")

#TAKEAWAY
#no effect of origin
#no interactive effect of background and treatment 
#biomass is less in drought 
#biomass is greatest in Stipa, lowest in BRHO 

#when we restrict the data to just forbs, there's no effect of treatment 
#when we restrict the data to just non-native, there's a marginal effect of treatment 
  #does this mean we need the full dataset to have enough power? 
    #look at model.biomass.native.treatment

#NEXT STEP 
#run post-hoc contrasts (TukeyHSD or Marina code - glht or emmeans); compare against the actual controls not just BRHO, you can also compare against Stipa to BRHO etc.

#for hypothesis 1a and 1b incorporate treatment as a variable 

#have one figure that shows drought treatment has an environmental effect (soil data)
###Carmen 

#clean figures (the prelim ones sent in Slack)

#EXTRA
## hypothesis (2b): native forbs may be more affected by drought conditions than non-native grasses due to local adaptations (workshop this question)

#reduce hypotheses; focus on "who's your neighbor" question (background), rather than the identity of the phytometer; check if there is interactive effect with precip.
#Stipa serves as a facilitator at the beginning, but once a species has invaded the system is unhappy
#do a trait comparison? put a pin in this until June (add as another hypothesis)
#focal species do best in stipa not bromus, in both drought and ammbient background (add this in my abstract)

#NOTES
#new model: treatment*fungroupphyto*fungroupofbackground = this will give a three way interaction, so you can simplify first
#use density as covariate, see if it impacts results, if it does, then perhaps only using H density and potentially drop species 
  #for example, include things like MAEL and TWIL grouping for functional group but no individual anaylsis 
