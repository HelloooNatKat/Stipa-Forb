library(tidyverse)
library(ggplot2)
library(lme4)
stipa.forb <- read.csv("~/Downloads/Stipa-Forb/stipa-forb_phyto-processing - stipa-forb_data_2022-07-27.csv")
blockkey <- read.csv("~/Downloads/Stipa-Forb/block-key.csv")%>%
  select(block, treatment)

## Carmen File Paths
#stipa.forb <- read.csv("stipa-forb_phyto-processing - stipa-forb_data_2022-07-27.csv")
#blockkey <- read.csv("block-key.csv")%>%
  #select(block, treatment)



stipa.forb <- stipa.forb %>% left_join(blockkey, by = "block")
## I'm guessing these are all phytos from the stipa background? We should explicitly include a background column here as we will need to compare the Stipa and BRHO backgrounds 

## this is accounted for in the new data cleaning script


ggplot(stipa.forb, aes(x=phyto, y=total.biomass.g, color=treatment)) + 
  facet_wrap(vars(name),scales="free") + 
  geom_point()

## 7 rows contain missing values?
missing_vals <- stipa.forb %>%
  filter(is.na(total.biomass.g)) 
    ## looks like phyto num is 0 for these 7 values. 
    ## are these part of what will become phyto survival data or are they accidentally in this processing dataframe?

## is total biomass per capita yet

stipa.forb.summary <- stipa.forb %>% 
  group_by(name, treatment) %>%
  dplyr::summarise(
    biomass.mean = mean(total.biomass.g, na.rm =TRUE),
    biomass.sd = sd(total.biomass.g, na.rm =TRUE),
    biomass.se = biomass.sd/sqrt(length(total.biomass.g)),
    phyto.mean = mean(phyto, na.rm =TRUE),
    phyto.sd = sd(phyto, na.rm =TRUE),
    phyto.se = biomass.sd/sqrt(length(phyto))
  )


ggplot(stipa.forb.summary, 
       aes(x = name, y = biomass.mean, ymin = biomass.mean - biomass.se, ymax = biomass.mean + biomass.se,
           color = treatment)) +
  geom_point() +
  geom_errorbar() +
  theme_classic() +
  xlab("Focal Species") +
  ylab("Biomass (g)") +
  scale_color_manual(values = c("darkblue", "red"), name = "Treatment", labels = c("Drought", "Ambient"))

ggplot(stipa.forb.summary, 
       aes(x = name, y = phyto.mean, ymin = phyto.mean - phyto.se, ymax = phyto.mean + phyto.se,
           color = treatment)) +
  geom_point() +
  geom_errorbar() +
  theme_classic() +
  xlab("Focal Species") +
  ylab("Number of Individuals") +
  scale_color_manual(values = c("darkblue", "red"), name = "Treatment", labels = c("Drought", "Ambient"))
#treatment may not affect emergence 

#we will model biomass and seed output as response variables; predictors are: species, background species, and treatment 

#MODEL A -- species and block as a random effect
model.biomass.1 <-(lmer(total.biomass.g~treatment + (1|name), data=stipa.forb))
model.biomass.2 <-(lmer(total.biomass.g~treatment + (1|name) + (1|block), data=stipa.forb))
summary(model.biomass.1)
#fixed effects -- variance due to different species, estimate for intercept, and effect of the treatment (in this case, ambient treatment has a positive effect (ex. 1.724 + 2.480 = average biomass); intercept is equivalent to drought effect)
summary(model.biomass.2)

#better model, since it's giving us a general trend

#MODEL B -- includes interaction effect of species and treatment 
model.biomass.3 <-(lmer(total.biomass.g~treatment * name + (1|block), data=stipa.forb))
summary(model.biomass.3)
# the "*" means = a + b AND the interaction between these two 
# the "Intercept" biomass is ANAR (R just chooses a random species to be intercept)
# not too great of a model, it's just summing what we already see...

###CODING WITH CARMEN
stipa.brho.all <- read.csv("stipa-brho_processing-data_20220916.csv")%>%
  #filter(phyto.n.indiv>0, phyto!="AMME")%>%
  mutate(percapita.totalbiomass=total.biomass.g/phyto.n.indiv)%>%
  rename_with(stipa.brho.all, THIR-I="THIR")%>%
  rename_with(stipa.brho.all, TWIL-I="TWIL")
#  I keep getting an error: unexpected '='..I've used rename_with, and rename.values


#!= means we want all the phytos that are not AMME

#unique(stipa.brho.all$phyto.n.indiv)
#to check all values you have present

stipa.brho.all.summary <- stipa.brho.all %>% 
  group_by(bkgrd, treatment, phyto) %>%
  dplyr::summarise(
    biomass.mean = mean(percapita.totalbiomass, na.rm =TRUE),
    biomass.sd = sd(percapita.totalbiomass, na.rm =TRUE),
    biomass.se = biomass.sd/sqrt(length(percapita.totalbiomass)),
    phyto.mean = mean(phyto.n.indiv, na.rm =TRUE),
    phyto.sd = sd(phyto.n.indiv, na.rm =TRUE),
    phyto.se = biomass.sd/sqrt(length(phyto.n.indiv))
  )

missing_vals_all <- stipa.brho.all %>%
  filter(is.na(total.biomass.g)) 
#stuff for Carmen; samples that still need to be processed 

ggplot(stipa.brho.all.summary, 
       aes(x = phyto, y = biomass.mean, ymin = biomass.mean - biomass.se, ymax = biomass.mean + biomass.se,
           color = bkgrd)) +
  facet_wrap(vars(treatment),scales="free") + 
  geom_point() +
  geom_errorbar() +
  theme_classic() +
  xlab("Focal Species") +
  ylab("Biomass (g)") +
  scale_color_manual(values = c("darkblue", "red", "purple"), name = "Treatment", labels = c("BRHO", "CONTROL", "STIPA"))

ggplot(stipa.brho.all.summary, 
       aes(x = phyto, y = biomass.mean, ymin = biomass.mean - biomass.se, ymax = biomass.mean + biomass.se,
           color = treatment)) +
  facet_wrap(vars(bkgrd),scales="free") + 
  geom_point() +
  geom_errorbar() +
  theme_classic() +
  xlab("Focal Species") +
  ylab("Biomass (g)") +
  scale_color_manual(values = c("darkblue", "red"), name = "Treatment", labels = c("Control", "Drought"))


#MODEL A 
model.biomass.1 <-(lmer(percapita.totalbiomass~treatment + bkgrd + (1|phyto), data=stipa.brho.all))
model.biomass.2 <-(lmer(percapita.totalbiomass~treatment + bkgrd + (1|phyto) + (1|block), data=stipa.brho.all))
summary(model.biomass.1)
summary(model.biomass.2)
#"Intercept" could mean biomass is still positive not taking into account the other variables???
    #R chose BRHO as the baseline; "bkgrdControl" and "bkgrdStipa" are being compared to BRHO
#do we want to model each species separately? or combine them into groups (ex. native forbs, native grasses..)

#MODEL 2A GITR ONLY
model.biomass.1 <-(lm(percapita.totalbiomass~treatment + bkgrd, data=stipa.brho.all[stipa.brho.all$phyto=="GITR",])) #the "," is important (everything before "," focus on rows, after the "," is column)
#lm is just a linear model, just comparing fixed affect of treatment and background
#the "+" means additive effects 
#lm model gives  a significance value (Pr())
model.biomass.2 <-(lmer(percapita.totalbiomass~treatment + bkgrd + (1|block), data=stipa.brho.all[stipa.brho.all$phyto=="GITR",]))
summary(model.biomass.1)
#"Estimate" is just comparing biomass between treatments or background 
summary(model.biomass.2)

#ggplot(stipa.brho.all.summary, 
#       aes(x = phyto, y = phyto.mean, ymin = phyto.mean - phyto.se, ymax = phyto.mean + phyto.se,
#           color = treatment)) +
#  facet_wrap(vars(bkgrd),scales="free") + 
#  geom_point() +
#  geom_errorbar() +
#  theme_classic() +
#  xlab("Focal Species") +
#  ylab("Number of Individuals") +
#  scale_color_manual(values = c("darkblue", "red"), name = "Treatment", labels = c("Drought", "Ambient"))
#treatment may not affect emergence 

###SURVIVAL 

