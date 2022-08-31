library(tidyverse)
library(ggplot2)
library(lme4)
stipa.forb <- read.csv("~/Downloads/Stipa-Forb/stipa-forb_phyto-processing - stipa-forb_data_2022-07-27.csv")
blockkey <- read.csv("~/Downloads/Stipa-Forb/block-key.csv")%>%
  select(block, treatment)
stipa.forb <- stipa.forb %>% left_join(blockkey, by = "block")

ggplot(stipa.forb, aes(x=phyto, y=total.biomass.g, color=treatment)) + 
  facet_wrap(vars(name),scales="free") + 
  geom_point()

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

