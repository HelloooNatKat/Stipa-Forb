library(lmerTest)
library(dplyr)
library(lme4)
library(MuMIn)
rm(list=ls())
library(emmeans)
library(ggplot2)
library(car)
source("data_cleaning/QAQC.R")

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

#Hypothesis (1a) native forb species will perform better in Stipa pulchra backgrounds than in Bromus hordeaceus backgrounds because of historical plant community relations. 

#Hypothesis (1b) non-native species will perform equally well in native and non-native backgrounds due to high seed output and survival rate. 

hypothesis_1<-(lmer(log(total.biomass.rounded.percap)~bkgrd*origin + (1|phyto) + (1|block), data=all.bkgrd))

summary(hypothesis_1)

plot(fitted(hypothesis_1), resid(hypothesis_1))
qqnorm(resid(hypothesis_1))
qqline(resid(hypothesis_1))
anova(hypothesis_1)
pairs(emmeans(hypothesis_1,~bkgrd),adjust="BH")

hypothesis_1_graph<-all.bkgrd %>%
  group_by(phyto, bkgrd, origin) %>% 
  summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
  mutate(bkgrd = ifelse(bkgrd == "BRHO", "Bromus", bkgrd)) %>%
  mutate(bkgrd = ifelse(bkgrd == "Control", "None", bkgrd))

hypothesis_1_graph$phyto <- as.factor(hypothesis_1_graph$phyto)
hypothesis_1_graph <- hypothesis_1_graph %>%
  mutate(phyto = fct_relevel(phyto, "ACAM", "GITR", "LENI", "MAEL", "PLER", "TWIL", "ANAR","THIR","BRHO", "LOMU")) %>%
  mutate(bkgrd = factor(bkgrd, levels = c("None", "Bromus", "Stipa")))

ggplot(hypothesis_1_graph,aes(x=bkgrd, y=mean_biomass, color=origin))+
  geom_point(size=3)+ #size can be adjusted here!
  geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  facet_wrap(vars(phyto), scales="free")+
  theme_bw()+
  facet_wrap(vars(phyto), scales="free",ncol=6,nrow=2)+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Origin")+
  scale_color_manual(values = c("#B452CD", "#000000"), labels = c("Native", "Non-Native"))+
  theme(legend.position = "bottom") 

average<-all.bkgrd %>%
  group_by(bkgrd, treatment) %>% 
  summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
  mutate(bkgrd = ifelse(bkgrd == "BRHO", "Bromus", bkgrd))

ggplot(average,aes(x=bkgrd, y=mean_biomass, color=treatment))+
  geom_point(size=3)+ #size can be adjusted here!
  geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  scale_color_manual(values = c("#008080", "#CA562C"), labels = c("Control", "Drought"))+
  theme(legend.position = "bottom") 

#Hypothesis (2a) drought will reduce biomass in all species, particularly in those growing in Bromus hordaceous backgrounds compared to Stipa pulchra. 

#Hypothesis (2b) native species may be less affected by drought conditions than non-native species due to local adaptations. 

hypothesis_2 <-(lmer(log(total.biomass.rounded.percap)~bkgrd * treatment + treatment * origin + bkgrd * origin + (1|phyto) + (1|block), data=all.bkgrd))

#hypothesis_2; this will be the model for all hypotheses

summary(hypothesis_2)

plot(fitted(hypothesis_2), resid(model.biomass.3.D))
qqnorm(resid(hypothesis_2))
qqline(resid(hypothesis_2))
anova(hypothesis_2)

hypothesis_2_graph<-all.bkgrd %>%
  group_by(phyto, bkgrd, origin, treatment) %>% 
  summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
  mutate(bkgrd = ifelse(bkgrd == "BRHO", "Bromus", bkgrd)) %>%
  mutate(bkgrd = ifelse(bkgrd == "Control", "None", bkgrd)) %>%
  mutate(bkgrd = factor(bkgrd, levels = c("None", "Bromus", "Stipa")))

hypothesis_2_graph$phyto <- as.factor(hypothesis_2_graph$phyto)
hypothesis_2_graph <- hypothesis_2_graph %>%
  mutate(phyto = fct_relevel(phyto, "ACAM", "GITR", "LENI", "MAEL", "PLER", "TWIL", "ANAR", "BRHO", "LOMU", "THIR"))

ggplot(hypothesis_2_graph,aes(x=bkgrd, y=mean_biomass, color=treatment))+
  geom_point(size=3)+ #size can be adjusted here!
  geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  facet_wrap(vars(phyto), scales="free",ncol=6,nrow=2)+
  theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Precipitation Treatment")+
  scale_color_manual(values = c("#008080", "#CA562C"), labels = c("Control", "Drought"))+
  theme(legend.position = "bottom") 



#STANDARD DEVIATION 
sd_data <- aggregate(value ~ group, data = data, FUN = sd)
print(sd_data)

hypothesis_1_graph<-all.bkgrd %>%
  group_by(phyto, bkgrd, origin) %>% 
  summarize(sd_biomass=sd(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  mutate(bkgrd = ifelse(bkgrd == "BRHO", "Bromus", bkgrd))

ggplot(hypothesis_1_graph,aes(x=bkgrd, y=sd_biomass, color=origin))+
  geom_point(size=3)+ #size can be adjusted here!
  facet_wrap(vars(phyto), scales="free")+
  theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Origin")+
  scale_color_manual(values = c("#B452CD", "#000000"), labels = c("Native", "Non-Native"))+
  theme(legend.position = "bottom") 



#MODEL PER SPECIES 

# change this code: mutate(bkgrd = ifelse(bkgrd == "BRHO", "Bromus", bkgrd))

GITR <-(lm(total.biomass.rounded.percap~treatment + bkgrd, data=all.bkgrd[all.bkgrd$phyto=="GITR",]))

GITR_graph <-all.bkgrd %>%
  filter(phyto == "GITR") %>% 
  group_by(bkgrd, treatment) %>% 
  summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
  mutate(bkgrd=ifelse(bkgrd=="BRHO", "Bromus", ifelse(bkgrd=="Stipa", "Nassella", bkgrd)))

ggplot(GITR_graph,aes(x=bkgrd, y=mean_biomass, color=treatment))+
  geom_point(size=3)+ #size can be adjusted here!
  geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  #facet_wrap(vars(phyto), scales="free",ncol=6,nrow=2)+
  theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Precipitation Treatment")+
  scale_color_manual(values = c("#008080", "#CA562C"), labels = c("Control", "Drought"))+
  theme(legend.position = "bottom") 

PLER <-(lm(total.biomass.rounded.percap~treatment + bkgrd, data=all.bkgrd[all.bkgrd$phyto=="PLER",]))

PLER_graph <-all.bkgrd %>%
  filter(phyto == "PLER") %>% 
  group_by(bkgrd, treatment) %>% 
  summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
  mutate(bkgrd=ifelse(bkgrd=="BRHO", "Bromus", ifelse(bkgrd=="Stipa", "Nassella", bkgrd)))

ggplot(PLER_graph,aes(x=bkgrd, y=mean_biomass, color=treatment))+
  geom_point(size=3)+ #size can be adjusted here!
  geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  #facet_wrap(vars(phyto), scales="free",ncol=6,nrow=2)+
  theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Precipitation Treatment")+
  scale_color_manual(values = c("#008080", "#CA562C"), labels = c("Control", "Drought"))+
  theme(legend.position = "bottom") 

LENI <-(lm(total.biomass.rounded.percap~treatment + bkgrd, data=all.bkgrd[all.bkgrd$phyto=="LENI",]))

LENI_graph <-all.bkgrd %>%
  filter(phyto == "LENI") %>% 
  group_by(bkgrd, treatment) %>% 
  summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
  mutate(bkgrd=ifelse(bkgrd=="BRHO", "Bromus", ifelse(bkgrd=="Stipa", "Nassella", bkgrd)))

ggplot(LENI_graph,aes(x=bkgrd, y=mean_biomass, color=treatment))+
  geom_point(size=3)+ #size can be adjusted here!
  geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  #facet_wrap(vars(phyto), scales="free",ncol=6,nrow=2)+
  theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Precipitation Treatment")+
  scale_color_manual(values = c("#008080", "#CA562C"), labels = c("Control", "Drought"))+
  theme(legend.position = "bottom") 

TWIL <-(lm(total.biomass.rounded.percap~treatment + bkgrd, data=all.bkgrd[all.bkgrd$phyto=="TWIL",]))

TWIL_graph <-all.bkgrd %>%
  filter(phyto == "TWIL") %>% 
  group_by(bkgrd, treatment) %>% 
  summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
  mutate(bkgrd=ifelse(bkgrd=="BRHO", "Bromus", ifelse(bkgrd=="Stipa", "Nassella", bkgrd)))

ggplot(TWIL_graph,aes(x=bkgrd, y=mean_biomass, color=treatment))+
  geom_point(size=3)+ #size can be adjusted here!
  geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  #facet_wrap(vars(phyto), scales="free",ncol=6,nrow=2)+
  theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Precipitation Treatment")+
  scale_color_manual(values = c("#008080", "#CA562C"), labels = c("Control", "Drought"))+
  theme(legend.position = "bottom") 

ACAM <-(lm(total.biomass.rounded.percap~treatment + bkgrd, data=all.bkgrd[all.bkgrd$phyto=="ACAM",]))

ACAM_graph <-all.bkgrd %>%
  filter(phyto == "ACAM") %>% 
  group_by(bkgrd, treatment) %>% 
  summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
  mutate(bkgrd=ifelse(bkgrd=="BRHO", "Bromus", ifelse(bkgrd=="Stipa", "Nassella", bkgrd)))

ggplot(ACAM_graph,aes(x=bkgrd, y=mean_biomass, color=treatment))+
  geom_point(size=3)+ #size can be adjusted here!
  geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  #facet_wrap(vars(phyto), scales="free",ncol=6,nrow=2)+
  theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Precipitation Treatment")+
  scale_color_manual(values = c("#008080", "#CA562C"), labels = c("Control", "Drought"))+
  theme(legend.position = "bottom") 

MAEL <-(lm(total.biomass.rounded.percap~treatment + bkgrd, data=all.bkgrd[all.bkgrd$phyto=="MAEL",]))

MAEL_graph <-all.bkgrd %>%
  filter(phyto == "MAEL") %>% 
  group_by(bkgrd, treatment) %>% 
  summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
  mutate(bkgrd=ifelse(bkgrd=="BRHO", "Bromus", ifelse(bkgrd=="Stipa", "Nassella", bkgrd)))

ggplot(MAEL_graph,aes(x=bkgrd, y=mean_biomass, color=treatment))+
  geom_point(size=3)+ #size can be adjusted here!
  geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  #facet_wrap(vars(phyto), scales="free",ncol=6,nrow=2)+
  theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Precipitation Treatment")+
  scale_color_manual(values = c("#008080", "#CA562C"), labels = c("Control", "Drought"))+
  theme(legend.position = "bottom") 

LOMU <-(lm(total.biomass.rounded.percap~treatment + bkgrd, data=all.bkgrd[all.bkgrd$phyto=="LOMU",]))

LOMU_graph <-all.bkgrd %>%
  filter(phyto == "LOMU") %>% 
  group_by(bkgrd, treatment) %>% 
  summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
  mutate(bkgrd=ifelse(bkgrd=="BRHO", "Bromus", ifelse(bkgrd=="Stipa", "Nassella", bkgrd)))

ggplot(LOMU_graph,aes(x=bkgrd, y=mean_biomass, color=treatment))+
  geom_point(size=3)+ #size can be adjusted here!
  geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  #facet_wrap(vars(phyto), scales="free",ncol=6,nrow=2)+
  theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Precipitation Treatment")+
  scale_color_manual(values = c("#008080", "#CA562C"), labels = c("Control", "Drought"))+
  theme(legend.position = "bottom") 

BRHO <-(lm(total.biomass.rounded.percap~treatment + bkgrd, data=all.bkgrd[all.bkgrd$phyto=="BRHO",]))

BRHO_graph <-all.bkgrd %>%
  filter(phyto == "BRHO") %>% 
  group_by(bkgrd, treatment) %>% 
  summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
  mutate(bkgrd=ifelse(bkgrd=="BRHO", "Bromus", ifelse(bkgrd=="Stipa", "Nassella", bkgrd)))

ggplot(BRHO_graph,aes(x=bkgrd, y=mean_biomass, color=treatment))+
  geom_point(size=3)+ #size can be adjusted here!
  geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  #facet_wrap(vars(phyto), scales="free",ncol=6,nrow=2)+
  theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Precipitation Treatment")+
  scale_color_manual(values = c("#008080", "#CA562C"), labels = c("Control", "Drought"))+
  theme(legend.position = "bottom") 

ANAR <-(lm(total.biomass.rounded.percap~treatment + bkgrd, data=all.bkgrd[all.bkgrd$phyto=="ANAR",]))

ANAR_graph <-all.bkgrd %>%
  filter(phyto == "ANAR") %>% 
  group_by(bkgrd, treatment) %>% 
  summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
  mutate(bkgrd=ifelse(bkgrd=="BRHO", "Bromus", ifelse(bkgrd=="Stipa", "Nassella", bkgrd)))

ggplot(ANAR_graph,aes(x=bkgrd, y=mean_biomass, color=treatment))+
  geom_point(size=3)+ #size can be adjusted here!
  geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  #facet_wrap(vars(phyto), scales="free",ncol=6,nrow=2)+
  theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Precipitation Treatment")+
  scale_color_manual(values = c("#008080", "#CA562C"), labels = c("Control", "Drought"))+
  theme(legend.position = "bottom") 

THIR <-(lm(total.biomass.rounded.percap~treatment + bkgrd, data=all.bkgrd[all.bkgrd$phyto=="THIR",]))

THIR_graph <-all.bkgrd %>%
  filter(phyto == "THIR") %>% 
  group_by(bkgrd, treatment) %>% 
  summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
  mutate(bkgrd=ifelse(bkgrd=="BRHO", "Bromus", ifelse(bkgrd=="Stipa", "Nassella", bkgrd)))

ggplot(THIR_graph,aes(x=bkgrd, y=mean_biomass, color=treatment))+
  geom_point(size=3)+ #size can be adjusted here!
  geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  #facet_wrap(vars(phyto), scales="free",ncol=6,nrow=2)+
  theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Precipitation Treatment")+
  scale_color_manual(values = c("#008080", "#CA562C"), labels = c("Control", "Drought"))+
  theme(legend.position = "bottom") 


#CHHAYA 
#I like the idea of grouping by native/non-native. Pretty sure it's Stipa now, not Nassella
#Biggest take-away: Stipa promotes growth of both native and non-native species

#FOR ME 
#Effect of precip varies compared to what background you’re in (interactive effect). 
#Additive means there may be an effect of precip regardless of background, and there is an effect of background regardless of precip. 


# RELATIVE PLOTS

relative_BRHO <- all.bkgrd %>%
  group_by(phyto) %>%
  summarize(
    control_biomass = total.biomass.rounded.percap[bkgrd == "Control"],
    BRHO_biomass = total.biomass.rounded.percap[bkgrd == "BRHO"],
    diff = (control_biomass - BRHO_biomass) / control_biomass) 

relative_Stipa <- all.bkgrd %>%
  group_by(phyto) %>%
  summarize(
    control_biomass = total.biomass.rounded.percap[bkgrd == "Control"],
    Stipa_biomass = total.biomass.rounded.percap[bkgrd == "Stipa"],
    diff = (control_biomass - Stipa_biomass) / control_biomass)

# Combine the data frames
relative_diff <- bind_rows(
  relative_BRHO %>% mutate(bkgrd = "BRHO"),
  relative_Stipa %>% mutate(bkgrd = "Stipa")
)

# Add a row for Control with a diff of 0
control_rows <- relative_diff %>%
  group_by(phyto) %>%
  summarize(diff = 0, bkgrd = "Control")

relative_diff <- bind_rows(relative_diff, control_rows)

# Ensure there are no empty or NA values in phyto or treatment
relative_diff <- relative_diff %>%
  filter(!is.na(phyto) & phyto != "")

# Plot the data
ggplot(relative_diff, aes(x = bkgrd, y = diff, fill = bkgrd)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ phyto) +
  theme_bw() +
  ylab("Relative Biomass Difference") +
  xlab("Treatment") +
  ggtitle("Relative Biomass Difference Between Control and Treatments") +
  scale_fill_manual(values = c("Control" = "gray", "BRHO" = "#1f78b4", "Stipa" = "#33a02c"))+
  theme(legend.position = "bottom")

