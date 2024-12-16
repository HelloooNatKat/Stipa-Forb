## Header 
## 
## Script Name: Biomass figures
##
## Script Description: make biomass figures
##
## Script Notes: 
## statistical model has been moved to 'models.R' script! - CW 12/10
## if needed, any older versions of the code can be accessed on the github repo, but I think I have trimmed this down to the necessary basics and kept everything we might need - CW 12/10

#CHHAYA 
#I like the idea of grouping by native/non-native. Pretty sure it's Stipa now, not Nassella
#Biggest take-away: Stipa promotes growth of both native and non-native species

#FOR ME 
#Effect of precip varies compared to what background you’re in (interactive effect). 
#Additive means there may be an effect of precip regardless of background, and there is an effect of background regardless of precip. 

## TWIL control C data missing - go look for this, seems like there should be some

# Set up ####
## clean env
rm(list=ls())

## load packages
library(lmerTest)
library(dplyr)
library(lme4)
library(MuMIn)
library(emmeans)
library(ggplot2)
library(car)
library(ggpubr)

## read in cleaned data
source("data_cleaning/calculate_RII.R")

## create standard error function
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## set theme for plots
theme_set(theme_classic())

# Figures ####
## Hypotheses ####
## writing here so we can try to focus the figs on these

## Hypothesis 1:
#Hypothesis (1a) native forb species will perform better in Stipa pulchra backgrounds than in Bromus hordeaceus backgrounds because of historical plant community relations. 

#Hypothesis (1b) non-native species will perform equally well in native and non-native backgrounds due to high seed output and survival rate. 

## Hypothesis 2: 
#Hypothesis (2a) drought will reduce biomass in all species, particularly in those growing in Bromus hordaceous backgrounds compared to Stipa pulchra. 
#Hypothesis (2b) native species may be less affected by drought conditions than non-native species due to local adaptations. 

## Biomass Figs ####
### species separate ####
## for this figure, I suggest showing only ambient rather than averaging across rainfall treatments
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
  #theme_bw()+
  facet_wrap(vars(phyto), scales="free",ncol=6,nrow=2)+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Origin")+
  scale_color_manual(values = c("#B452CD", "#000000"), labels = c("Native", "Non-Native"))+
  theme(legend.position = "bottom") 

### overall pattern ####
## leaving this here for the moment, but I don't think we will use this figure, because averaging across the species biomass values is really heavily affected by changes in big species and won't really show changes from smaller species
#average<-all.bkgrd %>%
  #group_by(bkgrd, treatment) %>% 
 # summarize(mean_biomass=mean(total.biomass.rounded.percap, na.rm = TRUE), se_biomass=calcSE(total.biomass.rounded.percap)) %>%
 # mutate(bkgrd = ifelse(bkgrd == "BRHO", "Bromus", bkgrd))

#ggplot(average,aes(x=bkgrd, y=mean_biomass, color=treatment))+
 # geom_point(size=3)+ #size can be adjusted here!
 # geom_errorbar(aes(ymin=mean_biomass-se_biomass, ymax=mean_biomass+se_biomass), width = 0.25)+ 
  #theme_bw()+
  #ylab("Total Biomass Per Capita")+
 # xlab("Background")+
  #scale_color_manual(values = c("#008080", "#CA562C"), labels = c("Control", "Drought"))+
  #theme(legend.position = "bottom") 

### sp separate; rainfall ####
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
 # theme_bw()+
  ylab("Total Biomass Per Capita")+
  xlab("Background")+
  labs(color = "Precipitation Treatment")+
  scale_color_manual(values = c("#008080", "#CA562C"), labels = c("Control", "Drought"))+
  theme(legend.position = "bottom") 
