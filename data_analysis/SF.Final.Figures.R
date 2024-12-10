## Header 
## 
## Script Name: SF.Final.Figures
##
## Script Description: Calculate the relative interaction intensity (Armas et al. 2004) and make figures
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
source("data_cleaning/QAQC.R")

## create standard error function
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## set theme for plots
theme_set(theme_classic())

# Calc RII ####
## calculate relative interaction intensity
relative = all.bkgrd %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group) %>%
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap)) %>%
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII_BRHO = (BRHO - Control)/(BRHO + Control),
         RII_Stipa = (Stipa - Control)/(Stipa + Control))

## summarise
relative_sum = relative %>%
  select(-BRHO, -Control, -Stipa) %>%
  
  ## put both RII in the same column
  pivot_longer(cols = c("RII_BRHO", "RII_Stipa"), names_to = "Background", values_to = "RII") %>%
  
  ## calculate the mean RII value for each species in each background
  group_by(phyto, Background, treatment, origin, functional_group) %>%
  summarise(mean_RII = mean(RII, na.rm = TRUE),
            se_RII = calcSE(RII)) %>%
  
  ## rename columns for graphing
  mutate(treatment = ifelse(treatment == "C", "Ambient", "Drought"), 
         Background = ifelse(Background == "RII_BRHO", "Bromus", "Stipa"), 
         origin = ifelse(origin == "native", "Native", "Non-Native"))


# Figures ####
## Hypotheses ####
## writing here so we can try to focus the figs on these

## Hypothesis 1:
#Hypothesis (1a) native forb species will perform better in Stipa pulchra backgrounds than in Bromus hordeaceus backgrounds because of historical plant community relations. 

#Hypothesis (1b) non-native species will perform equally well in native and non-native backgrounds due to high seed output and survival rate. 

## Hypothesis 2: 
#Hypothesis (2a) drought will reduce biomass in all species, particularly in those growing in Bromus hordaceous backgrounds compared to Stipa pulchra. 
#Hypothesis (2b) native species may be less affected by drought conditions than non-native species due to local adaptations. 

## Figure 1: ####
## Note for Nat: I learned how to make a summary dataframe for a figure without having to save a new object! I can edit the relative_sum dataframe and use a pipe (%>%) to put it directly into ggplot. 
relative_sum %>%
  
  ## calculate mean RII across species (within their respective origins & backgrounds)
  group_by(treatment, Background, origin) %>%
  summarise(RII_overall = mean(mean_RII, na.rm = T), 
            se_RII_overall = calcSE(mean_RII)) %>%
  
  ## Plot!
  ggplot(aes(x=origin, y=RII_overall, color = Background, shape = origin)) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = RII_overall - se_RII_overall, ymax = RII_overall + se_RII_overall), width = 0.2) +
  scale_color_manual(values = c("#E58606", "#5D69B1")) +
  xlab("Focal Species Origin") +
  ylab("Mean Relative Interaction Intensity") +
  theme(text = element_text(size = 15)) +
  scale_shape_manual(values = c(19, 15)) +
  facet_wrap(~treatment) +
  labs(shape = "Origin")

## save the figure - the line is commented out so we don't re-save a new version every time you run the script
## ggsave("preliminary_figures/figure1_RII_trt_origin_background.png", width = 8, height = 4)


## Figure 2: ####
## species specific RII patterns  
ggplot(relative_sum, aes(x=phyto, y=mean_RII, color = Background)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 3) +
  scale_color_manual(values = c("#E58606", "#5D69B1")) +
  facet_wrap(~treatment) +
  geom_errorbar(aes(ymin = mean_RII - se_RII, ymax = mean_RII + se_RII)) +
  ylab("Relative Interaction Intensity") +
  xlab("Focal Species")


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
