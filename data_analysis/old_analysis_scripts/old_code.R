## Unneeded code: leave here temporarily

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





#STANDARD DEVIATION 
#sd_data <- aggregate(value ~ group, data = data, FUN = sd)
#print(sd_data)
## not sure what this code is meant to be doing?

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

## hypothesis (1a): native forb species will perform better in Stipa pulchra backgrounds than in Bromus hordeaceus backgrounds because of historical plant community relations

ggplot(all.bkgrd[all.bkgrd$origin %in% "native",], aes(x=bkgrd, y=total.biomass.rounded.percap)) + 
  geom_boxplot() +
  facet_wrap(~phyto, scales="free") +
  scale_y_log10() +
  theme_bw()

ggsave("native_forbs_bkgrd.png", height = 4, width = 7)


ggplot(all.bkgrd[all.bkgrd$origin %in% "native",], aes(x=treatment, y=total.biomass.rounded.percap, color = bkgrd)) + 
  geom_boxplot() +
  facet_wrap(~phyto, scales="free") +
  scale_y_log10() +
  theme_bw()

ggsave("native_forbs_trt_by_bkgrd.png", height = 4, width = 7)

## hypothesis (1b): non-native grasses will perform equally well in native and non-native backgrounds due to high seed output and survival rate

ggplot(all.bkgrd[all.bkgrd$origin %in% "non_native" & all.bkgrd$functional_group %in% "grass",], aes(x=bkgrd, y=total.biomass.rounded.percap)) + 
  geom_boxplot() +
  facet_wrap(~phyto, scales="free")

## hypothesis (2a): all species will perform worse under drought due to overall stress from drought conditions and competition with background species
ggplot(all.bkgrd, aes(x=treatment, y=total.biomass.rounded.percap)) + 
  geom_boxplot() +
  facet_wrap(~phyto, scales="free") +
  scale_y_log10() 

## hypothesis (2b): native forbs may be less affected by drought conditions than non-native grasses due to local adaptations
ggplot(data=all.bkgrd[!all.bkgrd$phyto %in% c("ANAR","THIR"),], aes(x=treatment, y=total.biomass.rounded.percap, color=origin)) + 
  geom_boxplot() +
  facet_wrap(~phyto, scales="free") +
  scale_y_log10() 

ggplot(data=all.bkgrd[!all.bkgrd$phyto %in% c("ANAR","THIR"),], aes(x=treatment, y=total.biomass.rounded.percap)) + 
  geom_boxplot() +
  facet_wrap(~functional_group, scales="free") +
  scale_y_log10() 


# Species Specific Models #### 

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
