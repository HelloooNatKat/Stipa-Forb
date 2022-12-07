source("data_cleaning/merge_stipa_brho_data.R")
#Warning: bkgrd == c("BRHO", "Control") longer object length is not a multiple of shorter object length

#SE Function ####
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

colnames(all.bkgrd)
unique(all.bkgrd$block)
#block 11 is not a megacomp block 

ggplot(all.bkgrd, aes(x=phyto, y=total.biomass.rounded.percap)) + 
  geom_boxplot() 

ggsave("all.bkgrd.png", width = 8, height = 5)

no_total_biomass <- all.bkgrd %>%
  filter(is.na(total.biomass.rounded.percap)) 
#15 rows don't have total biomass 
  #remove AVBA
  #LENI still needs processing
  #MAEL still needed to be processed
  #remove ones no phytos

ggplot(all.bkgrd, aes(x=phyto, y=total.biomass.rounded.percap, color=bkgrd)) + 
  geom_boxplot() 

ggsave("all.bkgrd.treatments.png", width = 8, height = 5)
#notes
  #remove MICA
  #remove the "I" from THIR and TWIl in Bromus and Control background
  #missing LENI control 
  #MAEL still being processed 

new.bkgrd <- all.bkgrd %>%
  group_by(treatment, bkgrd, phyto) %>%
  summarise(reps = n())

BRHO.bkgrd <- all.bkgrd %>%
  group_by(treatment, bkgrd, phyto, dens) %>%
  filter(bkgrd == "BRHO") %>%
  summarise(reps = n())

#group_by = looking for unique combination of the variables you list
# n = counts the number of observations 

#Remove replicates with less than 3
final.bckrd <- left_join(all.bkgrd, new.bkgrd, by = c("treatment", "bkgrd", "phyto")) %>%
  filter(reps > 2)

ggplot(BRHO.bkgrd, aes(x=phyto, y=reps, color=dens)) + 
  geom_point() +
  facet_wrap(~treatment)
#you use ~ when using facet_wrap
  ###for group meeting, see what we have enough of in H and L density, which species these are
      ###have a graph of each species, use all.bkgrd

ggsave("BRHO.bkgrd.replicate.density.png", width = 8, height = 5)

#INDIVIDUAL SPECIES

summary.all.bkgrd <- all.bkgrd %>%
  group_by(treatment, bkgrd, phyto, origin, functional_group) %>%
  summarize(mean.biomass = mean(total.biomass.rounded.percap, na.rm = TRUE), se.biomass = calcSE(total.biomass.rounded.percap))
#summarize = creating a new column 

ggplot(summary.all.bkgrd[summary.all.bkgrd$phyto == "ACAM",], aes(x=bkgrd, y=mean.biomass, color=treatment)) + 
  geom_point() +
  geom_errorbar(aes(ymin = mean.biomass-se.biomass, ymax = mean.biomass + se.biomass), width = 0.25) +
  ggtitle("ACAM")
#[rows, columns]
ggsave("ACAM.png", width = 8, height = 5)

#COMPARE ALL VARIABLES

summary.all.bkgrd.origin <- all.bkgrd %>%
  group_by(treatment, bkgrd, origin, functional_group) %>%
  summarize(mean.biomass = mean(total.biomass.rounded.percap, na.rm = TRUE), se.biomass = calcSE(total.biomass.rounded.percap))

ggplot(summary.all.bkgrd.origin, aes(x=bkgrd, y=mean.biomass, color=origin, shape = functional_group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.biomass-se.biomass, ymax = mean.biomass + se.biomass), width = 0.25) +
  facet_wrap(~treatment)
#[rows, columns]
ggsave("all.variables.png", width = 8, height = 5)

#ORIGIN x TREATMENT

#FUNCTIONAL GROUP x TREATMENT


#TO-DO
#look for outliers in plotted data: visualize each species individually; take MAEL out to compare other species 

#replot with the most up to date data (use final.bkgrd dataframe)
  #compare functional group and treatment and bkgrd and origin 
#make a model to see if density has any affect on biomass
  #talk to group about this
#look for samples that still need to be processed 
#before 12/5 have new graphs 