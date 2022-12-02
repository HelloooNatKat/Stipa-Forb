source("data_cleaning/merge_stipa_brho_data.R")
#Warning: bkgrd == c("BRHO", "Control") longer object length is not a multiple of shorter object length

colnames(all.bkgrd)
unique(all.bkgrd$block)
#block 11 is not a megacomp block 

ggplot(all.bkgrd, aes(x=phyto, y=total.biomass.rounded.percap)) + 
  geom_boxplot() 

no_total_biomass <- all.bkgrd %>%
  filter(is.na(total.biomass.rounded.percap)) 
#15 rows don't have total biomass 
  #remove AVBA
  #LENI still needs processing
  #MAEL still needed to be processed
  #remove ones no phytos

ggplot(all.bkgrd, aes(x=phyto, y=total.biomass.rounded.percap, color=bkgrd)) + 
  geom_boxplot() 
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

#TO-DO
#look for outliers in plotted data: visualize each species individually; take MAEL out to compare other species 

#replot with the most up to date data (use final.bkgrd dataframe)
  #compare functional group and treatment and bkgrd and origin 
#make a model to see if density has any affect on biomass
  #talk to group about this
#look for samples that still need to be processed 
#before 12/5 have new graphs 