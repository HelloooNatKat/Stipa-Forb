source("data_cleaning/data_cleaning_all.R")
#Warning: bkgrd == c("BRHO", "Control") longer object length is not a multiple of shorter object length

colnames(all.bkgrd)
unique(all.bkgrd$block)
#block 11 is not a megacomp block 

drought <- c(1, 3, 4, 6, 12, 14)
all.bkgrd.treatment <- all.bkgrd %>% 
  mutate(treatment = ifelse(block %in% drought, "D", "C")) %>%
  filter(phyto != "AVBA", phyto != "MICA", phyto.n.indiv != 0) %>%
  mutate(phyto = ifelse(phyto == "THIR-I", "THIR", phyto)) %>%
  mutate(phyto = ifelse(phyto == "TWIL-I", "TWIL", phyto))
  #you don't need quotations for a number
  #you need == because it's a logical statement (ifelse)

unique(all.bkgrd.treatment$phyto.n.indiv)
#checking to see if we have 0's left
unique(all.bkgrd.treatment$phyto)

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

new.bkgrd <- all.bkgrd.treatment %>%
  group_by(treatment, bkgrd, phyto) %>%
  summarise(reps = n())
#group_by = looking for unique combination of the variables you list
# n = counts the number of observations 

#TO-DO
#look at the number of replicates (we need at least 3)
#look for outliers in plotted data 
#replot with the most up to date data
#look for samples that still need to be processed 
#before 12/5 have new graphs 