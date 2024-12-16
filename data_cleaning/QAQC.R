source("data_cleaning/merge_stipa_brho_data.R")

#SE Function ####
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## check for NAs in total biomass per capita
no_total_biomass <- all.bkgrd %>%
  filter(is.na(total.biomass.rounded.percap))
    ## no NAs here, good!

## check total biomass values for each species
ggplot(all.bkgrd, aes(x=phyto, y=total.biomass.rounded.percap)) + 
  geom_boxplot() 
    ## MAEL much larger values than other species (makes sense)

## check that all sp have values for all rainfall and background treatments
ggplot(all.bkgrd, aes(x=treatment, y=total.biomass.rounded.percap, color=bkgrd)) + 
  geom_boxplot() +
  facet_wrap(~phyto, scales="free")
    ## TWIL C has no control background data...

## clean env
rm(no_total_biomass)
