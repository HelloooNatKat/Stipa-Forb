source("data_cleaning/merge_stipa_brho_data.R")

#SE Function ####
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

no_total_biomass <- all.bkgrd %>%
  filter(is.na(total.biomass.rounded.percap))

ggplot(all.bkgrd, aes(x=phyto, y=total.biomass.rounded.percap)) + 
  geom_boxplot() 

ggplot(all.bkgrd, aes(x=treatment, y=total.biomass.rounded.percap, color=bkgrd)) + 
  geom_boxplot() +
  facet_wrap(~phyto, scales="free")
#check for LENI; it's missing Stipa 


