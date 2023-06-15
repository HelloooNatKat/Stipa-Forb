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



