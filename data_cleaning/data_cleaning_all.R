# load packages
library(tidyverse)

# Read in Data ####
# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/MegaComp_Stipa_Overlap/Data/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/MegaComp_Stipa_Overlap/Data/"
  
} else {
  # Nat
  lead <- "/Users/natkataoka/Dropbox/MegaComp_Stipa_Overlap/Data/"
} 

## Stipa BG data ####
acam <- read.csv(paste0(lead, "stipa-forb_phyto-processing_ACAM_20221025.csv"))
anar <- read.csv(paste0(lead, "stipa-forb_phyto-processing_ANAR_20221025.csv"))
brho <- read.csv(paste0(lead, "stipa-forb_phyto-processing_BRHO_20221025.csv"))
gitr <- read.csv(paste0(lead, "stipa-forb_phyto-processing_GITR_20221025.csv"))
leni <- read.csv(paste0(lead, "stipa-forb_phyto-processing_LENI_20221025.csv"))
lomu <- read.csv(paste0(lead, "stipa-forb_phyto-processing_LOMU_20221025.csv"))
mael <- read.csv(paste0(lead, "stipa-forb_phyto-processing_MAEL_20221025.csv"))
pler <- read.csv(paste0(lead, "stipa-forb_phyto-processing_PLER_20221025.csv"))
thir <- read.csv(paste0(lead, "stipa-forb_phyto-processing_THIR_20221025.csv"))
twil <- read.csv(paste0(lead, "stipa-forb_phyto-processing_TWIL_20221025.csv"))


## BRHO/Control BG data ####
brho_BG <- read.csv(paste0(lead, "phyto_merged-prelim-data_20221025.csv")) %>%
  filter(bkgrd == c("BRHO", "Control")) ## filter for BRHO & Controls


# Merge Stipa-BG data ####
colnames(acam)
colnames(anar)
colnames(brho)
colnames(gitr)
colnames(leni)
colnames(lomu)
head(lomu)
unique(lomu$seed.num)
unique(lomu$seed.num.1)
#unique: gives you all unique values in a column 
colnames(mael)
colnames(pler)
colnames(thir)
colnames(twil)

#block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete., total.biomass.g, total.stem.length.mm, empty.flower.num, flower.num, scale.ID, inflor.g, seed.num, process.notes, census.notes

acamC <- acam %>% 
  mutate(total.stem.length.mm = NA,
         scale.ID = NA,
         empty.flower.num = NA, 
         flower.num = NA) %>%
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete., total.biomass.g, total.stem.length.mm, empty.flower.num, flower.num, scale.ID, inflor.g, seed.num, process.notes, census.notes)

colnames(acamC)
#mutate in this case adding blank column for easier merging across sheets

anarC <- anar %>%
  mutate(total.stem.length.mm = NA,
         scale.ID = NA,
         empty.flower.num = NA, 
         flower.num = NA, 
         phyto.n.indiv = phyto,
         phyto = "ANAR") %>%
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete., total.biomass.g, total.stem.length.mm, empty.flower.num, flower.num, scale.ID, inflor.g, seed.num, process.notes, census.notes)

brhoC <- brho %>%
  mutate(total.stem.length.mm = NA,
         empty.flower.num = NA, 
         flower.num = NA,
         phyto.n.indiv = phyto,
         phyto = "BRHO") %>%
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete., total.biomass.g, total.stem.length.mm, empty.flower.num, flower.num, scale.ID, inflor.g, seed.num, process.notes, census.notes)

gitrC <- gitr %>% 
  mutate(total.stem.length.mm = NA,
         empty.flower.num = NA,
         inflor.g = NA, 
         seed.num = NA,
         phyto.n.indiv = phyto,
         phyto = "GITR") %>%
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete., total.biomass.g, total.stem.length.mm, empty.flower.num, flower.num, scale.ID, inflor.g, seed.num, process.notes, census.notes)

leniC <- leni %>%
  mutate(empty.flower.num = NA,
         inflor.g = NA, 
         seed.num = NA, 
         empty.flower.num = NA,
         flower.num = NA,
         scale.ID = NA,
         total.biomass.g = total.biomass,
         phyto.n.indiv = phyto,
         phyto = "LENI") %>%
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete., total.biomass.g, total.stem.length.mm, empty.flower.num, flower.num, scale.ID, inflor.g, seed.num, process.notes, census.notes)

lomuC <- lomu %>%
  mutate(total.stem.length.mm = NA,
         empty.flower.num = NA,
         flower.num = NA,
         scale.ID = NA, 
         phyto.n.indiv = phyto,
         phyto = "LOMU") %>%
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete., total.biomass.g, total.stem.length.mm, empty.flower.num, flower.num, scale.ID, inflor.g, seed.num, process.notes, census.notes)

maelC <- mael %>%
  mutate(total.stem.length.mm = NA,
         empty.flower.num = NA,
         scale.ID = NA) %>%
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete., total.biomass.g, total.stem.length.mm, empty.flower.num, flower.num, scale.ID, inflor.g, seed.num, process.notes, census.notes)


plerC <- pler %>%
  mutate(total.stem.length.mm = NA,
         scale.ID = NA,
         phyto.n.indiv = phyto,
         phyto = "PLER") %>%
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete., total.biomass.g, total.stem.length.mm, empty.flower.num, flower.num, scale.ID, inflor.g, seed.num, process.notes, census.notes)
  
thirC <- thir %>%
  mutate(total.stem.length.mm = NA,
         empty.flower.num = NA,
         flower.num = NA,
         scale.ID = NA, 
         phyto.n.indiv = phyto,
         phyto = "THIR") %>%
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete., total.biomass.g, total.stem.length.mm, empty.flower.num, flower.num, scale.ID, inflor.g, seed.num, process.notes, census.notes)

twilC <- twil %>%
  mutate(total.stem.length.mm = NA, 
         empty.flower.num = NA,
         flower.num = NA,
         scale.ID = NA,
         phyto.n.indiv = phyto,
         phyto = "TWIL") %>%
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete., total.biomass.g, total.stem.length.mm, empty.flower.num, flower.num, scale.ID, inflor.g, seed.num, process.notes, census.notes)

#select(-pod.num); this is if you just want to get rid of one column

stipa.background.merged <- do.call("rbind", list(acamC, anarC, brhoC, gitrC, lomuC, leniC, maelC, plerC, thirC, twilC))%>%
  mutate(bkgrd = "Stipa", 
         total.biomass.rounded.percap = total.biomass.g/phyto.n.indiv,
         total.stem.length.mm.percap = total.stem.length.mm/phyto.n.indiv, 
         empty.flower.num.percap = empty.flower.num/phyto.n.indiv,
         flower.num.percap = flower.num/phyto.n.indiv,
         inflor.g.rounded.percap =  inflor.g/phyto.n.indiv,
         seed.num.percap = seed.num/phyto.n.indiv,
         complete.sample = complete.) %>%
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.rounded.percap, total.stem.length.mm.percap, empty.flower.num.percap, flower.num.percap, scale.ID, inflor.g.rounded.percap, seed.num.percap, process.notes, census.notes)
 
brho_BG_clean <- brho_BG %>% 
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.rounded.percap, total.stem.length.mm.percap, empty.flower.num.percap, flower.num.percap, scale.ID, inflor.g.rounded.percap, seed.num.percap, process.notes, census.notes)

all.bkgrd <- rbind(stipa.background.merged,brho_BG_clean)

rm(list=setdiff(ls(), "all.bkgrd"))
#rm = remove, rm("name") = deletes it from environment, list=c("item1","item2")= deletes list of items, ls = lists everything in the environmnet, setdiff(ls()), "name" = lists everything then, compares them, then deletes everything that isn't shared between ls and "name" 

#TO-DO
#look at the number of replicates (we need at least 3)
#look for outliers in plotted data 
#look for samples that still need to be processed 
#clean up the environment 