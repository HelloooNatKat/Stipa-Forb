# Set up env
library(tidyverse)

# Read in Data ####
## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/MegaComp_Stipa_Overlap/Data/MegaComp_Data/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/MegaComp_Stipa_Overlap/Data/MegaComp_Data/"
  
} else {
  # Nat
  lead <- "/Users/natkataoka/Dropbox/MegaComp_Stipa_Overlap/Data/MegaComp_Data/"
} 

date_processing <- 20221018

## Processing data
acam <- read.csv(paste0(lead, "ACAM_phyto-processing_", date_processing, ".csv"))
anar <- read.csv(paste0(lead, "ANAR_phyto-processing_", date_processing, ".csv"))
brho <- read.csv(paste0(lead, "BRHO_phyto-processing_", date_processing, ".csv"))
gitr <- read.csv(paste0(lead, "GITR_phyto-processing_", date_processing, ".csv"))
leni <- read.csv(paste0(lead, "LENI_phyto-processing_", date_processing, ".csv"))
lomu <- read.csv(paste0(lead, "LOMU_phyto-processing_", date_processing, ".csv"))
pler <- read.csv(paste0(lead, "PLER_phyto-processing_", date_processing, ".csv"))
thir <- read.csv(paste0(lead, "THIR_phyto-processing_", date_processing, ".csv"))
twil <- read.csv(paste0(lead, "TWIL_phyto-processing_", date_processing, ".csv"))
mael <- read.csv(paste0(lead, "MAEL_phyto-processing_", "20221122", ".csv")) %>%
  mutate(scale.ID = NA)

# Define Cleaning Function ####
basic_cleaning_func <- function(phyto_data, ...) {
  
  drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vector
  
  temp <- phyto_data %>%
    mutate(complete.sample = complete., ## change column names
           unique.ID = unique,
           treatment = ifelse(block %in% drought, "D", "C")) %>%  ## add a treatment column
    
    mutate(across(where(is.character), str_trim)) %>% ## remove leading & trailing whitespace!!
    
    mutate(across(c(phyto.unique,scale.ID,complete.sample), toupper)) %>% ## capitalize all vals 
    
    mutate_all(na_if,"") %>% ## make blank values NAs
    
    filter(plot < 43, bkgrd == "BRHO" | bkgrd == "Control")
  
  return(temp)
  
}

# Clean DFs ####
## create vector of species to clean
sp.processed <- c("acam", "anar", "brho", "gitr", "leni", "lomu", "pler", "thir", "twil", "mael")

## clean each species and save output separately since colnames do not match between dataframes
for(i in 1:length(sp.processed)) {
  
  sp <- get(sp.processed[i]) ## fetch dataframe
  temp <- basic_cleaning_func(sp) ## apply cleaning function
  assign(paste0(sp.processed[i], "MC"), temp) ## rename the output, keeping species separate
  
}

# Clean Env ####
rm(list = c("acam", "anar", "brho", "gitr", "leni", "lomu", "pler", "thir", "twil", "mael", "temp", "sp"))


colnames(acamMC)
colnames(anarMC)
colnames(gitrMC)
colnames(thirMC)
colnames(twilMC)
colnames(lomuMC)
## important measured columns: total.biomass.g, flower.num, seed.num

colnames(brhoMC)
colnames(plerMC)
## important measured columns: inflor.g, seed.num, empty.flower.num, flower.num

colnames(leniMC)
## important measured columns: total.stem.length.mm, pod.num

colnames(maelMC)
## important measured columns: total.biomass.g, flower.num

acamMC2 <- acamMC %>%
  mutate(inflor.g = NA, 
         seed.num = NA, 
         empty.flower.num = NA,
         total.stem.length.mm = NA,
         pod.num = NA) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, total.stem.length.mm, pod.num, scale.ID, process.notes, census.notes, unique.ID)

anarMC2 <- anarMC %>%
  mutate(inflor.g = NA, 
         seed.num = NA, 
         empty.flower.num = NA,
         total.stem.length.mm = NA,
         pod.num = NA,
         census.notes = notes) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, total.stem.length.mm, pod.num, scale.ID, process.notes, census.notes, unique.ID)


gitrMC2 <- gitrMC %>%
  mutate(inflor.g = NA, 
         seed.num = NA, 
         empty.flower.num = NA,
         total.stem.length.mm = NA,
         pod.num = NA) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, total.stem.length.mm, pod.num, scale.ID, process.notes, census.notes, unique.ID)


thirMC2 <- thirMC %>%
  mutate(empty.flower.num = NA,
         total.stem.length.mm = NA,
         pod.num = NA, 
         flower.num = NA) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, total.stem.length.mm, pod.num, scale.ID, process.notes, census.notes, unique.ID)


twilMC2 <- twilMC %>%
  mutate(inflor.g = NA, 
         empty.flower.num = NA,
         total.stem.length.mm = NA,
         pod.num = NA, 
         flower.num = NA) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, total.stem.length.mm, pod.num, scale.ID, process.notes, census.notes, unique.ID)

lomuMC2 <- lomuMC %>%
  mutate(inflor.g = NA, 
         empty.flower.num = NA,
         total.stem.length.mm = NA,
         pod.num = NA, 
         flower.num = NA) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, total.stem.length.mm, pod.num, scale.ID, process.notes, census.notes, unique.ID)

brhoMC2 <- brhoMC %>%
  mutate(empty.flower.num = NA,
         total.stem.length.mm = NA,
         pod.num = NA, 
         flower.num = NA) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, total.stem.length.mm, pod.num, scale.ID, process.notes, census.notes, unique.ID)

plerMC2 <- plerMC %>%
  mutate(total.stem.length.mm = NA,
         pod.num = NA) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, total.stem.length.mm, pod.num, scale.ID, process.notes, census.notes, unique.ID)

leniMC2 <- leniMC %>%
  mutate(empty.flower.num = NA,
         inflor.g = NA, 
         flower.num = NA,
         total.biomass.g = total.biomass) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, total.stem.length.mm, pod.num, scale.ID, process.notes, census.notes, unique.ID)

maelMC2 <- maelMC %>%
  mutate(empty.flower.num = NA,
         inflor.g = NA,
         seed.num = NA,
         total.stem.length.mm = NA, 
         pod.num = NA, 
         census.notes = notes) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, total.stem.length.mm, pod.num, scale.ID, process.notes, census.notes, unique.ID)

BC.background.merged <- do.call("rbind", list(acamMC2, anarMC2, brhoMC2, gitrMC2, lomuMC2, leniMC2, maelMC2, plerMC2, thirMC2, twilMC2)) %>%
  mutate(total.biomass.rounded.percap = total.biomass.g/phyto.n.indiv,
         total.stem.length.mm.percap = total.stem.length.mm/phyto.n.indiv, 
         empty.flower.num.percap = empty.flower.num/phyto.n.indiv,
         flower.num.percap = flower.num/phyto.n.indiv,
         inflor.g.rounded.percap =  inflor.g/phyto.n.indiv,
         seed.num.percap = seed.num/phyto.n.indiv
         ) %>%
  mutate(phyto = ifelse(phyto == "THIR-I", "THIR", 
                        ifelse(phyto == "TWIL-I", "TWIL", phyto))) %>%
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.rounded.percap, total.stem.length.mm.percap, empty.flower.num.percap, flower.num.percap, scale.ID, inflor.g.rounded.percap, seed.num.percap, process.notes, census.notes)


## To Do Still: 
    ## filter out incompletes for all species except PLER
    ## connect this script to data_cleaning_all

rm(list = c("acamMC", "acamMC2", "anarMC", "anarMC2", "brhoMC", "brhoMC2", "gitrMC", "gitrMC2", "i", "lead", "leniMC", "leniMC2", "lomuMC", "lomuMC2", "maelMC", "maelMC2", "plerMC", "plerMC2", "thirMC", "thirMC2", "twilMC", "twilMC2"))

