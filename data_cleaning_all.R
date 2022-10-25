# load packages
library(tidyverse)

# Read in Data ####
# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/MegaComp_Stipa_Overlap/Data/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/MegaComp_Stipa_Overlap/Data/"
  
} else {
  # Nat
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/"
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


