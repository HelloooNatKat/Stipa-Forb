## Stipa - Forb Paper

## data from BRHO backgrounds

## load packages
library(googlesheets4)
library(plyr)
library(tidyverse)
library(googledrive)
library(openxlsx)

# Read in Data ####
lead <- "/Users/carme/Dropbox (University of Oregon)/MegaComp_Stipa_Overlap/Data/" # Carmen's file path
date <- 20220906

## Processing data
acam <- read.xlsx(paste0(lead, date, "_Phyto-Processing.xlsx"), sheet = 1)
anar <- read.xlsx(paste0(lead, date, "_Phyto-Processing.xlsx"), sheet = 2)
brho <- read.xlsx(paste0(lead, date, "_Phyto-Processing.xlsx"), sheet = 5)
#clpu <- read.xlsx(paste0(lead, date, "_Phyto-Processing.xlsx"), sheet = 8)
gitr <- read.xlsx(paste0(lead, date, "_Phyto-Processing.xlsx"), sheet = 9)
leni <- read.xlsx(paste0(lead, date, "_Phyto-Processing.xlsx"), sheet = 10)
lomu <- read.xlsx(paste0(lead, date, "_Phyto-Processing.xlsx"), sheet = 11)
mael <- read.xlsx(paste0(lead, date, "_Phyto-Processing.xlsx"), sheet = 12)
pler <- read.xlsx(paste0(lead, date, "_Phyto-Processing.xlsx"), sheet = 14)
thir <- read.xlsx(paste0(lead, date, "_Phyto-Processing.xlsx"), sheet = 17)
twil <- read.xlsx(paste0(lead, date, "_Phyto-Processing.xlsx"), sheet = 18)


# Standardize Stipa Data ####

drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vector
## change the column names on the stipa forb data sheet
stipa.forbC <- stipa.forb %>%
  mutate(bkgrd = "Stipa", 
         phyto.n.indiv = phyto, 
         phyto = name, 
         sub = NA, 
         dens = NA, 
         phyto.unique = NA,
         scale.ID = NA,
         process.notes = NA,
         treatment = ifelse(block %in% drought, "D", "C")) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, scale.ID, process.notes)

colnames(stipa.forbC) 

# Standardize ACAM ####
acamC <- acam %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
         complete = `complete?`,
         biomass.no.seed.g = NA, 
         inflor.g = NA, 
         seed.num = NA,
         total.stem.length.mm = NA, 
         empty.flower.num = NA) %>%
  filter(bkgrd == "BRHO" | bkgrd == "Control") %>% ## select only the BRHO & control backgrounds
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, scale.ID, process.notes) ## make sure column ordering works

## add in acam data
tempacam <- rbind(stipa.forbC, acamC)

# Standardize ANAR ####
anarC <- anar %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
         complete = `complete?`,
         #biomass.no.seed.g = NA, 
         #inflor.g = NA, 
         #seed.num = NA,
         flower.num = NA, 
         total.stem.length.mm = NA, 
         empty.flower.num = NA, 
         scale.ID = NA) %>% ## although I believe this was the most fine scale we have?
  filter(bkgrd == "BRHO" | bkgrd == "Control") %>% ## select only the BRHO & control backgrounds
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, scale.ID, process.notes) ## make sure column ordering works


## add in anar data
tempanar <- rbind(tempacam, anarC)

# Standardize BRHO ####
brhoC <- brho %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
         complete = `complete?`,
         #biomass.no.seed.g = NA, 
         #inflor.g = NA, 
         #seed.num = NA,
         flower.num = NA, 
         total.stem.length.mm = NA, 
         empty.flower.num = NA, 
         #scale.ID = NA
         ) %>% ## although I believe this was the most fine scale we have?
  filter(bkgrd == "BRHO" | bkgrd == "Control") %>% ## select only the BRHO & control backgrounds
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, scale.ID, process.notes) ## make sure column ordering works


## add in anar data
tempbrho <- rbind(tempanar, brhoC)

# Standardize GITR ####
gitrC <- gitr %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
         complete = `complete?`,
         biomass.no.seed.g = NA, 
         inflor.g = NA, 
         seed.num = NA,
         #flower.num = NA, 
         total.stem.length.mm = NA, 
         empty.flower.num = NA, 
         #scale.ID = NA
  ) %>% ## although I believe this was the most fine scale we have?
  filter(bkgrd == "BRHO" | bkgrd == "Control") %>% ## select only the BRHO & control backgrounds
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, scale.ID, process.notes) ## make sure column ordering works


## add in anar data
tempgitr <- rbind(tempbrho, gitrC)


# Standardize LENI ####
## fix the first column which doesn't have a name
colnames(leni)[colnames(leni) == ""] <- "block"

leniC <- leni %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
         complete = `complete?`,
         total.biomass.g = total.biomass,
         biomass.no.seed.g = NA, 
         inflor.g = NA, 
         #seed.num = NA,
         flower.num = pod.num, ## this is technically correct? 
         #total.stem.length.mm = NA, 
         empty.flower.num = NA, 
         #scale.ID = NA
  ) %>% ## although I believe this was the most fine scale we have?
  filter(bkgrd == "BRHO" | bkgrd == "Control") %>% ## select only the BRHO & control backgrounds
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, scale.ID, process.notes) ## make sure column ordering works


## add in leni data
templeni <- rbind(tempgitr, leniC)


# Standardize LOMU ####
#lomuC <- lomu %>%
 # mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
     #    complete = `complete?`,
      #   biomass.no.seed.g = NA, 
      #   inflor.g = NA, 
    #     seed.num = NA,
         #flower.num = NA, 
     #    total.stem.length.mm = NA, 
     #    empty.flower.num = NA, 
         #scale.ID = NA
 # ) %>% ## although I believe this was the most fine scale we have?
#  filter(bkgrd == "BRHO" | bkgrd == "Control") %>% ## select only the BRHO & control backgrounds
#  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, scale.ID, process.notes) ## make sure column ordering works


## add in lomu data
#tempgitr <- rbind(tempbrho, gitrC)

# Standardize PLER ####
plerC <- pler %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
         complete = `complete?`,
         biomass.no.seed.g = NA, 
         #inflor.g = NA, 
         #seed.num = NA,
         #flower.num = NA, 
         total.stem.length.mm = NA, 
         #empty.flower.num = NA, 
         #scale.ID = NA
  ) %>% ## although I believe this was the most fine scale we have?
  filter(bkgrd == "BRHO" | bkgrd == "Control") %>% ## select only the BRHO & control backgrounds
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, scale.ID, process.notes) ## make sure column ordering works


## add in pler data
temppler <- rbind(templeni, plerC)

# Standardize THIR ####
thirC <- thir %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
         complete = `complete?`,
         #biomass.no.seed.g = NA, 
         #inflor.g = NA, 
         #seed.num = NA,
         flower.num = NA, 
         total.stem.length.mm = NA, 
         empty.flower.num = NA, 
         #scale.ID = NA
  ) %>% ## although I believe this was the most fine scale we have?
  filter(bkgrd == "BRHO" | bkgrd == "Control", phyto == "THIR-I") %>% ## select only the BRHO & control backgrounds
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, scale.ID, process.notes) ## make sure column ordering works


## add in pler data
tempthir <- rbind(temppler, thirC)


# Standardize TWIL ####
twilC <- twil %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
         complete = `complete?`,
         biomass.no.seed.g = NA, 
         inflor.g = NA, 
         #seed.num = NA,
         flower.num = NA, 
         total.stem.length.mm = NA, 
         empty.flower.num = NA, 
         #scale.ID = NA
  ) %>% ## although I believe this was the most fine scale we have?
  filter(bkgrd == "BRHO" | bkgrd == "Control", phyto == "TWIL-I") %>% ## select only the BRHO & control backgrounds
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, scale.ID, process.notes) ## make sure column ordering works


## add in pler data
temptwil <- rbind(tempthir, twilC)


SF.all.dat <- temptwil
write.csv(SF.all.dat, "stipa-forb-brho_all-data.csv")
