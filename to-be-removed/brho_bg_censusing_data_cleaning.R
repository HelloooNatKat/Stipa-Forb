## load packages
library(googlesheets4)
library(plyr)
library(tidyverse)
library(googledrive)
library(openxlsx)


# Read in Data ####
lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/" # Carmen's file path
date <- 20220825

## Collections data
collections <- read.xlsx(paste0(lead, "Collections/Collections_merged/", date, "_MASTER_Collections_2-in-progress.xlsx"), sheet = 2)

## make sure the dates read in correctly
collections$phyto.date.collect <- as.Date(collections$phyto.date.collect, origin = "1899-12-30")
collections$phyto.date.census <- as.Date(collections$phyto.date.census, origin = "1899-12-30")
collections$bg.date.census <- as.Date(collections$bg.date.census, origin = "1899-12-30")
collections$phyto.unique <- as.character(collections$phyto.unique)


## subset the data for stipa-forb phytos only

sf_phytos <- c("BRHO", "GITR", "LENI", "PLER", "ANAR", "LOMU", "ACAM", "AMME", "THIR-I", "TWIL-I")
bgs <- c("BRHO", "Control")

census_data_brho_bgs <- collections %>%
  filter(phyto %in% sf_phytos, bkgrd %in% bgs) %>% ## select only relevant phytos and backgrounds
  mutate(phyto2 = ifelse(phyto == "TWIL-I", "TWIL", ## change the names of Trifolium phytos
                        ifelse(phyto == "THIR-I", "THIR", phyto))) %>%
  mutate(phyto = phyto2) %>% ## keep the correct column name
  select(-phyto2) ## drop the redundant column
#unique(census_data_brho_bgs$phyto2)

write.csv(census_data_brho_bgs, "brho_control_collection_data.csv")

