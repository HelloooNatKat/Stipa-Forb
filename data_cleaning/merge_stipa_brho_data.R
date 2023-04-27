# read in data ####
#set working directory to Downloads/Stipa-Forb
## first, stipa backgrounds
source("data_cleaning/stipa_initial_clean.R") #this will clear the entire environment, put it first

if(file.exists("/Users/carme/Dropbox (University of Oregon)/MegaComp_Stipa_Overlap/Data/MegaComp_Data/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/MegaComp_Stipa_Overlap/Data/MegaComp_Data/"
  
} else {
  # Nat
  lead <- "/Users/natkataoka/Dropbox/MegaComp_Stipa_Overlap/Data/MegaComp_Data/"
}

brho_control_clean <- read.csv(paste0(lead,"brho_control_bkgrd_20230419.csv"))

#check to see if columns match between datasheets
colnames(brho_control_clean)
colnames(stipa.background.merged)
  
brho_control_final <- brho_control_clean %>%
  select(-X, -unique.ID)

stipa.background.merged_final <- stipa.background.merged %>% 
  filter(complete.sample != "N") %>%
  select(-complete.sample, -total.stem.length.mm.percap)

# merge data frames ####

## first, define sp functional group/origin; creating vectors
native <- c("TWIL", "PLER", "LENI", "GITR", "ACAM", "MAEL")
non_native <- c("ANAR", "BRHO", "LOMU", "THIR")

grass <- c("BRHO", "LOMU")
forb <- c("TWIL", "PLER", "LENI", "GITR", "ACAM", "MAEL", "ANAR", "THIR")

drought <- c(1, 3, 4, 6, 12, 14)

## merge; rbind is adding more rows on the bottom using similar column names
all.bkgrd <- rbind(stipa.background.merged_final,brho_control_final) %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C")) %>%
  filter(phyto.n.indiv != 0) %>%
  mutate(functional_group = ifelse(phyto %in% grass, "grass","forb"),
         origin = ifelse(phyto %in% native, "native", "non_native"))

rm(brho_control_clean, brho_control_final, stipa.background.merged, stipa.background.merged_final)

#you don't need quotations for a number
#you need == because it's a logical statement (ifelse)
