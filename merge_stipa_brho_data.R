
# read in data ####
## first, stipa backgrounds
source("data_cleaning/stipa_initial_clean.R")

## then brho & control backgrounds
source("data_cleaning/brho-control_initial_clean.R")

# merge data frames ####

## first, define sp functional group/origin
native <- c("TWIL", "PLER", "LENI", "GITR", "ACAM", "MAEL")
non_native <- c("ANAR", "BRHO", "LOMU", "THIR")

grass <- c("BRHO", "LOMU")
forb <- c("TWIL", "PLER", "LENI", "GITR", "ACAM", "MAEL", "ANAR", "THIR")

drought <- c(1, 3, 4, 6, 12, 14)

## merge
all.bkgrd <- rbind(stipa.background.merged,BC.background.merged) %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C")) %>%
  filter(phyto.n.indiv != 0) %>%
  mutate(functional_group = ifelse(phyto %in% grass, "grass","forb"),
         origin = ifelse(phyto %in% native, "native", "non_native"))

#you don't need quotations for a number
#you need == because it's a logical statement (ifelse)
