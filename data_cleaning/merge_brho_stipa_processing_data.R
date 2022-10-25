
## Read in Data
## Read in all of the mega-comp processing data
source("../mega-competition/data_cleaning/clean_summer_phyto_processing_data.R")

## Read in the stipa background data
stipa.forb <- read.csv("stipa-forb_phyto-processing - stipa-forb_data_2022-07-27.csv")
blockkey <- read.csv("block-key.csv")%>%
  select(block, treatment)

# Filter for correct phytos ####
sf_phytos <- c("BRHO", "GITR", "LENI", "PLER", "ANAR", "LOMU", "ACAM", "THIR-I", "TWIL-I", "MAEL", "ERBO", "CLPU") ## all 12 phytos
bgs <- c("BRHO", "Control")


brho_bg_dat <- proc_dat_clean %>%
  filter(phyto %in% sf_phytos, 
         bkgrd %in% bgs) %>%
  select(-glume.num, -glume.num.percap, -biomass.no.seed.g, -total.biomass.g.rounded, -inflor.g.rounded)
## 8/12 phytos currently processed on the mega-comp end.

colnames(brho_bg_dat)
colnames(stipa.forb)

# Standardize Stipa Data ####
drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vector

stipa.forb$phyto <- as.numeric(stipa.forb$phyto)

## change the column names on the stipa forb data sheet
stipa.forbC <- stipa.forb %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"),
         sub = NA,
         bkgrd = "Stipa", 
         dens = NA,
         phyto.n.indiv = phyto,
         phyto = name,
         phyto.unique = NA,
         complete.sample = complete,
         pod.num = NA,
         seeds.present = NA,
         scale.ID = NA,
         process.notes = NA,
         census.notes = NA,
         unique.ID = NA,
         total.biomass.rounded.percap = total.biomass.g/phyto.n.indiv,
         inflor.g.rounded.percap = inflor.g/phyto.n.indiv,
         seed.num.percap = seed.num/phyto.n.indiv,
         flower.num.percap = flower.num/phyto.n.indiv,
         total.stem.length.mm.percap = total.stem.length.mm/phyto.n.indiv,
         empty.flower.num.percap = empty.flower.num/phyto.n.indiv,
         pod.num.percap = pod.num/phyto.n.indiv
         ) %>%
  select(colnames(brho_bg_dat))

colnames(stipa.forbC) 

# Merge the two ####
sf_all <- rbind(stipa.forbC, brho_bg_dat)
write.csv(sf_all, "stipa-brho_processing-data_20220916.csv")

