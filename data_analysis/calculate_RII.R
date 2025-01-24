## Header 
## 
## Script Name: Calculate Relative Interaction Intensity 
##
## Script Description: Calculate the relative interaction intensity (Armas et al. 2004)
##
## Script Notes: 
## RII is calculated within a block where possible 
## need to have a phyto in both a control plot and a BRHO or Stipa plot within a block for this
## this was not always possible
## when no control plot was present in a block with a phyto in BRHO or Stipa, took the mean control biomass (within a precip treatment) and substituted this value in

# Set up ####
## read in cleaned data
source("data_cleaning/QAQC.R")
unique(all.bkgrd$phyto)

# ACAM ####
## Stipa ####
acamS = all.bkgrd %>%
  filter(phyto == "ACAM", bkgrd != "BRHO") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group) #%>%
#  mutate(pair = ifelse(block == 1 & bkgrd == "BRHO", 1:4, NA))

table(acamS$bkgrd, acamS$block)
## need a control: 5, 6

### fill cols ####
## separate out controls & take mean of both ambient & drought
acamCmean = acamS %>%
  filter(bkgrd == "Control") %>%
  group_by(treatment) %>%
  summarise(mC = mean(total.biomass.rounded.percap, na.rm = T),
            reps = n())

## block 6
acam_b6C = acamS[acamS$block == 1 & acamS$bkgrd == "Control",] ## select a row to change values in
acam_b6C2 = acam_b6C %>% ## change
  mutate(block = 6, 
         total.biomass.rounded.percap = acamCmean[acamCmean$treatment == "D",]$mC)

## fill in a row for block 6 with the average ambient ppt, control bkgrd biomass
acam_b5C = acamS[acamS$block == 7 & acamS$bkgrd == "Control",] ## select a row to change values in
acam_b5C2 = acam_b5C %>% ## change
  mutate(block = 5,
         total.biomass.rounded.percap = acamCmean[acamCmean$treatment == "C",]$mC)

## merge back together
acamS2 = rbind(acamS, acam_b6C2, acam_b5C2)
table(acamS2$bkgrd, acamS2$block)

### Calc RII ####
## calculate response ratio
acamSRII = acamS2 %>%
 
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (Stipa - Control)/(Stipa + Control),
         bkgrd = "Stipa",
         pair = NA) %>%
  select(-Stipa, -Control)

## BRHO ####
acamB = all.bkgrd %>%
  filter(phyto == "ACAM", bkgrd != "Stipa") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group) 

table(acamB$bkgrd, acamB$block)
## need a control: 5, 6 - can use the same mean controls from Stipa above

### fill cols ####
acamB2 = rbind(acamB, acam_b6C2, acam_b5C2)
table(acamB2$bkgrd, acamB2$block)

### Calc RII ####
## calculate response ratio
acamBRII = acamB2 %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (BRHO - Control)/(BRHO + Control),
         bkgrd = "BRHO") %>%
  select(-BRHO, -Control)

## Merge ####
acamRII_all = rbind(acamBRII, acamSRII) %>%
  filter(!is.na(RII))

table(acamRII_all$bkgrd, acamRII_all$treatment)

acamRII_all %>%
  group_by(bkgrd, treatment) %>%
  summarise(meanRII = mean(RII), 
            seRII = calcSE(RII)) %>%
  ggplot(aes(x=treatment, y=meanRII, color = bkgrd)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = meanRII - seRII, ymax = meanRII + seRII), width = 0.25) +
  geom_hline(yintercept = 0)

# ANAR ####
## Stipa ####
anarS = all.bkgrd %>%
  filter(phyto == "ANAR", bkgrd != "BRHO") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group)

table(anarS$bkgrd, anarS$block)
## need a control: block 1

### fill cols ####
anarCmean = anarS %>%
  filter(bkgrd == "Control") %>%
  group_by(treatment) %>%
  summarise(mC = mean(total.biomass.rounded.percap, na.rm = T),
            reps = n())

## fill in a row for block 1 with the average drought ppt, control bkgrd biomass
anar_b1C = anarS[anarS$block == 6 & anarS$bkgrd == "Control",] ## select a row to change values in
anar_b1C2 = anar_b1C %>% ## change
  mutate(block = 1, 
         total.biomass.rounded.percap = anarCmean[anarCmean$treatment == "D",]$mC)

## merge 
anarS2 = rbind(anarS, anar_b1C2)
table(anarS2$bkgrd, anarS2$block)

### Calc RII ####
## calculate response ratio
anarSRII = anarS2 %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
    ## ok to do, only controls have mult plots / block, none of the Stipa bkgrds do
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (Stipa - Control)/(Stipa + Control),
         bkgrd = "Stipa") %>%
  select(-Stipa, -Control)

## BRHO ####
anarB = all.bkgrd %>%
  filter(phyto == "ANAR", bkgrd != "Stipa") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group)

table(anarB$bkgrd, anarB$block)
## block 1 needs a control

### fill cols ####
## merge 
anarB2 = rbind(anarB, anar_b1C2)
table(anarB2$bkgrd, anarB2$block)

### Calc RII ####
## calculate response ratio
anarBRII = anarB2 %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  ## ok to do, only controls have mult plots / block, none of the Stipa bkgrds do
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (BRHO - Control)/(BRHO + Control),
         bkgrd = "BRHO") %>%
  select(-BRHO, -Control)

## Merge ####
anarRII_all = rbind(anarBRII, anarSRII) %>%
  filter(!is.na(RII))

table(anarRII_all$bkgrd, anarRII_all$treatment)

anarRII_all %>%
  group_by(bkgrd, treatment) %>%
  summarise(meanRII = mean(RII), 
            seRII = calcSE(RII)) %>%
  ggplot(aes(x=treatment, y=meanRII, color = bkgrd)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = meanRII - seRII, ymax = meanRII + seRII), width = 0.25) +
  geom_hline(yintercept = 0)

# BRHO ####
## Stipa ####
brhoS = all.bkgrd %>%
  filter(phyto == "BRHO", bkgrd != "BRHO") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group) 

table(brhoS$bkgrd, brhoS$block)
## need controls in blocks 8 and 11

### fill cols ####
brhoCmean = brhoS %>%
  filter(bkgrd == "Control") %>%
  group_by(treatment) %>%
  summarise(mC = mean(total.biomass.rounded.percap, na.rm = T),
            reps = n())

## block 8
brho_b8C = brhoS[brhoS$block == 15 & brhoS$bkgrd == "Control",] ## select a row to change values in
brho_b8C2 = brho_b8C %>% ## change
  mutate(block = 8, 
         total.biomass.rounded.percap = brhoCmean[brhoCmean$treatment == "C",]$mC)

## block 11
brho_b11C = brhoS[brhoS$block == 15 & brhoS$bkgrd == "Control",] ## select a row to change values in
brho_b11C2 = brho_b11C %>% ## change
  mutate(block = 11, 
         total.biomass.rounded.percap = brhoCmean[brhoCmean$treatment == "C",]$mC)

## merge 
brhoS2 = rbind(brhoS, brho_b11C2, brho_b8C2)
table(brhoS2$bkgrd, brhoS2$block)

### Calc RII ####
## calculate response ratio
brhoSRII = brhoS2 %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  ## ok to do, only controls have mult plots / block, none of the Stipa bkgrds do
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (Stipa - Control)/(Stipa + Control),
         bkgrd = "Stipa") %>%
  select(-Stipa, -Control)

## BRHO ####
brhoB = all.bkgrd %>%
  filter(phyto == "BRHO", bkgrd != "Stipa") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group)

table(brhoB$bkgrd, brhoB$block)
## need controls in block 8

### fill cols ####
## merge 
brhoB2 = rbind(brhoB, brho_b8C2)
table(brhoB2$bkgrd, brhoB2$block)

### Calc RII ####
## calculate response ratio
brhoBRII = brhoB2 %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  ## ok to do, only controls have mult plots / block, none of the Stipa bkgrds do
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (BRHO - Control)/(BRHO + Control),
         bkgrd = "BRHO") %>%
  select(-BRHO, -Control)

## Merge ####
brhoRII_all = rbind(brhoBRII, brhoSRII) %>%
  filter(!is.na(RII))

table(brhoRII_all$bkgrd, brhoRII_all$treatment)

brhoRII_all %>%
  group_by(bkgrd, treatment) %>%
  summarise(meanRII = mean(RII), 
            seRII = calcSE(RII)) %>%
  ggplot(aes(x=treatment, y=meanRII, color = bkgrd)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = meanRII - seRII, ymax = meanRII + seRII), width = 0.25) +
  geom_hline(yintercept = 0)

# GITR ####
## Stipa ####
gitrS = all.bkgrd %>%
  filter(phyto == "GITR", bkgrd != "BRHO") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group) 

table(gitrS$bkgrd, gitrS$block)
## need controls in blocks 7 and 11

### fill cols ####
gitrCmean = gitrS %>%
  filter(bkgrd == "Control") %>%
  group_by(treatment) %>%
  summarise(mC = mean(total.biomass.rounded.percap, na.rm = T),
            reps = n())

## fill in a row for block 1 with the average drought ppt, control bkgrd biomass
gitr_b7C = gitrS[gitrS$block == 16 & gitrS$bkgrd == "Control",] ## select a row to change values in
gitr_b7C2 = gitr_b7C %>% ## change
  mutate(block = 7, 
         total.biomass.rounded.percap = gitrCmean[gitrCmean$treatment == "C",]$mC)

gitr_b11C = gitrS[gitrS$block == 16 & gitrS$bkgrd == "Control",] ## select a row to change values in
gitr_b11C2 = gitr_b11C %>% ## change
  mutate(block = 11, 
         total.biomass.rounded.percap = gitrCmean[gitrCmean$treatment == "C",]$mC)

## merge 
gitrS2 = rbind(gitrS, gitr_b7C2, gitr_b11C2)
table(gitrS2$bkgrd, gitrS2$block)

### Calc RII ####
## calculate response ratio
gitrSRII = gitrS2 %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  ## ok to do, only controls have mult plots / block, none of the Stipa bkgrds do
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (Stipa - Control)/(Stipa + Control),
         bkgrd = "Stipa") %>%
  select(-Stipa, -Control)

## BRHO ####
gitrB = all.bkgrd %>%
  filter(phyto == "GITR", bkgrd != "Stipa") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group)

table(gitrB$bkgrd, gitrB$block)
## need controls in blocks 1 and 7

### fill cols ####
## block 1
gitr_b1C = gitrS[gitrS$block == 12 & gitrS$bkgrd == "Control",] ## select a row to change values in
gitr_b1C2 = gitr_b1C %>% ## change
  mutate(block = 1, 
         total.biomass.rounded.percap = gitrCmean[gitrCmean$treatment == "D",]$mC)

## merge 
gitrB2 = rbind(gitrB, gitr_b7C2, gitr_b1C2)
table(gitrB2$bkgrd, gitrB2$block)

### Calc RII ####
## calculate response ratio
gitrBRII = gitrB2 %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  ## ok to do, only controls have mult plots / block, none of the Stipa bkgrds do
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (BRHO - Control)/(BRHO + Control),
         bkgrd = "BRHO") %>%
  select(-BRHO, -Control)

## Merge ####
gitrRII_all = rbind(gitrBRII, gitrSRII) %>%
  filter(!is.na(RII))

table(gitrRII_all$bkgrd, gitrRII_all$treatment)

gitrRII_all %>%
  group_by(bkgrd, treatment) %>%
  summarise(meanRII = mean(RII), 
            seRII = calcSE(RII)) %>%
  ggplot(aes(x=treatment, y=meanRII, color = bkgrd)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = meanRII - seRII, ymax = meanRII + seRII), width = 0.25) +
  geom_hline(yintercept = 0)

# LOMU ####
## Stipa ####
lomuS = all.bkgrd %>%
  filter(phyto == "LOMU", bkgrd != "BRHO") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group) 

table(lomuS$bkgrd, lomuS$block)
## need controls in blocks 6, 8, 11

### fill cols ####
lomuCmean = lomuS %>%
  filter(bkgrd == "Control") %>%
  group_by(treatment) %>%
  summarise(mC = mean(total.biomass.rounded.percap, na.rm = T),
            reps = n())

## block 6
lomu_b6C = lomuS[lomuS$block == 1 & lomuS$bkgrd == "Control",] ## select a row to change values in
lomu_b6C2 = lomu_b6C %>% ## change
  mutate(block = 6, 
         total.biomass.rounded.percap = lomuCmean[lomuCmean$treatment == "D",]$mC)

## block 8
lomu_b8C = lomuS[lomuS$block == 15 & lomuS$bkgrd == "Control",] ## select a row to change values in
lomu_b8C2 = lomu_b8C %>% ## change
  mutate(block = 8, 
         total.biomass.rounded.percap = lomuCmean[lomuCmean$treatment == "C",]$mC)

## block 11
lomu_b11C = lomuS[lomuS$block == 15 & lomuS$bkgrd == "Control",] ## select a row to change values in
lomu_b11C2 = lomu_b11C %>% ## change
  mutate(block = 11, 
         total.biomass.rounded.percap = lomuCmean[lomuCmean$treatment == "C",]$mC)

## merge 
lomuS2 = rbind(lomuS, lomu_b11C2, lomu_b8C2, lomu_b6C2)
table(lomuS2$bkgrd, lomuS2$block)

### Calc RII ####
## calculate response ratio
lomuSRII = lomuS2 %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  ## ok to do, only controls have mult plots / block, none of the Stipa bkgrds do
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (Stipa - Control)/(Stipa + Control),
         bkgrd = "Stipa") %>%
  select(-Stipa, -Control)

## BRHO ####
lomuB = all.bkgrd %>%
  filter(phyto == "LOMU", bkgrd != "Stipa") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group)

table(lomuB$bkgrd, lomuB$block)
## need controls in block 3, 4, 6, 8

### fill cols ####
## block 3
lomu_b3C = lomuB[lomuB$block == 1 & lomuB$bkgrd == "Control",] ## select a row to change values in
lomu_b3C2 = lomu_b3C %>% ## change
  mutate(block = 3, 
         total.biomass.rounded.percap = lomuCmean[lomuCmean$treatment == "D",]$mC)

## block 4
lomu_b4C = lomuB[lomuB$block == 1 & lomuB$bkgrd == "Control",] ## select a row to change values in
lomu_b4C2 = lomu_b4C %>% ## change
  mutate(block = 4, 
         total.biomass.rounded.percap = lomuCmean[lomuCmean$treatment == "D",]$mC)



## merge 
lomuB2 = rbind(lomuB, lomu_b8C2, lomu_b6C2, lomu_b3C2, lomu_b4C2)
table(lomuB2$bkgrd, lomuB2$block)

### Calc RII ####
## calculate response ratio
lomuBRII = lomuB2 %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  ## ok to do, only controls have mult plots / block, none of the Stipa bkgrds do
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (BRHO - Control)/(BRHO + Control),
         bkgrd = "BRHO") %>%
  select(-BRHO, -Control)

## Merge ####
lomuRII_all = rbind(lomuBRII, lomuSRII) %>%
  filter(!is.na(RII))

table(lomuRII_all$bkgrd, lomuRII_all$treatment)

lomuRII_all %>%
  group_by(bkgrd, treatment) %>%
  summarise(meanRII = mean(RII), 
            seRII = calcSE(RII)) %>%
  ggplot(aes(x=treatment, y=meanRII, color = bkgrd)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = meanRII - seRII, ymax = meanRII + seRII), width = 0.25) +
  geom_hline(yintercept = 0)

# LENI ####
## Stipa ####
leniS = all.bkgrd %>%
  filter(phyto == "LENI", bkgrd != "BRHO") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group) #%>%
#  mutate(pair = ifelse(block == 1 & bkgrd == "BRHO", 1:4, NA))

table(leniS$bkgrd, leniS$block)
## need controls in blocks 1, 5, 11, 14
## need to average control plots

### fill cols ####
leniCmean = leniS %>%
  filter(bkgrd == "Control") %>%
  group_by(treatment) %>%
  summarise(mC = mean(total.biomass.rounded.percap, na.rm = T),
            reps = n())

## fill in a row for block 1 with the average drought ppt, control bkgrd biomass
leni_b1C = leniS[leniS$block == 3 & leniS$bkgrd == "Control",] ## select a row to change values in
leni_b1C2 = leni_b1C %>% ## change
  mutate(block = 1, 
         total.biomass.rounded.percap = leniCmean[leniCmean$treatment == "D",]$mC)

## fill in a row for block 14 with the average drought ppt, control bkgrd biomass
leni_b14C = leniS[leniS$block == 3 & leniS$bkgrd == "Control",] ## select a row to change values in
leni_b14C2 = leni_b14C %>% ## change
  mutate(block = 14, 
         total.biomass.rounded.percap = leniCmean[leniCmean$treatment == "D",]$mC)

## fill in a row for block 5 with the average drought ppt, control bkgrd biomass
leni_b5C = leniS[leniS$block == 16 & leniS$bkgrd == "Control",] ## select a row to change values in
leni_b5C2 = leni_b5C %>% ## change
  mutate(block = 5, 
         total.biomass.rounded.percap = leniCmean[leniCmean$treatment == "C",]$mC)

## fill in a row for block 11 with the average drought ppt, control bkgrd biomass
leni_b11C = leniS[leniS$block == 16 & leniS$bkgrd == "Control",] ## select a row to change values in
leni_b11C2 = leni_b11C %>% ## change
  mutate(block = 11, 
         total.biomass.rounded.percap = leniCmean[leniCmean$treatment == "C",]$mC)

## merge 
leniS2 = rbind(leniS, leni_b11C2, leni_b5C2, leni_b14C2, leni_b1C2)
table(leniS2$bkgrd, leniS2$block)

### Calc RII ####
## calculate response ratio
leniSRII = leniS2 %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  ## ok to do, only controls have mult plots / block, none of the Stipa bkgrds do
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (Stipa - Control)/(Stipa + Control),
         bkgrd = "Stipa") %>%
  select(-Stipa, -Control)

## BRHO ####
leniB = all.bkgrd %>%
  filter(phyto == "LENI", bkgrd != "Stipa") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group)

table(leniB$bkgrd, leniB$block)
## need controls in block 5, 14

### fill cols ####
## block 14
leni_b14C = leniB[leniB$block == 12 & leniB$bkgrd == "Control",] ## select a row to change values in
leni_b14C2 = leni_b14C %>% ## change
  mutate(block = 14, 
         total.biomass.rounded.percap = leniCmean[leniCmean$treatment == "D",]$mC)

## block 5
leni_b5C = leniB[leniB$block == 8 & leniB$bkgrd == "Control",] ## select a row to change values in
leni_b5C2 = leni_b5C %>% ## change
  mutate(block = 5, 
         total.biomass.rounded.percap = leniCmean[leniCmean$treatment == "C",]$mC)

## merge 
leniB2 = rbind(leniB, leni_b5C2, leni_b14C2)
table(leniB2$bkgrd, leniB2$block)

### Calc RII ####
## calculate response ratio
leniBRII = leniB2 %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  ## ok to do, only controls have mult plots / block, none of the Stipa bkgrds do
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (BRHO - Control)/(BRHO + Control),
         bkgrd = "BRHO") %>%
  select(-BRHO, -Control)

## Merge ####
leniRII_all = rbind(leniBRII, leniSRII) %>%
  filter(!is.na(RII))

table(leniRII_all$bkgrd, leniRII_all$treatment)

leniRII_all %>%
  group_by(bkgrd, treatment) %>%
  summarise(meanRII = mean(RII), 
            seRII = calcSE(RII)) %>%
  ggplot(aes(x=treatment, y=meanRII, color = bkgrd)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = meanRII - seRII, ymax = meanRII + seRII), width = 0.25) +
  geom_hline(yintercept = 0)

# MAEL ####
## Stipa ####
maelS = all.bkgrd %>%
  filter(phyto == "MAEL", bkgrd != "BRHO") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group) #%>%
#  mutate(pair = ifelse(block == 1 & bkgrd == "BRHO", 1:4, NA))

table(maelS$bkgrd, maelS$block)
## need controls in blocks 1, 8, 11, 14, 15
## need to average control plots

### fill cols ####
maelCmean = maelS %>%
  filter(bkgrd == "Control") %>%
  group_by(treatment) %>%
  summarise(mC = mean(total.biomass.rounded.percap, na.rm = T),
            reps = n())

## fill in a row for block 1 with the average drought ppt, control bkgrd biomass
mael_b1C = maelS[maelS$block == 4 & maelS$bkgrd == "Control",] ## select a row to change values in
mael_b1C2 = mael_b1C %>% ## change
  mutate(block = 1, 
         total.biomass.rounded.percap = maelCmean[maelCmean$treatment == "D",]$mC)

## fill in a row for block 14 with the average drought ppt, control bkgrd biomass
mael_b14C = maelS[maelS$block == 4 & maelS$bkgrd == "Control",] ## select a row to change values in
mael_b14C2 = mael_b14C %>% ## change
  mutate(block = 14, 
         total.biomass.rounded.percap = maelCmean[maelCmean$treatment == "D",]$mC)

## fill in a row for block 8 with the average drought ppt, control bkgrd biomass
mael_b8C = maelS[maelS$block == 4 & maelS$bkgrd == "Control",] ## select a row to change values in
mael_b8C2 = mael_b8C %>% ## change
  mutate(block = 8, 
         treatment = "C",
         total.biomass.rounded.percap = maelCmean[maelCmean$treatment == "C",]$mC)

mael_b11C = maelS[maelS$block == 4 & maelS$bkgrd == "Control",] ## select a row to change values in
mael_b11C2 = mael_b11C %>% ## change
  mutate(block = 11, 
         treatment = "C",
         total.biomass.rounded.percap = maelCmean[maelCmean$treatment == "C",]$mC)

mael_b15C = maelS[maelS$block == 4 & maelS$bkgrd == "Control",] ## select a row to change values in
mael_b15C2 = mael_b15C %>% ## change
  mutate(block = 15, 
         treatment = "C",
         total.biomass.rounded.percap = maelCmean[maelCmean$treatment == "C",]$mC)

## merge 
maelS2 = rbind(maelS, mael_b15C2, mael_b11C2, mael_b8C2, mael_b14C2, mael_b1C2)
table(maelS2$bkgrd, maelS2$block)

### Calc RII ####
## calculate response ratio
maelSRII = maelS2 %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  ## ok to do, only controls have mult plots / block, none of the Stipa bkgrds do
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (Stipa - Control)/(Stipa + Control),
         bkgrd = "Stipa") %>%
  select(-Stipa, -Control)

## BRHO ####
maelB = all.bkgrd %>%
  filter(phyto == "MAEL", bkgrd != "Stipa") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group)

table(maelB$bkgrd, maelB$block)
## need controls in block 1, 15, 16

### fill cols ####
## block 14
mael_b16C = maelB[maelB$block == 4 & maelB$bkgrd == "Control",] ## select a row to change values in
mael_b16C2 = mael_b16C %>% ## change
  mutate(block = 16, 
         treatment = "C",
         total.biomass.rounded.percap = maelCmean[maelCmean$treatment == "C",]$mC)

## merge 
maelB2 = rbind(maelB, mael_b1C2, mael_b15C2, mael_b16C2)
table(maelB2$bkgrd, maelB2$block)

### Calc RII ####
## calculate response ratio
maelBRII = maelB2 %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  ## ok to do, only controls have mult plots / block, none of the Stipa bkgrds do
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (BRHO - Control)/(BRHO + Control),
         bkgrd = "BRHO") %>%
  select(-BRHO, -Control)

## Merge ####
maelRII_all = rbind(maelBRII, maelSRII) %>%
  filter(!is.na(RII))

table(maelRII_all$bkgrd, maelRII_all$treatment)

maelRII_all %>%
  group_by(bkgrd, treatment) %>%
  summarise(meanRII = mean(RII), 
            seRII = calcSE(RII)) %>%
  ggplot(aes(x=treatment, y=meanRII, color = bkgrd)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = meanRII - seRII, ymax = meanRII + seRII), width = 0.25) +
  geom_hline(yintercept = 0)

# PLER ####
## Stipa ####
plerS = all.bkgrd %>%
  filter(phyto == "PLER", bkgrd != "BRHO") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group) #%>%
#  mutate(pair = ifelse(block == 1 & bkgrd == "BRHO", 1:4, NA))

table(plerS$bkgrd, plerS$block)
## need controls in blocks 11
## need to average control plots

### fill cols ####
plerCmean = plerS %>%
  filter(bkgrd == "Control") %>%
  group_by(treatment) %>%
  summarise(mC = mean(total.biomass.rounded.percap, na.rm = T),
            reps = n())

## fill in a row for block 1 with the average drought ppt, control bkgrd biomass
pler_b11C = plerS[plerS$block == 5 & plerS$bkgrd == "Control",] ## select a row to change values in
pler_b11C2 = pler_b11C %>% ## change
  mutate(block = 11, 
         total.biomass.rounded.percap = plerCmean[plerCmean$treatment == "C",]$mC)

## merge 
plerS2 = rbind(plerS, pler_b11C2)
table(plerS2$bkgrd, plerS2$block)

### Calc RII ####
## calculate response ratio
plerSRII = plerS2 %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  ## ok to do, only controls have mult plots / block, none of the Stipa bkgrds do
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (Stipa - Control)/(Stipa + Control),
         bkgrd = "Stipa") %>%
  select(-Stipa, -Control)

## BRHO ####
plerB = all.bkgrd %>%
  filter(phyto == "PLER", bkgrd != "Stipa") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group)

table(plerB$bkgrd, plerB$block)
## all good!!

### Calc RII ####
## calculate response ratio
plerBRII = plerB %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  ## ok to do, only controls have mult plots / block, none of the Stipa bkgrds do
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (BRHO - Control)/(BRHO + Control),
         bkgrd = "BRHO") %>%
  select(-BRHO, -Control)

## Merge ####
plerRII_all = rbind(plerBRII, plerSRII) %>%
  filter(!is.na(RII))

table(plerRII_all$bkgrd, plerRII_all$treatment)

plerRII_all %>%
  group_by(bkgrd, treatment) %>%
  summarise(meanRII = mean(RII), 
            seRII = calcSE(RII)) %>%
  ggplot(aes(x=treatment, y=meanRII, color = bkgrd)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = meanRII - seRII, ymax = meanRII + seRII), width = 0.25) +
  geom_hline(yintercept = 0)


# THIR ####
## Stipa ####
thirS = all.bkgrd %>%
  filter(phyto == "THIR", bkgrd != "BRHO") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group) #%>%
#  mutate(pair = ifelse(block == 1 & bkgrd == "BRHO", 1:4, NA))

table(thirS$bkgrd, thirS$block)
## need controls in blocks 6, 8, 11
## need to average control plots

### fill cols ####
thirCmean = thirS %>%
  filter(bkgrd == "Control") %>%
  group_by(treatment) %>%
  summarise(mC = mean(total.biomass.rounded.percap, na.rm = T),
            reps = n())

## fill in a row for block 11 with the average drought ppt, control bkgrd biomass
thir_b11C = thirS[thirS$block == 5 & thirS$bkgrd == "Control",] ## select a row to change values in
thir_b11C2 = thir_b11C %>% ## change
  mutate(block = 11, 
         total.biomass.rounded.percap = thirCmean[thirCmean$treatment == "C",]$mC)

## fill in a row for block 8 with the average drought ppt, control bkgrd biomass
thir_b8C = thirS[thirS$block == 5 & thirS$bkgrd == "Control",] ## select a row to change values in
thir_b8C2 = thir_b8C %>% ## change
  mutate(block = 8, 
         total.biomass.rounded.percap = thirCmean[thirCmean$treatment == "C",]$mC)

## fill in a row for block 8 with the average drought ppt, control bkgrd biomass
thir_b6C = thirS[thirS$block == 12 & thirS$bkgrd == "Control",] ## select a row to change values in
thir_b6C2 = thir_b6C %>% ## change
  mutate(block = 6, 
         total.biomass.rounded.percap = thirCmean[thirCmean$treatment == "D",]$mC)

## merge 
thirS2 = rbind(thirS, thir_b6C2, thir_b8C2, thir_b11C2)
table(thirS2$bkgrd, thirS2$block)

### Calc RII ####
## calculate response ratio
thirSRII = thirS2 %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  ## ok to do, only controls have mult plots / block, none of the Stipa bkgrds do
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (Stipa - Control)/(Stipa + Control),
         bkgrd = "Stipa") %>%
  select(-Stipa, -Control)

## BRHO ####
thirB = all.bkgrd %>%
  filter(phyto == "THIR", bkgrd != "Stipa") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group)

table(thirB$bkgrd, thirB$block)
## need controls in block 4, 6, 8, 16

### fill cols ####
## block 4
thir_b4C = thirB[thirB$block == 1 & thirB$bkgrd == "Control",] ## select a row to change values in
thir_b4C2 = thir_b4C %>% ## change
  mutate(block = 4, 
         total.biomass.rounded.percap = thirCmean[thirCmean$treatment == "D",]$mC)

## block 16
thir_b16C = thirB[thirB$block == 15 & thirB$bkgrd == "Control",] ## select a row to change values in
thir_b16C2 = thir_b16C %>% ## change
  mutate(block = 16, 
         total.biomass.rounded.percap = thirCmean[thirCmean$treatment == "C",]$mC)

## merge 
thirB2 = rbind(thirB, thir_b6C2, thir_b8C2, thir_b4C2, thir_b16C2)
table(thirB2$bkgrd, thirB2$block)

### Calc RII ####
## calculate response ratio
thirBRII = thirB2 %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  ## ok to do, only controls have mult plots / block, none of the Stipa bkgrds do
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (BRHO - Control)/(BRHO + Control),
         bkgrd = "BRHO") %>%
  select(-BRHO, -Control)

## Merge ####
thirSRII_all = rbind(thirBRII, thirSRII) %>%
  filter(!is.na(RII))

table(thirSRII_all$bkgrd, thirSRII_all$treatment)

thirSRII_all %>%
  group_by(bkgrd, treatment) %>%
  summarise(meanRII = mean(RII), 
            seRII = calcSE(RII)) %>%
  ggplot(aes(x=treatment, y=meanRII, color = bkgrd)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = meanRII - seRII, ymax = meanRII + seRII), width = 0.25) +
  geom_hline(yintercept = 0)

# TWIL ####
## Stipa ####
twilS = all.bkgrd %>%
  filter(phyto == "TWIL", bkgrd != "BRHO") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group) #%>%
#  mutate(pair = ifelse(block == 1 & bkgrd == "BRHO", 1:4, NA))

table(twilS$bkgrd, twilS$block)
## need controls in blocks 8, 11, 12
## need to average control plots

### fill cols ####
twilCmean = twilS %>%
  filter(bkgrd == "Control") %>%
  group_by(treatment) %>%
  summarise(mC = mean(total.biomass.rounded.percap, na.rm = T),
            reps = n())

## fill in a row for block 8 with the average amb ppt, control bkgrd biomass
twil_b8C = twilS[twilS$block == 15 & twilS$bkgrd == "Control",] ## select a row to change values in
twil_b8C2 = twil_b8C %>% ## change
  mutate(block = 8, 
         total.biomass.rounded.percap = twilCmean[twilCmean$treatment == "C",]$mC)

## fill in a row for block 8 with the average amb ppt, control bkgrd biomass
twil_b11C = twilS[twilS$block == 15 & twilS$bkgrd == "Control",] ## select a row to change values in
twil_b11C2 = twil_b11C %>% ## change
  mutate(block = 11, 
         total.biomass.rounded.percap = twilCmean[twilCmean$treatment == "C",]$mC)

## fill in a row for block 8 with the average amb ppt, control bkgrd biomass
twil_b12C = twilS[twilS$block == 14 & twilS$bkgrd == "Control",] ## select a row to change values in
twil_b12C2 = twil_b12C %>% ## change
  mutate(block = 12, 
         total.biomass.rounded.percap = twilCmean[twilCmean$treatment == "D",]$mC)

## merge 
twilS2 = rbind(twilS, twil_b12C2, twil_b11C2, twil_b8C2)
table(twilS2$bkgrd, twilS2$block)

### Calc RII ####
## calculate response ratio
twilSRII = twilS2 %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  ## ok to do, only controls have mult plots / block, none of the Stipa bkgrds do
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (Stipa - Control)/(Stipa + Control),
         bkgrd = "Stipa") %>%
  select(-Stipa, -Control)

## BRHO ####
twilB = all.bkgrd %>%
  filter(phyto == "TWIL", bkgrd != "Stipa") %>%
  select(block, phyto, bkgrd, treatment, total.biomass.rounded.percap, origin, functional_group) #%>%
#  mutate(pair = ifelse(block == 1 & bkgrd == "BRHO", 1:4, NA))

table(twilB$bkgrd, twilB$block)
## need controls in 4, 5, 8, 12

### fill cols ####
## block 4
twil_b4C = twilB[twilB$block == 3 & twilB$bkgrd == "Control",] ## select a row to change values in
twil_b4C2 = twil_b4C %>% ## change
  mutate(block = 4, 
         total.biomass.rounded.percap = twilCmean[twilCmean$treatment == "D",]$mC)

## block 12
twil_b12C = twilB[twilB$block == 3 & twilB$bkgrd == "Control",] ## select a row to change values in
twil_b12C2 = twil_b12C %>% ## change
  mutate(block = 12, 
         total.biomass.rounded.percap = twilCmean[twilCmean$treatment == "D",]$mC)

## block 8
twil_b8C = twilB[twilB$block == 15 & twilB$bkgrd == "Control",] ## select a row to change values in
twil_b8C2 = twil_b8C %>% ## change
  mutate(block = 8, 
         total.biomass.rounded.percap = twilCmean[twilCmean$treatment == "C",]$mC)

## block 5
twil_b5C = twilB[twilB$block == 15 & twilB$bkgrd == "Control",] ## select a row to change values in
twil_b5C2 = twil_b5C %>% ## change
  mutate(block = 5, 
         total.biomass.rounded.percap = twilCmean[twilCmean$treatment == "C",]$mC)

## merge 
twilB2 = rbind(twilB, twil_b5C2, twil_b8C2, twil_b12C2, twil_b4C2)
table(twilB2$bkgrd, twilB2$block)

### Calc RII ####
## calculate response ratio
twilBRII = twilB2 %>%
  
  group_by(block, phyto, bkgrd, treatment, origin, functional_group) %>%
  
  ## calc mean biomass for a species at the block level
  summarise(mean.block.bio = mean(total.biomass.rounded.percap, na.rm = TRUE)) %>%
  ## ok to do, only controls have mult plots / block, none of the Stipa bkgrds do
  
  ## pivot wider to make separate columns for biomass in BRHO, Stipa, and Control
  pivot_wider(names_from = "bkgrd", values_from = mean.block.bio) %>%
  
  ## with these columns calculate the relative interaction intensity
  mutate(RII = (BRHO - Control)/(BRHO + Control),
         bkgrd = "BRHO") %>%
  select(-BRHO, -Control)

## Merge ####
twilRII_all = rbind(twilBRII, twilSRII) %>%
  filter(!is.na(RII))

table(twilRII_all$bkgrd, twilRII_all$treatment)

twilRII_all %>%
  group_by(bkgrd, treatment) %>%
  summarise(meanRII = mean(RII), 
            seRII = calcSE(RII)) %>%
  ggplot(aes(x=treatment, y=meanRII, color = bkgrd)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = meanRII - seRII, ymax = meanRII + seRII), width = 0.25) +
  geom_hline(yintercept = 0)

# Full Merge ####
RII_all = rbind(acamRII_all, anarRII_all, brhoRII_all, gitrRII_all, leniRII_all, lomuRII_all, maelRII_all, plerRII_all, thirSRII_all, twilRII_all)

RII_all %>%
  group_by(bkgrd, treatment, phyto) %>%
  summarise(meanRII = mean(RII), 
            seRII = calcSE(RII)) %>%
  ggplot(aes(x=phyto, y=meanRII, color = bkgrd)) +
  facet_wrap(~treatment) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = meanRII - seRII, ymax = meanRII + seRII), width = 0.25) +
  geom_hline(yintercept = 0)

RII_sum = RII_all %>%
  
  ## calculate the mean RII value for each species in each background
  group_by(phyto, bkgrd, treatment, origin, functional_group) %>%
  summarise(mean_RII = mean(RII, na.rm = TRUE),
            se_RII = calcSE(RII)) %>%
  
  ## rename columns for graphing
  mutate(treatment = ifelse(treatment == "C", "Ambient", "Drought"), 
         Background = ifelse(bkgrd == "BRHO", "Bromus", "Stipa"), 
         origin = ifelse(origin == "native", "Native", "Non-Native"))

# Clean Env ####
rm(acam_b5C, acam_b5C2, acam_b6C, acam_b6C2, acamB, acamB2, acamBRII, acamCmean, acamRII_all, acamS, acamS2, acamSRII, anar_b1C, anar_b1C2, anarB, anarB2, anarBRII, anarS, anarRII_all, anarCmean, anarS2, anarSRII, brho_b11C, brho_b11C2, brho_b8C, brho_b8C2, brhoB, brhoB2, brhoBRII, brhoCmean, brhoRII_all, brhoS, brhoS2, brhoSRII, gitr_b11C, gitr_b11C2, gitr_b1C, gitr_b1C2, gitr_b7C, gitr_b7C2, gitrB, gitrB2, gitrBRII, gitrCmean, gitrRII_all, gitrS, gitrS2, gitrSRII, leni_b11C, leni_b11C2, leni_b14C, leni_b14C2, leni_b1C, leni_b1C2, leni_b5C, leni_b5C2, leniB, leniB2, leniBRII, leniCmean, leniRII_all, leniS, leniS2, leniSRII, lomu_b11C, lomu_b11C2, lomu_b3C, lomu_b3C2, lomu_b4C, lomu_b4C2, lomu_b6C, lomu_b6C2, lomu_b8C, lomu_b8C2, lomuB, lomuB2, lomuBRII, lomuCmean, lomuRII_all, lomuS, lomuS2, lomuSRII, mael_b11C, mael_b11C2, mael_b14C, mael_b14C2, mael_b15C, mael_b15C2, mael_b16C, mael_b16C2, mael_b1C, mael_b1C2, mael_b8C, mael_b8C2, maelB, maelB2, maelBRII, maelCmean, maelRII_all, maelS, maelS2, maelSRII, pler_b11C, pler_b11C2, plerB, plerBRII, plerCmean, plerRII_all, plerS, plerS2, plerSRII, thir_b11C, thir_b11C2, thir_b16C, thir_b16C2, thir_b4C, thir_b4C2, thir_b6C, thir_b6C2, thir_b8C, thir_b8C2, thirB, thirB2, thirBRII, thirCmean, thirS, thirS2, thirSRII, thirSRII_all, twil_b11C, twil_b11C2, twil_b12C, twil_b12C2, twil_b4C, twil_b4C2, twil_b5C, twil_b5C2, twil_b8C, twil_b8C2, twilB, twilB2, twilBRII, twilCmean, twilRII_all, twilS, twilS2, twilSRII)

