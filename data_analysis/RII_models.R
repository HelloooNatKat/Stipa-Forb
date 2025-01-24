
## load packages
library(lmerTest)
library(performance)
library(car)


m1 = lmer(RII ~ bkgrd*origin + bkgrd*treatment + origin*treatment + (1|phyto), data = RII_all)
summary(m1)
Anova(m1)
## no evidence of interactions, remove from model

m2 = lmer(RII ~ bkgrd + origin + treatment + (1|phyto), data = RII_all)
summary(m2)
Anova(m2, type = 2, test.statistic = "F")

m2_tab = as.data.frame(Anova(m2, type = 2, test.statistic = "F")) %>%
  rownames_to_column()
write.csv(m2_tab, "tables/mm_RII_anova_table.csv", row.names = F)

m2_coeff = as.data.frame(summary(m2)$coefficients) %>%
  rownames_to_column()

check_model(m2)
