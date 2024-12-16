
# Set Up ####
## read in data
source("data_analysis/calculate_RII.R")

## load packages
library(ggpubr)

# Create Figs ####
## Figure 1 ####
RII_sum %>%
  
  ## calculate mean RII across species (within their respective origins & backgrounds)
  group_by(treatment, Background, origin) %>%
  summarise(RII_overall = mean(mean_RII, na.rm = T), 
            se_RII_overall = calcSE(mean_RII)) %>%
  
  ## Plot!
  ggplot(aes(x=origin, y=RII_overall, color = Background, shape = origin)) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = RII_overall - se_RII_overall, ymax = RII_overall + se_RII_overall), width = 0.2) +
  scale_color_manual(values = c("#E58606", "#5D69B1")) +
  xlab("Focal Species Origin") +
  ylab("Mean Relative Interaction Intensity") +
  theme(text = element_text(size = 15)) +
  scale_shape_manual(values = c(19, 15)) +
  facet_wrap(~treatment) +
  labs(shape = "Origin")

#ggsave("preliminary_figures/figure1_RII_trt_origin_background.png", width = 8, height = 4)


## Figure 2 ####
p1 = ggplot(RII_sum[RII_sum$origin == "Native", ], aes(x=phyto, y=mean_RII, color = Background, shape = origin)) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_RII - se_RII, ymax = mean_RII + se_RII), width = 0.2) +
  scale_color_manual(values = c("#E58606", "#5D69B1")) +
  xlab(NULL) +
  ylab("Mean Relative Interaction Intensity") +
  theme(text = element_text(size = 15)) +
  scale_shape_manual(values = c(19, 15)) +
  facet_wrap(~treatment, ncol = 1) +
  labs(shape = "Origin") +
  coord_cartesian(ylim = c(-0.9, 0.9))

p2 = ggplot(RII_sum[RII_sum$origin == "Non-Native", ], aes(x=phyto, y=mean_RII, color = Background, shape = origin)) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_RII - se_RII, ymax = mean_RII + se_RII), width = 0.2) +
  scale_color_manual(values = c("#E58606", "#5D69B1")) +
  xlab(NULL) +
  ylab(" ") +
  theme(text = element_text(size = 15)) +
  scale_shape_manual(values = c(15)) +
  facet_wrap(~treatment, ncol = 1) +
  labs(shape = "Origin") +
  coord_cartesian(ylim = c(-0.9, 0.9))
 
ggarrange(p1, p2, common.legend = TRUE, labels = "AUTO", legend = "bottom")

#ggsave("preliminary_figures/figure2_RII_species_trt_origin_background.png", width = 8, height = 7)

