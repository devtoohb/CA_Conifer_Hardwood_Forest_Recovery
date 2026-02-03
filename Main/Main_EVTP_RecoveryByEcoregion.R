library(tidyverse)
library(ggplot2)
library(reshape2)
library(ggh4x)
 
load("Bhootetal_EVT_code_data_figures/Data/GrandTable.data")
BigTable_SpatRecovery_Ecoregion_coarse <- BigTable_SpatRecovery %>%
  group_by(BurnYear, YearMinusBurn, EVT_PHYS_coarse, Ecoregion_coarse, Reburn) %>%
  summarise(
    nPix_wAll = sum(nPix_wAll),
    nPix_expected = sum(nPix_expected)
  ) %>% ungroup()

Visualization_Ecos <- BigTable_SpatRecovery_Ecoregion_coarse %>% 
  filter(EVT_PHYS_coarse %in% c('Conifer', 'Hardwood', 'Shrubland') & 
    BurnYear %in% 1986:2007 & !Reburn &
    YearMinusBurn <= 15 & YearMinusBurn >= -1) %>%
  group_by(YearMinusBurn, EVT_PHYS_coarse, Ecoregion_coarse) %>% 
  summarise(nPix = sum(nPix_wAll), 
            nPix_expected = sum(nPix_expected), 
            Recovery = nPix/nPix_expected )


Example_Absolute <- Visualization_Ecos %>% 
  select(YearMinusBurn,EVT_PHYS_coarse,nPix,nPix_expected, Ecoregion_coarse) %>% 
  group_by(YearMinusBurn, EVT_PHYS_coarse) %>% 
  mutate(`EVT Physiognomy` = EVT_PHYS_coarse,
         `Control Cover\n(Expected Cover)` = nPix_expected,
         `Actual Cover` = nPix) %>%
  filter(`EVT Physiognomy` %in% c('Conifer', 'Hardwood', 'Shrubland') ) %>%
  summarise(melt(., measure.vars = c('Actual Cover', 'Control Cover\n(Expected Cover)'))) %>% 
  ungroup() %>%
  mutate(Area = value,
         `Area Type` = variable) %>%
  ggplot(aes(x = YearMinusBurn)) + 
  geom_line(aes(y = Area * 30^2 * 1e-6, 
                color = `EVT Physiognomy`, linetype = `Area Type`),
            linewidth = 1.5) +
  scale_color_manual(values = c("Conifer" = "green4",
                                "Hardwood" = "#CFF79A",
                                "Shrubland" =  "#9D0000")
  )+
  ggtitle("Aggregated absolute area recovery curves" ) +
  ylab(expression("km"^2)) + xlab("Stand Age") +
  facet_grid2(.~Ecoregion_coarse, scales = 'free_y', independent = 'y') + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = 'bold'),
        strip.text = element_text(size = 18),
        strip.background = element_blank(),
        legend.key.spacing.y = unit(2.25, units = 'mm'),
        legend.background = element_rect("transparent"),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.box.background = element_rect(color = 'black', linewidth = 0.75),
        legend.title = element_blank(),
        legend.position = c(0.925,0.5)) 






Visualization_Ecos_relative <- BigTable_SpatRecovery_Ecoregion_coarse %>% 
  filter(EVT_PHYS_coarse %in% c('Conifer', 'Hardwood', 'Shrubland') & 
    BurnYear %in% 1986:2007 & YearMinusBurn >= -1 & YearMinusBurn <= 15 & !Reburn) %>%
  group_by(YearMinusBurn, EVT_PHYS_coarse, Ecoregion_coarse, BurnYear) %>% 
  summarise(nPix = sum(nPix_wAll), 
            nPix_expected = sum(nPix_expected), 
            Recovery = nPix/nPix_expected
            # , 
            # Deficit = nPix_expected - nPix
            ) %>%
  ungroup() %>%
  group_by(YearMinusBurn, EVT_PHYS_coarse, Ecoregion_coarse) %>%
  filter(is.finite(Recovery)) %>%
  mutate(q25_recov = quantile(Recovery, 0.25),
         q50_recov = quantile(Recovery, 0.5),
         q75_recov = quantile(Recovery, 0.75))%>% 
  mutate(`EVT Physiognomy` = EVT_PHYS_coarse) %>%
  filter(`EVT Physiognomy` %in% c('Conifer', 'Hardwood', 'Shrubland'))





Example_Relative <- Visualization_Ecos_relative %>%
  ggplot(aes(x = YearMinusBurn)) + 
  geom_hline(yintercept = 100, linewidth = 0.5) +
  ggborderline::geom_borderline(aes(y = q50_recov*100, 
                                    color = `EVT Physiognomy`),
                                linewidth = 1, bordercolour = "gray30")+
  geom_ribbon(data=Visualization_Ecos_relative,
              aes(x = YearMinusBurn,ymin = q25_recov*100, ymax = q75_recov*100, fill = `EVT Physiognomy`),
              alpha = 0.25)+
  
  scale_color_manual(values = c("Conifer" = "green4",
                                "Hardwood" = "#CFF79A",
                                "Shrubland" =  "#9D0000")
                     )+
  scale_fill_manual(values = c("Conifer" = "green4",
                               "Hardwood" = "#CFF79A",
                               "Shrubland" =  "#9D0000")
  )+
  ggtitle("Wildfire Years 1986-2007") +
  ylab("Recovery (%)") + xlab("Stand Age") +
  facet_grid2(.~Ecoregion_coarse) + 
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = 'bold'),
        strip.text = element_text(size = 18),
        strip.background = element_blank(),
        legend.key.spacing.y = unit(2.25, units = 'mm'),
        legend.box.background = element_rect(color = 'black', linewidth = 0.75),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.title = element_blank(),
        legend.position = c(0.9,0.75)
  ) 




ggsave(filename =
         "Bhootetal_EVT_code_data_figures/plots/Main/Ecoregion_Recovery.png",
       plot = Example_Relative,
       dpi = 300, height = 7, width = 18
)

ggsave(filename =
         "Bhootetal_EVT_code_data_figures/plots/Supplement/Supp_Ecoregion_AggregatedAbsoluteRecovery.png",
       plot =  Example_Absolute ,
       dpi = 300, height = 7, width = 18
)


