library(ggplot2)
library(tidyverse)
library(Kendall)
library(cowplot)

load("Bhootetal_EVT_code_data_figures/Data/BurnArea_ts.data")  
  
BurnArea_numbers <- BurnArea_numbers_wEco %>% 
  mutate(EVT_PHYS = factor(EVT_PHYS, levels = c('Conifer', 'Hardwood', 'Shrubland', "Herb"))) %>%
  group_by(BurnYear, EVT_PHYS) %>%
  summarise( 
    EVTtotal_prefire_sum = sum(nPix_expected) * 30^2 * 1e-6)

BurnArea_numbers_byEco <- BurnArea_numbers_wEco %>% 
  mutate(EVT_PHYS = factor(EVT_PHYS, levels = c('Conifer', 'Hardwood', 'Shrubland', "Herb"))) %>%
  group_by(BurnYear, EVT_PHYS, Ecoregion_coarse) %>% 
  summarise( 
    EVTtotal_prefire_sum = sum(nPix_expected) * 30^2 * 1e-6)


# Ho = There is no trend in the composition of what burns
# Ha = There is a monotonic increasing/decreasing trend in the composition of what burns

# First two rows:
  # tau = Kendall tau, direction of monotonic
  # sl = p-value for significance 
sapply(c('Conifer', 'Hardwood', 'Shrubland', "Herb"), function(x)
MannKendall(unlist(BurnArea_numbers %>% 
                     group_by(BurnYear) %>%
                     mutate(BurnedProp = EVTtotal_prefire_sum / sum(EVTtotal_prefire_sum)) %>%
      filter(EVT_PHYS %in% x) %>% 
      arrange(BurnYear) %>%
  ungroup() %>%
  select(BurnedProp)))
  )


Total_BurnArea_ConiferHardwood <- BurnArea_numbers %>%
  mutate(EVT_PHYS = mgsub::mgsub(as.character(EVT_PHYS), 
                                 c('Conifer', 'Hardwood', 'Shrubland', "Herb") , 
                                 c('Conifer * +', 'Hardwood  -', 'Shrubland * -', "Herb * -") ),
         EVT_PHYS = factor(EVT_PHYS, levels = c('Conifer * +','Hardwood  -',  'Shrubland * -', "Herb * -"))) %>%
  ggplot(aes(x = BurnYear, y = EVTtotal_prefire_sum, fill = factor(EVT_PHYS))) +
  geom_bar(stat = 'identity', color = 'black', linewidth = 0.35) +
  scale_fill_manual(values = c("Conifer * +" =  "green4",
                               "Hardwood  -" =  "#CFF79A",
                               "Shrubland * -" = "#9D0000",
                               "Herb * -" = "cyan3")) +
  ggtitle("Wildfire burn area") + 
  xlab("Year") + ylab(~paste("km"^2)) +
  # ylim(c(0,1))+
  labs(fill = "")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'), 
        axis.title = element_text(size = 17), 
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.box.background = element_rect(color = 'black', linewidth = 1.5),
        legend.title = element_blank(),
        legend.position = c(0.2,0.7)) 

Total_BurnArea_ConiferHardwood_prop <- BurnArea_numbers %>%
  mutate(EVT_PHYS = mgsub::mgsub(as.character(EVT_PHYS), 
                                 c('Conifer', 'Hardwood', 'Shrubland', "Herb") , 
                                 c('Conifer * +', 'Hardwood  -', 'Shrubland * -', "Herb * -") ),
         EVT_PHYS = factor(EVT_PHYS, levels = c('Conifer * +','Hardwood  -',  'Shrubland * -', "Herb * -"))) %>%
  ggplot(aes(x = BurnYear, y = EVTtotal_prefire_sum, fill = factor(EVT_PHYS))) +
  geom_bar(stat = 'identity', position = 'fill', color = 'black', linewidth = 0.35) +
  scale_fill_manual(values = c("Conifer * +" =  "green4",
                               "Hardwood  -" =  "#CFF79A",
                               "Shrubland * -" = "#9D0000",
                               "Herb * -" = "cyan3")) +
  ggtitle("Composition of burn area") +
  xlab("Year") + ylab("Proportion") +
  labs(fill = "")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'), 
        axis.title = element_text(size = 17), 
        axis.text = element_text(size = 17)) 

BA_byEVT <- plot_grid(Total_BurnArea_ConiferHardwood,
                           Total_BurnArea_ConiferHardwood_prop + theme(legend.position = 'none'),
                           nrow = 2)

ggsave("Bhootetal_EVT_code_data_figures/plots/Main/BurnArea_timeseries.png",
  plot = BA_byEVT , 
  dpi = 300, height = 7.5, width = 6.5 
)
