library(ggplot2)
library(tidyverse)

load("Bhootetal_EVT_code_data_figures/Data/ProportionChange.data")
ProportionChange$BurnCondition_Name <- gsub("Burned", "Burned Area", ProportionChange$BurnCondition_Name)

OverallCover_prop_ts_Line_Version <- ProportionChange %>%
  ggplot(aes(x = layer, y = proportion, color = EVT_PHYS_coarse) ) +
  ggborderline::geom_borderline(linewidth=1.5, bordercolour = "gray30")+
  scale_color_manual(values = c(
    c("Conifer" = "green4",
      "Hardwood" = "#CFF79A",
      "Shrubland" =  "#9D0000",
      "Herb" = "cyan3")
  )) +
  xlab("Year") + ylab("Proportion") +
  facet_grid(.~BurnCondition_Name) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold', size = 20),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 17),
        axis.text.x = element_text(size = 17, angle = 35, hjust = 1),
        plot.title = element_text(size = 17, face = 'bold'),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.box.background = element_rect(color = 'black', linewidth = 1.5),
        # legend.position = c(0.125, 0.675)
        legend.position = c(0.85, 0.225)
        
  )


OverallCover_prop_ts_Line_Version_deltafromstart <-
  ProportionChange %>%
  group_by(EVT_PHYS_coarse, BurnCondition_Name) %>%
  mutate(delta_start_relative = 100 * (count - count[layer == 1985]) / count[layer == 1985]) %>%
  ggplot(aes(x = layer, y = delta_start_relative, color = EVT_PHYS_coarse) ) +
  ggborderline::geom_borderline(linewidth=1, bordercolour = "black")+
  scale_color_manual(values = c(
    c("Conifer" = "green4",
      "Hardwood" = "#CFF79A",
      "Shrubland" =  "#9D0000",
      "Herb" = "cyan3")
  )) +
  xlab("Year") + ylab("Change from 1985 cover (relative, %)") +
  facet_grid(.~BurnCondition_Name) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold', size = 20),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 17),
        plot.title = element_text(size = 17, face = 'bold'),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.box.background = element_rect(color = 'black', linewidth = 1.5),
        # legend.position = c(0.125, 0.675)
        legend.position = c(0.85, 0.225)
        
  )


data.frame(
  ProportionChange %>%
    group_by(EVT_PHYS_coarse, BurnCondition_Name) %>%
    mutate(delta_comp_start_prop = (proportion - proportion[layer == 1985]) / proportion[layer == 1985],
           delta_comp_start = (proportion - proportion[layer == 1985]),
           delta_amount_start_prop = (count - count[layer == 1985]) / count[layer == 1985],
           delta_amount_start_prop = (count - count[layer == 1985]) * 30^2 * 1e-6 ,
           km2 = count * 30^2 * 1e-6) %>% filter(layer %in% c(1985,2022)  & EVT_PHYS_coarse == 'Hardwood') %>% filter(layer == 2022))




ggsave(filename = 
         paste0("Bhootetal_EVT_code_data_figures/plots/Main/Proportional_Change.png"),
       plot =  OverallCover_prop_ts_Line_Version,
       dpi = 300, width = 10.5, height = 7.5
)
