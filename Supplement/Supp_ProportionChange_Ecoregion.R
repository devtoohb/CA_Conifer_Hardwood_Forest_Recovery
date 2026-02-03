library(ggplot2)
library(tidyverse)
library(ggh4x)

load("Bhootetal_EVT_code_data_figures/Data/ProportionChange_eco.data")
OverallCover_prop_ts_eco_bar_Version <- ProportionChange_eco %>%
  ggplot(aes(x = layer, y = count_prop, fill = EVT_PHYS_coarse) ) + 
  # ggborderline::geom_borderline(linewidth = 1.5, bordercolour = 'gray30')+ # as lines, comment out geom_bar and adjust fill to color and scale_fill... to scale_color
  geom_bar(position = 'fill', stat = 'identity', color = 'black', linewidth = 0.15) +
  scale_fill_manual(values = c(
    c("Conifer" = "green4",
      "Hardwood" = "#CFF79A",
      "Shrubland" =  "#9D0000",
      "Herb" = "cyan3")
  )) +
  xlab("Year") + ylab("Proportion") +
  # ylim(c(0,1)) +
  facet_grid2(BurnCondition_Name~Ecoregion_Name) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = 'bold', size = 12),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 35, hjust = 1),
    plot.title = element_text(size = 15, face = 'bold'),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    strip.placement = 'right')


ggsave(filename = "Bhootetal_EVT_code_data_figures/plots/Supplement/Supp_ProportionalChangeEcoregion.png",
       plot =  OverallCover_prop_ts_eco_bar_Version,
       dpi = 300, width = 10, height = 10
)





