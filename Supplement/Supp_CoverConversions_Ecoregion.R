library(ggplot2)
library(cowplot)

load("Bhootetal_EVT_code_data_figures/Data/CoverConversions.data")

CoverConversion_table_eco <- data.frame(rbind(data.frame(CoverConversion_tables$All, condition = "All Cover"),
                                              data.frame(CoverConversion_tables$Unburned, condition = "Unburned 1985-2022"),
                                              data.frame(CoverConversion_tables$Burned, condition = "Burned Area 1985-2022")) )  %>%
  mutate(Ecoregion_coarse = factor(Ecoregion_coarse, 
                                   levels = unique(CoverConversion_tables$All$Ecoregion_coarse)[c(3,1,5,2,4)]))

CC_panel_Eco <- CoverConversion_table_eco %>%
  rename(`km.2` = X1000.km.2) %>%
  ggplot(
    aes(axis1 = EVTP_coarse, axis2 = EVTP_coarse22, y = `km.2`)) +
  geom_alluvium(aes(fill = EVTP_coarse), width = 0.1, alpha = 0.85) +
  scale_fill_manual(values = c(
    c("Conifer" = "green4",
      "Hardwood" = "#CFF79A",
      "Shrubland" =  "#9D0000",
      "Herb" = "cyan3")
  )) +
  geom_stratum(width = 0.1, fill = "lightgray", color = "black") +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 1, as.character(after_stat(stratum)), "")),
    stat = "stratum", size = 4, direction = "y", nudge_x = -0.45) +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 2, as.character(after_stat(stratum)), "")),
    stat = "stratum", size = 4, direction = "y", nudge_x = 0.45) +
  scale_x_discrete(limits = c("1985", "2022"), expand = c(.15, .15)) +
  labs(
    y = ~paste("10"^3," km"^2),
    x = "") +
  theme_minimal() + 
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'),
        axis.text.x = element_text(size = 18),
        # ,  angle = 35, hjust = 1),  
        axis.text.y = element_text(size = 18),
        axis.title.y.left = element_text(size = 20),
        strip.text = element_text(size = 20, face = 'bold')
  ) +
  facet_grid2(condition~Ecoregion_coarse,scales = 'free_y', independent = 'y')


ggsave(filename = "Bhootetal_EVT_code_data_figures/plots/Supplement/Supp_ConversionEco.png",
       plot =  CC_panel_Eco,
       dpi = 300, width = 25, height = 12.5
)



