library(ggplot2)
library(cowplot)
library(ggalluvial)
library(ggpattern)
library(ggh4x)

load("Bhootetal_EVT_code_data_figures/Data/CoverConversions.data")
load("Bhootetal_EVT_code_data_figures/Data/OverallCoverSummary_barplot.data")


CC_other_grob <- do.call(rbind,lapply(names(CoverConversion_tables), function(x){ 
  df <- CoverConversion_tables[[x]] 
  df$Condition <- x
  return(df)
})) %>%
  mutate(Condition = gsub("All", "All Cover", Condition),
         Condition = gsub("Burned", "Burned Area 1985-2022", Condition),
         Condition = gsub("Unburned", "Unburned 1985-2022", Condition)) %>%
  group_by(Condition, EVTP_coarse, EVTP_coarse22) %>%
  summarise(`1000 km^2` = sum(`1000 km^2`)) %>%
  ggplot(
    aes(axis1 = EVTP_coarse, axis2 = EVTP_coarse22, y = `1000 km^2`)) +
  geom_alluvium(aes(fill = EVTP_coarse), width = 0.1, alpha = 1) +
  scale_fill_manual(values = c(
    c("Conifer" = "green4",
      "Hardwood" = "#CFF79A",
      "Shrubland" =  "#9D0000",
      "Herb" = "cyan3")
  )) +
  geom_stratum(width = 0.1, fill = "lightgray", color = "black") +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 1, as.character(after_stat(stratum)), "")),
    stat = "stratum", size = 5, direction = "y", nudge_x = -0.45) +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 2, as.character(after_stat(stratum)), "")),
    stat = "stratum", size = 5, direction = "y", nudge_x = 0.45) +
  scale_x_discrete(limits = c("1985", "2022"), expand = c(.15, .15)) +
  labs(
    y = ~paste("10"^3," km"^2),
    x = "") +
  facet_wrap(.~Condition, scales = 'free_y', nrow = 3) +
  theme_minimal() + 
  theme(legend.position = 'none',
        strip.text = element_text(size = 20, face = 'bold'),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.y.left = element_text(size = 17)
  ) 



OverallCover_summary_melt <- OverallCover_summary_melt %>% 
  mutate(Condition = gsub("Unburned Cover",
                          "Unburned 1985-2022", 
                          Condition),
         Condition = gsub("Burn Area Cover \\((1985-2022)\\)", "Burned Area \\1", 
                          Condition))
Overall_Cover_Change_barplot <- OverallCover_summary_melt %>%
  ggplot(aes(x = EVTP_coarse, y = value/1e3, 
             fill = EVTP_coarse, group = variable, pattern = variable)) + 
  scale_pattern_manual(
    "Position",
    values = c("1985" = "none",
               "2022" = "stripe") ) +
  scale_fill_manual(values = c(
    c("Conifer" = "green4",
      "Hardwood" = "#CFF79A",
      "Shrubland" =  "#9D0000",
      "Herb" = "cyan3")
  )) +
  geom_col_pattern(position = 'dodge', color = 'black', show.legend = T) +
  facet_wrap2(.~Condition, nrow = 3) +
  ylim(c(0,max(OverallCover_summary_melt$value / 1e3)+5)) +
  xlab("") + ylab(~paste("10"^3," km"^2)) + guides(fill = "none") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 20),
        axis.text.x = element_text( size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.y.left = element_text(size = 17),
        legend.title = element_blank(),
        legend.key.size = unit(0.5, 'cm'),
        legend.text = element_text(size = 17)
  )



Overall_and_Conversions <- plot_grid(Overall_Cover_Change_barplot + 
                                       theme(legend.position=c(.75,.95), 
                                             legend.direction = 'horizontal', 
                                             legend.key.size = unit(10, units = 'mm'),
                                             legend.background = element_rect(fill = 'transparent'),
                                             strip.text = element_text(face = 'bold') ),
                                     CC_other_grob,
                                     ncol = 2)


ggsave(filename = "Bhootetal_EVT_code_data_figures/plots/Main/OverallChange_and_Conversions.png",
       plot =  Overall_and_Conversions, 
       dpi = 300, width = 12.5, height = 10
)

