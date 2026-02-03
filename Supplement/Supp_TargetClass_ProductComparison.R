library(tidyverse)
library(ggplot2)
library(ggh4x)
library(cowplot)

load("Bhootetal_EVT_code_data_figures/Data/Comparison_df.data")

rfEVTP_LFRDB <- rfEVTP_compare[['LFRDB']] %>%
  mutate(CompareTo = paste0("LFRDB_", LFRDB_VALUE),
         rfEVTP = rfEVTP,
         CompareClass = LFRDB_VALUE,
         Comparison = "LFRDB") %>%
  select(Comparison, rfEVTP, CompareTo, CompareClass, count, Freq_prop, Freq_prop_perc)
rfEVTP_FLD <- rfEVTP_compare[['FLD']] %>%
  mutate(CompareTo = paste0( "TreeMapFLD_", FLD_xWalk_PHYS),
         rfEVTP = EVTP_name,
         CompareClass = FLD_xWalk_PHYS,
         Comparison = "TreeMapFLD") %>%
  select(Comparison,rfEVTP, CompareTo, CompareClass, count, Freq_prop, Freq_prop_perc)
rfEVTP_FOR <- rfEVTP_compare[['FOR']] %>%
  mutate(CompareTo = paste0("TreeMapFOR_", FOR_xWalk_PHYS),
         rfEVTP = EVTP_name,
         CompareClass = FOR_xWalk_PHYS,
         Comparison = "TreeMapFOR") %>%
  select(Comparison,rfEVTP, CompareTo, CompareClass, count, Freq_prop, Freq_prop_perc)
rfEVTP_Fveg <- rfEVTP_compare[['Fveg']] %>%
  mutate(CompareTo = paste0("Fveg_", Fveg_xWalk_PHYS),
         rfEVTP = EVTP_name,
         CompareClass = Fveg_xWalk_PHYS,
         Comparison = "Fveg"
  ) %>%
  select(Comparison, rfEVTP, CompareTo, CompareClass, count, Freq_prop, Freq_prop_perc)
rfEVTP_nlcd <- rfEVTP_compare[['nlcd']] %>%
  mutate(CompareTo = paste0("nlcd_", nlcd_xWalk_PHYS),
         rfEVTP = EVTP_name,
         CompareClass = nlcd_xWalk_PHYS,
         Comparison = "NLCD"
  ) %>%
  select(Comparison, rfEVTP, CompareTo, CompareClass, count, Freq_prop, Freq_prop_perc)

summary_df <- rbind(rfEVTP_LFRDB,
                    rfEVTP_Fveg,
                    rfEVTP_FLD,
                    rfEVTP_FOR,
                    rfEVTP_nlcd)

TargetClassComparisons <- summary_df %>%
  ggplot(aes(x = factor(rfEVTP, (unique(CompareClass))) , y = factor(CompareClass, levels = (unique(CompareClass))))) +
  geom_tile(aes(fill = Freq_prop), color = "black", linewidth = 0.5) +
  geom_text(aes(label = Freq_prop_perc, vjust = 1)) +
  scale_fill_gradient(low = "white", high = "dodgerblue2" ) + #dodgerblue2") +
  theme_bw() +
  labs(x = "EVT Physiognomy (Random Forest)", y = "", fill = "Percentage") +
  ggtitle("") + 
  facet_wrap2(.~Comparison, ncol = 1, scales = 'free_y', strip.position="right") +
  theme(axis.text.x = element_text(hjust = 1, angle = 35, size = 12),
        axis.text.y = element_text(size = 12))


ggsave(filename = "Bhootetal_EVT_code_data_figures/plots/Supplement/Supp_TargetClassComparisons.png",
       plot =  TargetClassComparisons,
       dpi = 300, width = 4.5, height = 10
)

