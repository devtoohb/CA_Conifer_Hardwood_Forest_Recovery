library(ggplot2)

load("Bhootetal_EVT_code_data_figures/Data/FeatureImportance_rfEVT.data")

FeatureImportance <- ggplot(data = impTable) + 
  geom_bar(aes(y = reorder(Vars, as.numeric(V2)), 
               x = round(as.numeric(V2))),
           stat = 'identity', fill = 'dodgerblue2') +
  xlab("Importance [Unitless]") + ylab("") +
  ggtitle("Feature Importance", 
          subtitle = paste0("EVT Accuracy = ", 
                            round(0.67, digits = 3),
                            " | EVT PHYS Accuracy = ", round(0.78, digits = 3))) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = -30, hjust = 0.05))

ggsave(filename = "Bhootetal_EVT_code_data_figures/plots/Supplement/Supp_FeatureImportance.png",
       plot = FeatureImportance, 
       dpi = 300, width = 7.5, height = 5
)
