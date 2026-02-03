library(ggplot2)

load("Bhootetal_EVT_code_data_figures/Data/ConfusionMatrix_PredictionActual.data")
# Plot confusion matrix
cnf_mtx <- ggplot(data = confusion_df %>% 
         group_by(Prediction) %>% 
         mutate(Freq_prop =  round(100 * Freq/sum(Freq)),
                Freq_prop_perc = paste0(Freq_prop, "%") ), 
       aes(x = Prediction, y = Reference)) +
  geom_tile(aes(fill = Freq_prop), color = "black", linewidth = 0.5) +
  geom_text(aes(label = Freq_prop_perc, vjust = 1)) +
  scale_fill_gradient(low = "white", high = "dodgerblue2") +
  theme_bw() +
  labs(x = "Predicted", y = "Reference", fill = "%") +
  ggtitle("EVT Physiognomy, confusion matrix") + theme(axis.text.x = element_text(hjust = 1, angle = 35))


ggsave(filename = "Bhootetal_EVT_code_data_figures/plots/Supplement/Supp_ConfusionMatrix.png",
       plot =  cnf_mtx,
       dpi = 300, width = 6, height = 5
)

