library(ggplot2)
library(tidyverse)
library(reshape2)
library(Metrics)

load("Bhootetal_EVT_code_data_figures/Data/ControlEval.data")

EVT_PHYS_target <- c('Conifer', 'Hardwood', 'Shrubland')

annotate_abs <- Big_Table_SpatRecovery_controleval %>% 
  filter(YearMinusBurn < -1) %>% 
  mutate(`expected cover` = (nPix_expected)*30^2*1e-6, 
         `actual cover` = (nPix_wAll)*30^2*1e-6 ) %>%
  group_by(EVT_PHYS) %>%
  summarise(q5_e = quantile(`expected cover`, 0.05),
            max_a = max(`expected cover`),
            min_a = min(`actual cover`),
            r2 = cor(`actual cover`, `expected cover`)^2,
            rmse = rmse(`expected cover`,`actual cover`),
            r2_rmse_label = paste0("atop(italic(R)^2 == ", format(round(r2, 3), nsmall = 2), 
                                   ", italic(RMSE) == ", format(round(rmse, 3), nsmall = 2), " )" )
            )

absolute_eval <- Big_Table_SpatRecovery_controleval %>% 
  filter(YearMinusBurn < -1) %>% 
  mutate(`expected cover (km^2)` = (nPix_expected)*30^2*1e-6, 
         `actual cover (km^2)` = (nPix_wAll)*30^2*1e-6 ) %>% 
  ggplot(aes( y = `expected cover (km^2)`, x = `actual cover (km^2)`)) + 
  stat_binhex(bins = 50) + 
  scale_fill_gradient2(trans = scales::log_trans(base = 10),  mid = 'blue', high = "gold") +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'gray60') + theme_bw() +
  geom_text(
    data = annotate_abs,
    aes(x = min_a, y = max_a, label = r2_rmse_label),
    hjust = 0, vjust = 1, size = 5,
    parse = TRUE,
    inherit.aes = FALSE
  ) +
  labs(
    x = ~paste("Actual Cover (km"^2,")"),
    y = ~paste("Expected Cover (km"^2,")")
    ) +
  facet_wrap2(.~EVT_PHYS, scales = 'free') 




ggsave(filename = "Bhootetal_EVT_code_data_figures/plots/Supplement/Supp_ControlEval.png",
       plot =  absolute_eval,
       dpi = 300, width = 12, height = 5
)
