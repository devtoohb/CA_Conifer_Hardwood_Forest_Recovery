library(ggplot2)
library(tidyverse)
library(Kendall)
library(ggh4x)
library(cowplot)

load("Bhootetal_EVT_code_data_figures/Data/GrandTable.data")

BigTable_SpatRecovery_CA <- BigTable_SpatRecovery %>%
  filter(!Reburn) %>%
  group_by(BurnYear, YearMinusBurn, EVT_PHYS_coarse) %>%
  summarise(
            nPix_wAll = sum(nPix_wAll),
            nPix_expected = sum(nPix_expected),
            SpatRecov_wAll = nPix_wAll / nPix_expected
  ) %>% ungroup() 

# Ho = There is no trend in Recovery at Stand Age 15
# Ha = There is a monotonic increasing/decreasing trend Recovery at Stand Age 15

# First two rows:
  # tau = Kendall tau, direction of monotonic
  # sl = p-value for significance 
sapply( c('Conifer', 'Hardwood', 'Shrubland'), function(x) MannKendall(unlist(BigTable_SpatRecovery_CA %>% 
                                        filter(YearMinusBurn == 15 & 
                                                 EVT_PHYS_coarse == x) %>% 
                                        ungroup() %>% 
                                        arrange(BurnYear) %>%
                                        select(SpatRecov_wAll)) ))


Recov_FirstHalf_v2 <- BigTable_SpatRecovery_CA %>% 
  filter(YearMinusBurn %in% -1:15 & EVT_PHYS_coarse == 'Conifer' & BurnYear %in% c(1986:1996)) %>%
  select(YearMinusBurn, SpatRecov_wAll, EVT_PHYS_coarse)
Recov_SecondHalf_v2 <- BigTable_SpatRecovery_CA %>% 
  filter(YearMinusBurn %in% -1:15 & EVT_PHYS_coarse == 'Conifer' & BurnYear %in% c(1997:2007)) %>%
  select(YearMinusBurn, SpatRecov_wAll, EVT_PHYS_coarse)

Recov_FirstHalf_v2 <- cbind(Recov_FirstHalf_v2, rep("1986-1996", nrow(Recov_FirstHalf_v2)) )
colnames(Recov_FirstHalf_v2)[4] <- "Burn Years"
Recov_SecondHalf_v2 <- cbind(Recov_SecondHalf_v2, rep("1997-2007", nrow(Recov_SecondHalf_v2)) )
colnames(Recov_SecondHalf_v2)[4] <- "Burn Years"


Recov_FirstHalf2_v2 <- BigTable_SpatRecovery_CA %>% 
  filter(YearMinusBurn %in% -1:15 & EVT_PHYS_coarse == 'Hardwood' & BurnYear %in% c(1986:1996)) %>%
  select(YearMinusBurn, SpatRecov_wAll, EVT_PHYS_coarse)
Recov_SecondHalf2_v2 <- BigTable_SpatRecovery_CA %>% 
  filter(YearMinusBurn %in% -1:15 & EVT_PHYS_coarse == 'Hardwood' & BurnYear %in% c(1997:2007)) %>%
  select(YearMinusBurn, SpatRecov_wAll, EVT_PHYS_coarse)

Recov_FirstHalf2_v2 <- cbind(Recov_FirstHalf2_v2, rep("1986-1996", nrow(Recov_FirstHalf2_v2)) )
colnames(Recov_FirstHalf2_v2)[4] <- "Burn Years"
Recov_SecondHalf2_v2 <- cbind(Recov_SecondHalf2_v2, rep("1997-2007", nrow(Recov_SecondHalf2_v2)) )
colnames(Recov_SecondHalf2_v2)[4] <- "Burn Years"


Recov_FirstHalf3_v2 <- BigTable_SpatRecovery_CA %>% 
  filter(YearMinusBurn %in% -1:15 & EVT_PHYS_coarse == 'Shrubland' & BurnYear %in% c(1986:1996)) %>%
  select(YearMinusBurn, SpatRecov_wAll, EVT_PHYS_coarse)
Recov_SecondHalf3_v2 <- BigTable_SpatRecovery_CA %>% 
  filter(YearMinusBurn %in% -1:15 & EVT_PHYS_coarse == 'Shrubland' & BurnYear %in% c(1997:2007)) %>%
  select(YearMinusBurn, SpatRecov_wAll, EVT_PHYS_coarse)

Recov_FirstHalf3_v2 <- cbind(Recov_FirstHalf3_v2, rep("1986-1996", nrow(Recov_FirstHalf3_v2)) )
colnames(Recov_FirstHalf3_v2)[4] <- "Burn Years"
Recov_SecondHalf3_v2 <- cbind(Recov_SecondHalf3_v2, rep("1997-2007", nrow(Recov_SecondHalf3_v2)) )
colnames(Recov_SecondHalf3_v2)[4] <- "Burn Years"


Recov_Halves2 <- rbind(Recov_FirstHalf_v2, Recov_SecondHalf_v2, 
                       Recov_FirstHalf2_v2, Recov_SecondHalf2_v2,
                       Recov_FirstHalf3_v2, Recov_SecondHalf3_v2
) 


# Boxplot recovery and xsection shaded

conifer_color = "green4"
hardwood_color = "#CFF79A"
shrubland_color = "#9D0000"

stand5_color <- 'firebrick'
stand15_color <- "white"

BurnYear8696_color <- 'skyblue2'
BurnYear9707_color <- '#FF4A00'


Facet_Xsections_shaded <- BigTable_SpatRecovery_CA %>% 
  
  filter(YearMinusBurn %in% c(15)  & EVT_PHYS_coarse %in% c('Conifer', 'Hardwood', 'Shrubland') ) %>%
  mutate(EVT_PHYS_coarse = gsub('Conifer', 'Conifer *',gsub('Hardwood', 'Hardwood ', EVT_PHYS_coarse))  ) %>%
  mutate(`EVT Physiognomy` = EVT_PHYS_coarse,
         Observation_Year = BurnYear + YearMinusBurn,
         `Burn Year` = BurnYear,
         YearMinusBurn = factor(
           ifelse(YearMinusBurn == 10, "Stand Age 10", "Stand Age 15"),
           levels = c( "Stand Age 10", "Stand Age 15")),
         lwd = ifelse(`EVT Physiognomy` == "Conifer", 1.25, 0.75)) %>%
  ggplot(aes(x = `Burn Year`, y = SpatRecov_wAll*100, color = `EVT Physiognomy`, linewidth =  `EVT Physiognomy`, size = `EVT Physiognomy`)) + 
  annotate("rect", xmin = 1986, xmax = 1996.45, ymin = 1.9*100, ymax = 1.95*100, colour = BurnYear8696_color, fill = BurnYear8696_color, alpha = 0.15) +
  annotate("rect", xmin = 1996.5, xmax = 2007, ymin = 1.9*100, ymax = 1.95*100, colour = BurnYear9707_color, fill = BurnYear9707_color, alpha = 0.15 ) +
  annotate("rect", xmin = 1986, xmax = 1996.45, ymin = 1.9*100, ymax = 1.95*100, colour = BurnYear8696_color, fill = BurnYear8696_color, alpha = 0.15) +
  annotate("rect", xmin = 1996.5, xmax = 2007, ymin = 1.9*100, ymax = 1.95*100, colour = BurnYear9707_color, fill = BurnYear9707_color, alpha = 0.15 ) +
  scale_color_manual(values = c('1986-1996' = BurnYear8696_color,
                                '1997-2007' = BurnYear9707_color)) + 
  ggborderline::geom_borderline(linewidth = 1, bordercolour = "gray25")+
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, se = F,
              linetype = 'dashed',
              linewidth = 1) +
  scale_color_manual(values = c("Conifer *" = conifer_color,
                                "Hardwood " = hardwood_color,
                                "Shrubland" = shrubland_color
  )) +
  scale_linewidth_manual(values = c("Conifer *" = 1.35,
                                    "Hardwood " = 0.85,
                                    "Shrubland" = 0.85
  )) +
  scale_size_manual(values = c("Conifer *" = 1.35,
                               "Hardwood " = 0.75,
                               "Shrubland" = 0.75
  )) +
  geom_hline(yintercept =  100) +
  ylim(c(0,2*100)) +
  ggtitle("Recovery cross-section for 15-y stands")+
  ylab("Recovery (%)") + xlab("Burn Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'),
        axis.title  = element_text(size = 17),
        axis.text  = element_text(size = 17),
        legend.text = element_text(size = 17),
        strip.text.x = element_text(size = 17),
        legend.title = element_blank(),
        strip.text = element_text(face = 'bold'),
        legend.box.background = element_rect(color = 'black', linewidth = 1.5),
        legend.position = c(0.815, 0.15),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray100"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray100"))


SpatRecov_dist_box_shaded_data <- Recov_Halves2 %>%
  group_by(EVT_PHYS_coarse, YearMinusBurn, `Burn Years`) %>%
  mutate(median_recov = median(SpatRecov_wAll),
         q25_recov = quantile(SpatRecov_wAll, 0.25),
         q75_recov = quantile(SpatRecov_wAll, 0.75),
         sd_recov = sd(SpatRecov_wAll)) %>%
  ungroup() 


SpatRecov_dist_box_shaded_line <- SpatRecov_dist_box_shaded_data %>% 
  ggplot(aes(x = YearMinusBurn, y = median_recov*100, color = `Burn Years`)) + 
  ggborderline::geom_borderline(linewidth = 1, bordercolour = "gray25")+
  scale_color_manual(values = c('1986-1996' = BurnYear8696_color, 
                                '1997-2007' = BurnYear9707_color)) +
  geom_ribbon(data=SpatRecov_dist_box_shaded_data[SpatRecov_dist_box_shaded_data$`Burn Years` == "1997-2007",],
              aes(x = YearMinusBurn,ymin = q25_recov*100, ymax = q75_recov*100, fill = `Burn Years`, alpha = `Burn Years`),
              inherit.aes = FALSE) +
  geom_ribbon(data=SpatRecov_dist_box_shaded_data[SpatRecov_dist_box_shaded_data$`Burn Years` == "1986-1996",],
              aes(x = YearMinusBurn,ymin = q25_recov*100, ymax = q75_recov*100, 
                  fill = `Burn Years`, alpha =`Burn Years`),
              inherit.aes = FALSE)+
  scale_fill_manual(values = c('1986-1996' = BurnYear8696_color, 
                               '1997-2007' = BurnYear9707_color)) +
  ggtitle("Recovery curves by burn year bin") +
  geom_hline(yintercept = 100) + geom_vline(xintercept = 0) +
  xlab("Stand Age") + ylab("Recovery (%)") +
  facet_wrap2(.~EVT_PHYS_coarse, nrow = 1, scales='free_y') +
  scale_alpha_manual(values=c(0.25,0.25)) +
  ylim(c(0,1.4*100)) +
  xlim(c(-1,15)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'),
        axis.text = element_text(size = 17),
        axis.title = element_text(size = 17),
        strip.text.x = element_text(size = 17),
        strip.background = element_blank(),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 17, hjust = 0.5),
        legend.box.background = element_rect(color = 'black', linewidth = 1.25),
        legend.position = c(0.855, 0.2),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray100"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray100"))


Recov_Xsection <- plot_grid(SpatRecov_dist_box_shaded_line, Facet_Xsections_shaded, 
                            nrow = 2,
                            rel_heights = c(0.65,0.75))


ggsave(filename =
         "Bhootetal_EVT_code_data_figures/plots/Main/Recovery_CrossSection.png",
       plot = Recov_Xsection , 
       dpi = 300, width = 8.5, height = 10
)
