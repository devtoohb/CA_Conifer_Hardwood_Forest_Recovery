library(ggplot2)
library(tidyverse)
library(ggh4x)

load("Bhootetal_EVT_code_data_figures/Data/GrandTable.data")
load("Bhootetal_EVT_code_data_figures/Data/BurnArea_ts.data")  

BurnArea_numbers <- BurnArea_numbers_wEco %>% 
  mutate(EVT_PHYS = factor(EVT_PHYS, levels = c('Conifer', 'Hardwood', 'Shrubland', "Fine"))) %>%
  group_by(BurnYear, EVT_PHYS) %>%
  summarise( 
    EVTtotal_prefire_sum = sum(nPix_expected) * 30^2 * 1e-6)

BurnArea_numbers_byEco <- BurnArea_numbers_wEco %>% 
  mutate(EVT_PHYS = factor(EVT_PHYS, levels = c('Conifer', 'Hardwood', 'Shrubland', "Fine"))) %>%
  group_by(BurnYear, EVT_PHYS, Ecoregion_coarse) %>% 
  summarise( 
    EVTtotal_prefire_sum = sum(nPix_expected) * 30^2 * 1e-6)


BigTable_SpatRecovery_Ecoregion_coarse <- BigTable_SpatRecovery %>%
  group_by(BurnYear, YearMinusBurn, EVT_PHYS_coarse, Ecoregion_coarse, Reburn) %>%
  summarise(
    nPix_wAll = sum(nPix_wAll),
    nPix_expected = sum(nPix_expected)
    # ,
    # SpatRecov_wAll = nPix_wAll / nPix_expected,
    # CovDef_wAll =  nPix_expected - nPix_wAll
  ) %>% ungroup()

CD_Eco <- BigTable_SpatRecovery_Ecoregion_coarse %>%
  filter(YearMinusBurn >= -1 & EVT_PHYS_coarse != "Fine" & BurnYear > 1985) %>%
  group_by(Ecoregion_coarse, BurnYear, EVT_PHYS_coarse, Reburn) %>% 
  mutate(Ecoregion_coarse,
         CovDef_wAll = -1 * nPix_wAll +  ifelse(length(nPix_wAll[YearMinusBurn == -1]) == 0, NA, 
                                                nPix_wAll[YearMinusBurn == -1]),
         Year = BurnYear + YearMinusBurn
         
  ) %>%
  group_by(Ecoregion_coarse, Year, EVT_PHYS_coarse) %>% 
  summarise(`Cover Deficit` = sum(CovDef_wAll, na.rm=T),
            `Burn Area` = sum(nPix_wAll[YearMinusBurn == -1], na.rm = T)
  )

CD_Eco$`Burn Area` <- BurnArea_numbers_wEco$count[match(apply(CD_Eco[,c("Ecoregion_coarse", "Year", "EVT_PHYS_coarse")],
                                                              1, paste0, collapse = "_"), 
                                                        apply(BurnArea_numbers_wEco[,c("Ecoregion_coarse", "BurnYear", "EVT_PHYS")],
                                                              # [,c(1,4,2)]
                                                               1, paste0, collapse = "_")  )]
CD_Eco$`Burn Area`[is.na(CD_Eco$`Burn Area`)] <- 0

CD_Eco <- CD_Eco %>% mutate(
  EVT_PHYS_coarse_def = gsub('Shrubland', 'Shrubland Deficit', 
                             gsub('Conifer', 'Conifer Forest Deficit', 
                                  gsub('Hardwood', 'Hardwood Forest Deficit', 
                                       EVT_PHYS_coarse) ) ),
  EVT_PHYS_coarse = gsub('Shrubland', 'Shrubland Cumulative Burn Area', 
                         gsub('Conifer', 'Conifer Cumulative Burn Area', 
                              gsub('Hardwood', 'Hardwood Cumulative Burn Area', 
                                   EVT_PHYS_coarse) ) ))

data.frame(CD_Eco %>% 
             filter(Year == 2022) %>% 
             group_by(EVT_PHYS_coarse_def) %>% 
             summarise(`Cover Deficit Total` = sum(`Cover Deficit`, na.rm=T) * 30^2 * 1e-6,
                       `Burn Area Total` = sum(`Burn Area`, na.rm = T) * 30^2 * 1e-6 )
)

CD_Eco_list <- CD_Eco %>% 
  group_by(EVT_PHYS_coarse, Ecoregion_coarse) %>% 
  reframe(
    Year = Year,
    `Cover Deficit Total` = `Cover Deficit` * 30^2 * 1e-6,
    `Burn Area Total` = cumsum(`Burn Area`) * 30^2 * 1e-6 ) %>%
  group_by(EVT_PHYS_coarse, Ecoregion_coarse) %>% 
  group_split()


CoverDeficit <- CD_Eco %>%
  ggplot() +
  geom_bar(aes(x = Year, 
               y = `Cover Deficit` * 30^2 * 1e-6 , fill = Ecoregion_coarse), 
           stat = 'identity',color = 'black', linewidth = 0.35) +
  geom_hline(yintercept = 0) +
  xlab('Year') + ylab( ~paste("Cover Deficit (",km^2,")") ) + 
  
  scale_fill_manual(values = 
                      c("N. Coast\nand Klamath" =  "darkblue",
                        "Sierra Nevada" = "olivedrab3",
                        "Cn CA Fthills\nCoastal Mtns" = "gold",
                        "S. Coast\nand Mtns" = "tomato2",
                        "Cascades"  = "lightyellow2"))+ 
  
  labs(fill = "") +
  xlim(c(1985,2023)) + # only goes to 2022, but 2023 is needed to not remove the bar in 2022
  facet_wrap2(.~EVT_PHYS_coarse_def, nrow = 3, scales = 'free_y') +
  theme_bw() +
  theme(legend.key.spacing.y = unit(0.2, "cm"),
        legend.key.width=unit(0.5, "cm"),
        legend.text = element_text(size = 17),
        legend.title = element_blank(),
        legend.box.background = element_rect(color = 'black', linewidth = 1.5),
        axis.title  = element_text(size = 20),
        axis.text  = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_text(face = 'bold', size = 25),
        legend.position = c(0.325,0.905)
  ) +
  guides(fill = guide_legend(byrow = TRUE,
                             ncol = 2))

BA_total <- CD_Eco %>% group_by(Ecoregion_coarse, EVT_PHYS_coarse) %>% 
  mutate(`Burn Area Total` = cumsum(`Burn Area`)* 30^2*1e-6) %>% 
  ggplot(aes(x = Year, y =`Burn Area Total` , color = Ecoregion_coarse)) + 
  ggborderline::geom_borderline(size=1.5, bordercolour = "black")+
  # geom_line(linewidth = 1.75) +  
  scale_color_manual(values = 
                       c("N. Coast\nand Klamath" =  "darkblue",
                         "Sierra Nevada" = "olivedrab3",
                         "Cn CA Fthills\nCoastal Mtns" = "gold",
                         "S. Coast\nand Mtns" = "tomato2",
                         "Cascades"  = "lightyellow2"))+ 
  xlab('Year') + ylab( ~paste("Burn Area (",km^2,")") ) + 
  labs(fill = "") + facet_wrap2(.~EVT_PHYS_coarse, nrow = 3, scales = 'free_y') +
  theme_bw() + theme(axis.title  = element_text(size = 20),
                     axis.text  = element_text(size = 20),
                     strip.background = element_blank(),
                     strip.text = element_text(face = 'bold', size = 25),
                     legend.position = 'none'
  )

ggsave(filename = 
         "Bhootetal_EVT_code_data_figures/plots/Main/CoverDeficit_BATotal.png",
       plot = plot_grid(CoverDeficit, BA_total, ncol = 2) , 
       dpi = 300, height = 12.5, width = 17.5
)
