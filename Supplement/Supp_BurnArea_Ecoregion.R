library(ggplot2)
library(tidyverse)
library(ggh4x)
library(cowplot)
library(Kendall)

load("Bhootetal_EVT_code_data_figures/Data/BurnArea_ts.data")  



BurnArea_numbers <- BurnArea_numbers_wEco %>% 
  mutate(EVT_PHYS = factor(EVT_PHYS, levels = c('Conifer', 'Hardwood', 'Shrubland', "Herb"))) %>%
  group_by(BurnYear, EVT_PHYS) %>%
  summarise( 
    EVTtotal_prefire_sum = sum(nPix_expected) * 30^2 * 1e-6)

BurnArea_numbers_byEco <- BurnArea_numbers_wEco %>% 
  mutate(EVT_PHYS = factor(EVT_PHYS, levels = c('Conifer', 'Hardwood', 'Shrubland', "Herb"))) %>%
  group_by(BurnYear, EVT_PHYS, Ecoregion_coarse) %>% 
  summarise( 
    EVTtotal_prefire_sum = sum(nPix_expected) * 30^2 * 1e-6)


totalBA_df <- BurnArea_numbers_wEco %>% 
  filter(EVT_PHYS %in% c('Conifer', 'Hardwood', 'Shrubland', "Herb")  ) %>%
  mutate(EVT_PHYS = mgsub::mgsub(EVT_PHYS, 
                                 c('Conifer', 'Hardwood', 'Shrubland', "Herb") , 
                                 c('Conifer * +', 'Hardwood  -', 'Shrubland * -', "Herb * -") ),
         EVT_PHYS = factor(EVT_PHYS, levels = c('Conifer * +','Hardwood  -',  'Shrubland * -', "Herb * -"))) %>%
  group_by(BurnYear, EVT_PHYS) %>% 
  summarise(EVTtotal_prefire_sum = sum(nPix_expected) * 30^2 * 1e-6)


totalBA_df_forTrendTest <- totalBA_df %>% 
  group_by(BurnYear) %>%
  mutate(proportion_EVT = EVTtotal_prefire_sum / sum(EVTtotal_prefire_sum)) 
totalBA_df_TrendTest<- lapply(c('Conifer * +', 'Hardwood  -', 'Shrubland * -') , function(x) {
  print(x)
  return(MannKendall(unlist(totalBA_df_forTrendTest %>%
                              filter(EVT_PHYS == x) %>%
                              ungroup() %>%
                              select(proportion_EVT))))
})

# Numbers for Results section
totalBA_df %>% 
  group_by(EVT_PHYS) %>% 
  mutate(cumsum_BA = cumsum(EVTtotal_prefire_sum)) %>% 
  filter(BurnYear == 2022)

BurnArea_numbers_byEco %>% 
  group_by(EVT_PHYS, Ecoregion_coarse) %>% 
  mutate(cumsumArea = cumsum(EVTtotal_prefire_sum),
         Area_2000to2010 = sum(EVTtotal_prefire_sum[BurnYear %in% c(2000:2010)]),
         Prop_Area_2000to2010_toTotal = Area_2000to2010 / cumsumArea[BurnYear == 2022]) 



forBA <- BurnArea_numbers_wEco %>%
  filter(YearMinusBurn == -1) %>%
  mutate(EVT_PHYS = factor(EVT_PHYS, levels = c("Conifer", "Hardwood",
                                                "Shrubland", "Herb"
  )),
  Ecoregion_coarse = factor(Ecoregion_coarse, levels = c("N. Coast\nand Klamath",
                                                         "Cascades",
                                                         "Cn CA Fthills\nCoastal Mtns",
                                                         "Sierra Nevada",
                                                         "S. Coast\nand Mtns"))) %>%
  group_by(BurnYear, Ecoregion_coarse, EVT_PHYS) %>% 
  
  summarise(area = sum(count)*30^2*1e-6 )

forBA %>%
  mutate(BurnYear_bin = ifelse(BurnYear %in% c(1986:2000), "86-00",
                               ifelse(BurnYear %in% c(2001:2010), "01-10",
                                      ifelse(BurnYear %in% c(2011:2020), "11-20",
                                             "21-22")))) %>%
  group_by(BurnYear_bin, EVT_PHYS, Ecoregion_coarse) %>%
  summarise(area = sum(area))


For_xSection_BurnArea_eco_total <- forBA %>% 
  ggplot(aes(x = BurnYear, y = area, fill = Ecoregion_coarse)) + 
  ylab(~paste("Burn Area (km"^2, ")")) + xlab("Burn Year") +
  # ggtitle("Burn Area for Single Burns by Ecoregion") +
  geom_bar(stat = 'identity', color = 'black', linewidth = 0.2) + 
  scale_fill_manual(values =c("N. Coast\nand Klamath" =  "darkblue",
                              "Sierra Nevada" = "olivedrab3",
                              "Cn CA Fthills\nCoastal Mtns" = "gold",
                              "S. Coast\nand Mtns" = "tomato2",
                              "Cascades"  = "lightyellow2"
  )) +
  facet_grid2(.~EVT_PHYS, scales = 'free_y', independent = 'y') +
  theme_bw() 


For_xSection_BurnArea_eco_prop <- forBA %>% 
  ggplot(aes(x = BurnYear, y = area, fill = Ecoregion_coarse)) + 
  ylab("Burn Area Proportion") + xlab("Burn Year") +
  geom_bar(stat = 'identity', position = 'fill', color = 'black', linewidth = 0.2) + 
  scale_fill_manual(values =c("N. Coast\nand Klamath" =  "darkblue",
                              "Sierra Nevada" = "olivedrab3",
                              "Cn CA Fthills\nCoastal Mtns" = "gold",
                              "S. Coast\nand Mtns" = "tomato2",
                              "Cascades"  = "lightyellow2"
  )) +
  facet_grid2(.~EVT_PHYS, scales = 'free_y', independent = 'y') +
  theme_bw() 

grob_BA_plots <-  plot_grid(For_xSection_BurnArea_eco_total  + 
                              theme(
                                plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'),
                                axis.text = element_text(size = 15),
                                axis.title = element_text(size = 15),
                                strip.text.x = element_text(size = 15),
                                strip.background = element_blank(),
                                legend.text = element_text(size = 12),
                                strip.text = element_text(face = 'bold'),
                                legend.position = c(0.1,0.75), 
                                legend.title = element_blank(), 
                                legend.key.spacing.y = unit(2.25, units = 'mm'),
                                legend.box.background = element_rect(color = 'black',
                                                                     linewidth = 1.25)) + 
                              guides(fill = guide_legend(byrow = TRUE, ncol = 2)),
                            For_xSection_BurnArea_eco_prop + theme(legend.position = 'none',
                                                                   plot.title = element_text(hjust = 0.5,
                                                                                             size = 20,
                                                                                             face = 'bold'),
                                                                   axis.text = element_text(size = 15),
                                                                   axis.title = element_text(size = 15),
                                                                   strip.text.x = element_text(size = 15),
                                                                   strip.background = element_blank(),
                                                                   strip.text = element_text(face = 'bold')),
                            nrow = 2)








eco_evt_trend_BA <- do.call(rbind, lapply(unique(forBA$Ecoregion_coarse), function(x) {
  evt_trend <- lapply(unique(forBA$EVT_PHYS), function(y) {
    print(x);print(y)
    vec <- data.frame(t(unlist(MannKendall(unlist(forBA %>%
                                                    filter(Ecoregion_coarse == x) %>%
                                                    group_by(BurnYear) %>%
                                                    mutate(prop = area / sum(area)) %>%
                                                    ungroup()%>%
                                                    filter(EVT_PHYS == y) %>%
                                                    ungroup() %>%
                                                    select(prop) ) ))))
    
    vec$Ecoregion_coarse <- x
    return(vec)
  })
  evt_trend <- data.frame(do.call(rbind, evt_trend))
  
  evt_trend$EVT_PHYS <- unique(forBA$EVT_PHYS)
  return(evt_trend)
}))

eco_evt_trend_BA %>% filter(sl < 0.05)




eco_evt_trend_BA <- do.call(rbind, lapply(unique(forBA$Ecoregion_coarse), function(x) {
  evt_trend <- lapply(unique(forBA$EVT_PHYS), function(y) {
    print(x);print(y)
    vec <- data.frame(t(unlist(MannKendall(unlist(forBA %>%
                                                    filter(EVT_PHYS == y) %>%
                                                    group_by(BurnYear) %>%
                                                    mutate(prop = area / sum(area)) %>%
                                                    ungroup()%>%
                                                    filter(Ecoregion_coarse == x) %>%
                                                    ungroup() %>%
                                                    select(prop) ) ))))
    
    vec$Ecoregion_coarse <- x
    return(vec)
  })
  evt_trend <- data.frame(do.call(rbind, evt_trend))
  
  evt_trend$EVT_PHYS <- unique(forBA$EVT_PHYS)
  return(evt_trend)
}))

eco_evt_trend_BA %>% filter(sl < 0.05)

data.frame(t(unlist(MannKendall(unlist(forBA %>%
                                         mutate(Ecoregion_coarse = gsub("Sierra Nevada", "N. Coast\nand Klamath", Ecoregion_coarse)) %>%
                                         group_by(Ecoregion_coarse, EVT_PHYS, BurnYear) %>%
                                         summarise(area = sum(area)) %>%
                                         ungroup() %>%
                                         filter(EVT_PHYS == "Conifer") %>%
                                         group_by(BurnYear) %>%
                                         mutate(prop = area / sum(area)) %>%
                                         ungroup()%>%
                                         filter(Ecoregion_coarse == "N. Coast\nand Klamath") %>%
                                         mutate(Ecoregion_coarse = gsub("N. Coast\nand Klamath", "combined SN + Klamath", Ecoregion_coarse)) %>%
                                         ungroup() %>%
                                         select(prop) ) ))))



ggsave(filename = "Bhootetal_EVT_code_data_figures/plots/Supplement/Supp_BurnAreaByEcoregion.png",
       plot = grob_BA_plots , 
       dpi = 300, width = 17.5, height = 10
)

