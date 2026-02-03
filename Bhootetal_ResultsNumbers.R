library(tidyverse)
# Values used for Results section

################################################################################
# 3.1 Vegetation Cover change in California 
################################################################################

table_results <- read.csv("Bhootetal_EVT_code_data_figures/plots/Main/OverallCover_summary_table.csv")
load("Bhootetal_EVT_code_data_figures/Data/CoverConversions.data")
load("Bhootetal_EVT_code_data_figures/Data/ProportionChange_eco.data")
load("Bhootetal_EVT_code_data_figures/Data/ProportionChange.data")
load("Bhootetal_EVT_code_data_figures/Data/CoverConversions.data")
CoverConversion_table_eco <- data.frame(rbind(data.frame(CoverConversion_tables$All, condition = "All Area"),
                                              data.frame(CoverConversion_tables$Unburned, condition = "Unburned Area"),
                                              data.frame(CoverConversion_tables$Burned, condition = "Burn Area (1985-2022)")) )  %>%
  mutate(Ecoregion_coarse = factor(Ecoregion_coarse, 
                                   levels = unique(CoverConversion_tables$All$Ecoregion_coarse)[c(3,1,5,2,4)]))

load("Bhootetal_EVT_code_data_figures/Data/BurnArea_ts.data")

table_results %>%
  group_by(Condition) %>%
  summarise(forest1985 = X1985..km2.[X == 'Conifer'] + X1985..km2.[X == 'Hardwood'],
            forest2022 = X2022..km2.[X == 'Conifer'] + X2022..km2.[X == 'Hardwood'],
            diff_forest = forest2022 - forest1985,
            diff_prop = diff_forest / forest1985 * 100)

CoverConversion_tables$Burned %>% 
  filter(EVTP_coarse == "Conifer") %>% 
  summarise(`Conifer to Hardwood km^2` = sum(`1000 km^2`[EVTP_coarse22 == "Hardwood"]) * 1000, 
            `Conifer to Hardwood, %` = sum(`1000 km^2`[EVTP_coarse22 == "Hardwood"])/sum(`1000 km^2`) * 100)

CoverConversion_tables$Burned %>% 
  filter(EVTP_coarse == "Hardwood") %>% 
  summarise(`Hardwood to Conifer km^2` = sum(`1000 km^2`[EVTP_coarse22 == "Conifer"]) * 1000, 
            `Hardwood to Conifer, %` = sum(`1000 km^2`[EVTP_coarse22 == 'Conifer'])/sum(`1000 km^2`) * 100)

CoverConversion_tables$Burned %>% 
  filter(EVTP_coarse == "Conifer" & EVTP_coarse22 == "Hardwood") %>% 
  summarise(`Conifer to Hardwood km^2` = sum(`1000 km^2`[Ecoregion_coarse == "Cn CA Fthills\nCoastal Mtns"]) * 1000, 
            `Conifer to Hardwood, %` = sum(`1000 km^2`[Ecoregion_coarse == "Cn CA Fthills\nCoastal Mtns"])/sum(`1000 km^2`) * 100)

CoverConversion_tables$Burned %>% 
  filter(EVTP_coarse == "Hardwood" ) %>% 
  summarise(`Hardwood to Conifer km^2` = sum(`1000 km^2`[Ecoregion_coarse == "Cn CA Fthills\nCoastal Mtns" & 
                                                           EVTP_coarse22 == "Conifer"]) * 1000, 
            `Hardwood to Conifer, ratio` = sum(`1000 km^2`[Ecoregion_coarse == "Cn CA Fthills\nCoastal Mtns" & 
                                                             EVTP_coarse22 == "Conifer"])/sum(`1000 km^2`) * 100)

CoverConversion_tables$Unburned %>% 
  filter(EVTP_coarse == "Hardwood") %>% 
  summarise(`Hardwood to Conifer km^2` = sum(`1000 km^2`[EVTP_coarse22 == "Conifer"]) * 1000, 
            `Hardwood to Conifer, %` = sum(`1000 km^2`[EVTP_coarse22 == 'Conifer'])/sum(`1000 km^2`) * 100)


Eco_BurnC2S <- CoverConversion_tables$Burned %>% 
  filter(EVTP_coarse == "Conifer") %>%
  group_by(Ecoregion_coarse) %>%
  summarise(`Conifer to Shrubland km^2` = sum(`1000 km^2`[EVTP_coarse22 == "Shrubland"]) * 1000, 
            `Conifer to Shrubland, %` = sum(`1000 km^2`[EVTP_coarse22 == 'Shrubland'])/sum(`1000 km^2`) * 100)
Eco_BurnC2S

Eco_BurnConifer_Prop <- ProportionChange_eco %>% filter(EVT_PHYS_coarse == "Conifer" & 
                                                          BurnCondition_Name == "Burned 1985-2022" & 
                                                          layer == 1985) %>% 
  group_by(EVT_PHYS_coarse, BurnCondition_Name, Ecoregion_Name, layer) %>%
  summarise(Start_Year = layer,
            `km^2` = (count)*30^2*1e-6) %>%
  ungroup() %>% select(-layer)
Eco_BurnConifer_Prop

Eco_numbers <- cbind(Eco_BurnConifer_Prop[match(Eco_BurnC2S$Ecoregion_coarse, Eco_BurnConifer_Prop$Ecoregion_Name),], Eco_BurnC2S)

Eco_numbers %>%
  mutate(`Conifer to Shrubland by Burned Area, %` = `Conifer to Shrubland km^2`/`km^2` * 100) %>%
  select(EVT_PHYS_coarse, BurnCondition_Name, Ecoregion_Name, Start_Year, `Conifer to Shrubland by Burned Area, %`)


ProportionChange_eco %>% filter(EVT_PHYS_coarse == "Conifer") %>% 
  group_by(EVT_PHYS_coarse, BurnCondition_Name, Ecoregion_Name) %>%
  summarise(Start_Year = layer,
            `km^2` = (count[Start_Year == 2022] - count[Start_Year == 1985])*30^2*1e-6,
            `prop delta start, %` = `km^2` / (count[Start_Year == 1985] * 30^2*1e-6) * 100
  ) %>%
  ungroup() %>% 
  filter(Start_Year==2022) %>%
  select(-Start_Year) %>%
  arrange(Ecoregion_Name)

CoverConversion_table_eco %>% 
  filter(EVTP_coarse22 == 'Conifer') %>% 
  group_by(Ecoregion_coarse, condition) %>% 
  summarise(`km^2` = sum(X1000.km.2 * 1000))

CoverConversion_table_eco %>% 
  filter(EVTP_coarse %in% c('Conifer', 'Hardwood') & 
           EVTP_coarse22 %in% c('Conifer', 'Hardwood') ) %>% 
  group_by(Ecoregion_coarse, condition, EVTP_coarse, EVTP_coarse22) %>%
  summarise(`km^2` = sum(X1000.km.2) * 1000)

ProportionChange %>%
  group_by(EVT_PHYS_coarse, BurnCondition_Name) %>%
  summarise(
    `Rel Cover, delta %` = (proportion[layer == 2022] - proportion[layer == 1985]) * 100 ) 

BurnArea_numbers <- BurnArea_numbers_wEco %>% 
  mutate(EVT_PHYS = factor(EVT_PHYS, levels = c('Conifer', 'Hardwood', 'Shrubland', "Herb"))) %>%
  group_by(BurnYear, EVT_PHYS) %>%
  summarise( 
    EVTtotal_prefire_sum = sum(nPix_expected) * 30^2 * 1e-6)

# Ho = There is no trend in the composition of what burns
# Ha = There is a monotonic increasing/decreasing trend in the composition of what burns

# First two rows:
# tau = Kendall tau, direction of monotonic
# sl = p-value for significance 
sapply(c('Conifer', 'Hardwood', 'Shrubland', "Herb"), function(x)
  Kendall::MannKendall(unlist(BurnArea_numbers %>% 
                                group_by(BurnYear) %>%
                                mutate(BurnedProp = EVTtotal_prefire_sum / sum(EVTtotal_prefire_sum)) %>%
                                filter(EVT_PHYS %in% x) %>% 
                                arrange(BurnYear) %>%
                                ungroup() %>%
                                select(BurnedProp)))
)


################################################################################
# 3.2 Forests loss from wildfire is outpacing recovery
################################################################################
rm(list = ls())
table_results <- read.csv("Bhootetal_EVT_code_data_figures/plots/Main/OverallCover_summary_table.csv")
load("Bhootetal_EVT_code_data_figures/Data/GrandTable.data")
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

BigTable_SpatRecovery_Ecoregion_coarse <- BigTable_SpatRecovery %>%
  group_by(BurnYear, YearMinusBurn, EVT_PHYS_coarse, Ecoregion_coarse, Reburn) %>%
  summarise(
    nPix_wAll = sum(nPix_wAll),
    nPix_expected = sum(nPix_expected)) %>% 
  ungroup()

CD_Eco <- BigTable_SpatRecovery_Ecoregion_coarse %>%
  filter(YearMinusBurn >= -1 & EVT_PHYS_coarse != "Herb" & BurnYear > 1985) %>%
  group_by(Ecoregion_coarse, BurnYear, EVT_PHYS_coarse, Reburn) %>% 
  mutate(Ecoregion_coarse,
         CovDef_wAll = -1 * nPix_wAll +  ifelse(length(nPix_wAll[YearMinusBurn == -1]) == 0, NA, 
                                                nPix_wAll[YearMinusBurn == -1]),
         Year = BurnYear + YearMinusBurn) %>%
  group_by(Ecoregion_coarse, Year, EVT_PHYS_coarse) %>% 
  summarise(`Cover Deficit` = sum(CovDef_wAll, na.rm=T),
            `Burn Area` = sum(nPix_wAll[YearMinusBurn == -1], na.rm = T) )

CD_Eco$`Burn Area` <- BurnArea_numbers_wEco$count[match(apply(CD_Eco[,c("Ecoregion_coarse", "Year", "EVT_PHYS_coarse")],
                                                              1, paste0, collapse = "_"), 
                                                        apply(BurnArea_numbers_wEco[,c("Ecoregion_coarse", "BurnYear", "EVT_PHYS")],
                                                              1, paste0, collapse = "_")  )]
CD_Eco$`Burn Area`[is.na(CD_Eco$`Burn Area`)] <- 0

CD_Eco <- CD_Eco %>% mutate(
  EVT_PHYS_coarse_def = gsub('Shrubland', 'Shrubland Cover Deficit', 
                             gsub('Conifer', 'Conifer Cover Deficit', 
                                  gsub('Hardwood', 'Hardwood Cover Deficit', 
                                       EVT_PHYS_coarse) ) ),
  EVT_PHYS_coarse = gsub('Shrubland', 'Shrubland Cumulative Burn Area', 
                         gsub('Conifer', 'Conifer Cumulative Burn Area', 
                              gsub('Hardwood', 'Hardwood Cumulative Burn Area', 
                                   EVT_PHYS_coarse) ) ))

CD_total <- CD_Eco %>% 
  filter(Year == 2022) %>% 
  group_by(EVT_PHYS_coarse_def) %>% 
  summarise(`Cover Deficit Total` = sum(`Cover Deficit`, na.rm=T) * 30^2 * 1e-6) 

CD_total
CD_total$`Cover Deficit Total`  + (table_results %>% 
                                     filter(Condition == "Burned area") %>%
                                     select(Change..km2.))[1:3,]


CD_Eco_list <- CD_Eco %>% 
  group_by(EVT_PHYS_coarse, Ecoregion_coarse) %>% 
  reframe(
    Year = Year,
    `Cover Deficit Total` = `Cover Deficit` * 30^2 * 1e-6,
    `Burn Area Total` = cumsum(`Burn Area`) * 30^2 * 1e-6 ) %>%
  group_by(EVT_PHYS_coarse, Ecoregion_coarse) %>% 
  group_split()
names(CD_Eco_list) <- sapply(CD_Eco_list, function(x){
  paste0(unique(x$Ecoregion_coarse), "_", unique(x$EVT_PHYS_coarse) )
})

CD_Eco_list$`Sierra Nevada_Conifer Cumulative Burn Area` %>% 
  filter(Year == 2022) %>% select(`Burn Area Total`) +
  CD_Eco_list$`Sierra Nevada_Hardwood Cumulative Burn Area` %>% 
  filter(Year == 2022) %>% select(`Burn Area Total`)

CD_Eco_list$`N. Coast
and Klamath_Conifer Cumulative Burn Area` %>% 
  filter(Year == 2022) %>% select(`Burn Area Total`) +
  CD_Eco_list$`N. Coast
and Klamath_Hardwood Cumulative Burn Area` %>% 
  filter(Year == 2022) %>% select(`Burn Area Total`)


CD_Eco_list$`Sierra Nevada_Conifer Cumulative Burn Area` %>%
  summarise(`Cover Deficit Total, diff, km^2` = `Cover Deficit Total`[Year == 2022] - `Cover Deficit Total`[Year == 2018])
CD_Eco_list$`Sierra Nevada_Hardwood Cumulative Burn Area` %>%
  summarise(`Cover Deficit Total, diff, km^2` = `Cover Deficit Total`[Year == 2022] - `Cover Deficit Total`[Year == 2018])


CD_Eco_list$`N. Coast
and Klamath_Conifer Cumulative Burn Area` %>%
  summarise(`Cover Deficit Total, diff, km^2` = `Cover Deficit Total`[Year == 2022] - `Cover Deficit Total`[Year == 2018])
CD_Eco_list$`N. Coast
and Klamath_Hardwood Cumulative Burn Area` %>%
  summarise(`Cover Deficit Total, diff, km^2` = `Cover Deficit Total`[Year == 2022] - `Cover Deficit Total`[Year == 2018])



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
Kendall::MannKendall(unlist(forBA %>% 
                              filter(EVT_PHYS == 'Hardwood') %>% 
                              group_by(BurnYear, EVT_PHYS) %>% 
                              mutate(prop = area / sum(area)) %>% 
                              ungroup() %>% 
                              filter(Ecoregion_coarse == "N. Coast\nand Klamath") %>% 
                              arrange(BurnYear) %>% ungroup() %>% select(prop)))


###### 
# Discussion 4.3
######
CD_Eco %>%
  group_by(Ecoregion_coarse,EVT_PHYS_coarse) %>% 
  mutate(BA_cumulative = cumsum(`Burn Area`)) %>%
  ungroup() %>%
  filter(Year %in% c(2001, 2010) & EVT_PHYS_coarse %in% c("Conifer Cumulative Burn Area",
                                                          "Hardwood Cumulative Burn Area")) %>%
  group_by(Ecoregion_coarse,EVT_PHYS_coarse) %>% 
  summarise(BA_diff = (BA_cumulative[Year == 2010] - BA_cumulative[Year == 2001]) * 30^2 * 1e-6 ) %>%
  ungroup() %>%
  group_by(Ecoregion_coarse) %>% 
  summarise(BA_diff = sum(BA_diff))

CD_Eco %>%
  group_by(Ecoregion_coarse,EVT_PHYS_coarse) %>% 
  mutate(BA_cumulative = cumsum(`Burn Area`)) %>%
  ungroup() %>%
  filter(Year %in% c(2022) & EVT_PHYS_coarse %in% c("Conifer Cumulative Burn Area",
                                                    "Hardwood Cumulative Burn Area")) %>%
  group_by(Ecoregion_coarse) %>% 
  summarise(BA_total = sum(BA_cumulative) * 30^2 * 1e-6)

1972/3653

CD_Eco %>%
  group_by(Ecoregion_coarse,EVT_PHYS_coarse) %>% 
  mutate(BA_cumulative = cumsum(`Burn Area`)) %>%
  ungroup() %>%
  filter(Year %in% c(2010) & EVT_PHYS_coarse %in% c("Conifer Cumulative Burn Area",
                                                    "Hardwood Cumulative Burn Area")) %>%
  mutate(`Cover Deficit` = `Cover Deficit` * 30^2 * 1e-6, BA_cumulative = BA_cumulative * 30^2 * 1e-6
  )


CD_Eco %>%
  group_by(Ecoregion_coarse,EVT_PHYS_coarse) %>% 
  mutate(BA_cumulative = cumsum(`Burn Area`)) %>%
  ungroup() %>%
  filter(Year %in% c(2022) & EVT_PHYS_coarse %in% c("Conifer Cumulative Burn Area",
                                                    "Hardwood Cumulative Burn Area")) %>%
  mutate(`Cover Deficit` = `Cover Deficit` * 30^2 * 1e-6, BA_cumulative = BA_cumulative * 30^2 * 1e-6
  )


CD_Eco %>%
  group_by(Ecoregion_coarse,EVT_PHYS_coarse) %>% 
  mutate(BA_cumulative = cumsum(`Burn Area`)) %>%
  ungroup() %>%
  filter(Year %in% c(2022) & EVT_PHYS_coarse %in% c("Conifer Cumulative Burn Area")) %>%
  group_by(Ecoregion_coarse) %>% 
  summarise(BA_total = sum(BA_cumulative) * 30^2 * 1e-6,
            `Cover Deficit` = `Cover Deficit` * 30^2 * 1e-6)
805/1988

CD_Eco %>%
  group_by(Ecoregion_coarse,EVT_PHYS_coarse) %>% 
  mutate(BA_cumulative = cumsum(`Burn Area`)) %>%
  ungroup() %>%
  filter(Year %in% c(2022) & EVT_PHYS_coarse %in% c("Hardwood Cumulative Burn Area")) %>%
  group_by(Ecoregion_coarse) %>% 
  summarise(BA_total = sum(BA_cumulative) * 30^2 * 1e-6,
            `Cover Deficit` = `Cover Deficit` * 30^2 * 1e-6)
272/1665


load("Bhootetal_EVT_code_data_figures/Data/ProportionChange_eco.data")
# to compare with Wang et al. 2022 AGU Advances 
ProportionChange_eco %>% 
  filter(layer %in% c(1985, 2021)) %>%
  mutate(isforest = ifelse(EVT_PHYS_coarse %in% c("Conifer", "Hardwood"), T, F)) %>%
  group_by(isforest, layer, BurnCondition_Name) %>%
  summarise(count = sum(count) * 30^2 * 1e-6) %>%
  ungroup() %>% group_by(isforest, BurnCondition_Name) %>%
  summarise(diff = (count[layer == 2021] - count[layer == 1985]))


################################################################################
# 3.3	Regional post-fire recovery curves
################################################################################
load("Bhootetal_EVT_code_data_figures/Data/GrandTable.data")
BigTable_SpatRecovery_Ecoregion_coarse <- BigTable_SpatRecovery %>%
  group_by(BurnYear, YearMinusBurn, EVT_PHYS_coarse, Ecoregion_coarse, Reburn) %>%
  summarise(
    nPix_wAll = sum(nPix_wAll),
    nPix_expected = sum(nPix_expected)
  ) %>% ungroup()




Visualization_Ecos_relative <- BigTable_SpatRecovery_Ecoregion_coarse %>% 
  filter(EVT_PHYS_coarse %in% c('Conifer', 'Hardwood', 'Shrubland') & 
           BurnYear %in% 1986:2007 & YearMinusBurn >= -1 & YearMinusBurn <= 15 & !Reburn) %>%
  group_by(YearMinusBurn, EVT_PHYS_coarse, Ecoregion_coarse, BurnYear) %>% 
  summarise(nPix = sum(nPix_wAll), 
            nPix_expected = sum(nPix_expected), 
            Recovery = nPix/nPix_expected
  ) %>%
  ungroup() %>%
  group_by(YearMinusBurn, EVT_PHYS_coarse, Ecoregion_coarse) %>%
  filter(is.finite(Recovery)) %>%
  mutate(q25_recov = quantile(Recovery, 0.25),
         q50_recov = quantile(Recovery, 0.5),
         q75_recov = quantile(Recovery, 0.75))%>% 
  mutate(`EVT Physiognomy` = EVT_PHYS_coarse) %>%
  filter(`EVT Physiognomy` %in% c('Conifer', 'Hardwood', 'Shrubland'))

Visualization_Ecos_relative %>% 
  group_by(EVT_PHYS_coarse, Ecoregion_coarse) %>%
  summarise( standage_at_min = unique(YearMinusBurn[q50_recov == min(q50_recov)]) ,
             `Recovery diff` = unique(q50_recov[YearMinusBurn==15] - min(q50_recov)) )


################################################################################
# 3.4	Changes in Post-fire Recovery
################################################################################

load("Bhootetal_EVT_code_data_figures/Data/GrandTable.data")

BigTable_SpatRecovery_CA <- BigTable_SpatRecovery %>%
  filter(!Reburn) %>%
  group_by(BurnYear, YearMinusBurn, EVT_PHYS_coarse) %>%
  summarise(
    nPix_wAll = sum(nPix_wAll),
    nPix_expected = sum(nPix_expected),
    SpatRecov_wAll = nPix_wAll / nPix_expected
  ) %>% ungroup() 

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


SpatRecov_dist_box_shaded_data <- Recov_Halves2 %>%
  group_by(EVT_PHYS_coarse, YearMinusBurn, `Burn Years`) %>%
  mutate(median_recov = median(SpatRecov_wAll),
         q25_recov = quantile(SpatRecov_wAll, 0.25),
         q75_recov = quantile(SpatRecov_wAll, 0.75),
         sd_recov = sd(SpatRecov_wAll)) %>%
  ungroup() 

SpatRecov_dist_box_shaded_data %>% 
  filter( YearMinusBurn == 15) %>% 
  group_by(`Burn Years`, EVT_PHYS_coarse)%>% summarise(unique(median_recov))

sapply( c('Conifer', 'Hardwood', 'Shrubland'),
        function(x){ 
          Kendall::MannKendall(unlist(BigTable_SpatRecovery_CA %>% 
                                        filter(YearMinusBurn == 15 & 
                                                 EVT_PHYS_coarse == x) %>% 
                                        ungroup() %>% 
                                        arrange(BurnYear) %>%
                                        select(SpatRecov_wAll)) 
          )
        }
)




BigTable_SpatRecovery_CA %>% 
  filter(BurnYear %in% c(1986, 2007) & YearMinusBurn == 15) 
BigTable_SpatRecovery_CA %>% 
  filter(YearMinusBurn == 15) %>%
  group_by(EVT_PHYS_coarse) %>%
  summarise(sd_recov = sd(SpatRecov_wAll))


BigTable_SpatRecovery_CA %>% 
  filter(YearMinusBurn == 15) %>%
  mutate(group_86to96_97to07 = ifelse(BurnYear > 2007, NA, ifelse(BurnYear %in% 1986:1996, "Fire Years 86-96","Fire Years 97-07") )) %>%
  na.omit() %>%
  group_by(group_86to96_97to07, EVT_PHYS_coarse) %>%
  summarise(SpatRecov_wAll_mean = mean(SpatRecov_wAll),
            SpatRecov_wAll_sd = sd(SpatRecov_wAll)
  ) %>%
  ungroup() %>%
  arrange(EVT_PHYS_coarse)








# 
# 
# #############
# # 4.1
# #############
# klamath_hardwood_change_all <- unlist(ProportionChange_eco %>% 
#   filter(EVT_PHYS_coarse == "Hardwood" &
#            layer %in%  c(1985,2022) &
#            Ecoregion_Name %in% "N. Coast\nand Klamath" &
#            BurnCondition_Name == "All Cover"
#   ) %>%
#   summarise(diff = (count[layer == 2022] - count[layer == 1985]) *30^2 * 1e-6 ))
# 
# CoverConversion_table_eco %>%
#   filter(EVTP_coarse == "Conifer" & 
#            EVTP_coarse22 == "Hardwood" & 
#            Ecoregion_coarse %in% "N. Coast\nand Klamath" &
#            BurnCondition_Name == "All Cover")
