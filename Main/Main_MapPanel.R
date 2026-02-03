library(terra)
library(tidyterra)
library(ggplot2)
library(ggspatial)
library(cowplot)

# EPA Level 3: EPA.gov
caeco_vect <- vect("Bhootetal_EVT_code_data_figures/Data/MapFigure_data/us_eco_l3_state_boundaries/")
caeco_rast <- rast("Bhootetal_EVT_code_data_figures/Data/MapFigure_data/CAeco_aggregated_mercator.tif")

# catalog.data.gov
Mexico_shp <- vect("Bhootetal_EVT_code_data_figures/Data/MapFigure_data/Mexico_shp")
WUS_shp <- vect("Bhootetal_EVT_code_data_figures/Data/MapFigure_data/cb_2018_us_state_500k/")

# Developed using FRAP: https://www.fire.ca.gov/what-we-do/fire-resource-assessment-program
nFireSince1985 <- rast("Bhootetal_EVT_code_data_figures/Data/MapFigure_data/nFire_1985to2022_mercator.tif")

hardwood_330m_insameout <- rast("Bhootetal_EVT_code_data_figures/Data/MapFigure_data/HardwoodChange_1985to2022_mercator.tif")
conifer_330m_insameout <- rast("Bhootetal_EVT_code_data_figures/Data/MapFigure_data/ConiferChange_1985to2022_mercator.tif")

caeco_vect <- project(caeco_vect, hardwood_330m_insameout)

caeco_vect <- caeco_vect[caeco_vect$STATE_NAME == 'California']

WUS_shp <- WUS_shp[WUS_shp$NAME %in% c('California', 'Oregon', 'Idaho', 'Nevada', 'Arizona', 'Utah')]
WUS_shp <- project(WUS_shp, caeco_vect)

ca_shp <- WUS_shp[WUS_shp$NAME == 'California']
WUS_shp <- WUS_shp[WUS_shp$NAME %in% c('Oregon', 'Idaho', 'Nevada', 'Arizona', 'Utah')]

target_eco <- c("Coast Range",                                    
                "Cascades",
                "Sierra Nevada", 
                "Central California Foothills and Coastal Mountains",
                "Klamath Mountains/California High North Coast Range",
                "Southern California Mountains", 
                "Southern California/Northern Baja Coast",
                "Eastern Cascades Slopes and Foothills")

nontarget_eco <- c("Central Basin and Range",
                   "Mojave Basin and Range",
                   "Central California Valley",
                   "Northern Basin and Range", 
                   "Sonoran Basin and Range")

caeco_vect_target <- caeco_vect[caeco_vect$US_L3NAME %in% target_eco]
caeco_vect_nontarget <- caeco_vect[caeco_vect$US_L3NAME %in% nontarget_eco]


holder_eco <- lapply(which(caeco_vect_target$US_L3NAME %in% c("Coast Range", 
                                                              "Klamath Mountains/California High North Coast Range")), 
                     function(x)caeco_vect_target[x])
for(i in 1:length(holder_eco)){
  if(i == 1) {
    northcoast <- holder_eco[[i]]
  }else{
    northcoast <- combineGeoms(northcoast, holder_eco[[i]])
  }
}

holder_eco <- lapply(which(caeco_vect_target$US_L3NAME %in% c("Sierra Nevada")), function(x)caeco_vect_target[x])
for(i in 1:length(holder_eco)){
  if(i == 1) {
    sierra <- holder_eco[[i]]
  }else{
    sierra <- combineGeoms(sierra, holder_eco[[i]])
  }
}

holder_eco <- lapply(which(caeco_vect_target$US_L3NAME %in% c("Southern California Mountains", "Southern California/Northern Baja Coast")), function(x)caeco_vect_target[x])
for(i in 1:length(holder_eco)){
  if(i == 1) {
    socal <- holder_eco[[i]]
  }else{
    socal <- combineGeoms(socal, holder_eco[[i]])
  }
}

holder_eco <- lapply(which(caeco_vect_target$US_L3NAME %in% c("Central California Foothills and Coastal Mountains")), function(x)caeco_vect_target[x])
for(i in 1:length(holder_eco)){
  if(i == 1) {
    cencoast <- holder_eco[[i]]
  }else{
    cencoast <- combineGeoms(cencoast, holder_eco[[i]])
  }
}

holder_eco <- lapply(which(caeco_vect_target$US_L3NAME %in% c("Cascades", "Eastern Cascades Slopes and Foothills")), function(x)caeco_vect_target[x])
for(i in 1:length(holder_eco)){
  if(i == 1) {
    ecasc <- holder_eco[[i]]
  }else{
    ecasc <- combineGeoms(ecasc, holder_eco[[i]])
  }
}

caeco_vect_target <- northcoast + cencoast + ecasc + sierra + socal



h330iso_plot <- ggplot() + 
  geom_spatvector(data = ca_shp, fill = 'gray70', lwd = 1) +
  geom_spatraster(data = hardwood_330m_insameout, na.rm=T) +
  scale_fill_gradientn(colours = alpha(c("firebrick4", "red2", "gold", "white", "skyblue", "deepskyblue", "blue"), 
                                       c(1, 1, 1, 1, 1, 1, 1)),
                       values = scales::rescale(c(-1, -0.75, -0.2, 0, 0.2, 0.75, 1)),
                       breaks = c(-1, 0, 1),
                       limits = c(-1, 1),
                       name = "Hardwood\nCover Change\n1985 to 2022",
                       labels = c("-1 (Loss)\n", "0", "\n1 (Gain)"),
                       na.value = "transparent") +
  geom_spatvector(data = caeco_vect_target, fill = 'transparent', lwd = 0.2) +
  geom_spatvector(data = WUS_shp, fill = 'gray70', lwd = 1) +
  geom_spatvector(data = Mexico_shp, fill = 'gray70', lwd = 1) +
  xlab("") + ylab("") +
  theme_bw() +
  geom_sf(data = sf::st_graticule(lon = seq(-124,-114, 2),
                                  lat = seq(30,45, 2)) %>%
            vect(), lwd = 0.25, color = 'lightgray') + 
  coord_sf(ylim = c(3875000, 3875000 + 1.25e6 ),
           xlim = c(-13850000, -13850000 + 1.15e6)) +
  annotation_north_arrow(location = 'bl', which_north = "true", style = north_arrow_orienteering()) +
  annotation_scale(location = 'bl', pad_x = unit(2.25, 'cm'), text_cex = 2) +
  annotate(geom = "text", x = -13750000, y = 4250000, label = 'Pacific\n  Ocean', size = 7.5) +
  theme(panel.background = element_rect(fill = 'white'),
        legend.position = c(0.7,0.775), 
        legend.text = element_text(size = 15), 
        
        # legend.direction = 'horizontal',
        legend.title.position = 'left',
        legend.title = element_text(size = 15, vjust = 0.5, hjust = 0.5, face = 'bold'),
        legend.box.background = element_rect(color = 'black', linewidth = 1.5),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18), legend.key.height  =  unit(7.5, 'mm'))

c330iso_plot <- ggplot() + 
  geom_spatvector(data = ca_shp, fill = 'gray70', lwd = 1) +
  geom_spatraster(data = conifer_330m_insameout, na.rm=T) +
  scale_fill_gradientn(colours = alpha(c("firebrick4", "red2", "gold", "white", "skyblue", "deepskyblue", "blue"), 
                                       c(1, 1, 1, 1, 1, 1, 1)),
                       values = scales::rescale(c(-1, -0.75, -0.2, 0, 0.2, 0.75, 1)),
                       breaks = c(-1, 0, 1),
                       limits = c(-1, 1),
                       name = "Conifer\nCover Change\n1985 to 2022",
                       labels = c("-1 (Loss)\n", "0", "\n1 (Gain)"),
                       na.value = "transparent") +
  geom_spatvector(data = caeco_vect_target, fill = 'transparent', lwd = 0.2) +
  geom_spatvector(data = WUS_shp, fill = 'gray70', lwd = 1) +
  geom_spatvector(data = Mexico_shp, fill = 'gray70', lwd = 1) +
  xlab("") + ylab("") +
  theme_bw() +
  geom_sf(data = sf::st_graticule(lon = seq(-124,-114, 2),
                                  lat = seq(30,45, 2)) %>%
            vect(), lwd = 0.25, color = 'lightgray') + 
  coord_sf(ylim = c(3875000, 3875000 + 1.25e6 ),
           xlim = c(-13850000, -13850000 + 1.15e6)) +
  annotation_north_arrow(location = 'bl', which_north = "true", style = north_arrow_orienteering()) +
  annotation_scale(location = 'bl', pad_x = unit(2.25, 'cm'), text_cex = 2) +
  annotate(geom = "text", x = -13750000, y = 4250000, label = 'Pacific\n  Ocean', size = 7.5) +
  theme(panel.background = element_rect(fill = 'white'),
        legend.position = c(0.7,0.775), 
        legend.text = element_text(size = 15), 
        
        # legend.direction = 'horizontal',
        legend.title.position = 'left',
        legend.title = element_text(size = 15, vjust = 0.5, hjust = 0.5, face = 'bold'),
        legend.box.background = element_rect(color = 'black', linewidth = 1.5),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18), legend.key.height  =  unit(7.5, 'mm'))



coltab(caeco_rast,layer=1) <- cbind(ID = 0:5,t(col2rgb(c(
  "transparent",
  "darkblue",# "#882299", #1
  "olivedrab3",# "#F0E442", #4
  "yellow",# "#999933", #8
  "tomato1",# "red2", #9
  "lightyellow2"# "lightgreen"  #13
))))
levels(caeco_rast) <- data.frame(ID = 0:5,
                                 category = c("",
                                              "N. Coast\nand Klamath",
                                              "Sierra Nevada",
                                              "Cn CA Foothills\nCoastal Mtns",
                                              "S. Coast\nand Mtns",
                                              "Cascades"))
eco_plot <- ggplot() + 
  geom_spatvector(data = ca_shp, fill = 'gray70', lwd = 1) +
  geom_spatraster(data = caeco_rast, na.rm=T, use_coltab = T) +
  geom_spatvector(data = caeco_vect_target, fill = 'transparent', lwd = 0.2) +
  geom_spatvector(data = WUS_shp, fill = 'gray70', lwd = 1) +
  geom_spatvector(data = Mexico_shp, fill = 'gray70', lwd = 1) +
  xlab("") + ylab("")+
  theme_bw() +
  geom_sf(data = sf::st_graticule(lon = seq(-124,-114, 2),
                                  lat = seq(30,45, 2)) %>%
            vect(), lwd = 0.25, color = 'lightgray') + 
  coord_sf(ylim = c(3875000, 3875000 + 1.25e6 ),
           xlim = c(-13850000, -13850000 + 1.15e6)) +
  annotation_north_arrow(location = 'bl',
                         which_north = "true",
                         style = north_arrow_orienteering()) +
  annotation_scale(location = 'bl', pad_x = unit(2.25, 'cm'), text_cex = 2) +
  annotate(geom = "text", x = -13750000, y = 4250000, label = 'Pacific\n  Ocean', size = 7.5) +
  theme(panel.background = element_rect(fill = 'white'),
        legend.key.spacing.y = unit(0.2, "cm"),
        legend.spacing.y = unit(0.25, 'cm'), 
        legend.spacing.x = unit(0.1, 'cm'), 
        legend.title = element_blank(),
        legend.text = element_text(size = 35),
        legend.position = c(0.7,0.775), legend.box.background = element_rect(color = 'black', linewidth = 1.5),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))+
  guides(fill = guide_legend(byrow = T))


coltab(nFireSince1985) <-  data.frame(values = 1:2, cols = c('orange', 'purple3'))
levels(nFireSince1985) <- data.frame(ID = 1:2,
                                     category = c("Single Burn",  #"Single Burn\n(Recovery Analysis)",
                                                  "Multiple Burns"))

nFire_plot <- ggplot() + 
  geom_spatvector(data = ca_shp, fill = 'gray70', lwd = 1) +
  geom_spatraster(data = nFireSince1985, na.rm=T, use_coltab = T) +
  geom_spatvector(data = ca_shp, fill = 'transparent', lwd = 1) +
  geom_spatvector(data = WUS_shp, fill = 'gray70', lwd = 1) +
  geom_spatvector(data = Mexico_shp, fill = 'gray70', lwd = 1) +
  geom_spatvector(data = caeco_vect_target, fill = 'transparent', lwd = 0.2) +
  xlab("") + ylab("")+
  theme_bw() +
  geom_sf(data = sf::st_graticule(lon = seq(-124,-114, 2),
                                  lat = seq(30,45, 2)) %>%
            vect(), lwd = 0.25, color = 'lightgray') + 
  coord_sf(ylim = c(3875000, 3875000 + 1.25e6 ),
           xlim = c(-13850000, -13850000 + 1.15e6)) +
  annotation_north_arrow(location = 'bl',
                         which_north = "true",
                         style = north_arrow_orienteering()) +
  annotation_scale(location = 'bl', pad_x = unit(2.25, 'cm'), text_cex = 2) +
  annotate(geom = "text", x = -13750000, y = 4250000, label = 'Pacific\n  Ocean', size = 7.5) +
  theme(panel.background = element_rect(fill = 'white'),
        legend.key.spacing.y = unit(0.2, "cm"),
        legend.spacing.y = unit(0.2, 'cm'), 
        legend.spacing.x = unit(0.1, 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 35),
        legend.position = c(0.7,0.775), 
        legend.box.background = element_rect(color = 'black', linewidth = 1.5),
        axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18)) 


addSmallLegend <- function(myPlot, pointSize = 0.75, textSize = 8, spaceLegend = 0.2, 
                           titleadd = FALSE) {
  if(titleadd){
    plotadj <- myPlot +
      guides(shape = guide_legend(override.aes = list(size = pointSize)),
             color = guide_legend(override.aes = list(size = pointSize))) +
      theme(legend.text  = element_text(size = textSize),
            legend.key.size = unit(spaceLegend, "lines"))
  }else{
    plotadj <- myPlot +
      guides(shape = guide_legend(override.aes = list(size = pointSize)),
             color = guide_legend(override.aes = list(size = pointSize))) +
      theme(legend.title = element_blank(), 
            legend.text  = element_text(size = textSize),
            legend.key.size = unit(spaceLegend, "lines"))
  }
}



mapplot <- plot_grid(
  addSmallLegend(eco_plot, spaceLegend = 0.35, textSize = 18),
  addSmallLegend(nFire_plot, spaceLegend = 1, textSize = 18),
  addSmallLegend(c330iso_plot, spaceLegend = 1, textSize = 18, titleadd = T),
  addSmallLegend(h330iso_plot, spaceLegend = 1, textSize = 18, titleadd = T),
  ncol = 2, nrow = 2
)



ggsave(filename = "Bhootetal_EVT_code_data_figures/plots/Main/Map_Panel_Plots.png",
       plot = mapplot, 
       dpi = 300, width = 15, height = 15
)