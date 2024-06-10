# Packages
pacman::p_load(dplyr,
               tidyr,
               readxl,
               ggplot2,
               readr,
               ggpubr,
               here,
               treemap,
               sf,
               rnaturalearth,
               networkD3,
               mapproj,
               ggrepel,
               ggdist,
               wesanderson,
               RColorBrewer,
               jsonlite,
               lattice,
               ggVennDiagram,
               venneuler,
               forcats)

# Data
info_publi <- read_excel(here("data","extraction_grid.xlsx"))
health_outcome <- read_excel(here("data","extraction_grid_health_outcome.xlsx"))
quality_eval <- read_excel(here("data","quality_eval.xlsx"))
quality_fr <- read_excel(here("data","quality_fr.xlsx"))


###### Geo scale #####

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create a data frame with country names and the corresponding numeric variable

data_multi <- data.frame(country = c("China", "United Kingdom", "India", "United States",
                                     "France", "Poland", "Korea","Japan",  
                                     "Brazil", "Germany", "Indonesia", "Nigeria", "South Africa",
                                     "Argentina", "Australia", "Austria", "Belgium", "Bulgaria", 
                                     "Canada", "Cyprus", "Czech Rep.", "Denmark", "Spain", "Estonia", 
                                     "Finland", "Greece", "Croatia", "Hungary", "Ireland",
                                     "Italy", "Lithuania", "Luxembourg", "Latvia", "Mexico", 
                                     "Malta", "Netherlands", "Portugal", "Romania", "Russia", "Saudi Arabia", 
                                     "Slovakia", "Slovenia", "Sweden", "Turkey",
                                     "Bangladesh", "Cambodia", "Iran", "Laos", "Malaysia", "Mongolia", 
                                     "Nepal", "Pakistan", "Philippines", "Thaïland", "Vietnam"
),
value = c(15, 6, 6, 3, 
          4,5, 3,2,
          2,4,3,1,1,
          1,1,3,3,3,
          1,3,3,3,3,3,
          3,3,2,3,3,
          3,2,2,3,1,
          2,3,3,2,1,1,
          2,2,3,1,
          1,1,1,1,1,1,
          1,1,1,1,1))



# Color scale
data_multi$color_group <- cut(data_multi$value,
                              breaks = c(1,2,4,6,15),
                              labels = c("1-2", "3-4","5-6", "15"),
                              include.lowest = TRUE)

# Take out Antartica and merge the data with the world map data
world <- world[world$name != "Antarctica", ]

world_data_multi <- merge(world, data_multi, by.x = "name", by.y = "country")


# Take out non continental island for coloring France, Spain
world_data_multi$geometry[world_data_multi$name == "France"] <- st_union(st_cast(world_data_multi$geometry[world_data_multi$name == "France"], "POLYGON")[c(8,10)])
world_data_multi$geometry[world_data_multi$name == "Spain"] <- st_cast(world_data_multi$geometry[world_data_multi$name == "Spain"], "POLYGON")[12]
world_data_multi$geometry[world_data_multi$name == "United States"] <- st_union(st_cast(world$geometry[world$name == "United States"], "POLYGON")[c(20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,127)])

# Robinson projection
robinson_proj <- st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
world_data_multi <- st_transform(world_data_multi, crs = robinson_proj)

# Define latitude and longitude for specific regions
points_data <- data.frame(
  # Shaanxi,   Beijing  , Shandong , California (LA), Anhui,     Sichuan, California (SF), California (CC), London (&Milton keynes), Barcelona, Freidburg, Malmö, 
  lat = c(34.274342, 39.916668, 36.066898, 34.052235      , 31.848398, 31.456781,37.773972,     41.755749,        51.5085300,             41.3850639, 47.997791, 55.60587, 42.698334) ,  #  latitudes
  lon = c(108.889191, 116.383331, 120.382698,-118.243683,  117.272362, 102.843018,-122.431297,   -124.202591,     -0.1257400,             2.1734035, 7.842609, 13.00073, 23.319941)  #  longitudes
)


# Transform the points data into a spatial object
points_sf <- st_as_sf(points_data, coords = c("lon", "lat"), crs = 4326)



# Map with regions
Map1 = ggplot() +
  geom_sf(data = world, fill = "grey90", color = "transparent",
          show.legend = FALSE, size = 5) +
  geom_sf(data = world_data_multi, aes(fill = color_group), color ="transparent") +
  geom_sf(data = points_sf, aes(color = "Sub-national investigation        "), size = 2, shape = 16) +
  scale_fill_manual(values = c("orange","firebrick2","firebrick4","mediumpurple4"),
                    breaks = c("1-2", "3-4","5-6", "15")) +
  scale_color_manual(values = c("black"), guide = guide_legend(title = NULL)) +
  labs(title = "",fill = "Number of investigation:") +
  theme_pubr() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12))+
  coord_sf(crs = robinson_proj)

Map1


Map1fr = ggplot() +
  geom_sf(data = world, fill = "grey90", color = "transparent",
          show.legend = FALSE, size = 5) +
  geom_sf(data = world_data_multi, aes(fill = color_group), color ="transparent") +
  geom_sf(data = points_sf, aes(color = "Études régionales"), size = 2, shape = 16) +
  scale_fill_manual(values = c("orange","firebrick2","firebrick4","mediumpurple4"),
                    breaks = c("1-2", "3-4","5-6", "15")) +
  scale_color_manual(values = c("black"), guide = guide_legend(title = NULL)) +
  labs(title = "",fill = "Nombre d'étude:") +
  theme_pubr() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 15))+
  coord_sf(crs = robinson_proj)

Map1fr





# 1st author map
data_multi <- data.frame(country = c("China", "United Kingdom", "United States","France", "Poland", "Korea", "Germany", "Spain", "Austria", "Italy"
),
value = c(17,5,9,1,2,1,4,4,4,1))

# Color scale
data_multi$color_group <- cut(data_multi$value,
                              breaks = c(1,2,5,9,17),
                              labels = c("1-2", "4-5", "9","17"),
                              include.lowest = TRUE)

# Take out Antartica and
# Merge the data with the world map data
world <- world[world$name != "Antarctica", ]

world_data_multi <- merge(world, data_multi, by.x = "name", by.y = "country")

# Take out non continental island for coloring France, Spain
world_data_multi$geometry[world_data_multi$name == "France"] <- st_union(st_cast(world_data_multi$geometry[world_data_multi$name == "France"], "POLYGON")[c(8,10)])
world_data_multi$geometry[world_data_multi$name == "Spain"] <- st_cast(world_data_multi$geometry[world_data_multi$name == "Spain"], "POLYGON")[12]
world_data_multi$geometry[world_data_multi$name == "United States"] <- st_union(st_cast(world$geometry[world$name == "United States"], "POLYGON")[c(20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,127)])


# Robinson projection
robinson_proj <- st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
world_data_multi <- st_transform(world_data_multi, crs = robinson_proj)

# Map with regions
Map2= ggplot() +
  geom_sf(data = world, fill = "grey90", color = "transparent",
          show.legend = FALSE, size = 5) +
  geom_sf(data = world_data_multi, aes(fill = color_group), color ="transparent") +
  scale_fill_manual(values = c("cadetblue1", "cornflowerblue", "blue", "darkblue"),
                    breaks = c("1-2", "4-5", "9", "17"),
                    guide = guide_legend(title = "Number of investigations")) +
  scale_color_manual(values = c("black"), guide = guide_legend(title = NULL)) +
  labs(title = "Spatial distribution of 1st authors' institution",
       fill = "") +
  theme_pubr() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.5, -0.14),
        legend.direction = "horizontal",)+
  guides(fill = guide_legend(title = NULL)) +
  coord_sf(crs = robinson_proj)


Map2


Map2fr= ggplot() +
  geom_sf(data = world, fill = "grey90", color = "transparent",
          show.legend = FALSE, size = 5) +
  geom_sf(data = world_data_multi, aes(fill = color_group), color ="transparent") +
  scale_fill_manual(values = c("cadetblue1", "cornflowerblue", "blue", "darkblue"),
                    breaks = c("1-2", "4-5", "9", "17"),
                    guide = guide_legend(title = "Number of investigations")) +
  scale_color_manual(values = c("black"), guide = guide_legend(title = NULL)) +
  labs(title = "Pays de l'institution du 1er auteur",
       fill = "Nombre d'étude:") +
  theme_pubr() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.5, -0.14),
        legend.direction = "horizontal",)+
  guides(fill = guide_legend(title = NULL)) +
  coord_sf(crs = robinson_proj)


Map2fr



# Year of publi



##### Time-scale #####
# Time-scale
info_publi$end_point <- as.numeric(info_publi$end_point)
info_publi$bau <- as.numeric(info_publi$bau)
info_publi$res_point5 <- as.numeric(info_publi$res_point5)


timescale1 = info_publi %>% 
  ggplot(aes(x = as.numeric(rank_plot)))+
  geom_linerange(aes(ymin=begin_point,ymax=end_point),linetype=1,color="#ADEFD1FF", size = 2, linewidth = 2)+
  geom_linerange(aes(ymin=bau,ymax=bau_proj), color= "#00203FFF",linetype= "dotted", linewidth = 0.5)+
  geom_point(aes(y=baseline, col = "baseline"),size=1,shape = 15)+
  geom_point(aes(y=res_point, col = "res_point"),size=1.5,shape = 15)+
  geom_point(aes(y=res_point2),col = "#ADEFD1FF", size=1.5,shape = 15)+
  geom_point(aes(y=res_point3),col = "#ADEFD1FF", size=1.5,shape = 15)+
  geom_point(aes(y=res_point4),col = "#ADEFD1FF", size=1.5,shape = 15)+
  geom_point(aes(y=res_point5),col = "#ADEFD1FF", size=1.5,shape = 15)+
  geom_text(aes(y= 1995,label = author_date), size = 3)+
  geom_segment(x=48, y=1985, xend=36.5, yend=1985 )+
  geom_text(x=42.5, y= 1981, label="World")+
  geom_segment(x=36, y=1987, xend=29.5, yend=1987 )+
  geom_text(x=33, y= 1981, label="Multi-country")+
  geom_segment(x=29, y=1985, xend=8.5, yend=1985 )+
  geom_text(x=20.5, y= 1980, label="National")+
  geom_segment(x=8, y=1987, xend=0.5, yend=1987 )+
  geom_text(x=4.5, y= 1980, label="Sub-national")+
  theme_bw()+
  coord_flip()+
  scale_y_continuous(breaks = c(2000, 2010, 2020,2030,2040,2050,2060,2070,2100), limits = c(1980,2100))+
  ylab("Year")+
  xlab("")+
  theme(legend.position = "top",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.y = element_blank(),
        legend.title = element_blank())+
  scale_color_manual(values = c("#00203FFF", "#ADEFD1FF"), 
                     labels = c("· · ·   Baseline scenario for exposure", "Health impact assessment"))

timescale1


info_publi$time_scale_max <- as.numeric(info_publi$time_scale_max)


# Couleur = Type de scenario 

timescale2 = info_publi %>%
  ggplot(aes(x = as.numeric(rank_plot))) +
  geom_linerange(aes(ymin = time_scale_min, ymax = time_scale_max, col=mitigation_type), linewidth= 1.5) +
  geom_text(aes(x = as.numeric(rank_plot), y = 1995, label = author_date), size = 3) +
  geom_segment(x=48, y=1985, xend=36.5, yend=1985 )+
  geom_text(x=42.5, y= 1981, label="World")+
  geom_segment(x=36, y=1987, xend=29.5, yend=1987 )+
  geom_text(x=33, y= 1981, label="Multi-country")+
  geom_segment(x=29, y=1985, xend=8.5, yend=1985 )+
  geom_text(x=20.5, y= 1980, label="National")+
  geom_segment(x=8, y=1987, xend=0.5, yend=1987 )+
  geom_text(x=4.5, y= 1980, label="Sub-national")+
  geom_segment(aes(x=44, y=2020, xend=44, yend=2067), size = 1.5, col = "grey40", arrow = arrow(length=unit(0.5, 'cm')))+
  geom_text(x=44, y= 2070, label="2100", size = 4, angle = 45)+
  geom_segment(aes(x=30, y=2021, xend=30, yend=2067), size = 1.5, col = "#4DAF4A"  ,arrow = arrow(length=unit(0.5, 'cm')))+
  geom_text(x=30, y= 2070, label="2100", size = 4, angle = 45)+
  geom_segment(aes(x=12, y=2011, xend=12, yend=2067), size = 1.5, col = "#377EB8"  , arrow = arrow(length=unit(0.5, 'cm')))+
  geom_text(x=12, y= 2070, label="2154", size = 4, angle = 45)+
  theme_bw() +
  coord_flip() +
  scale_y_continuous(breaks = c(2010, 2020, 2030, 2040, 2050, 2060, 2070, 2100), limits = c(1980, 2070)) +
  ylab("Year") +
  xlab("") +
  theme(legend.position = "top",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.y = element_blank(),
        legend.title = element_blank()) +
  scale_color_manual(values = c("#377EB8", "#4DAF4A", "yellow","#E41A1C", "#984EA3","orange","pink","grey40","black" ))


timescale2

# Sans couleur 
timescale3 = info_publi %>%
  ggplot(aes(x = as.numeric(rank_plot))) +
  geom_linerange(aes(ymin = time_scale_min, ymax = time_scale_max), linewidth= 0.5) +
  geom_text(aes(x = as.numeric(rank_plot), y = (time_scale_min -7) , label = author_date), size = 3.5, fontface = "bold") +
  geom_segment(x=48, y=1986, xend=36.5, yend=1986)+
  geom_text(x=42.5, y= 1981, label="World", size = 5)+
  geom_segment(x=36, y=1991, xend=29.5, yend=1991 )+
  geom_text(x=33, y= 1982.5, label="Multi-country", size = 5)+
  geom_segment(x=29, y=1988, xend=8.5, yend=1988 )+
  geom_text(x=20.5, y= 1980, label="National", size = 5)+
  geom_segment(x=8, y=1991, xend=0.5, yend=1991 )+
  geom_text(x=4.5, y= 1982.5, label="Sub-national", size = 5)+
  geom_segment(aes(x=44, y=2020, xend=44, yend=2067), arrow = arrow(length=unit(0.5, 'cm')))+
  geom_text(x=44, y= 2070, label="2100", size = 4, angle = 45)+
  geom_segment(aes(x=30, y=2021, xend=30, yend=2067), arrow = arrow(length=unit(0.5, 'cm')))+
  geom_text(x=30, y= 2070, label="2100", size = 4, angle = 45)+
  geom_segment(aes(x=12, y=2011, xend=12, yend=2067), arrow = arrow(length=unit(0.5, 'cm')))+
  geom_text(x=12, y= 2070, label="2154", size = 4, angle = 45)+
  theme_bw() +
  coord_flip() +
  scale_y_continuous(breaks = c(2010, 2020, 2030, 2040, 2050, 2060, 2070, 2100), limits = c(1980, 2070)) +
  ylab("Year") +
  xlab("") +
  theme(legend.position = "top",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.ticks.y = element_blank(),
        legend.title = element_blank())


timescale3



# Sankey
links <- data.frame(
  source=c("All-encompassing", "All-encompassing",
           "Energy", "Energy",
           "Transport",  "Transport", 
           "Agriculture","Agriculture",
           "Housing", "Housing",
           "Industry", "Industry",
           "Other",
           
           "Air pollution","Air pollution","Air pollution","Air pollution","Air pollution",
           "Air pollution",
           "Diet","Diet","Diet",
           "Physical activity","Physical activity","Physical activity","Physical activity",
           "Indoor pollution","Indoor pollution","Indoor pollution","Indoor pollution"
  ), 
  
  target=c("Air pollution","Indoor pollution",
           "Air pollution","Indoor pollution",
           "Air pollution", "Physical activity",
           "Air pollution", "Diet",
           "Air pollution", "Indoor pollution",
           "Air pollution","Indoor pollution",
           "Air pollution",
           
           "Deaths", "YLL", "Economic","Life expectancy","DALYs",
           "Morbidity",
           "Deaths", "YLL", "DALYs",
           "Deaths", "YLL","Economic","Life expectancy",
           "YLL","Morbidity","Deaths", "Economic"
  ), 
  
  value=c(18,1,
          26,3,
          11,4,
          7,3,
          10,3,
          11,1,
          9,
          
          34,4,24,3,1,
          14,
          1,1,1,
          2,2,1,1,
          1,1,2,1
  )
)

nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

links$group <- as.factor(c("type_a","type_a",
                           "type_b","type_b",
                           "type_c","type_c",
                           "type_d","type_d",
                           "type_e","type_e",
                           "type_f","type_f",
                           "type_k",
                           
                           "type_h","type_h","type_h","type_h","type_h",
                           "type_h",
                           "type_i","type_i","type_i",
                           "type_j","type_j","type_j","type_j",
                           "type_g","type_g","type_g","type_g"
))
nodes$group <- as.factor(c("my_unique_group"))

my_color <- 'd3.scaleOrdinal() .domain(["type_a", "type_b","type_c","type_d","type_e","type_f","type_g","type_h","type_i","type_j","type_k","type_l", "my_unique_group"])
.range(["#1f77b4", "#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2","#aec7e8","#bcbd22","#17becf","#7f7f7f","#ffbb78", "black"])'


sankeyplot <- sankeyNetwork(Links = links, Nodes = nodes,
                            Source = "IDsource", Target = "IDtarget",
                            Value = "value", NodeID = "name", 
                            colourScale=my_color, LinkGroup="group", NodeGroup="group",
                            fontSize = 17, nodeWidth = 2 )




htmlwidgets::onRender(
  sankeyplot,
  '
  function(el) {
    d3.select(el).selectAll(".node text").attr("font-weight", "bold");
  }
  '
)




# Sankey2
links <- data.frame(
  source=c("Energy decarbonation","Energy decarbonation","Energy decarbonation",
           "Energy decarbonation","Energy decarbonation","Energy decarbonation",
           "Health","Health","Health","Health","Health","Health","Health",
           "Sufficiency","Sufficiency","Sufficiency","Sufficiency","Sufficiency",
           "Financial","Financial","Financial","Financial","Financial","Financial","Financial",
           "Not detailed","Not detailed","Not detailed","Not detailed","Not detailed","Not detailed","Not detailed",
           
          "All-encompassing", "All-encompassing",
           "Energy", "Energy",
           "Transport",  "Transport", 
           "Food system","Food system",
           "Housing", "Housing",
           "Industry", "Industry",
           "Other",
           
           "Air pollution","Air pollution","Air pollution","Air pollution","Air pollution",
           "Air pollution",
           "Diet","Diet","Diet",
           "Physical activity","Physical activity","Physical activity","Physical activity",
           "Indoor pollution","Indoor pollution","Indoor pollution","Indoor pollution"
  ), 
  
  target=c("Energy","Transport", "Food system",
           "All-encompassing","Housing","Industry",
           "Energy","Food system","Industry","Transport", "Other","Housing", "All-encompassing", 
           "Energy", "All-encompassing","Housing","Transport", "Food system",
           "All-encompassing", "Transport", "Energy", "Housing","Industry","Food system","Other",
           "All-encompassing","Energy","Industry","Transport","Housing","Food system","Other",
    
    
          "Air pollution","Indoor pollution",
           "Air pollution","Indoor pollution",
           "Air pollution", "Physical activity",
           "Air pollution", "Diet",
           "Air pollution", "Indoor pollution",
           "Air pollution","Indoor pollution",
           "Air pollution",
           
           "Deaths", "YLL", "Economic","Life expectancy","DALYs",
           "Morbidity",
           "Deaths", "YLL", "DALYs",
           "Deaths", "YLL","Economic","Life expectancy",
           "YLL","Morbidity","Deaths", "Economic"
  ), 
  
  value=c(13,4,1,
          3,3,1,
          6,3,2,4,2,2,2,
          3,1,1,2,1,
          1,2,1,1,1,1,1,
          9,10,8,8,5,5,4,
          
    
          18,1,
          26,3,
          11,4,
          7,3,
          10,3,
          11,1,
          9,
          
          34,4,24,3,1,
          14,
          1,1,1,
          2,2,1,1,
          1,1,2,1
  )
)

nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

links$group <- as.factor(c("type_m","type_m","type_m",
                           "type_m","type_m","type_m",
                           "type_o","type_o","type_o","type_o","type_o","type_o","type_o",
                           "type_n","type_n","type_n","type_n","type_n",
                           "type_l","type_l","type_l","type_l","type_l","type_l","type_l",
                           "type_k","type_k","type_k","type_k","type_k","type_k","type_k",
                           
              
                            "type_a","type_a",
                           "type_b","type_b",
                           "type_c","type_c",
                           "type_d","type_d",
                           "type_e","type_e",
                           "type_f","type_f",
                           "type_k",
                           
                           "type_h","type_h","type_h","type_h","type_h",
                           "type_h",
                           "type_i","type_i","type_i",
                           "type_j","type_j","type_j","type_j",
                           "type_g","type_g","type_g","type_g"
))
nodes$group <- as.factor(c("my_unique_group"))

my_color <- 'd3.scaleOrdinal() .domain(["type_a", "type_b","type_c","type_d","type_e","type_f","type_g","type_h","type_i","type_j","type_k","type_l","type_m","type_n","type_o", "my_unique_group"])
.range(["#1f77b4", "#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2","#aec7e8","#bcbd22","#17becf","#7f7f7f","#ffbb78","#6daed5","#98df8a","#ff9896", "black"])'


sankeyplot2 <- sankeyNetwork(Links = links, Nodes = nodes,
                            Source = "IDsource", Target = "IDtarget",
                            Value = "value", NodeID = "name", 
                            colourScale=my_color, LinkGroup="group", NodeGroup="group",
                            fontSize = 17, nodeWidth = 2 )



htmlwidgets::onRender(
  sankeyplot2,
  '
  function(el) {
    d3.select(el).selectAll(".node text").attr("font-weight", "bold");

    // Add white background for text labels
    d3.select(el).selectAll(".node text")
      .each(function() {
        var bbox = this.getBBox();
        d3.select(this.parentNode)
          .insert("rect", "text")
          .attr("x", bbox.x - 3)
          .attr("y", bbox.y - 3)
          .attr("width", bbox.width + 6)
          .attr("height", bbox.height + 6)
          .style("fill", "white")
          .style("stroke", "black");
      });
  }
  '
)



# Sankey2fr
links <- data.frame(
  source=c("Décarbonation de l'énergie","Décarbonation de l'énergie","Décarbonation de l'énergie",
           "Décarbonation de l'énergie","Décarbonation de l'énergie","Décarbonation de l'énergie",
           "Santé intégré","Santé intégré","Santé intégré","Santé intégré","Santé intégré","Santé intégré","Santé intégré",
           "Sobriété","Sobriété","Sobriété","Sobriété","Sobriété",
           "Incitation financière","Incitation financière","Incitation financière","Incitation financière","Incitation financière","Incitation financière","Incitation financière",
           "Pas détaillé","Pas détaillé","Pas détaillé","Pas détaillé","Pas détaillé","Pas détaillé","Pas détaillé",
           
           "Tous confondus", "Tous confondus",
           "Énergie", "Énergie",
           "Transport",  "Transport", 
           "Système alimentaire","Système alimentaire",
           "Logement", "Logement",
           "Industrie", "Industrie",
           "Autres",
           
           "Pollution de l'air","Pollution de l'air","Pollution de l'air","Pollution de l'air","Pollution de l'air",
           "Pollution de l'air",
           "Alimentation","Alimentation","Alimentation",
           "Activité physique","Activité physique","Activité physique","Activité physique",
           "Pollution intérieur","Pollution intérieur","Pollution intérieur","Pollution intérieur"
  ), 
  
  target=c("Énergie","Transport", "Système alimentaire",
           "Tous confondus","Logement","Industrie",
           "Énergie","Système alimentaire","Industrie","Transport", "Autres","Logement", "Tous confondus", 
           "Énergie", "Tous confondus","Logement","Transport", "Système alimentaire",
           "Tous confondus", "Transport", "Énergie", "Logement","Industrie","Système alimentaire","Autres",
           "Tous confondus","Énergie","Industrie","Transport","Logement","Système alimentaire","Autres",
           
           
           "Pollution de l'air","Pollution intérieur",
           "Pollution de l'air","Pollution intérieur",
           "Pollution de l'air", "Activité physique",
           "Pollution de l'air", "Alimentation",
           "Pollution de l'air", "Pollution intérieur",
           "Pollution de l'air","Pollution intérieur",
           "Pollution de l'air",
           
           "Mortalité", "YLL", "Economique","Espérance de vie","DALYs",
           "Morbidité",
           "Mortalité", "YLL", "DALYs",
           "Mortalité", "YLL","Economique","Espérance de vie",
           "YLL","Morbidité","Mortalité", "Economique"
  ), 
  
  value=c(13,4,1,
          3,3,1,
          6,3,2,4,2,2,2,
          3,1,1,2,1,
          1,2,1,1,1,1,1,
          9,10,8,8,5,5,4,
          
          
          18,1,
          26,3,
          11,4,
          7,3,
          10,3,
          11,1,
          9,
          
          34,4,24,3,1,
          14,
          1,1,1,
          2,2,1,1,
          1,1,2,1
  )
)

nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

links$group <- as.factor(c("type_m","type_m","type_m",
                           "type_m","type_m","type_m",
                           "type_o","type_o","type_o","type_o","type_o","type_o","type_o",
                           "type_n","type_n","type_n","type_n","type_n",
                           "type_l","type_l","type_l","type_l","type_l","type_l","type_l",
                           "type_k","type_k","type_k","type_k","type_k","type_k","type_k",
                           
                           
                           "type_a","type_a",
                           "type_b","type_b",
                           "type_c","type_c",
                           "type_d","type_d",
                           "type_e","type_e",
                           "type_f","type_f",
                           "type_k",
                           
                           "type_h","type_h","type_h","type_h","type_h",
                           "type_h",
                           "type_i","type_i","type_i",
                           "type_j","type_j","type_j","type_j",
                           "type_g","type_g","type_g","type_g"
))
nodes$group <- as.factor(c("my_unique_group"))

my_color <- 'd3.scaleOrdinal() .domain(["type_a", "type_b","type_c","type_d","type_e","type_f","type_g","type_h","type_i","type_j","type_k","type_l","type_m","type_n","type_o", "my_unique_group"])
.range(["#6daed5", "#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2","#aec7e8","#bcbd22","#17becf","#7f7f7f","#ffbb78","#1f77b4","#98df8a","#ff9896", "black"])'


sankeyplot2 <- sankeyNetwork(Links = links, Nodes = nodes,
                             Source = "IDsource", Target = "IDtarget",
                             Value = "value", NodeID = "name", 
                             colourScale=my_color, LinkGroup="group", NodeGroup="group",
                             fontSize = 17, nodeWidth = 2 )



htmlwidgets::onRender(
  sankeyplot2,
  '
  function(el) {
    d3.select(el).selectAll(".node text").attr("font-weight", "bold");

    // Add white background for text labels
    d3.select(el).selectAll(".node text")
      .each(function() {
        var bbox = this.getBBox();
        d3.select(this.parentNode)
          .insert("rect", "text")
          .attr("x", bbox.x - 3)
          .attr("y", bbox.y - 3)
          .attr("width", bbox.width + 6)
          .attr("height", bbox.height + 6)
          .style("fill", "white")
          .style("stroke", "black");
      });
  }
  '
)





# Sankey3
links <- data.frame(
  source=c("Energy decarbonation","Energy decarbonation","Energy decarbonation",
           "Energy decarbonation","Energy decarbonation","Energy decarbonation","Energy decarbonation",
           "Health","Health","Health","Health","Health","Health","Health",
           "Sufficiency","Sufficiency","Sufficiency","Sufficiency","Sufficiency",
           "Financial","Financial","Financial","Financial","Financial","Financial","Financial",
           "Not detailed","Not detailed","Not detailed","Not detailed","Not detailed","Not detailed","Not detailed",
           
           "All-encompassing", "All-encompassing",
           "Energy", "Energy",
           "Transport",  "Transport", 
           "Food system","Food system",
           "Housing", "Housing",
           "Industry", "Industry",
           "Other",
           
           "Air pollution","Air pollution","Air pollution","Air pollution","Air pollution",
           "Air pollution",
           "Diet","Diet","Diet",
           "Physical activity","Physical activity","Physical activity","Physical activity",
           "Indoor pollution","Indoor pollution","Indoor pollution","Indoor pollution"
  ), 
  
  target=c("Energy","Transport", "Food system",
           "Housing","Industry","All-encompassing","Other",
           "Energy","Food system","Industry","Transport", "Other","Housing", "All-encompassing", 
           "Energy", "All-encompassing","Housing","Transport", "Food system",
           "All-encompassing", "Transport", "Energy", "Housing","Industry","Food system","Other",
           "All-encompassing","Energy","Industry","Transport","Housing","Food system","Other",
           
           
           "Air pollution","Indoor pollution",
           "Air pollution","Indoor pollution",
           "Air pollution", "Physical activity",
           "Air pollution", "Diet",
           "Air pollution", "Indoor pollution",
           "Air pollution","Indoor pollution",
           "Air pollution",
           
           "Deaths", "YLL", "Economic","Life expectancy","DALYs",
           "Morbidity",
           "Deaths", "YLL", "DALYs",
           "Deaths", "YLL","Economic","Life expectancy",
           "YLL","Morbidity","Deaths", "Economic"
  ), 
  
  value=c(17,8,3,
          7,4,2,3,
          7,3,4,6,3,4,1,
          3,1,1,2,1,
          1,2,1,1,1,1,1,
          7,11,9,9,5,6,4,
          
          
          12,1,
          31,3,
          17,4,
          9,3,
          15,3,
          16,1,
          11,
          
          34,4,24,3,1,
          14,
          1,1,1,
          2,2,1,1,
          1,1,2,1
  )
)

nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

links$group <- as.factor(c("type_m","type_m","type_m",
                           "type_m","type_m","type_m","type_m",
                           "type_o","type_o","type_o","type_o","type_o","type_o","type_o",
                           "type_n","type_n","type_n","type_n","type_n",
                           "type_l","type_l","type_l","type_l","type_l","type_l","type_l",
                           "type_k","type_k","type_k","type_k","type_k","type_k","type_k",
                           
                           
                           "type_a","type_a",
                           "type_b","type_b",
                           "type_c","type_c",
                           "type_d","type_d",
                           "type_e","type_e",
                           "type_f","type_f",
                           "type_k",
                           
                           "type_h","type_h","type_h","type_h","type_h",
                           "type_h",
                           "type_i","type_i","type_i",
                           "type_j","type_j","type_j","type_j",
                           "type_g","type_g","type_g","type_g"
))
nodes$group <- as.factor(c("my_unique_group"))

my_color <- 'd3.scaleOrdinal() .domain(["type_a", "type_b","type_c","type_d","type_e","type_f","type_g","type_h","type_i","type_j","type_k","type_l","type_m","type_n","type_o", "my_unique_group"])
.range(["#1f77b4", "#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2","#aec7e8","#bcbd22","#17becf","#7f7f7f","#ffbb78","#6daed5","#98df8a","#ff9896", "black"])'


sankeyplot2 <- sankeyNetwork(Links = links, Nodes = nodes,
                             Source = "IDsource", Target = "IDtarget",
                             Value = "value", NodeID = "name", 
                             colourScale=my_color, LinkGroup="group", NodeGroup="group",
                             fontSize = 20, nodeWidth = 2 )



htmlwidgets::onRender(
  sankeyplot2,
  '
  function(el) {
    d3.select(el).selectAll(".node text").attr("font-weight", "bold");

    // Add white background for text labels
    d3.select(el).selectAll(".node text")
      .each(function() {
        var bbox = this.getBBox();
        d3.select(this.parentNode)
          .insert("rect", "text")
          .attr("x", bbox.x - 3)
          .attr("y", bbox.y - 3)
          .attr("width", bbox.width + 6)
          .attr("height", bbox.height + 6)
          .style("fill", "white")
          .style("stroke", "black");
      });
  }
  '
)





# Pie chart

info_publi %>%
  count(mitigation_type, name = "num") 

pie(c(20,3,10,15,5), labels = c("Energy decarbonation","Financial instrument",
                                "Health in climate policies","Not detailed","Sufficiency"))


info_publi %>%
  count(baseline_scenario, name = "num") 

pie(c(5,31,9),labels= c("Decreasing GHG emissons","Increasing GHG emissons","Reference year"))    





# Quality assessment

quality_eval_data <- quality_eval %>%
  gather(key = "Criteria", value = "Value", -Article) %>%
  count(Criteria, Value) %>%
  spread(key = "Value", value = "n", fill = 0)

quality_eval_long <- quality_eval_data %>%
  gather(key = "Variable", value = "Count", -Criteria)


criteria_order <- rev(c("Specification of target population","Demographic and exposure allocation",
                    "Describe exposure-response functions","Appropriate health metrics",
                    "Defined timeframes","Describe mitigation policies",
                    "Correspondance with agreed-upon scenarios","Equity impact",
                    "Adverse consequences of mitigation actions",
                    "Limitations and source of uncertainty","Sensitivity analysis","Data sources",
                    "Publicly shared data and code"))
quality_eval_long$Criteria <- factor(quality_eval_long$Criteria, levels = criteria_order)


quality = ggplot(quality_eval_long, aes(x = Criteria, y = Count, fill = factor(Variable, levels = c("Yes", "Yes partially", "Unclear", "No")))) +
  geom_bar(stat = "identity") +
  labs(title = "",
       x = "",
       y = "Number of article",
       fill = "Response") +
  scale_fill_manual(values = c("#1A9850", "#B5E5B5", "#FF7F00", "#AA3939")) + 
  theme_pubr() +
  coord_flip()+
  guides(fill = guide_legend(title = "", keywidth = 1, keyheight = 1, reverse = TRUE))+
  theme(legend.text = element_text(size = 15),
        axis.text.y = element_text(size = 20))


quality



# Quality FR
quality_eval_data <- quality_fr %>%
  gather(key = "Criteria", value = "Value", -Article) %>%
  count(Criteria, Value) %>%
  spread(key = "Value", value = "n", fill = 0)

quality_eval_long <- quality_eval_data %>%
  gather(key = "Variable", value = "Count", -Criteria)


criteria_order <- rev(c("Population cible identifiée","Affectation démographique et de l'exposition",
                    "Relations exposition-réponse décrites", "Métriques sanitaire adaptées",
                    "Horizons temporels définis", "Projections détaillées",
                    "Correspondance avec un scénario convenu", "Équité des impacts",
                    "Conséquences négatives des mesures d'atténuation",
                    "Limites et sources d'incertitudes discutés","Analyses de sensibilités", "Source des données",
                    "Données et codes partagés publiquement"
))
quality_eval_long$Criteria <- factor(quality_eval_long$Criteria, levels = criteria_order)


quality = ggplot(quality_eval_long, aes(x = Criteria, y = Count, fill = factor(Variable, levels = c("Oui", "Oui partiellement", "Pas clair", "Non")))) +
  geom_bar(stat = "identity") +
  labs(title = "",
       x = "",
       y = "Number of article",
       fill = "Response") +
  scale_fill_manual(values = c("#1A9850", "#B5E5B5", "#FF7F00", "#AA3939")) + 
  theme_pubr() +
  coord_flip()+
  guides(fill = guide_legend(title = "", keywidth = 1, keyheight = 1, reverse = TRUE))+
  theme(legend.text = element_text(size = 15),
        axis.text.y = element_text(size = 20))


quality




# Health outcome


p0 = health_outcome %>%
  filter(HIA_type != "Microsimulation") %>%
  ggplot(aes(x = HIA_type, y = mortality_proj, color = HIA_type, shape = HIA_type))+
  #geom_violin(show.legend = F)+
  geom_point(size = 2,position=position_jitter(h=NULL,w=0.2), show.legend = F)+
  theme_pubr()+
  xlab("")+
  ylab("Attributable mortality fraction")+
  scale_color_manual(values = wes_palette("BottleRocket2"))+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)


health_outcome$pathway_co_benefits2 <- factor(health_outcome$pathway_co_benefits2, 
                                              levels = c("Air pollution", "Physical activity", "Diet"))

p1 = health_outcome %>%
  ggplot()+
  #geom_violin(aes(x = pathway_co_benefits2, y = mortality_proj, fill =pathway_co_benefits2), show.legend = F)+
  geom_point(aes(x = pathway_co_benefits2, y = mortality_proj, color =pathway_co_benefits2, shape = pathway_co_benefits2), 
             size = 2,position=position_jitter(h=NULL,w=0.3), show.legend = F)+
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  scale_color_manual(values = wes_palette("Darjeeling1"))+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)


health_outcome$emission_sector_cat <- factor(health_outcome$emission_sector_cat, 
                                              levels = c("All-encompassing", "Multi", "Energy", "Food system",
                                                         "Housing","Transport"))

p12 = health_outcome %>%
  ggplot()+
  geom_point(aes(x = emission_sector_cat, y = mortality_proj, color =emission_sector_cat, shape = emission_sector_cat), 
             size = 2,position=position_jitter(h=NULL,w=0.3), show.legend = F)+
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  scale_color_manual(values = c(wes_palette("Darjeeling1"), "#D4A5A5"))+
  scale_fill_manual(values = c(wes_palette("Darjeeling1"), "#D4A5A5"))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)


health_outcome$scenario_cat <- factor(health_outcome$scenario_cat, 
                                      levels = c("energy decarbonation", "financial instrument", 
                                                 "health in climate policies","sufficiency", "not detailed"))

p13 = health_outcome %>%
  ggplot()+
  geom_point(aes(x = scenario_cat, y = mortality_proj, color =scenario_cat, shape = scenario_cat), 
             size = 2,position=position_jitter(h=NULL,w=0.3), show.legend = F)+
  scale_x_discrete(labels = c('Energy shift','Finance','Health','Sufficiency' ,'Not detailed')) +
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  scale_color_manual(values = c(wes_palette("Darjeeling1"), "#D4A5A5"))+
  scale_fill_manual(values = c(wes_palette("Darjeeling1"), "#D4A5A5"))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)


p00 = health_outcome %>%
  ggplot()+
  geom_boxplot(aes( y = mortality_proj), fill ="grey")+
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size = 10))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)


plot_mortality = ggarrange(p0,p1,p00, ncol =  , nrow = 1, align = "h", 
                           labels = c("HIA method", "Exposure", "Overall"), hjust = c(-0.8,-1,-1.5))
plot_mortality



plot_mortality1 = ggarrange(p0,p1,p12,p13, ncol =2  , nrow = 2, align = "h", 
                           labels = c("HIA method", "Exposure", "Emission sector","Typology of scenario"), 
                           hjust = c(-0.8,-1,-0.5,-0.5))

plot_mortality12 = ggarrange(plot_mortality1,p00, ncol = 2, nrow = 1,labels = c("", "Overall"), 
                            widths = c(1,0.5))
plot_mortality12



p0 = health_outcome %>%
  filter(HIA_type != "Microsimulation") %>%
  ggplot(aes(x = HIA_type, y = mortality_proj, fill = HIA_type, shape = HIA_type))+
  geom_violin(show.legend = F)+
  #geom_point(size = 2,position=position_jitter(h=NULL,w=0.2), show.legend = F)+
  theme_pubr()+
  xlab("")+
  ylab("Attributable mortality fraction")+
  scale_fill_manual(values = wes_palette("BottleRocket2"))+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)


health_outcome$pathway_co_benefits2 <- factor(health_outcome$pathway_co_benefits2, 
                                              levels = c("Air pollution", "Physical activity", "Diet"))

p1 = health_outcome %>%
  ggplot()+
  geom_violin(aes(x = pathway_co_benefits2, y = mortality_proj, fill =pathway_co_benefits2), show.legend = F)+
  #geom_point(aes(x = pathway_co_benefits2, y = mortality_proj, color =pathway_co_benefits2, shape = pathway_co_benefits2), 
  #           size = 2,position=position_jitter(h=NULL,w=0.3), show.legend = F)+
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  scale_color_manual(values = wes_palette("Darjeeling1"))+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)


p12 = health_outcome %>%
  ggplot()+
  geom_violin(aes(x = emission_sector_cat, y = mortality_proj, fill =emission_sector_cat), 
            show.legend = F)+
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  scale_color_manual(values = c(wes_palette("Darjeeling1"), "#D4A5A5"))+
  scale_fill_manual(values = c(wes_palette("Darjeeling1"), "#D4A5A5"))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)



p13 = health_outcome %>%
  ggplot()+
  geom_violin(aes(x = scenario_cat, y = mortality_proj, fill =scenario_cat), 
              show.legend = F)+
  scale_x_discrete(labels = c('Energy shift','Finance','Health','Sufficiency' ,'Not detailed')) +
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  scale_color_manual(values = c(wes_palette("Darjeeling1"), "#D4A5A5"))+
  scale_fill_manual(values = c(wes_palette("Darjeeling1"), "#D4A5A5"))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)


p14 = health_outcome %>%
  ggplot()+
  geom_boxplot(aes(x = emission_sector_cat, y = mortality_proj, fill =emission_sector_cat), 
              show.legend = F)+
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  scale_color_manual(values = c(wes_palette("Darjeeling1"), "#D4A5A5"))+
  scale_fill_manual(values = c(wes_palette("Darjeeling1"), "#D4A5A5"))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)



p15 = health_outcome %>%
  ggplot()+
  geom_boxplot(aes(x = scenario_cat, y = mortality_proj, fill =scenario_cat), 
              show.legend = F)+
  scale_x_discrete(labels = c('Energy shift','Finance','Health','Sufficiency' ,'Not detailed')) +
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  scale_color_manual(values = c(wes_palette("Darjeeling1"), "#D4A5A5"))+
  scale_fill_manual(values = c(wes_palette("Darjeeling1"), "#D4A5A5"))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)

plot_mortality2 = ggarrange(p0,p1,p00, ncol =  , nrow = 1, align = "h", 
                            labels = c("HIA method", "Exposure", "Overall"), hjust = c(-0.8,-1,-1.5))
plot_mortality2


plot_mortality21 = ggarrange(p0,p1,p12,p13, ncol =2  , nrow = 2, align = "h", 
                            labels = c("HIA method", "Exposure", "Emission sector","Typology of scenario"), 
                            hjust = c(-0.8,-1,-0.5,-0.5))

plot_mortality22 = ggarrange(plot_mortality21,p00, ncol = 2, nrow = 1,labels = c("", "Overall"), 
                             widths = c(1,0.5))
plot_mortality22


plot_mortality23 = ggarrange(p0,p1,p14,p15, ncol =2  , nrow = 2, align = "h", 
                             labels = c("HIA method", "Exposure", "Emission sector","Typology of scenario"), 
                             hjust = c(-0.8,-1,-0.5,-0.5))

plot_mortality24 = ggarrange(plot_mortality23,p00, ncol = 2, nrow = 1,labels = c("", "Overall"), 
                             widths = c(1,0.5))
plot_mortality24



p2 = health_outcome %>%
  ggplot(aes(x = pathway_co_benefits2, fill = include_mortality))+
  geom_bar( width = 0.5, show.legend = F)+
  theme_pubr()+
  xlab("")+
  ylab("Number of scenario")+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  scale_fill_manual(values = c("steelblue1","steelblue4"))+
  scale_y_continuous(limits =c(0,90))+
  coord_flip()+
  theme(legend.title = element_blank(),
    text = element_text(size = 10))

health_outcome$scenario_cat <- factor(health_outcome$scenario_cat, levels = rev(c("energy decarbonation", 
                    "financial instrument", "health in climate policies","sufficiency", "not detailed")))



p3 =  health_outcome %>%
  ggplot(aes(x = scenario_cat,fill = include_mortality)) +
  geom_bar( width = 0.5,  show.legend = F) +
  theme_pubr() +
  scale_x_discrete(labels = c('Not detailed','Sufficiency','Health', 'Finance','Energy shift')) +
  xlab("") +
  ylab("") +
  theme(
    legend.title = element_blank(),
    text = element_text(size = 10),
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) +
  scale_fill_manual(values = c("steelblue1","steelblue4"))+
  scale_y_continuous(limits =c(0,90))+
  coord_flip()








health_outcome$emission_sector_cat <- factor(health_outcome$emission_sector_cat,
                                      levels = rev(c('All-encompassing','Energy','Food system',
                                                     'Housing', 'Transport','Multi')))

p4 = health_outcome %>%
  ggplot(aes(x = emission_sector_cat, fill = include_mortality))+
  geom_bar( width = 0.5, show.legend = F)+
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(
    legend.title = element_blank(),
    text = element_text(size = 10),
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) +
  scale_fill_manual(values = c("steelblue1","steelblue4"))+
  scale_y_continuous(limits =c(0,90))+
  coord_flip()



p5 = info_publi %>%
  ggplot(aes(x = publi_yr, fill =include_mortality))+
  geom_bar( width = 0.5)+
  theme_pubr()+
  xlab("Year of publication")+
  ylab("Number of studies")+
  scale_fill_manual(values = c("steelblue1","steelblue4"))+
  theme(legend.title = element_text(),
        legend.position = "top",
        text = element_text(size = 10))+
  guides(fill=guide_legend(title="Adjustable mortality analysis:"))





plot_outcome = ggarrange(ggarrange(p3,p4,p2, ncol = 1 , nrow = 3, align = "v", 
                         labels = c("Typology of scenario","Emission sector","Exposition"),
                         hjust = c(-2,-2.6,-4)),
                         p5,ncol = 2, nrow = 1, common.legend = T)

plot_outcome



health_outcome %>%
  group_by(HIA_type) %>%
  filter(HIA_type != "Microsimulation")%>%
  ggplot()+
  geom_point(aes(x = scenario_cat, y = mortality_proj, shape = scenario_cat)
             , size = 2, position=position_jitter(h=NULL,w=0.2))+
  theme_pubr()+
  xlab("")+
  ylab("")+
  scale_shape_manual(values = c(16,15,17,18,3))+
  scale_x_discrete(labels = c('Energy','Finance','Health', 'Sufficiency','Not detailed'))+
  theme(legend.title = element_blank(),
        text = element_text(size = 10),
        axis.text.x = element_text(angle = 35, vjust = 0.5, hjust=0.5))

health_outcome %>%
  filter(HIA_type != "Microsimulation")%>%
  ggplot()+
  geom_boxplot(aes(x = emission_sector_cat, y = mortality_proj, fill = HIA_type))+
  geom_point(aes(x = emission_sector_cat, y = mortality_proj, col = HIA_type), size = 3,shape = 18)+
  theme_pubr()+
  xlab("")+
  ylab("")+
  scale_fill_manual(values = wes_palette("BottleRocket2"))+
  scale_color_manual(values = wes_palette("BottleRocket2"))+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))


health_outcome %>%
  filter(HIA_type != "Microsimulation")%>%
  ggplot()+
  geom_boxplot(aes(x = pathway_co_benefits2, y = mortality_proj, fill = HIA_type))+
  geom_point(aes(x = pathway_co_benefits2, y = mortality_proj, col = HIA_type), size = 3,shape = 18)+
  theme_pubr()+
  xlab("")+
  ylab("")+
  scale_fill_manual(values = wes_palette("BottleRocket2"))+
  scale_color_manual(values = wes_palette("BottleRocket2"))+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))






p5 = health_outcome %>%
  filter(HIA_type == "CRA") %>%
  ggplot(aes(x = emission_sector_cat, y = mortality_proj,
             fill = factor(emission_sector_cat, levels = c("All", "Energy", "Food system", "Transport","Multi"))))+
  geom_violin()+
  geom_point()+
  theme_pubr()+
  xlab("")+
  ylab("")+
  scale_fill_manual(values = c(wes_palette("Darjeeling1"), "#D4A5A5"))+
  theme(legend.title = element_blank())
  

p6 = health_outcome %>%
  filter(HIA_type == "Life tables") %>%
  ggplot(aes(x = emission_sector_cat, y = mortality_proj,
             fill = factor(emission_sector_cat, levels = c("All", "Energy", "Food system", "Transport","Multi","Housing"))))+
  geom_boxplot()+
  geom_point()+
  theme_pubr()+
  xlab("")+
  ylab("")+
  scale_fill_manual(values = c(wes_palette("Darjeeling1"), "#D4A5A5"))+
  theme(legend.title = element_blank())


p7 = health_outcome %>%
  filter(HIA_type == "CRA") %>%
  ggplot(aes(x = pathway_co_benefits2, y = mortality_proj, fill = pathway_co_benefits2))+
  geom_violin()+
  geom_point()+
  theme_pubr()+
  xlab("")+
  ylab("")+
  scale_fill_manual(values = c(wes_palette("Darjeeling2"), "#D4A5A5"))+
  theme(legend.title = element_blank())

p8 = health_outcome %>%
  filter(HIA_type == "Life tables") %>%
  ggplot(aes(x = pathway_co_benefits2, y = mortality_proj, fill = pathway_co_benefits2))+
  geom_boxplot()+
  geom_point()+
  theme_pubr()+
  xlab("")+
  ylab("")+
  scale_fill_manual(values = c(wes_palette("Darjeeling2"), "#D4A5A5"))+
  theme(legend.title = element_blank())


plot_health_outcome2.1 = ggarrange(p5,p6, ncol = 2 , nrow = 1, common.legend = T,
                                align = "h", labels = c("CRA","Life tables"), hjust = c(-2,-0.9))

plot_health_outcome2.1

plot_health_outcome2.2 = ggarrange(p7,p8, ncol = 2 , nrow = 1, common.legend = T,
                                   align = "h", labels = c("CRA","Life tables"), hjust = c(-2,-0.9))

plot_health_outcome2.2

plot_health_outcome2 = ggarrange (plot_health_outcome2.1,plot_health_outcome2.2, ncol = 1 , nrow = 2)

plot_health_outcome2




##### Air pollution
a1 = health_outcome %>%
  filter(HIA_type != "Microsimulation" & pathway_co_benefits2 == "Air pollution") %>%
  ggplot(aes(x = HIA_type, y = mortality_proj, fill = HIA_type))+
  geom_violin()+
  geom_point()+
  theme_pubr()+
  xlab("")+
  ylab("")+
  scale_fill_manual(values = wes_palette("BottleRocket2"))+
  ggtitle("Air pollution focus")



a2 = health_outcome %>%
  filter(pathway_co_benefits2 == "Air pollution" & emission_sector_cat != "Transport") %>%
  ggplot(aes(x = emission_sector_cat, y = mortality_proj, fill = emission_sector_cat))+
  geom_boxplot()+
  geom_point()+
  theme_pubr()+
  xlab("")+
  ylab("")+
  scale_fill_manual(values = wes_palette("Royal1"))+
  ggtitle("Air pollution focus")

health_outcome %>%
  filter(pathway_co_benefits2 == "Air pollution" & emission_sector_cat != "Transport") %>%
  ggplot(aes(x = scenario_cat, y = mortality_proj, fill = scenario_cat))+
  geom_boxplot()+
  geom_point()+
  theme_pubr()+
  xlab("")+
  ylab("")+
  scale_fill_manual(values = c(wes_palette("Royal1"),"grey"))+
  ggtitle("Air pollution focus")


plot_health_outcome_air = ggarrange (a1,a2, ncol = 1 , nrow = 2)
plot_health_outcome_air

# Health outcome v1
health_outcome %>% 
  filter(emission_sector_cat != "Housing") %>%
  ggplot(aes(x = emission_sector_cat, y = mortality_proj, color = HIA_type))+
  geom_point()+
  theme_pubr()+
  xlab("")+
  ylab("Réduction de mortalité associé")+
  #theme(legend.position = "none")+
  #scale_x_discrete(label = c("all"="Tous", "Energy"="Énergie","Food system"="Système alimentaire","Multi", "Transport"))+
  scale_fill_manual(values = wes_palette("Darjeeling1"))


health_outcome %>% 
  ggplot(aes(x = pathway_co_benefits, y = mortality_proj, color = HIA_type))+
  geom_point()+
  theme_pubr()+
  xlab("")+
  ylab("Mortality reduction")+
  #theme(legend.position = "none")+
  #scale_x_discrete(label = c("all"="Tous", "Energy"="Énergie","Food system"="Système alimentaire","Multi", "Transport"))+
  scale_fill_manual(values = wes_palette("Darjeeling1"))





health_outcome %>% 
  ggplot(aes(x = emission_sector_cat, y = mortality_proj, fill = emission_sector_cat))+
  geom_violin()+
  geom_point()+
  theme_pubr()+
  xlab("")+
  ylab("Mortality reduction")+
  theme(legend.position = "none")+
  #scale_x_discrete(label = c("all"="Tous", "Energy"="Énergie","Food system"="Système alimentaire","Multi", "Transport"))+
  scale_fill_manual(values = c(wes_palette("Darjeeling1"), "#D4A5A5"))

health_outcome %>% 
  filter(emission_sector_cat != "Housing")%>%
  filter(pathway_co_benefits == "Air & Indoor pollution"|
         pathway_co_benefits == "Air pollution"|
         pathway_co_benefits == "Diet"|
         pathway_co_benefits == "Physical activity") %>%
  ggplot(aes(x = pathway_co_benefits, y = mortality_proj, fill = pathway_co_benefits))+
  geom_violin()+
  geom_point()+
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  #scale_x_discrete(label = c("Air & Indoor pollution"="Pollution\natmosphérique & intérieur", "Air pollution"="Pollution atmosphérique",
  #                           "Diet"="Alimentation","Physical activity"="Activité physique"))+
  scale_fill_manual(values = wes_palette("AsteroidCity2"))

# plot_health_outcome_fr = ggarrange(health_outcome1,health_outcome2, ncol = 2 , nrow = 1,
#                            align = "h", labels = c("Secteur d'émission","Exposition"), hjust = c(-0.5,-0.8))

plot_health_outcome = ggarrange(health_outcome1,health_outcome2, ncol = 2 , nrow = 1,
                                   align = "h", labels = c("Emission sector","Co-benefit pathway"))

plot_health_outcome





mortality_scenario <- health_outcome %>%
  drop_na(mortality_proj) %>%
  ggplot(aes(x = factor(author_date, levels = unique(reorder(author_date, as.numeric(author_date)))),
             y = mortality_proj, fill = pathway_co_benefits)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  coord_flip()

mortality_scenario


yll_exposure <- health_outcome %>%
  drop_na(yll_per_capita) %>%
  ggplot(aes(x = factor(author_date, levels = unique(reorder(author_date, as.numeric(author_date)))),
             y = yll_per_capita, fill = pathway_co_benefits)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  coord_flip()

yll_exposure





mortality_scenario <- health_outcome %>%
  drop_na(mortality_proj) %>%
  ggplot(aes(x = factor(author_date, levels = unique(reorder(author_date, -as.numeric(author_date)))),
             y = mortality_proj, fill = scenario_cat)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  coord_flip()

mortality_scenario



yll_scenario <- health_outcome %>%
  drop_na(yll_per_capita) %>%
  ggplot(aes(x = factor(author_date, levels = unique(reorder(author_date, -as.numeric(author_date)))),
             y = yll_per_capita, fill = scenario_cat)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  coord_flip()

yll_scenario






mortality_scenario <- health_outcome %>% 
  drop_na(mortality_proj) %>%
  filter(scenario_cat != "not detailed") %>%
  ggplot(aes(x = scenario, y = mortality_proj, fill = scenario_cat)) +
  geom_bar(position = position_dodge(), stat = "identity")+
  coord_flip()

mortality_scenario


yll_scenario <- health_outcome %>% 
  drop_na(yll_per_capita) %>%
  filter(scenario_cat != "not detailed") %>%
  ggplot(aes(x = scenario, y = yll_per_capita, fill = scenario_cat)) +
  geom_bar(position = position_dodge(), stat = "identity")+
  coord_flip()

yll_scenario






mortality_scenario <- health_outcome %>% 
  drop_na(mortality_proj) %>%
  filter(author_date == "Hamilton, 2021" | author_date == "Yang, 2019"|
           author_date == "Polonik, 2021" | author_date == "Reis, 2022"| author_date == "Sampedro, 2020") %>%
  ggplot(aes(x = reorder(scenario, rank_pop), y = mortality_proj, fill = scenario_cat)) +
  geom_bar(position = position_dodge(), stat = "identity")+
  coord_flip()+
  facet_wrap(~ author_date)

mortality_scenario



mortality_scenario <- health_outcome %>% 
  drop_na(mortality_proj) %>%
  ggplot(aes(x = fct_reorder(scenario, mortality_proj), y = mortality_proj, fill = pathway_co_benefits)) +
  geom_bar(position = position_dodge(), stat = "identity")+
  coord_flip()+
  facet_wrap(~ scenario_cat)

mortality_scenario


mortality_scenario <- health_outcome %>% 
  drop_na(mortality_proj) %>%
  ggplot(aes(x = fct_reorder(scenario, mortality_proj), y = mortality_proj, fill = scenario_cat)) +
  geom_bar(position = position_dodge(), stat = "identity")+
  coord_flip()+
  facet_wrap(~ pathway_co_benefits)


mortality_scenario




# Saving plots
ggsave(here("figures","Map1.png"), plot = Map1 , width = 10, height = 7)
ggsave(here("figures","Map2.png"), plot = Map2 , width = 10, height = 7)
ggsave(here("figures","timescale1.png"), plot = timescale1 , width = 13, height = 7)
ggsave(here("figures","timescale2.png"), plot = timescale2 , width = 13, height = 7)
ggsave(here("figures","timescale3.png"), plot = timescale3 , width = 13, height = 7)
ggsave(here("figures","quality.png"), plot = quality , width = 13, height = 7)
ggsave(here("figures","healthoutcome.png"), plot = plot_health_outcome , width = 15, height = 7)
ggsave(here("figures","plot_mortality.png"), plot = plot_mortality , width = 15, height = 7)
ggsave(here("figures","plot_mortality2.png"), plot = plot_mortality2 , width = 15, height = 7)
ggsave(here("figures","plot_mortality21.png"), plot = plot_mortality12 , width = 15, height = 7)
ggsave(here("figures","plot_mortality22.png"), plot = plot_mortality22 , width = 15, height = 7)
ggsave(here("figures","plot_mortality24.png"), plot = plot_mortality24 , width = 15, height = 7)
ggsave(here("figures","plot_outcome.png"), plot = plot_outcome , width = 15, height = 7)





info_publi %>%
  ggplot(aes(x = exposure_cat))+
  geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust=-1)

info_publi %>%
  ggplot(aes(x = `health outcome`))+
  geom_bar()

info_publi %>%
  ggplot(aes(x = mitigation_type))+
  geom_bar()


