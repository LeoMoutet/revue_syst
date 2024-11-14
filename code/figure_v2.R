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
               forcats,
               ggalluvial,
               webshot)

# Data
info_publi <- read_excel(here("data","extraction_grid_article.xlsx"))
health_outcome <- read_excel(here("data","extraction_grid_scenario.xlsx"))
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
value = c(20, 6, 6, 3, 
          4,5, 3,3,
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
                              breaks = c(1,2,4,6,20),
                              labels = c("1-2", "3-4","5-6", "20"),
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
  #         Sofia, Santiago, Virginia, Guangdong
  lat = c(34.274342, 39.916668, 36.066898, 34.052235      , 31.848398, 31.456781,37.773972,     41.755749,        51.5085300,             41.3850639, 47.997791, 55.60587, 42.698334,
          -33.447487,37.926868, 23.128994) ,  #  latitudes
  lon = c(108.889191, 116.383331, 120.382698,-118.243683,  117.272362, 102.843018,-122.431297,   -124.202591,     -0.1257400,             2.1734035, 7.842609, 13.00073, 23.319941,
          -70.673676,-78.024902, 113.253250) ,  #  longitudes
  region = c("Shaanxi", "Beijing", "Shandong", " ", "Anhui", "Sichuan", "California (x3)", 
             " ", "London", "Barcelona", "Freiburg", "Malmö", "Sofia", "Santiago", "Virginia", "Guangdong"),
  lat_nudged = c(37.274342, 43.916668, 37.066898, 34.052235      , 31.848398, 31.456781,37.773972,     
                 41.755749, 50.5085300, 41.3850639, 47.997791, 59.60587, 42.698334, -33.447487,37.926868, 23.128994),
  
  lon_nudged = c(97.889191, 116.383331, 135.382698,-116.243683,  130.272362, 90.843018,-140.431297,   
                 -122.202591, -13.1257400, -12.1734035, 20.842609, 6.00073, 15.319941,-82.673676,-66.024902, 130.253250)
  )


# Transform the points data into a spatial object
points_sf <- st_as_sf(points_data, coords = c("lon", "lat"), crs = 4326)
points_sf <- st_transform(points_sf, crs = robinson_proj)

# Transform the nudged points data into a spatial object
points_sf_nudged <- st_as_sf(points_data, coords = c("lon_nudged", "lat_nudged"), crs = 4326)
points_sf_nudged <- st_transform(points_sf_nudged, crs = robinson_proj)


# Map with regions
Map1 = ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey", linewidth = 0.1,
          show.legend = FALSE, size = 5) +
  geom_sf(data = world_data_multi, aes(fill = color_group), color ="grey", linewidth = 0.1) +
  geom_sf(data = points_sf, aes(color = "Sub-national investigation        "), size = 2, shape = 16) +
  geom_sf_text(data = points_sf_nudged, aes(label = region), size = 3, check_overlap = F) +
  scale_fill_manual(values = c("orange","firebrick2","firebrick4","mediumpurple4"),
                    breaks = c("1-2", "3-4","5-6", "20")) +
  scale_color_manual(values = c("black"), guide = guide_legend(title = NULL)) +
  labs(title = "",fill = "Number of articles:") +
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

ggsave(here("figures","Map1.png"), plot = Map1 , width = 10, height = 8)



# 1st author map
data_multi <- data.frame(country = c("China", "United Kingdom", "United States","France", "Poland", "Korea", "Germany", "Spain", "Austria", "Italy","Japan"
),
value = c(22,6,12,1,2,1,4,4,4,1,1))

# Color scale
data_multi$color_group <- cut(data_multi$value,
                              breaks = c(1,2,6,12,22),
                              labels = c("1-2", "4-6", "12","22"),
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
                    breaks = c("1-2", "4-6", "12", "22"),
                    guide = guide_legend(title = "Number of article:")) +
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
  coord_sf(crs = robinson_proj)


Map2


# Sankey
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
           "AFOLU","AFOLU",
           "Housing", "Housing",
           "Industry", "Industry",
           "Other",
           
           "Air pollution","Air pollution","Air pollution","Air pollution","Air pollution",
           "Air pollution",
           "Diet","Diet",
           "Physical activity","Physical activity","Physical activity","Physical activity",
           "Indoor air quality","Indoor air quality","Indoor air quality","Indoor air quality"
  ), 
  
  target=c("Energy","Transport", "AFOLU",
           "Housing","Industry","All-encompassing","Other",
           "Energy","AFOLU","Industry","Transport", "Other","Housing", "All-encompassing", 
           "Energy", "All-encompassing","Housing","Transport", "AFOLU",
           "All-encompassing", "Transport", "Energy", "Housing","Industry","AFOLU","Other",
           "All-encompassing","Energy","Industry","Transport","Housing","AFOLU","Other",
           
           
           "Air pollution","Indoor air quality",
           "Air pollution","Indoor air quality",
           "Air pollution", "Physical activity",
           "Air pollution", "Diet",
           "Air pollution", "Indoor air quality",
           "Air pollution","Indoor air quality",
           "Air pollution",
           
           "Deaths", "YLL", "Economic","Life expectancy","DALYs",
           "Morbidity",
           "Deaths", "YLL", 
           "Deaths", "YLL","Economic","Life expectancy",
           "YLL","Morbidity","Deaths", "Health economics"
  ), 
  
  value=c(24,9,3,
          8,5,2,3,
          8,4,5,7,3,5,2,
          3,1,1,2,1,
          1,2,1,1,1,1,1,
          10,12,9,10,5,6,5,
          
          
          16,1,
          39,3,
          20,4,
          11,3,
          18,3,
          19,1,
          12,
          
          44,4,26,3,1,
          15,
          1,1,
          2,2,1,1,
          2,1,2,1
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
                           "type_i","type_i",
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



sankey = htmlwidgets::onRender(
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

saveNetwork(sankey, here("figures","sankey_plot.html"))
webshot("sankey_plot.html", here("figures","sankey_plot.png"))



# Manually correcting the lengths for the vectors
source_vector <- c("Energy decarbonation","Energy decarbonation","Energy decarbonation",
                   "Energy decarbonation","Energy decarbonation","Energy decarbonation","Energy decarbonation",
                   "Health","Health","Health","Health","Health","Health","Health",
                   "Sufficiency","Sufficiency","Sufficiency","Sufficiency","Sufficiency",
                   "Financial","Financial","Financial","Financial","Financial","Financial","Financial",
                   "Not detailed","Not detailed","Not detailed","Not detailed","Not detailed","Not detailed","Not detailed",
                   
                   "All-encompassing", "All-encompassing",
                   "Energy", "Energy",
                   "Transport",  "Transport", 
                   "AFOLU","AFOLU",
                   "Housing", "Housing",
                   "Industry", "Industry",
                   "Other",
                   
                   "Air pollution","Air pollution","Air pollution","Air pollution","Air pollution",
                   "Air pollution",
                   "Diet","Diet",
                   "Physical activity","Physical activity","Physical activity","Physical activity",
                   "Indoor air quality","Indoor air quality","Indoor air quality","Indoor air quality")

target_vector <- c("Energy","Transport", "AFOLU",
                   "Housing","Industry","All-encompassing","Other",
                   "Energy","AFOLU","Industry","Transport", "Other","Housing", "All-encompassing", 
                   "Energy", "All-encompassing","Housing","Transport", "AFOLU",
                   "All-encompassing", "Transport", "Energy", "Housing","Industry","AFOLU","Other",
                   "All-encompassing","Energy","Industry","Transport","Housing","AFOLU","Other",
                   
                   
                   "Air pollution","Indoor air quality",
                   "Air pollution","Indoor air quality",
                   "Air pollution", "Physical activity",
                   "Air pollution", "Diet",
                   "Air pollution", "Indoor air quality",
                   "Air pollution","Indoor air quality",
                   "Air pollution",

                   "Deaths", "YLL", "Economic","Life expectancy","DALYs",
                   "Morbidity",
                   "Deaths", "YLL", 
                   "Deaths", "YLL","Economic","Life expectancy",
                   "YLL","Morbidity","Deaths", "Health economics")

value_vector <- c(24, 9, 3, 8, 5, 2, 3, 8, 4, 5, 7, 3, 5, 2, 3, 1, 1, 2, 1, 
                  1, 2, 1, 1, 1, 1, 1, 10, 12, 9, 10, 5, 6, 5, 16, 1, 39, 3, 
                  20, 4, 11, 3, 18, 3, 19, 1, 12, 44, 4, 26, 3, 1, 15, 1, 1, 2, 
                  2, 1, 1, 2, 1, 2, 1)


# Now combine them into a data frame
links <- data.frame(
  source = source_vector,
  target = target_vector,
  value = value_vector
)


# Load the 'alluvial' library
library(alluvial)

alluvial(links, 
         freq = links$value,  
         col = c("blue", "green"))




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
  scale_y_continuous(breaks= c(0,30,60))+
  theme(legend.text = element_text(size = 15),
        axis.text.y = element_text(size = 20))


quality


# Quality of individual article 
quality_eval_article <- quality_eval %>%
  gather(key = "Criteria", value = "Value", -Article) %>%
  mutate(yes = ifelse(Value == "Yes",1,0)) %>%
  count(Article, Value) %>%
  spread(key = "Value", value = "n", fill = 0)


# Health outcome

health_outcome$scenario_cat <- factor(health_outcome$scenario_cat,
                                      levels = rev(c("energy decarbonation","financial instrument", 
                                                     "health in climate policies","sufficiency", 
                                                     "not detailed")))

p1 = health_outcome %>%
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
  scale_y_continuous(limits =c(0,115))+
  coord_flip()



health_outcome$emission_sector_cat <- factor(health_outcome$emission_sector_cat,
                                             levels = rev(c('All-encompassing','Energy','AFOLU',
                                                            'Housing', 'Transport','Multisectorial')))

p2 = health_outcome %>%
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
  #scale_x_discrete(labels = c('Multi*','Transport','Logement','Alimentation','Énergie','Tous')) +
  scale_y_continuous(limits =c(0,115))+
  coord_flip()


p3 = health_outcome %>%
  ggplot(aes(x = pathway_co_benefits2, fill = include_mortality))+
  geom_bar( width = 0.5, show.legend = F)+
  theme_pubr()+
  xlab("")+
  ylab("Number of scenario")+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  scale_fill_manual(values = c("steelblue1","steelblue4"))+
  scale_y_continuous(limits =c(0,115), breaks= c(0,10,25,50,75,100,120))+
  #scale_x_discrete(labels = c('Pollution','Alimentation','Activité physique')) +
  coord_flip()+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))



p4 = info_publi %>%
  ggplot(aes(x = publi_yr, fill =include_mortality))+
  geom_bar( width = 0.5)+
  theme_pubr()+
  xlab("Year")+
  ylab("Number of studies")+
  scale_fill_manual(values = c("steelblue1","steelblue4"))+
  scale_y_continuous(limits = c(0, 15), breaks= c(0,5,10,15))+
  scale_x_continuous( breaks= c(2012,2015,2020,2023))+
  theme(legend.title = element_text(),
        legend.position = "top",
        text = element_text(size = 10))+
  guides(fill=guide_legend(title="Studies or scenario with a scalable health outcome (see methods):"))





plot_outcome = ggarrange(p4,ggarrange(p1,p2,p3, ncol = 1 , nrow = 3, align = "v", 
                                   labels = c("B: Typology of scenario","C: Emission sector","D: Co-benefit pathway"),
                                   hjust = c(-0.68,-0.85,-0.72),vjust = c(0.5,0.5,0.5)),
                         ncol = 2, nrow = 1, common.legend = T,  widths = c(0.8,1), legend = "bottom", labels = c("A: Year of publication",""), hjust = c(-0.28,0))+
  theme(plot.margin = margin(2,0.1,0.1,0.1, "cm"))

plot_outcome

ggsave(here("figures","plot_outcome.png"), plot = plot_outcome , width = 10, height = 7)

summary(health_outcome$mortality_proj)*100

summary(health_outcome$mortality_proj[health_outcome$HIA_type == "Life tables"])*100
summary(health_outcome$mortality_proj[health_outcome$HIA_type == "CRA"])*100

p5 = health_outcome %>%
  filter(HIA_type != "Microsimulation") %>%
  ggplot(aes(x = HIA_type, y = 100*mortality_proj, color = HIA_type, shape = HIA_type))+
  geom_point(size = 2,position=position_jitter(h=NULL,w=0.2), show.legend = F)+
  theme_pubr()+
  xlab("")+
  ylab("")+
  scale_color_manual(values =c("#5F5647","#A42820","#9B9987",wes_palette("Darjeeling1")))+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)+
  #scale_x_discrete(labels = c('Évaluation comparative','Tables de vie')) +
  scale_y_continuous( breaks= c(1,5,10,15,20), limits = c(-1,20))


health_outcome$pathway_co_benefits2 <- factor(health_outcome$pathway_co_benefits2, 
                                              levels = c("Air pollution", "Physical activity", "Diet"))

p6 = health_outcome %>%
  ggplot(aes(x = pathway_co_benefits2, y = 100*mortality_proj, color =pathway_co_benefits2, shape = pathway_co_benefits2))+
  geom_point(size = 2,position=position_jitter(h=NULL,w=0.3), show.legend = F)+
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  scale_color_manual(values = c("#5F5647","#A42820","#9B9987",wes_palette("Darjeeling1")))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)+
  #scale_x_discrete(labels = c('Pollution','Activité physique','Alimentation')) +
  scale_y_continuous( breaks= c(1,5,10,15,20), limits = c(-1,20))


health_outcome$emission_sector_cat <- factor(health_outcome$emission_sector_cat, 
                                             levels = c("All-encompassing", "Energy", "AFOLU",
                                                        "Housing","Transport", "Multisectorial"))

p7 = health_outcome %>%
  ggplot(aes(x = emission_sector_cat, y = 100*mortality_proj, color =emission_sector_cat, shape = emission_sector_cat))+
  geom_point(size = 2,position=position_jitter(h=NULL,w=0.3), show.legend = F)+
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  scale_color_manual(values = c("#5F5647","#A42820","#9B9987",wes_palette("Darjeeling1")))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)+
  scale_y_continuous( breaks= c(1,5,10,15,20), limits = c(-1,20))+
  #scale_x_discrete(labels = c('Tous','Énergie','Alimentation','Logement','Transport','Multisectorial'))
  scale_x_discrete(labels = c("All", "Energy", "AFOLU","Housing","Transport","Multi\nsectorial"))


baseline_year <- merge(health_outcome,info_publi, by = "author_date")

p8 = baseline_year %>%
  ggplot(aes(x = baseline_scenario, y = 100*mortality_proj, color =baseline_scenario, shape = baseline_scenario))+
  geom_point(size = 2,position=position_jitter(h=NULL,w=0.3), show.legend = F)+
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  scale_color_manual(values = c("#5F5647","#A42820","#9B9987"))+
  scale_fill_manual(values = c("#5F5647","#A42820","#9B9987"))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)+
  scale_y_continuous( breaks= c(1,5,10,15,20), limits = c(-1,20))+
  #scale_x_discrete(labels = c("Diminution\ndes GES", "Augmentation\ndes GES", "Année de\nréférence"))
  scale_x_discrete(labels = c("Decreasing\nGHG emission", "Increasing\nGHG emission", "Reference\nyear"))


plot_mortality = annotate_figure(ggarrange(p5,p8,p6,p7, ncol = 2, nrow = 2,labels = c("B: Quantitative modelling methods","C: Baseline scenario",
                                                                                      "D: Co-benefit pathway","E: Sector of emission"),
                                           align ="h", hjust = c(-0.2,-0.3,-0.3,-0.3)))


plot_mortality


summary(health_outcome$mortality_proj)*100

p9 = baseline_year %>%
  filter(include_mortality.x == "Yes")%>%
  ggplot(aes( y = 100*mortality_proj, x= include_mortality.x))+
  geom_violin(fill ="lightcyan2")+
  geom_point(size = 1,position=position_jitter(h=NULL,w=0.2), show.legend = F) +
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size = 10),
        plot.margin = margin(1,0.1,1,0.1,"cm"))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)+
  stat_summary( geom = "crossbar", fun = "median",  size = 0.2,  col = "black")+
  stat_summary( geom = "text", fun = "median",  size = 3,  col = "black",aes(label = round(after_stat(y),1)),
                position = position_nudge(x = -0.42, y = 0.5))+
  scale_y_continuous( breaks= c(1,5,10,15,20), limits = c(-1,20))


plot_mortality2 = annotate_figure(ggarrange(p9,plot_mortality, ncol = 2, nrow = 1,labels = c("A: Overall", ""),
                                           align ="v", hjust = c(-0.6,1),vjust = c(4,1), widths = c(0.4, 1)),
                                 left = "Preventable mortality fraction (%)")

plot_mortality2


baseline_year %>%
  filter(include_mortality.x == "Yes") %>%
  ggplot(aes(y = 100 * mortality_proj, x = include_mortality.x)) +
  geom_violin(fill = "lightcyan2") +
  geom_point(aes( color = geo_scale.y), 
             size = 2, position = position_jitter(h = NULL, w = 0.2), show.legend = TRUE) +
  theme_pubr() +
  xlab("") +
  ylab("") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size = 10),
        plot.margin = margin(1, 0.1, 1, 0.1, "cm")) +
  geom_hline(aes(yintercept = 0), color = "black", linetype = 2) +
  stat_summary(geom = "crossbar", fun = "median", size = 0.2, col = "black") +
  stat_summary(geom = "text", fun = "median", size = 3, col = "black",
               aes(label = round(after_stat(y), 1)),
               position = position_nudge(x = -0.42, y = 0.5)) +
  scale_y_continuous(breaks = c(1, 5, 10, 15, 20), limits = c(-1, 20)) +
  scale_color_manual(values = c("red", "blue", "green", "purple"))

# Life-year gained
baseline_year %>%
  filter(include_yll == "Yes")%>%
  ggplot(aes( y = life_years_100000, x= include_yll))+
  geom_violin(fill ="lightcyan2")+
  geom_point(size = 1,position=position_jitter(h=NULL,w=0.2), show.legend = F) +
  theme_pubr()+
  xlab("")+
  ylab("Life-year gained per 100 000")+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size = 10),
        plot.margin = margin(1,0.1,1,0.1,"cm"))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)+
  stat_summary( geom = "crossbar", fun = "median",  size = 0.2,  col = "black")+
  stat_summary( geom = "text", fun = "median",  size = 3,  col = "black",aes(label = round(after_stat(y),1)),
                position = position_nudge(x = -0.42, y = 50))+
  scale_y_continuous( breaks= c(100,500,1000,2000,3000,4000), limits = c(-15,4600))

# China sub-analysis
china_impact = health_outcome %>%
  filter(include_mortality == "Yes" & geo_scale == "China")%>%
  ggplot(aes( y = 100*mortality_proj, x= include_mortality))+
  geom_violin(fill ="lightcyan2")+
  geom_point(size = 1,position=position_jitter(h=NULL,w=0.2), show.legend = F) +
  theme_pubr()+
  xlab("")+
  ylab("Preventable mortality fraction (%)")+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size = 10),
        plot.margin = margin(1,0.1,1,0.1,"cm"))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)+
  stat_summary( geom = "crossbar", fun = "median",  size = 0.2,  col = "black")+
  stat_summary( geom = "text", fun = "median",  size = 3,  col = "black",aes(label = round(after_stat(y),1)),
                position = position_nudge(x = -0.42, y = 0.5))+
  scale_y_continuous( breaks= c(1,5,10,15,20), limits = c(-1,20))


# Air pollution sub-analysis
AP_methods = health_outcome %>%
  filter(HIA_type != "Microsimulation" & pathway_co_benefits2 == "Air pollution") %>%
  ggplot(aes(x = HIA_type, y = 100*mortality_proj, color = HIA_type, shape = HIA_type))+
  geom_point(size = 2,position=position_jitter(h=NULL,w=0.2), show.legend = F)+
  theme_pubr()+
  xlab("")+
  ylab("Preventable mortality fraction (%)")+
  scale_color_manual(values =c("#5F5647","#A42820","#9B9987",wes_palette("Darjeeling1")))+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)+
  #scale_x_discrete(labels = c('Évaluation comparative','Tables de vie')) +
  scale_y_continuous( breaks= c(1,5,10,15,20), limits = c(-1,20))

AP_baseline = baseline_year %>%
  filter(pathway_co_benefits2 == "Air pollution") %>%
  ggplot(aes(x = baseline_scenario, y = 100*mortality_proj, color =baseline_scenario, shape = baseline_scenario))+
  geom_point(size = 2,position=position_jitter(h=NULL,w=0.3), show.legend = F)+
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  scale_color_manual(values = c("#5F5647","#A42820","#9B9987"))+
  scale_fill_manual(values = c("#5F5647","#A42820","#9B9987"))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)+
  scale_y_continuous( breaks= c(1,5,10,15,20), limits = c(-1,20))+
  #scale_x_discrete(labels = c("Diminution\ndes GES", "Augmentation\ndes GES", "Année de\nréférence"))
  scale_x_discrete(labels = c("Decreasing\nGHG emission", "Increasing\nGHG emission", "Reference\nyear"))

plot_AP = ggarrange(AP_methods,AP_baseline, ncol = 2, nrow = 1,labels = c("A: Quantitative modelling methods\n    (only air pollution exposure)", 
                                                                          "B: Baseline scenario\n(only air pollution exposure)"),
                                            hjust = c(-0.2,-0.3),vjust = c(1.1,1.1))

plot_AP


health_outcome %>%
  filter(pathway_co_benefits2 == "Air pollution") %>%
  ggplot(aes(x = only_CCS, y = 100*mortality_proj, color = only_CCS, shape = only_CCS))+
  geom_point(size = 2,position=position_jitter(h=NULL,w=0.2), show.legend = F)+
  theme_pubr()+
  xlab("Only CCS")+
  ylab("Preventable mortality fraction (%)")+
  scale_color_manual(values =c("#5F5647","#A42820","#9B9987",wes_palette("Darjeeling1")))+
  theme(legend.title = element_blank(),
        text = element_text(size = 10))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)+
  #scale_x_discrete(labels = c('Évaluation comparative','Tables de vie')) +
  scale_y_continuous( breaks= c(1,5,10,15,20), limits = c(-1,20))+
  ggtitle("Only air pollution exposure")

## Tests
health_outcome %>%
  filter(HIA_type != "Microsimulation" & pathway_co_benefits == "Air pollution") %>%
  filter(include_mortality == "Yes")%>%
  ggplot(aes( y = 100*mortality_proj, x= geo_scale, color = geo_scale))+
  geom_point()+
  theme_pubr()+
  xlab("")+
  ylab("")+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)+
  stat_summary( geom = "crossbar", fun = "median",  size = 0.2,  col = "black")+
  stat_summary( geom = "text", fun = "median",  size = 3,  col = "black",aes(label = round(after_stat(y),1)),
                position = position_nudge(x = -0.42, y = 0.5))+
  scale_y_continuous( breaks= c(1,5,10,15,20), limits = c(-1,20))

baseline_year %>%
  filter(HIA_type != "Microsimulation" & pathway_co_benefits == "Air pollution") %>%
  filter(include_mortality.y == "Yes")%>%
  ggplot(aes( y = 100*mortality_proj, x= publi_yr, color = publi_yr))+
  geom_point()+
  theme_pubr()+
  xlab("")+
  ylab("")+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)+
  stat_summary( geom = "crossbar", fun = "median",  size = 0.2,  col = "black")+
  stat_summary( geom = "text", fun = "median",  size = 3,  col = "black",aes(label = round(after_stat(y),1)),
                position = position_nudge(x = -0.42, y = 0.5))+
  scale_y_continuous( breaks= c(1,5,10,15,20), limits = c(-1,20))

kruskal.test(mortality_proj ~ publi_yr, data = baseline_year)

baseline_year$test <- ifelse(baseline_year$publi_yr < 2023, 2018,2023)

kruskal.test(mortality_proj ~ test, data = baseline_year)


cor.test(baseline_year$mortality_proj, baseline_year$publi_yr, method = c( "kendall"))

test_hia_type <- baseline_year %>% 
  filter(HIA_type != "Microsimulation") %>%
  select(c(HIA_type,mortality_proj)) %>%
  na.omit()


wilcox.test(test_hia_type$mortality_proj)


fisher.test(baseline_year$mortality_proj, baseline_year$baseline_scenario)

health_outcome %>%
  filter(HIA_type != "Microsimulation" & mortality_proj > 0.036) %>%
  filter(include_mortality == "Yes" & author_date != "Hamilton, 2021")%>%
  filter(pathway_co_benefits == "Air pollution" )%>%
  ggplot(aes( y = 100*mortality_proj, x= include_mortality, color = `Geographical scale`))+
  geom_point(size = 2,position=position_jitter(h=NULL,w=0.3))+
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size = 10),
        plot.margin = margin(1,0.1,1,0.1,"cm"))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)+
  stat_summary( geom = "crossbar", fun = "median",  size = 0.2,  col = "black")+
  stat_summary( geom = "text", fun = "median",  size = 3,  col = "black",aes(label = round(after_stat(y),1)),
                position = position_nudge(x = -0.42, y = 0.5))+
  scale_y_continuous( breaks= c(1,5,10,15,20), limits = c(-1,20))


health_outcome %>%
  filter(HIA_type != "Microsimulation") %>%
  filter(include_mortality == "Yes" & `Geographical scale`== "China")%>%
  filter(pathway_co_benefits == "Air pollution" )%>%
  ggplot(aes( y = 100*mortality_proj, x= include_mortality, color = `Geographical scale`))+
  geom_point(size = 2,position=position_jitter(h=NULL,w=0.3))+
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size = 10),
        plot.margin = margin(1,0.1,1,0.1,"cm"))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)+
  stat_summary( geom = "crossbar", fun = "median",  size = 0.2,  col = "black")+
  stat_summary( geom = "text", fun = "median",  size = 3,  col = "black",aes(label = round(after_stat(y),1)),
                position = position_nudge(x = -0.42, y = 0.5))+
  scale_y_continuous( breaks= c(1,5,10,15,20), limits = c(-1,20))




baseline_year %>%
  filter(HIA_type != "Microsimulation") %>%
  filter(include_mortality.y == "Yes")%>%
  filter(pathway_co_benefits == "Air pollution")%>%
  ggplot(aes( y = 100*mortality_proj, x= include_mortality.y, color = `publi_yr`))+
  geom_point(size = 2,position=position_jitter(h=NULL,w=0.3))+
  theme_pubr()+
  xlab("")+
  ylab("")+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size = 10),
        plot.margin = margin(1,0.1,1,0.1,"cm"))+
  geom_hline(aes(yintercept = 0), color= "black", linetype = 2)+
  stat_summary( geom = "crossbar", fun = "median",  size = 0.2,  col = "black")+
  stat_summary( geom = "text", fun = "median",  size = 3,  col = "black",aes(label = round(after_stat(y),1)),
                position = position_nudge(x = -0.42, y = 0.5))+
  scale_y_continuous( breaks= c(1,5,10,15,20), limits = c(-1,20))+
  scale_color_gradientn(colours = c("blue", "green", "yellow", "orange", "red","darkred"))


# Saving plots
ggsave(here("figures","Map1.png"), plot = Map1 , width = 10, height = 7)
ggsave(here("figures","Map2.png"), plot = Map2 , width = 10, height = 7)
ggsave(here("figures","quality.png"), plot = quality , width = 13, height = 7)
ggsave(here("figures","plot_outcome.png"), plot = plot_outcome , width = 10, height = 7)
ggsave(here("figures","plot_mortality.png"), plot = plot_mortality2 , width = 15, height = 7)
ggsave(here("figures","china_impact.png"), plot = china_impact , width = 10, height = 7)
ggsave(here("figures","plot_AP.png"), plot = plot_AP , width = 10, height = 7)





