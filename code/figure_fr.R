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

# Sankey Fr
links <- data.frame(
  source=c("Décarbonation de l'énergie","Décarbonation de l'énergie","Décarbonation de l'énergie",
           "Décarbonation de l'énergie","Décarbonation de l'énergie","Décarbonation de l'énergie","Décarbonation de l'énergie",
           "Santé","Santé","Santé","Santé","Santé","Santé","Santé",
           "Frugalité","Frugalité","Frugalité","Frugalité","Frugalité",
           "Financier","Financier","Financier","Financier","Financier","Financier","Financier",
           "Non détaillé","Non détaillé","Non détaillé","Non détaillé","Non détaillé","Non détaillé","Non détaillé",
           
           "Tous secteurs", "Tous secteurs",
           "Énergie", "Énergie",
           "Transport",  "Transport", 
           "AFOLU","AFOLU",
           "Bâtiment", "Bâtiment",
           "Industry", "Industry",
           "Autre",
           
           "Pollution atmosphérique","Pollution atmosphérique","Pollution atmosphérique","Pollution atmosphérique","Pollution atmosphérique",
           "Pollution atmosphérique",
           "Diet","Diet",
           "Activité physique","Activité physique","Activité physique","Activité physique",
           "Pollution intérieur","Pollution intérieur","Pollution intérieur","Pollution intérieur"
  ), 
  
  target=c("Énergie","Transport", "AFOLU",
           "Bâtiment","Industry","Tous secteurs","Autre",
           "Énergie","AFOLU","Industry","Transport", "Autre","Bâtiment", "Tous secteurs", 
           "Énergie", "Tous secteurs","Bâtiment","Transport", "AFOLU",
           "Tous secteurs", "Transport", "Énergie", "Bâtiment","Industry","AFOLU","Autre",
           "Tous secteurs","Énergie","Industry","Transport","Bâtiment","AFOLU","Autre",
           
           
           "Pollution atmosphérique","Pollution intérieur",
           "Pollution atmosphérique","Pollution intérieur",
           "Pollution atmosphérique", "Activité physique",
           "Pollution atmosphérique", "Diet",
           "Pollution atmosphérique", "Pollution intérieur",
           "Pollution atmosphérique","Pollution intérieur",
           "Pollution atmosphérique",
           
           "Décès", "YLL", "Économique","Espérence de vie","DALYs",
           "Morbidity",
           "Décès", "YLL", 
           "Décès", "YLL","Économique","Espérence de vie",
           "YLL","Morbidité","Décès", "Économique"
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
