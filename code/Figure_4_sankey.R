# Packages
pacman::p_load(dplyr,
               networkD3)

# Sankey
links <- data.frame(
  source=c("Energy decarbonisation","Energy decarbonisation","Energy decarbonisation",
           "Energy decarbonisation","Energy decarbonisation","Energy decarbonisation","Energy decarbonisation",
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
           "YLL","Morbidity","Deaths", "Health economic"
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
                             fontSize = 10, nodeWidth = 2 )

sankey = htmlwidgets::onRender(
  sankeyplot2,
  '
  function(el) {
    // Make the text bold and add white background as done before
    d3.select(el).selectAll(".node text")
      .attr("font-weight", "bold")
      .attr("font-size", "10px");  // Reduced font size for node labels

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

    // Reduce the font size of link labels as well
    d3.select(el).selectAll(".link text")
      .attr("font-size", "10px");  // Reduced font size for link labels

    // Decrease the SVG height for the preview (small screen size)
    var svg = d3.select(el).select("svg")
      .attr("height", 300);  // Reduced height for preview

    // Add headings to the top of each column
    // Add "Scenario" heading
    var scenarioText = svg.append("text")
      .attr("x", 20)  // Adjust x-position as needed
      .attr("y", 12)  // Adjust y-position for preview
      .attr("font-size", "14px")  // Reduced font size for headings
      .attr("font-weight", "bold")
      .text("SCENARIO");

    // Underline "Scenario" heading
    svg.append("line")
      .attr("x1", 20)  // Same x-position as heading
      .attr("x2", 95) // Adjust to length of text
      .attr("y1", 15)  // Position slightly below the heading
      .attr("y2", 15)  // Align with y1
      .attr("stroke", "black")
      .attr("stroke-width", 2);

    // Add "Sector" heading
    var sectorText = svg.append("text")
      .attr("x", 160)  // Adjust x-position as needed to center above middle column
      .attr("y", 12)   // Same y-position
      .attr("font-size", "14px")  // Reduced font size for headings
      .attr("font-weight", "bold")
      .text("SECTOR");

    // Underline "Sector" heading
    svg.append("line")
      .attr("x1", 160)
      .attr("x2", 220)  // Adjust to length of text
      .attr("y1", 15)
      .attr("y2", 15)
      .attr("stroke", "black")
      .attr("stroke-width", 2);

    // Add "Pathway" heading
    var impactText = svg.append("text")
      .attr("x", 320)  // Adjust x-position as needed for right column
      .attr("y", 12)   // Same y-position
      .attr("font-size", "14px")  // Reduced font size for headings
      .attr("font-weight", "bold")
      .text("PATHWAY");

    // Underline "Pathway" heading
    svg.append("line")
      .attr("x1", 320)
      .attr("x2", 390)  // Adjust to length of text
      .attr("y1", 15)
      .attr("y2", 15)
      .attr("stroke", "black")
      .attr("stroke-width", 2);

    // Add "Outcome" heading
    var outcomeText = svg.append("text")
      .attr("x", 460)  // Adjust x-position as needed
      .attr("y", 12)   // Same y-position
      .attr("font-size", "14px")  // Reduced font size for headings
      .attr("font-weight", "bold")
      .text("OUTCOME");

    // Underline "Outcome" heading
    svg.append("line")
      .attr("x1", 460)
      .attr("x2", 534)  // Adjust to length of text
      .attr("y1", 15)
      .attr("y2", 15)
      .attr("stroke", "black")
      .attr("stroke-width", 2);

    // Now, move the Sankey diagram itself down below the headings in preview
    var sankeyPlot = svg.select(".sankey");  // Select the Sankey diagram (make sure you have the correct class or ID for it)
    sankeyPlot.attr("transform", "translate(0, 40)");  // Move the plot down (adjust as needed)
  }
  '
)


sankey
library(htmlwidgets)
saveWidget(sankey, here("figures", "sankey.html"))
