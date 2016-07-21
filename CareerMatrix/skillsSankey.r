library(googleVis)
skills <- read.csv("skills.csv")
skillsSankey <- plot(gvisSankey(
  skills, 
  from="Title",
  to="Element.Name",
  weight = "Weight",
  options=list(height =10000,
               width = 2000, 
               sankey = "{link:{
                color:{fill:'black'}
               }}"
               )
))



