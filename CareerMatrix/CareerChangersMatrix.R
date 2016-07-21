##Necessary Packages
library(networkD3)
library(igraph)

##Import File
careers <- read.csv("CareerChangersMatrix.csv",
                    header=TRUE)

##Network Plot
g <- graph.data.frame(careers, directed = TRUE)
par(mar=c(0,0,0,0))
plot(g, 
     layout=layout.fruchterman.reingold, 
     vertex.size=2,
     edge.arrow.size=0.05,
     vertex.label=V(g)$name, 
     vertex.label.color = "black"
     )

##D3 integration
r <- get.data.frame(g, what="edges")
simpleNetwork(r)



