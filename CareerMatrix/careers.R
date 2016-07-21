##Network Plot
source()
g <- graph.data.frame(careers, directed = TRUE)

##D3 integration
r <- get.data.frame(g, what="edges")
simpleNetwork(r)

