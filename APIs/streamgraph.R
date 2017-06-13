load("industryData.RData")
library(streamgraph)
industryData <- data

industryData <- industryData %>% filter(industry_name != "Total Nonfarm" & industry_name != "Total Private" & industry_name != "Goods Producing"
                                        & industry_name != "Service-Providing" & industry_name != "Private Service Providing" &
                                          industry_name != "Durable Goods" & industry_name != "Non-Durable Goods")

streamgraph(industryData, key="industry_name", value="employees", date="year") %>% 
  sg_legend(show=TRUE, label="Industry: ")
