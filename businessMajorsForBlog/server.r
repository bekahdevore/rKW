
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(googleVis)
library(dplyr)

kyPums <- read.csv("pumsKY.csv")
socNames <- read.csv("socCodeTitleCrosswalk.csv")

#Louisville PUMA's
pums14 <- c(1701, 1702, 1703, 1704, 1705, 1800)

#Data clean and visualize function

       workforce <- (kyPums %>%
                            filter(PUMA %in% pums14) %>% ## filter to Louisville MSA (approximate)
                            filter(FOD1P == 6200)    %>% ## filter to pop 16 and over
                            filter(SOCP != 395012)   %>%
                            select(PUMA, FOD1P, OCCP, SOCP, PWGTP)) ## Select variables of interest 
       
       workforce <- merge(workforce, socNames, by= "SOCP")
       
       businessMajorsOccuptations <- count(workforce, title, wt=PWGTP, sort = TRUE)
       
       dataForApp <- businessMajorsOccuptations  %>%
              mutate(major = "General Business") %>%
              select(major, title, n)
       
       
#server
shinyServer(function(input, output) {
       
       
       output$sankey <- renderGvis({
              gvisSankey(dataForApp, 
                         from = "major", 
                         to = "title", 
                         weight = "n", 
                         options=list(height =1200,
                                      width  = 650,
                                      sankey ="{node:{label:{fontSize:14}}}"
                         ))})
       })
