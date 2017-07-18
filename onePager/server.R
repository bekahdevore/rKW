# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(dplyr)

allData <- read.csv("mainDataForOnePager.csv", check.names = FALSE)  
allData <- allData %>% select(MSA, Population, `Labor Force Participation Rate`, 
                              `Labor Force Size`, `Unemployment Rate`,
                              `Median Home Value`, `Median Household Wage`, 
                              `Median Monthly Rent`, `Annual Median Wage (USD)`)
allData$Population <- sort(allData$Population, decreasing = TRUE)


mitLivingWageLouisvilleMSA <- read.csv("mitLivingWageData.csv", check.names = FALSE)
mitLivingWageLouisvilleMSA <- mitLivingWageLouisvilleMSA %>% select(-1)

mitLivingWageLouisvilleMSAFamilyOfFour <- mitLivingWageLouisvilleMSA %>% 
  select(1, 8)

louisvilleRankings <- read.csv("louisvilleRankings.csv", check.names = FALSE) 
louisvilleRankings <- louisvilleRankings %>% select(-1) 
louisvilleRankings$Rank <- sort(louisvilleRankings$Rank) 
louisvilleRankings <- louisvilleRankings %>% select(2, 1, 3)


shinyServer(function(input, output) {

  output$allData <- DT::renderDataTable(
          allData, options = list(lengthChange = FALSE, pageLength = 15, paging = FALSE, 
                                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                  rowCallback = DT::JS(
            'function(row, data) {
            // Bold cells for those >= 5 in the first column
            if (data[0] === "Louisville")
            $("td", row).css("background", "#f76b6b");
          }')), 
          rownames = FALSE
  )
  
  output$louisvilleRankings <- DT::renderDataTable(
    louisvilleRankings, options = list(lengthChange = FALSE, pageLength = 15, paging = FALSE, autoWidth = FALSE,
                                       columnDefs = list(list(className = 'dt-center', targets = "_all" 
                                                              ))), 
    rownames = FALSE
  )
 
  output$mitLivingWageTableFamilyOfFour <-  DT::renderDataTable(
    mitLivingWageLouisvilleMSAFamilyOfFour, options = list(pageLength = 3, searching = FALSE, paging = FALSE),
    rownames = FALSE
  )
  
  output$mitLivingWageTable <- DT::renderDataTable(
     mitLivingWageLouisvilleMSA, options = list(pageLength = 3, searching = FALSE, paging = FALSE,
                        rowCallback = DT::JS(
                          'function(row, data) {
                          // Bold cells for those >= 5 in the first column
                          if (data[1] === "Living Wage")
                          $("td:eq(8)", row).css("color", "#f76b6b");
      }')), rownames = FALSE
  )
})

