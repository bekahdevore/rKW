
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(dplyr)
library(RCurl)
library(shiny)
library(stringr)

burningGlassDataConnection <- getURL('https://docs.google.com/spreadsheets/d/1iH9ZPkjY594jkKaGryy80Zsv6S_NUK6O_YaoZ-s1zqg/pub?gid=0&single=true&output=csv')
emsiDataConnection         <- getURL('https://docs.google.com/spreadsheets/d/1sxKx3waBIHnPxTdvRZQ7PVwevEx2l2v16YFhpWrFnfU/pub?gid=0&single=true&output=csv')        

burningGlassDataJCTC <- read.csv(textConnection(burningGlassDataConnection), check.names = FALSE)
emsiDataJCTC         <- read.csv(textConnection(emsiDataConnection), check.names = FALSE)


jctcData <- left_join(emsiDataJCTC, burningGlassDataJCTC, by = 'SOC')
#jctcData <- jctcData %>%
#              select(1:10, 12)

#Remove commas, dollar signs, and make replacements
jctcData <- as.data.frame(lapply(jctcData, function(x) {
                     gsub(',|$', '', x)
              }))

jctcData <- as.data.frame(lapply(jctcData, function(x) {
                     gsub('<10', '5', x)
              }))

cols.num            <- c("Age.55.64","Age.65.", "X2016...2026.Change")
jctcData[cols.num]  <- sapply(jctcData[cols.num], as.character)
jctcData[cols.num]  <- sapply(jctcData[cols.num], as.numeric)
jctcData$retirement <- (jctcData$Age.55.64) + (jctcData$Age.65.)

jctcData$retirement <- as.numeric(as.character(jctcData$retirement))
jctcData$jobsAdded  <- (jctcData$X2016...2026.Change) + (jctcData$retirement)

jctcData <- jctcData %>%
                     select(1:2, 11, 3:7, 13)

colnames(jctcData)[3] <- "Number of Job Postings (Oct. 2015 - Sept. 2016)"
colnames(jctcData)[5] <- "2018 Jobs (Projected)"
colnames(jctcData)[6] <- "2021 Jobs (Projected)"
colnames(jctcData)[9] <- "Jobs Added + Possible Retirements (projected 2026)"





shinyServer(function(input, output) {

  output$dataTable <- DT::renderDataTable({
         DT::datatable(jctcData, 
                       options = list(
                              pageLength = 10))
  })
  
  output$downloadData <- downloadHandler(
         filename = function() {
                paste('jctcData.csv', sep = '')
         },
         content = function(file) {
                write.csv(jctcData, file)
         }
  )

})
