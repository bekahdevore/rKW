
library(shiny)
library(dplyr)

occupations <- read.csv("allOccupations_2.csv")

#Remove dollar signs
occupations$Pct..25.Hourly.Earnings <- str_replace_all(occupations$Pct..25.Hourly.Earnings, '\\$','')
occupations$Median.Hourly.Earnings  <- str_replace_all(occupations$Median.Hourly.Earnings,  '\\$','')

occupations <- as.data.frame(lapply(occupations, function(x) {
                                      gsub("<10", "9", x)
}))

occupations <- as.data.frame(lapply(occupations, function(x) {
  gsub(",", "", x)
}))


#Change to character in order to change to numeric in next function
occupations[,"X2016...2026.Change"] <- (as.numeric(as.character(occupations[,"X2016...2026.Change"])))
occupations[,"Regional.Completions..2014."] <- (as.numeric(as.character(occupations[,"Regional.Completions..2014."])))


#Change variables to numeric
x <- c("Pct..25.Hourly.Earnings", "Median.Hourly.Earnings", "Age.55.64", "Age.65.", "Regional.Completions..2014." )
occupations[,x] <- sapply(occupations[,x], as.character)
occupations[,x] <- sapply(occupations[,x], as.numeric)

#Get rid of na entries
#dd <- is.na(occupations)
#occupations[dd] <- 0 ##change NA to 0



occGrowth <-
  occupations %>% 
  mutate(growthPlusRetirements = c(Age.55.64 + Age.65.+ X2016...2026.Change))%>% #Sum 55-64, 65 plus, Job change
  mutate(education = ifelse(
      Typical.Entry.Level.Education == "Bachelor's degree" | 
      Typical.Entry.Level.Education == "High school diploma or equivalent" |
      Typical.Entry.Level.Education == "Associate's degree" | 
      Typical.Entry.Level.Education == "No formal educational credential" | 
      Typical.Entry.Level.Education == "Postsecondary nondegree award" | 
      Typical.Entry.Level.Education == "Some college, no degree" |
      Typical.On.The.Job.Training   == "Apprenticeship", 1, 0
  ))

  
occGrowth <- occGrowth %>%
  filter(growthPlusRetirements >= 10) %>%
  filter(education > 0)%>%
  #filter(Regional.Completions..2014. > 0)%>%
  arrange(Pct..25.Hourly.Earnings, growthPlusRetirements)
 
  
  occupationsGrowth <- occGrowth%>%
    select(Description, 
         Pct..25.Hourly.Earnings,
         Median.Hourly.Earnings,
         growthPlusRetirements, 
         Typical.Entry.Level.Education, 
         Typical.On.The.Job.Training, 
         Regional.Completions..2014.
         )

  colnames(occupationsGrowth)[2] <- "Pct 25 Hourly Earnings"
  colnames(occupationsGrowth)[4] <- "Jobs Added (2016-2026)"
  colnames(occupationsGrowth)[5] <- "Typical Entry Education"
  colnames(occupationsGrowth)[6] <- "Typical on the Job Training"
  colnames(occupationsGrowth)[7] <- "Regional Completions"
  colnames(occupationsGrowth)[3] <- "Median Hourly Earnings"


# Define a server for the Shiny app
shinyServer(function(input, output) {
  # Filter data based on selections
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <-  occupationsGrowth
    data <- data[data$'Pct 25 Hourly Earnings' >= input$earnings,]
    data <- data[data$"Jobs Added (2016-2026)">= input$growth,]
   }
  ))
  
  occupationsData <- reactive({
    data <-  occupationsGrowth
    data <- data[data$Pct..25.Hourly.Earnings >= input$earnings,]
    data <- data[data$growthPlusRetirements>= input$growth,]
    
  })
 
   
   output$downloadData <- downloadHandler(
     filename = function() {
       paste("trainingOccupations.csv",sep = '')
     },
     content = function(file) {
       write.csv(occupationsData(), file)
     }
   )
})

