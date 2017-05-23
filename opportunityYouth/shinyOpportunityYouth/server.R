#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(treemap)
library(dplyr)
library(datasets)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

load("kentucky.RData")
load("peerCities.RData")
load("louisvilleMSA.RData")
  
datasetInput <- reactive({
  switch(input$dataset,
         "Kentucky" = kentucky,
         "Peer Cities" = peerCities,
         "Louisville MSA" = louisvilleMSA)
})

  output$race <- renderPlot({
    opportunityYouth <- datasetInput()
        race <- count(opportunityYouth, race, wt=PWGTP)
        race <- na.omit(race)
        race$percent <- scales::percent(race$n/sum(race$n))
        race$label <- paste(race$race, "\n", race$percent)
        treemap(race, "label", "n", title = "Race")
  })
  
  output$disability <- renderPlot({
    opportunityYouth <- datasetInput()      
        disability <- count(opportunityYouth, disability, wt=PWGTP)
        disability <- na.omit(disability)
        disability$percent <- scales::percent(disability$n/sum(disability$n))
        disability$label <- paste(disability$disability, "\n", disability$percent)  
        treemap(disability, "label", "n", title = "Disability")
  })
  
  output$sex <- renderPlot({
    opportunityYouth <- datasetInput()      
        sex <- count(opportunityYouth, sex, wt=PWGTP)
        sex <- na.omit(sex)
        sex$percent <- scales::percent(sex$n/sum(sex$n))
        sex$label <- paste(sex$sex, "\n", sex$percent)  
        treemap(sex, "label", "n", title = "Sex")
  })
  
  output$workStatus <- renderPlot({
    opportunityYouth <- datasetInput()
        workStatus <- count(opportunityYouth, employmentStatus, wt=PWGTP)
        workStatus <- na.omit(workStatus)
        workStatus$percent <- scales::percent(workStatus$n/sum(workStatus$n))
        workStatus$label <- paste(workStatus$employmentStatus, "\n", workStatus$percent)
        treemap(workStatus, "label", "n", title = "Work Status")
  })
  
  output$age <- renderPlot({
    opportunityYouth <- datasetInput()
        age <- count(opportunityYouth, age, wt=PWGTP)
        age <- na.omit(age)
        age$percent <- scales::percent(age$n/sum(age$n))
        age$label <- paste(age$age, "\n", age$percent)
        treemap(age, "label", "n", title = "Age")
  })
  
  output$lastWorked <- renderPlot({
    opportunityYouth <- datasetInput()         
        lastWorked <- count(opportunityYouth, lastWorked, wt=PWGTP)
        lastWorked <- na.omit(lastWorked)
        lastWorked$percent <- scales::percent(lastWorked$n/sum(lastWorked$n))
        lastWorked$label <- paste(lastWorked$lastWorked, "\n", lastWorked$percent) 
        treemap(lastWorked, "label", "n", title = "Last Worked")
  })
  
  output$foodStamps <- renderPlot({
    opportunityYouth <- datasetInput()
        foodStamps <- count(opportunityYouth, foodStamps, wt=PWGTP)
        foodStamps <- na.omit(foodStamps)
        foodStamps$percent <- scales::percent(foodStamps$n/sum(foodStamps$n))
        foodStamps$label <- paste(foodStamps$foodStamps, "\n", foodStamps$percent)  
        treemap(foodStamps, "label", "n", title = "Food Stamps")
  })
  
  output$socialSecurity <- renderPlot({
    opportunityYouth <- datasetInput()
        socialSecurity <- count(opportunityYouth, socialSecurity, wt=PWGTP)
        socialSecurity <- na.omit(socialSecurity)
        socialSecurity$percent <- scales::percent(socialSecurity$n/sum(socialSecurity$n))
        socialSecurity$label <- paste(socialSecurity$socialSecurity, "\n", socialSecurity$percent)  
        treemap(socialSecurity, "label", "n", title = "Social Security")
  })
  
  output$ssi <- renderPlot({
    opportunityYouth <- datasetInput()
        supplementarySecurityIncome <- count(opportunityYouth, supplementarySecurityIncome, wt=PWGTP)
        supplementarySecurityIncome <- na.omit(supplementarySecurityIncome)
        supplementarySecurityIncome$percent <- scales::percent(supplementarySecurityIncome$n/sum(supplementarySecurityIncome$n))
        supplementarySecurityIncome$label <- paste(supplementarySecurityIncome$supplementarySecurityIncome, "\n", supplementarySecurityIncome$percent)  
        treemap(supplementarySecurityIncome, "label", "n", title = "Supplementary Security Income")
  })
  
  output$children <- renderPlot({
    opportunityYouth <- datasetInput()
        children <- count(opportunityYouth, children, wt=PWGTP)
        children <- na.omit(children)
        children$percent <- scales::percent(children$n/sum(children$n))
        children$label <- paste(children$children, "\n", children$percent)  
        treemap(children, "label", "n", title = "Children")
  })      
  
  output$child12Months <- renderPlot({
    opportunityYouth <- datasetInput()      
        childWithin12Months <- count(opportunityYouth, childWithin12Months, wt=PWGTP)
        childWithin12Months <- na.omit(childWithin12Months)
        childWithin12Months$percent <- scales::percent(childWithin12Months$n/sum(childWithin12Months$n))
        childWithin12Months$label <- paste(childWithin12Months$childWithin12Months, "\n", childWithin12Months$percent)  
        treemap(childWithin12Months, "label", "n", title = "Child within 12 Months")
  })
  
  output$cognitive <- renderPlot({
    opportunityYouth <- datasetInput()
        cognitiveDifficulty <- count(opportunityYouth, cognitiveDifficulty, wt=PWGTP)
        cognitiveDifficulty <- na.omit(cognitiveDifficulty)
        cognitiveDifficulty$percent <- scales::percent(cognitiveDifficulty$n/sum(cognitiveDifficulty$n))
        cognitiveDifficulty$label <- paste(cognitiveDifficulty$cognitiveDifficulty, "\n", cognitiveDifficulty$percent)  
        treemap(cognitiveDifficulty, "label", "n", title = "Cognitive Difficulty")
  })
  
  output$school <- renderPlot({
    opportunityYouth <- datasetInput()
        school <- count(opportunityYouth, education, wt=PWGTP)
        school <- na.omit(school)
        school$percent <- scales::percent(school$n/sum(school$n))
        school$label <- paste(school$education, "\n", school$percent)
        treemap(school, "label", "n", title = "Educational Attainment")
  })

})
 

  


