library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(reshape2)
library(cowplot)
library(googlesheets)

load("allData.RData")

puertoRico <- allData %>% filter(area_code == 7200000) %>% select(series_title, occupation_code, value, datatype_code)
# test <- spread(puertoRico, datatype_code, value) 
louisvilleAndPuertoRico <- allData %>% filter(area_code == 0031140 | area_code == 7200000) 

dataTable <- louisvilleAndPuertoRico %>% filter((datatype_code == 04 | datatype_code == 01 | datatype_code == 13)) %>% 
  select(13, 4, 20, 11, 9)

dataTable <- dataTable %>% mutate(datatype_name = ifelse(datatype_code == 01, "Employment", 
                                                         ifelse(datatype_code == 04, "Annual Mean Wage", 
                                                                ifelse(datatype_code == 13, "Annual Median Wage", "Other")))) %>%
  mutate(area = ifelse(area_code == 0031140, "Louisville", 
                       ifelse(area_code == 7200000, "Puerto Rico", "Other")))

dataTable <- dataTable %>% mutate(datapoint = paste(area, ",", datatype_name)) %>% select(1, 8, 2)

dataTable$series_title <- gsub(pattern = "Employment for","", dataTable$series_title, ignore.case=T)
dataTable$series_title <- gsub(pattern = "in All Industries in Louisville/Jefferson County, KY-IN","", dataTable$series_title, ignore.case=T)
dataTable$series_title <- gsub(pattern = "in All Industries in Puerto Rico","", dataTable$series_title, ignore.case=T)
dataTable$series_title <- gsub(pattern = "Occupations","", dataTable$series_title, ignore.case=T)
dataTable$series_title <- gsub(pattern = "Annual mean wage for","", dataTable$series_title, ignore.case=T)
dataTable$series_title <- gsub(pattern = "Annual median wage for" ,"", dataTable$series_title, ignore.case=T)


test0 <- dataTable %>% spread(datapoint, value)
# puertoRicoData <- puertoRicoData %>% spread(datatype_code, value) %>% select(1, 4:6)
# colnames(puertoRicoData)[2] <- "Employment"
# colnames(puertoRicoData)[3] <- "Annual Mean Wage"
# colnames(puertoRicoData)[4] <- "Annual Median Wage"

write.csv(louisvilleData, file = "louisvilleData.csv")

## DATA FOR VISUALIZATION
employment <- louisvilleAndPuertoRico %>% filter(datatype_code == 01)
medianWage <- louisvilleAndPuertoRico %>% filter(datatype_code == 13)

simpleBar <- function(dataHere, titleHere, yAxisLabel, numberFormat, subTitle) {
  g <- ggplot(dataHere, aes(x = reorder(series_title, difference),
                            y = difference, fill = difference > 0)) +
  coord_flip() + labs(title = titleHere, subtitle = subTitle)
  g + geom_bar(stat = "identity") +
    scale_y_continuous(
      #limits = c(0, upperLimit),
      labels = numberFormat) +
    scale_fill_manual(values = setNames(c("#C4D18B", "#FA7089"), c(T, F))) +
    theme_minimal() +
    ylab(yAxisLabel) +
    theme(
      legend.position = "none", 
      axis.title.y = element_blank(),
      axis.ticks = element_blank()
    )
}

str(gs_new())
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
employmentData <- employment
employmentData$series_title <- gsub(pattern = "Employment for","", employmentData$series_title, ignore.case=T)
employmentData$series_title <- gsub(pattern = "in All Industries in Louisville/Jefferson County, KY-IN","", employmentData$series_title, ignore.case=T)
employmentData$series_title <- gsub(pattern = "Occupations","", employmentData$series_title, ignore.case=T)
employmentData$series_title <- gsub(pattern = "in All Industries in Puerto Rico","", employmentData$series_title, ignore.case=T)
employmentData <- employmentData %>% filter(occupation_code != 0) 
employmentData$majorOccupation <- substrRight(employmentData$occupation_code, 4)
employmentData <- employmentData %>% filter(majorOccupation == "0000") %>% select(area_name, series_title, value)
employmentData$value <- as.numeric(employmentData$value)
employmentData <- employmentData %>% spread(area_name, value)
employmentData$difference <- employmentData$`Louisville/Jefferson County, KY-IN`- employmentData$`Puerto Rico`

medianWageData <- medianWage
medianWageData$series_title <- gsub(pattern = "Annual Median wage for","", medianWageData$series_title, ignore.case=T)
medianWageData$series_title <- gsub(pattern = "in All Industries in Louisville/Jefferson County, KY-IN","", medianWageData$series_title, ignore.case=T)
medianWageData$series_title <- gsub(pattern = "Occupations","", medianWageData$series_title, ignore.case=T)
medianWageData$series_title <- gsub(pattern = "in All Industries in Puerto Rico","", medianWageData$series_title, ignore.case=T)
medianWageData <- medianWageData %>% filter(occupation_code != 0) 
medianWageData$majorOccupation <- substrRight(medianWageData$occupation_code, 4)
medianWageData <- medianWageData %>% filter(majorOccupation == "0000") %>% select(area_name, series_title, value)
medianWageData$value <- as.numeric(medianWageData$value)
medianWageData <- medianWageData %>% spread(area_name, value)
medianWageData$difference <- medianWageData$`Louisville/Jefferson County, KY-IN`- medianWageData$`Puerto Rico`


plot1 <- simpleBar(employmentData, "Who employs more people?", "", comma_format(), "Green = Louisville has this many more jobs.\n  Red = Puerto Rico has this many more jobs.")
plot2 <- simpleBar(medianWageData, "Louisville's median wage is this much more\nacross each occupation group",
                   "", dollar_format(), "")
plot_grid(plot1, plot2,
          labels = "", ncol = 1, align = 'v')
