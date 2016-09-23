#Load libraries, RCurl and RJSONIO are required to load data, ggplot2 is only requred for the visualization
library(blsAPI)
library(RCurl)
library(RJSONIO)
library(ggplot2)
library(dplyr)
library(scales)

#Start and End Years for BLS data and Month to Filter
startYear <- 2006
endYear   <- 2016
blsMonth  <- 'July'
apiKey    <- '2691a2506f514617823b4e653111fdc9'
#,'startyear'= startYear, 'endyear' = endYear
#Load data and parse it out with RJSONIO
#BLS series LNS14000000 returns the Seasonally Adjusted Unemployment Rate for individuals 16 and over.

###########################################################################################################
                                                 ## FUNCTIONS ##
###########################################################################################################
#Convert Data from BLS API to DataFrame
apiDataConverter <- function(seriesId) {
                     payload     <- list('seriesid'        = seriesId,
                                         'startyear'       = startYear, 
                                         'endyear'         = endYear, 
                                         'registrationKey' = apiKey)
                     bls.content <- blsAPI(payload)
                     bls.json    <- fromJSON(bls.content, simplify=TRUE)
                     tmp         <- bls.json$Results[[1]][[1]]
                     bls.df      <- data.frame(year=sapply(tmp$data,"[[","year"),
                                          period=sapply(tmp$data,"[[","period"),
                                          periodName=sapply(tmp$data,"[[","periodName"),
                                          value=as.numeric(sapply(tmp$data,"[[","value")),
                                          stringsAsFactors=FALSE)
}

###########################################################################################################
                                          ## CREATE DATAFRAMES AND FORMAT ##
###########################################################################################################

#### Labor Force Series ID's ###
JeffersonCountyLaborForce <- apiDataConverter("LAUCN211110000000006")
KentuckyLaborForce        <- apiDataConverter("LAUST210000000000006")
LouisvilleLaborForce      <- apiDataConverter("LAUMT213114000000006")
UnitedStatesLaborForce    <- apiDataConverter("LNU01000000")

### Unemployment Rate Series ID's ###
JeffersonCountyUnemployment <- apiDataConverter("LAUCN211110000000003")
KentuckyUnemployment        <- apiDataConverter("LAUST210000000000003")
LouisvilleUnemployment      <- apiDataConverter("LAUMT213114000000003")
UnitedStatesUnemployment    <- apiDataConverter("LNU04000000")




#Add area variable to data
### Labor Force ###
JeffersonCountyLaborForce$area   <- "Jefferson County"
KentuckyLaborForce$area          <- "Kentucky"
LouisvilleLaborForce$area        <- "Louisville"
UnitedStatesLaborForce$area      <- "United States"
#UnitedStatesLaborForce$value    <- (UnitedStatesLaborForce$value)*1000

### Unemployment ###
JeffersonCountyUnemployment$area <- "Jefferson County"
KentuckyUnemployment$area        <- "Kentucky"
LouisvilleUnemployment$area      <- "Louisville"
UnitedStatesUnemployment$area    <- "United States"


################### Combine dataframes
### Labor Force ###
laborForceData       <- rbind(JeffersonCountyLaborForce, 
                        KentuckyLaborForce, 
                        LouisvilleLaborForce)

### Unemployment ###
unemploymentRateData <- rbind(JeffersonCountyUnemployment, 
                        KentuckyUnemployment, 
                        LouisvilleUnemployment, 
                        UnitedStatesUnemployment)

################## Filter for specified month
### Labor Force ###
laborForceData       <- laborForceData%>%
                            filter(periodName == blsMonth)

### Unemployment ###
unemploymentRateData <- unemploymentRateData%>%
                            filter(periodName == blsMonth)


###################### Convert values to numeric 
### Labor Force ###
laborForceData$value         <- as.numeric(as.character(laborForceData$value))
UnitedStatesLaborForce$value <- as.numeric(as.character(UnitedStatesLaborForce$value))

### Unemployment ###
unemploymentRateData$value   <- as.numeric(as.character(unemploymentRateData$value))


###########################################################################################################
                                                 ## PLOTS ##
###########################################################################################################

#Labor Force Plot 
ggplot(laborForceData, aes(x=year, y=value, group=area, color=area)) +
       geom_line(size=1.2) +
       geom_point(size=3)  +
       xlab("Year")        +
       ylab("Labor Force") +
       labs(title = "Size of Labor Force, July 2006-2016") +
       scale_y_continuous(labels = comma)                  +
       scale_color_hue(
              name = "test", 
              breaks = c("Kentucky", "Louisville", "Jefferson County"),
              labels = c("Kentucky", "Louisville", "Jefferson County")
       ) +
       theme(plot.title = element_text(size = rel(2.5)), 
             axis.title.y = element_text(size = rel(2)), 
             axis.title.x = element_text(size = rel(2)),
             axis.text.y  = element_text(size = 18),
             axis.text.x  = element_text(size = 18, angle = 45),
             axis.ticks   = element_blank(), 
             legend.position = c(.8, .63), 
             legend.background = element_blank(), 
             #legend.background = element_rect(color = "black"),
             legend.text  = element_text(size = 18), 
             legend.title = element_blank() 
       )

#United States Labor Force Plot


#Unemployment Plot 
ggplot(unemploymentRateData, aes(x=year, y=value, group=area, color=area)) +
       geom_line(size=1.2) +
       geom_point(size=3)  +
       xlab("Year")        +
       ylab("Unemployment Rate") +
       labs(title = "Unemployment Rate, July 2006-2016") +
       scale_y_continuous(labels = comma)                  +
       scale_color_hue(
              #name = "test", 
              breaks = c("United States", "Kentucky", "Louisville", "Jefferson County"),
              labels = c("United States", "Kentucky", "Louisville", "Jefferson County")
       ) +
       theme(plot.title = element_text(size = rel(2.5)), 
             axis.title.y = element_text(size = rel(2)), 
             axis.title.x = element_text(size = rel(2)),
             axis.text.y  = element_text(size = 18),
             axis.text.x  = element_text(size = 18, angle = 45),
             axis.ticks   = element_blank(), 
             legend.position = c(.8, .8), 
             legend.background = element_blank(), 
             #legend.background = element_rect(color = "black"),
             legend.text  = element_text(size = 18), 
             legend.title = element_blank() 
       )

