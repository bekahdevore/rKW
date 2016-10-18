library(googleVis)

library(plyr)
library(dplyr)
library(cwhmisc)
library(stringr)


ky               <- read.csv("ss14pky.csv") 
ind              <- read.csv("ss14pin.csv")
socNames         <- read.csv("socBlsNames.csv")
majorNames       <- read.csv("fod1pNames.csv")

kyPUMA           <- (c(1600, 1701,1702,1703,1704,1705, 1706,1800))
inPUMA           <- (c(3500, 3600))

pumaFilter       <- function(dataInput, stateCode, pumaObject){
                     dataInput%>%
                            filter(ST == stateCode)%>%
                            filter(PUMA %in% pumaObject)
}

kentucky         <- pumaFilter(ky,  21, kyPUMA)
indiana          <- pumaFilter(ind, 18, inPUMA)

regionalAreaData <- rbind(kentucky, indiana)

socNames$SOCP    <- str_replace_all(socNames$SOCP, "-", "")

regionalAreaData <- regionalAreaData                           %>%
                     filter(FOD1P != "<NA>")                   %>%
                     filter(SOCP  != "<NA>")                   %>%
                     filter(WAGP  >   0)                       %>%
                     select(FOD1P, SOCP, WAGP, PWGTP, SCHL)

regionalData     <- merge(regionalAreaData, majorNames, by="FOD1P")

#regionalData <- regionalData %>%
 #      mutate(wageRange = ifelse(WAGP < 30000, "Less than $30k", 
  #                               ifelse(WAGP >= 30000 & WAGP < 40000, "$30k - 39k", 
   #                              ifelse(WAGP >= 40000 & WAGP < 50000, "$40k - 49k", 
    #                             ifelse(WAGP >= 50000 & WAGP < 80000, "$50k - 79k", 
     #                            ifelse(WAGP >= 80000 & WAGP < 100000, "$80k - 99k", 
      #                           ifelse(WAGP >= 100000, "$100k or more", "Other" 
       #                          ))))))) %>%
       #select(majorShort, wageRange, PWGTP)

regionalData     <- regionalData %>%
                     mutate(wageRange = ifelse(WAGP >= 47273, "More than Living Wage",
                          ifelse(WAGP < 47273, "Less than Living Wage", "Other" 
                          )))    %>%
                     select(majorShort, wageRange, PWGTP)

regionalData$majorShort <- as.character(regionalData$majorShort)


colors_link       <- c('#8DB6CD', '#698B22')
colors_link_array <- paste0("[", paste0("'", colors_link,"'", collapse = ','), "]")

colors_node       <- c('#000000', '#000000', '#000000', '#000000', '#000000', '#000000', '#000000', '#000000', '#000000', '#000000')
colors_node_array <- paste0("[", paste0("'", colors_node,"'", collapse = ','), "]")
opts              <- paste0("{
                      link: { colorMode: 'target',
                             colors: ", colors_link_array ," },
                             node: { colors: ", colors_node_array ,", label: {fontSize: 16} }
                             }" )


skillsSankey      <- plot(gvisSankey(
                     regionalData, 
                     from="majorShort",
                     to="wageRange",
                     weight = "PWGTP",
                     options=list(height = 800,
                                  width = 1000, 
                                  sankey = opts
                                  
                             )
                     ))

filterAndCount <- function(majorShortName){
                     data <- regionalData %>%
                                   filter(majorShort == majorShortName)
                     count(data, wageRange, wt = PWGTP)
}

percent       <- function(data){
                     data <-  data %>% mutate(per=n/sum(n))
                     print(data)
              }

social    <- filterAndCount("Social Science")
arts      <- filterAndCount("Arts")
business  <- filterAndCount("Business & Finance")
education <- filterAndCount("Education")
physical  <- filterAndCount("Physical Science")
health    <- filterAndCount("Health & Medical")
computer  <- filterAndCount("Computer Science & Mathematics")
engineer  <- filterAndCount("Engineering")
all       <- count(regionalData, wageRange, wt=PWGTP)


percent(all)
percent(social)
percent(arts)
percent(business)
percent(education)
percent(physical)
percent(health)
percent(computer)
percent(engineer)
percent(regionalData)
