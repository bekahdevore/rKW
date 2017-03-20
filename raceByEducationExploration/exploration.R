library(plyr)
library(scales)

fourStateRegion <- read.csv("fourStateData.csv")

youth <- fourStateRegion %>% filter(AGEP >= 16 & AGEP <= 19)


cognitiveDiff <- function(enterData) {
  this         <- enterData %>% dplyr::count(cognitiveDifficulty, wt = PWGTP)
  this$total   <- sum(this$n)
  this$percent <- percent(this$n/this$total)
  this
}


youthHousehold <- youth %>% mutate(householdIncome = ifelse(HINCP < 30000, "< 30k", 
                                                            ifelse((HINCP >= 30000 & HINCP < 60000), "30k - 60k", 
                                                                   ifelse((HINCP >= 60000 & HINCP < 90000), "60k - 90k",
                                                                          ifelse((HINCP >= 90000 & HINCP <120000), "90k - 120k",
                                                                                 ifelse((HINCP >= 120000 & HINCP < 180000), "120k - 180k", 
                                                                                        ifelse((HINCP >= 180000 & HINCP < 240000), "180k - 240k",
                                                                                               ifelse(HINCP >= 240000, "> 240k", "Other")))))))) %>% 
                          mutate(employment = ifelse((ESR == 1 | ESR == 2), "Employed", 
                                                     ifelse(ESR == 3, "Unemployed",
                                                            ifelse(ESR == 6, "Not in Labor Force", "Other")))) %>% 
                          mutate(cognitiveDifficulty = ifelse(DREM == 1, "Yes", "No")) 

unemployed <- youthHousehold %>% filter(employment == "Unemployed")
notInLaborForce <- youthHousehold %>% filter(employment == "Not in Labor Force")
employed <- youthHousehold %>% filter(employment == "Employed")


# cognitiveDiff(unemployed)
# cognitiveDiff(notInLaborForce)


countVariable <- function(enterData, variable) {
  this <- count(enterData, variable, wt = "PWGTP")
  this$percent <- percent(this$freq/sum(this$freq))
  this
}


countVariable(notInLaborForce, "WKL") ## 21% of 16 -19 year olds who are not in the labor force, have worked in the past 12 months
countVariable(unemployed, "WKL") 
countVariable(employed, "WKL")

countVariable(notInLaborForce, "WKW") ## Weeks worked in the past 12 months

countVariable(notInLaborForce, "ENG")
countVariable(employed, "JWTR")
countVariable(notInLaborForce, "NWAV")


countVariable(notInLaborForce, "SCHG") ## Visualize this
countVariable(employed, "SCHG")
countVariable(unemployed, "SCHG")

countVariable(employed, "SEX")
countVariable(unemployed, "SEX")
countVariable(notInLaborForce, "SEX")


countVariable(employed, "ESP")
countVariable(unemployed, "ESP")
countVariable(notInLaborForce, "ESP") # Unemployed and not in labor force 

countVariable(employed, "JWAP")
countVariable(employed, "NAICSP")

