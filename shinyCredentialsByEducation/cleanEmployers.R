library(dplyr)
library(stringr)

employers <- read.csv("employers.csv")

employers$Employer <- str_replace_all(
  employers$Employer, "United Parcel Service Incorporated", "United Parcel Service Incorporated (UPS)"
)

employers$Employer <- str_replace_all(employers$Employer, "Louisville Metro Government", "City Of Louisville")
employers$Employer <- str_replace_all(employers$Employer, "A & R Logistics",  "A&R Logistics")
employers$Employer <- str_replace_all(employers$Employer, "A & R Transport",  "A&R Logistics")
employers$Employer <- str_replace_all(employers$Employer, "Aaa East Central", "AAA")
employers$Employer <- str_replace_all(employers$Employer, "Aaa Motor Club",   "AAA")
employers$Employer <- str_replace_all(employers$Employer, "Abc Supply Company Incorporated",   "Abc Supply")
employers$Employer <- str_replace_all(employers$Employer, "Advance Ready Concrete Mix Co",   "Advance Ready Concrete")
employers$Employer <- str_replace_all(employers$Employer, "Advanced Lifeline Services",   "Advanced Lifeline")
employers$Employer <- str_replace_all(employers$Employer, "Advantage Sales & Marketing",  "Advantage Solutions")
employers$Employer <- str_replace_all(employers$Employer, "Aeges Therapies",   "Aegis Therapies")
employers$Employer <- str_replace_all(employers$Employer, "Aegis Therapies",   "TESTING123")
employers$Employer <- str_replace_all(employers$Employer, "Aeg",   "AEG Worldwide")
employers$Employer <- str_replace_all(employers$Employer, "TESTING123",   "Aegis Therapies")
employers$Employer <- str_replace_all(employers$Employer, "American Health Network Incorporated", "American Health Network")
employers$Employer <- str_replace_all(employers$Employer, "American National Insurance Company",             "American National Insurance")
employers$Employer <- str_replace_all(employers$Employer, "American National Insurance Property & Casualty", "American National Insurance")
employers$Employer <- str_replace_all(employers$Employer, "American National Property & Casualty Company",   "American National Insurance")
employers$Employer <- str_replace_all(employers$Employer, "Amerigas Propane",   "Amerigas")
employers$Employer <- str_replace_all(employers$Employer, "Anthem Insurance Company",   "Anthem Blue Cross")
employers$Employer <- str_replace_all(employers$Employer, "Arthur Wright And Associa",  "Arthur Wright And Associates")
employers$Employer <- str_replace_all(employers$Employer, "Arthur Wright And Associatestes",   "Arthur Wright And Associates")
employers$Employer <- str_replace_all(employers$Employer, "Asset Controlservices",   "Asset Control Services")
employers$Employer <- str_replace_all(employers$Employer, "Atlantic Aviation Fbo Holdings Llc",   "Atlantic Aviation")
employers$Employer <- str_replace_all(employers$Employer, "Atria Senior Living Group",   "Atria Senior Living")
employers$Employer <- str_replace_all(employers$Employer, "Averitt Express",   "Averitt")
employers$Employer <- str_replace_all(employers$Employer, "Autoneum North America, Inc",   "Autoneum")
employers$Employer <- str_replace_all(employers$Employer, "Axa Advisors",   "Axa")
employers$Employer <- str_replace_all(employers$Employer, "Babcock Power Incorporated",   "Babcock Power")
employers$Employer <- str_replace_all(employers$Employer, "Badger Daylighting Corporation",   "Badger Daylight")
employers$Employer <- str_replace_all(employers$Employer, "Bassett Furniture Industries",   "Bassett Home Furnishings")
employers$Employer <- str_replace_all(employers$Employer, "Blue & Company Llc",   "Blue & Company")
employers$Employer <- str_replace_all(employers$Employer, "",   "")
employers$Employer <- str_replace_all(employers$Employer, "",   "")
employers$Employer <- str_replace_all(employers$Employer, "",   "")
employers$Employer <- str_replace_all(employers$Employer, "",   "")


test <- as.data.frame(unique(employers$Employer))



#### SPECIAL CASE ... CITY OF LOUISVILLE
louisville <- employers %>% filter(Employer == "Louisville")
## CHANGE TO CITY OF LOUISVILLE
louisville$Employer <- str_replace_all(louisville$Employer, "Louisville", "City Of Louisville")
## FILTER MAIN DATA TO NOT INCLUDE EMPLOYER == LOUISVILLE
employers <- employers %>% filter(Employer != "Louisville" & 
                                  Employer != "Archdiocese Of Los Angeles")
## BIND LOUISVILLE DATA TO EMPLOYER MAIN DATA
employers <- rbind(employers, louisville)
employersList <- unique(employers$Employer)

write.csv(employers,     file = "employers.csv")
write.csv(employersList, file = "employersList.csv")


test <- employers %>% 
  group_by(SOC, Employer) %>%
  tally  %>%
  group_by(Employer) 
