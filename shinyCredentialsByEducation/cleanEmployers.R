library(dplyr)
library(stringr)

employers <- read.csv("employers.csv")
employers <- employers %>% select(5:9)



#employers$Employer <- str_replace_all(employers$Employer, "United Parcel Service Incorporated", "United Parcel Service Incorporated (UPS)")
# employers$Employer <- str_replace_all(employers$Employer, "Louisville Metro Government", "City Of Louisville")
# employers$Employer <- str_replace_all(employers$Employer, "A & R Logistics",  "A&R Logistics")
# employers$Employer <- str_replace_all(employers$Employer, "A & R Transport",  "A&R Logistics")
# employers$Employer <- str_replace_all(employers$Employer, "Aaa East Central", "AAA")
# employers$Employer <- str_replace_all(employers$Employer, "Aaa Motor Club",   "AAA")
# employers$Employer <- str_replace_all(employers$Employer, "Abc Supply Company Incorporated",   "Abc Supply")
# employers$Employer <- str_replace_all(employers$Employer, "Advance Ready Concrete Mix Co",   "Advance Ready Concrete")
# employers$Employer <- str_replace_all(employers$Employer, "Advanced Lifeline Services",   "Advanced Lifeline")
# employers$Employer <- str_replace_all(employers$Employer, "Advantage Sales & Marketing",  "Advantage Solutions")
# employers$Employer <- str_replace_all(employers$Employer, "Aeges Therapies",   "Aegis Therapies")
# employers$Employer <- str_replace_all(employers$Employer, "Aegis Therapies",                                 "TESTING123")
# employers$Employer <- str_replace_all(employers$Employer, "Aeg",                                             "AEG Worldwide")
# employers$Employer <- str_replace_all(employers$Employer, "TESTING123",                                      "Aegis Therapies")
# employers$Employer <- str_replace_all(employers$Employer, "American Health Network Incorporated",            "American Health Network")
# employers$Employer <- str_replace_all(employers$Employer, "American National Insurance Company",             "American National Insurance")
# employers$Employer <- str_replace_all(employers$Employer, "American National Insurance Property & Casualty", "American National Insurance")
# employers$Employer <- str_replace_all(employers$Employer, "American National Property & Casualty Company",   "American National Insurance")
# employers$Employer <- str_replace_all(employers$Employer, "Amerigas Propane",                        "Amerigas")
# employers$Employer <- str_replace_all(employers$Employer, "Anthem Insurance Company",                "Anthem Blue Cross")
# employers$Employer <- str_replace_all(employers$Employer, "Arthur Wright And Associa",               "Arthur Wright And Associates")
# employers$Employer <- str_replace_all(employers$Employer, "Arthur Wright And Associatestes",         "Arthur Wright And Associates")
# employers$Employer <- str_replace_all(employers$Employer, "Asset Controlservices",                   "Asset Control Services")
# employers$Employer <- str_replace_all(employers$Employer, "Atlantic Aviation Fbo Holdings Llc",      "Atlantic Aviation")
# employers$Employer <- str_replace_all(employers$Employer, "Atria Senior Living Group",               "Atria Senior Living")
# employers$Employer <- str_replace_all(employers$Employer, "Averitt Express",                         "Averitt")
# employers$Employer <- str_replace_all(employers$Employer, "Autoneum North America, Inc",             "Autoneum")
# employers$Employer <- str_replace_all(employers$Employer, "Axa Advisors",                            "Axa")
# employers$Employer <- str_replace_all(employers$Employer, "Babcock Power Incorporated",              "Babcock Power")
# employers$Employer <- str_replace_all(employers$Employer, "Badger Daylighting Corporation",          "Badger Daylight")
# employers$Employer <- str_replace_all(employers$Employer, "Bassett Furniture Industries",            "Bassett Home Furnishings")
# employers$Employer <- str_replace_all(employers$Employer, "Blue & Company Llc",                      "Blue & Company")
# employers$Employer <- str_replace_all(employers$Employer, "Atlas Transportation Incorporated",       "Atlas World Group Inc.")
# employers$Employer <- str_replace_all(employers$Employer, "Atlas World Group Incorporated",          "Atlas World Group Inc.")
# employers$Employer <- str_replace_all(employers$Employer, "Baptist Health & Baptist Health Paducah", "Baptist Health")
# employers$Employer <- str_replace_all(employers$Employer, "Baptist Health Kentucky",     "Baptist Health")
# employers$Employer <- str_replace_all(employers$Employer, "Baptist Health Lagrange",     "Baptist Health")
# employers$Employer <- str_replace_all(employers$Employer, "Baptist Health Louisville",   "Baptist Health")
# employers$Employer <- str_replace_all(employers$Employer, "Baptist Health Medical Group","Baptist Health")
# employers$Employer <- str_replace_all(employers$Employer, "Baptist Health Of Kentucky",  "Baptist Health")
# employers$Employer <- str_replace_all(employers$Employer, "Baptist Health Plan",         "Baptist Health")
# employers$Employer <- str_replace_all(employers$Employer, "Baptist Healthcare System",   "Baptist Health")
# employers$Employer <- str_replace_all(employers$Employer, "Baptist Healthlouisville",    "Baptist Health")
# employers$Employer <- str_replace_all(employers$Employer, "Baptist Hospital",            "Baptist Health")
# employers$Employer <- str_replace_all(employers$Employer, "Beacon Health Strategies",  "Beacon Health Options")
# employers$Employer <- str_replace_all(employers$Employer, "Beacon Roofing Supply",                "Beacon Roofing Supply Inc.")
# employers$Employer <- str_replace_all(employers$Employer, "Beacon Roofing Supply Inc. Incorporated",   "Beacon Roofing Supply Inc.")
# employers$Employer <- str_replace_all(employers$Employer, "Boyd Bros",   "Boyd Bros Transportation Inc.")
# employers$Employer <- str_replace_all(employers$Employer, "Boyd Bros Transportation Inc. Transportation Incorporated", "Boyd Bros Transportation Inc")
# employers$Employer <- str_replace_all(employers$Employer, "Boyd Bros Transportation Inc",   "Boyd Bros Transportation Inc.")
# employers$Employer <- str_replace_all(employers$Employer, "Boyd Bros Transportation Inc..",   "Boyd Bros Transportation Inc.")
# employers$Employer <- str_replace_all(employers$Employer, "Boyd Bros Transportation Incorporated",   "Boyd Bros Transportation Inc.")
# employers$Employer <- str_replace_all(employers$Employer, "Boyd Brothers",   "Boyd Bros Transportation Inc.")
# employers$Employer <- str_replace_all(employers$Employer, "Boyd Brothers Transportation",   "Boyd Bros Transportation Inc.")
# employers$Employer <- str_replace_all(employers$Employer, "Boyd Bros Transportation Inc. Transportation",   "Boyd Bros Transportation Inc.")
# employers$Employer <- str_replace_all(employers$Employer, "Boys And Girls Clubs Of Kentuckiana",   "Boys and Girls Club of Kentuckiana")
# employers$Employer <- str_replace_all(employers$Employer, "Boys And Girls Club Of Kentuckiana",   "Boys and Girls Club of Kentuckiana")
# employers$Employer <- str_replace_all(employers$Employer, "Bradley Consulting And Management",  "Bradley Consulting & Management")
# employers$Employer <- str_replace_all(employers$Employer, "Brinks Incorporated",   "Brinks")
# employers$Employer <- str_replace_all(employers$Employer, "Brookdale Incorporated",   "Brookdale Senior Living")
# employers$Employer <- str_replace_all(employers$Employer, "Buddy Mooretrucking", "Buddy Moore Trucking")
# employers$Employer <- str_replace_all(employers$Employer, "Builders Transportation Company, Llc",   "Builders Transportation Company")
# employers$Employer <- str_replace_all(employers$Employer, "Cadence Premier Global",   "Cadence Premier Logistics")
# employers$Employer <- str_replace_all(employers$Employer, "Cadence Premier Logistics / Tm Global Trans",   "Cadence Premier Logistics")
# employers$Employer <- str_replace_all(employers$Employer, "Cafepress",           "Cafepress Inc.")
# employers$Employer <- str_replace_all(employers$Employer, "Cafepress Inc",       "Cafepress Inc.")
# employers$Employer <- str_replace_all(employers$Employer, "Cafepress, Inc",      "Cafepress Inc.")
# employers$Employer <- str_replace_all(employers$Employer, "Cafepress Inc..",     "Cafepress Inc.")
# employers$Employer <- str_replace_all(employers$Employer, "Cafepress Inc.. Inc", "Cafepress Inc.")
# employers$Employer <- str_replace_all(employers$Employer, "Cafepress Inc.., Inc", "Cafepress Inc.")
# employers$Employer <- str_replace_all(employers$Employer, "Cafepress Inc. Inc",   "Cafepress Inc.")
# employers$Employer   <- str_replace_all(employers$Employer, "General Electric Company", "General Electric (GE)")
# employers$Employer   <- str_replace_all(employers$Employer, "Ge Appliance & Lighting",  "General Electric (GE)")
# employers$Employer   <- str_replace_all(employers$Employer, "Ge Appliance And Lighting","General Electric (GE)")
# employers$Employer   <- str_replace_all(employers$Employer, "Ge Appliances",            "General Electric (GE)")
# employers$Employer   <- str_replace_all(employers$Employer, "Ge Lighting",              "General Electric (GE)")
# employers$Employer   <- str_replace_all(employers$Employer, "Ge Healthcare Worldwide",  "GE Healthcare")
# employers$Employer   <- str_replace_all(employers$Employer, "Ge Healthcare",            "GE Healthcare")
# employers$Employer   <- str_replace_all(employers$Employer, "Deloitte", "Deloite")
# employers$Employer   <- str_replace_all(employers$Employer, "Deloite", "Delloite")
# employers$Employer   <- str_replace_all(employers$Employer, "H & Rblock",             "H&R Block")
# employers$Employer   <- str_replace_all(employers$Employer, "H And R Block Seasonal", "H&R Block")
# employers$Employer   <- str_replace_all(employers$Employer, "Jp Morgan Chase",         "JPMorgan Chase")
# employers$Employer   <- str_replace_all(employers$Employer, "JP Morgan Chase Company", "JPMorgan Chase")
# employers$Employer   <- str_replace_all(employers$Employer, "Cardinal Logistics Driver", "Cardinal Logistics")
# employers$Employer   <- str_replace_all(employers$Employer, "Cardinal Logistics Management", "Cardinal Logistics")
# employers$Employer   <- str_replace_all(employers$Employer, "Cardinal / Greatwide Logistics", "Cardinal Logistics")
# employers$Employer   <- str_replace_all(employers$Employer, "Cardno Usa", "Cardno")
# employers$Employer   <- str_replace_all(employers$Employer, "Carewise Health And Certilytics", "Carewise Health")
# employers$Employer   <- str_replace_all(employers$Employer, "Carmax Business Services, Llc", "Carmax")
# employers$Employer   <- str_replace_all(employers$Employer, "Catholic Health Inititiaves", "Catholic Health Initiatives")
# employers$Employer   <- str_replace_all(employers$Employer, "Catholic Healthinitiatives",  "Catholic Health Initiatives")
# employers$Employer   <- str_replace_all(employers$Employer, "Cdl Job Offers",      "CDL Labor Logistics")
# employers$Employer   <- str_replace_all(employers$Employer, "Cdl Labor Logistics", "CDL Labor Logistics")
# employers$Employer   <- str_replace_all(employers$Employer, "Cdl Trans",           "CDL Labor Logistics")
# employers$Employer   <- str_replace_all(employers$Employer, "Cenveo Discount Labels", "Cenveo")
# employers$Employer   <- str_replace_all(employers$Employer, "Cenveo Incorporated",    "Cenveo")
# employers$Employer   <- str_replace_all(employers$Employer, "Charter Communication",   "Charter Communications")
# employers$Employer   <- str_replace_all(employers$Employer, "Charter Communicationss", "Charter Communications")
# employers$Employer   <- str_replace_all(employers$Employer, "Clarke Power Services / Vehicare Fleet Maintenance", "Clarke Power Services")
# employers$Employer   <- str_replace_all(employers$Employer, "Clean Harbors Environmental Services", "Clean Harbors")
# employers$Employer   <- str_replace_all(employers$Employer, "Compassairlines", "Compass Airlines")
# employers$Employer   <- str_replace_all(employers$Employer, "Cornerstone", "Cornerstone Company Incorporated")
# employers$Employer   <- str_replace_all(employers$Employer, "Cornerstone Company Incorporated Company Incorporated", "Cornerstone Company Incorporated")
# employers$Employer   <- str_replace_all(employers$Employer, "Correctional Healthcare Companies", "Correction Care Solutions, Llc")
# employers$Employer   <- str_replace_all(employers$Employer, "Cpa Firm", "CPA Firm")
# employers$Employer   <- str_replace_all(employers$Employer, "Cpa",      "CPA Firm")
# employers$Employer   <- str_replace_all(employers$Employer, "Baldwin CPA Firms", "Baldwin Cpas")
# employers$Employer   <- str_replace_all(employers$Employer, "Top Tier CPA Firm And Advisory Firm", "Top Tier Cpa And Advisory Firm")
# employers$Employer   <- str_replace_all(employers$Employer, "Kentucky Society Of CPA Firms", "Kentucky Society Of Certified Public Accountants (CPA)")
# employers$Employer   <- str_replace_all(employers$Employer, "Lindemeyer CPA Firm", "Lindemeyer Cpa")
# employers$Employer   <- str_replace_all(employers$Employer, "Verbeck & Kaleher CPA Firm", "Verbeck & Kaleher Cpa")
# employers$Employer   <- str_replace_all(employers$Employer, "Crs Student",             "CRST International")
# employers$Employer   <- str_replace_all(employers$Employer, "Crst Expedited",          "CRST International")
# employers$Employer   <- str_replace_all(employers$Employer, "Crst Incorporated",       "CRST International")
# employers$Employer   <- str_replace_all(employers$Employer, "Crst Lease Purchase",     "CRST International")
# employers$Employer   <- str_replace_all(employers$Employer, "Crst Leasepurchase",      "CRST International")
# employers$Employer   <- str_replace_all(employers$Employer, "Crst Services",           "CRST International")
# employers$Employer   <- str_replace_all(employers$Employer, "Crst Services, Inc",      "CRST International")
# employers$Employer   <- str_replace_all(employers$Employer, "Crst Student",            "CRST International")
# employers$Employer   <- str_replace_all(employers$Employer, "CRST International, Inc", "CRST International")
# employers$Employer   <- str_replace_all(employers$Employer, "Crst",                    "CRST International")
# employers$Employer   <- str_replace_all(employers$Employer, "Csx", "Csx Transportation")
# employers$Employer   <- str_replace_all(employers$Employer, "Csx Transportation Transportation", "Csx Transportation")
# employers$Employer   <- str_replace_all(employers$Employer, "Daifuku Incorporated",                  "Daifuku")
# employers$Employer   <- str_replace_all(employers$Employer, "Daifuku North America Holding Company", "Daifuku")
# employers$Employer   <- str_replace_all(employers$Employer, "Daifuku Web Holding Company",           "Daifuku")
# employers$Employer   <- str_replace_all(employers$Employer, "Daikin Mcquay", "Daikin Applied")
# employers$Employer   <- str_replace_all(employers$Employer, "Dana",                         "Dana Corporation")
# employers$Employer   <- str_replace_all(employers$Employer, "Dana Corporation Corporation", "Dana Corporation")
# employers$Employer   <- str_replace_all(employers$Employer, "Dart Transitcompany", "Dart Transit Company")
# employers$Employer   <- str_replace_all(employers$Employer, "Dart",                "Dart Transit Company")
# employers$Employer   <- str_replace_all(employers$Employer, "Dart Transit Company Transit Company", "Dart Transit Company")
# employers$Employer   <- str_replace_all(employers$Employer, "Dawn Food Products", "Dawn Food")
# employers$Employer   <- str_replace_all(employers$Employer, "Dawn Food", "Dawn Foods")
# employers$Employer   <- str_replace_all(employers$Employer, "Dawn Foodss", "Dawn Foods")
# employers$Employer   <- str_replace_all(employers$Employer, "Decker Truck Line Incorporated", "Decker Truck Line")
# employers$Employer   <- str_replace_all(employers$Employer, "Diageo North America", "Diageo")

test <- as.data.frame(unique(employers$Employer))
employers$Employer   <- str_replace_all(employers$Employer, "", "")
employers$Employer   <- str_replace_all(employers$Employer, "", "")
employers$Employer   <- str_replace_all(employers$Employer, "", "")
employers$Employer   <- str_replace_all(employers$Employer, "", "")
employers$Employer   <- str_replace_all(employers$Employer, "", "")
employers$Employer   <- str_replace_all(employers$Employer, "", "")
employers$Employer   <- str_replace_all(employers$Employer, "", "")
employers$Employer   <- str_replace_all(employers$Employer, "", "")
employers$Employer   <- str_replace_all(employers$Employer, "", "")
employers$Employer   <- str_replace_all(employers$Employer, "", "")
employers$Employer   <- str_replace_all(employers$Employer, "", "")
employers$Employer   <- str_replace_all(employers$Employer, "", "")


##Automotive TO Automotive Resources International
## Blue TO Blue & Company


#### SPECIAL CASE ... CITY OF LOUISVILLE
# louisville <- employers %>% filter(Employer == "Louisville")
# ## CHANGE TO CITY OF LOUISVILLE
# louisville$Employer <- str_replace_all(louisville$Employer, "Louisville", "City Of Louisville")
# ## FILTER MAIN DATA TO NOT INCLUDE EMPLOYER == LOUISVILLE
# employers <- employers %>% filter(Employer != "Louisville" & 
#                                   Employer != "Archdiocese Of Los Angeles")
# ## BIND LOUISVILLE DATA TO EMPLOYER MAIN DATA
# employers <- rbind(employers, louisville)
employersList <- as.data.frame(unique(employers$Employer))

write.csv(employers,     file = "testEmployers.csv")
write.csv(employersList, file = "testEmployersList.csv")

write.csv(employers,     file = "employers.csv")
write.csv(employersList, file = "employersList.csv")

test <- employers %>% 
  group_by(SOC, Employer) %>%
  tally  %>%
  group_by(Employer) 
