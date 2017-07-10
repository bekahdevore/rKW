library(RCurl)
library(dplyr)



## BLS MEDIAN WAGE 
medianWage <- allData %>% 
  filter((area_code %in% peerAreaCodes$area_code & datatype_code == 13 & occupation_code == 0)) %>% 
  select(20, 4, 13)

medianWage$rank <- rank(desc(medianWage$value))

## BLS LAUS
metros$area_code <- substr(metros$series_id, 8, 12)
metros <- metros %>% filter(area_code %in% peerAreaCodes$area_code)
metros <- left_join(metros, lausSeries, by = "series_id")



## QCEW DATA





