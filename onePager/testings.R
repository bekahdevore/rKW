lausDataManipulation <- function(lausData) {
  lausTEST <- lausMetros %>% filter(year == latestYearBlsLaus)
  lausTEST[,"area_code"] <- substr(lausTEST[,"series_id"], 8,12)
  lausTEST[,"datapoint"] <- substr(lausTEST[,"series_id"], 19,20)
  lausTEST[,"type"] <- substr(lausTEST[,"series_id"], 1,5)
  lausTEST[,"month"] <- substr(lausTEST[,"period"], 2,3)
  
  lausTEST[,"area_code"] <- as.numeric(as.character(lausTEST[,"area_code"]))
  lausTEST[,"month"] <- as.numeric(as.character(lausTEST[,"month"]))
  
  ## LATEST MONTH
  latestMonth <- as.numeric(max(lausTEST$month))
  
  lausTEST <- lausTEST %>% filter((datapoint == "03" | datapoint == "06") & month == latestMonth )
  lausData <- left_join(lausData, lausPeriod, by = "period") 
}