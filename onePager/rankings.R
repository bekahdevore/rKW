###  RANKINGS, WOULD BE CLEANER IF UPDATED WITH A FUNCTION
lfprRanking <- allData %>%
  mutate("Rank" = rank(-`Labor Force Participation Rate`))
lfprRanking <- lfprRanking %>% select(1, 2, 10) %>% 
  filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Labor Force Participation")
colnames(lfprRanking)[2] <- "Value"

lfSizeRanking <- allData %>% 
  mutate("Rank" = rank(-`Labor Force Size`)) %>% 
  select(1, 3, 10) %>% 
  filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Labor Force Size")
colnames(lfSizeRanking)[2] <- "Value"

medianHomeValueRanking <- allData %>% 
  mutate("Rank" = rank(-`Median Home Value`)) %>% 
  select(1, 4, 10) %>% 
  filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Median Home Value")
colnames(medianHomeValueRanking)[2] <- "Value"

medianHouseholdWageRanking <- allData %>% 
  mutate("Rank" = rank(-`Median Household Wage`)) %>% 
  select(1, 5, 10)  %>% 
  filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Median Household Wage")
colnames(medianHouseholdWageRanking)[2] <- "Value"

medianMonthlyRentRank <- allData %>% 
  mutate("Rank" = rank(-`Median Monthly Rent`)) %>% 
  select(1, 6, 10) %>% 
  filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Median Monthly Rent")
colnames(medianMonthlyRentRank)[2] <- "Value"

annualMedianWageRank <- allData %>% 
  mutate("Rank" = rank(-`Annual Median Wage (USD)`)) %>% 
  select(1, 7, 10) %>% 
  filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Annual Median Wage")
colnames(annualMedianWageRank)[2] <- "Value"

populationRanking <- allData %>%
  mutate("Rank" = rank(-Population)) %>%
  select(1, 8, 10)  %>% 
  filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Population")
colnames(populationRanking)[2] <- "Value"

unemploymentRateRanking <- allData %>% 
  mutate("Rank" = rank(`Unemployment Rate`)) %>%
  select(1, 9 , 10) %>% 
  filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Unemployment Rate")
colnames(unemploymentRateRanking)[2] <- "Value"


louisvilleRankings <- rbind(lfprRanking, lfSizeRanking,
                            medianHomeValueRanking, medianHouseholdWageRanking,
                            medianMonthlyRentRank, annualMedianWageRank, 
                            populationRanking, unemploymentRateRanking)

louisvilleRankings <- louisvilleRankings %>% select(4, 3, 2)
louisvilleRankings$Value <- round(louisvilleRankings$Value)
louisvilleRankings <- format(louisvilleRankings, big.mark = ',')
