require(blsAPI)
require(rjson)

payload <- list(
  'seriesid'=c('LAUCN040010000000005','LAUCN040010000000006'),
  'startyear'=2010,
  'endyear'=2012,
  'catalog'=FALSE,
  'calculations'=TRUE,
  'annualaverage'=TRUE,
  'registrationKey'='2691a2506f514617823b4e653111fdc9')
response <- blsAPI(payload, 2)
json <- fromJSON(response)


msaData2015 <- read.csv("http://data.bls.gov/cew/data/api/2015/a/industry/10.csv")
msaData2012 <- read.csv("http://data.bls.gov/cew/data/api/2012/a/industry/10.csv")

write.csv(msa1990and2015, file = "msa1990and2015.csv")