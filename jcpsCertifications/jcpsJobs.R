library(RCurl)
library(dplyr)


######### FUNCTIONS

removeReplaceSomething <- function(dataToUse, replaceThis, withThis){
                            dataToUse <- as.data.frame(lapply(dataToUse, function(x){
                                                gsub(replaceThis, 
                                                     withThis, x)}))
}

occupationList        <- function(dataToEnter){
                            jcpsCredentials <- allOccupations %>% filter(SOC %in% dataToEnter)
                            jcpsCredentials <- unique(jcpsCredentials["Description"])
                            jcpsCredentials
}


growthOutput          <- function(dataToEnter, turnoverRate){
                            jcpsCredentials             <- allOccupations %>% filter(SOC %in% dataToEnter)
                            jcpsCredentials$SOC         <- as.character(jcpsCredentials$SOC)
                            
                            jcpsCredentials             <- removeReplaceSomething(jcpsCredentials, "<10", "5")
                            jcpsCredentials             <- removeReplaceSomething(jcpsCredentials, ",", "")
                            
                            variables                   <- c(colnames(jcpsCredentials)[3],
                                                             colnames(jcpsCredentials)[5:12],
                                                             colnames(jcpsCredentials)[14])
                            
                            jcpsCredentials[,variables] <- lapply(jcpsCredentials[,variables] , as.character)
                            jcpsCredentials[,variables] <- lapply(jcpsCredentials[,variables] , as.numeric)
                            
                            turnover2017                <- sum(jcpsCredentials$X2017.Jobs)* turnoverRate
                            turnover2018                <- sum(jcpsCredentials$X2018.Jobs)* turnoverRate
                            turnover2019                <- sum(jcpsCredentials$X2019.Jobs)* turnoverRate
                            turnover2020                <- sum(jcpsCredentials$X2020.Jobs)* turnoverRate
                            turnover2021                <- sum(jcpsCredentials$X2021.Jobs)* turnoverRate
                            
                            sum2016                     <- sum(jcpsCredentials$X2016.Jobs)
                            sum2017                     <- sum(jcpsCredentials$X2017.Jobs)
                            sum2018                     <- sum(jcpsCredentials$X2018.Jobs)
                            sum2019                     <- sum(jcpsCredentials$X2019.Jobs)
                            sum2020                     <- sum(jcpsCredentials$X2020.Jobs)
                            sum2021                     <- sum(jcpsCredentials$X2021.Jobs)
                            sum55                       <- sum(jcpsCredentials$Age.55.64)
                            sum65                       <- sum(jcpsCredentials$Age.65.)
                            
                            one                         <- round((sum2017 - sum2016) + ((sum65)*.4) + turnover2017)
                            two                         <- round((sum2018 - sum2017) + ((sum65)*.6) + turnover2018) 
                            three                       <- round((sum2019 - sum2018) + ((sum55)*.2) + turnover2019) 
                            four                        <- round((sum2020 - sum2019) + ((sum55)*.3) + turnover2020) 
                            five                        <- round((sum2021 - sum2020) + ((sum55)*.5) + turnover2021) 
                            
                            #healthData <- as.data.frame(cbind(one, two, three, four, five))
                            
                            #colnames(healthData)[1] <- "2016-2017"
                            #colnames(healthData)[2] <- "2017-2018"
                            #colnames(healthData)[3] <- "2018-2019"
                            #colnames(healthData)[4] <- "2019-2020"
                            #colnames(healthData)[5] <- "2020-2021"
                            
                            dataToEnter                 <- as.data.frame(cbind(one, two, three, four, five))
                            
                            colnames(dataToEnter)[1]    <- "2016-2017"
                            colnames(dataToEnter)[2]    <- "2017-2018"
                            colnames(dataToEnter)[3]    <- "2018-2019"
                            colnames(dataToEnter)[4]    <- "2019-2020"
                            colnames(dataToEnter)[5]    <- "2020-2021"
                            
                            print(dataToEnter)
}
##################################################################################################################

########## DATA CONNECTIONS
dataConnection        <- getURL('https://docs.google.com/spreadsheets/d/e/2PACX-1vSPSZDIHBIb26yy1QEbnL9NvbVOqoiQj2yVFQryDjjtClEXzCRtDXef-wUFGygLKfneWjRHNvkWU1a2/pub?gid=0&single=true&output=csv')
originalData          <- read.csv(textConnection(dataConnection))
splitSOC              <- as.data.frame(t(sapply(originalData$SOC, function(x) substring(x, first=c(1, 1), last=c(2, 7)))))
colnames(splitSOC)[1] <- "socGroup"
colnames(splitSOC)[2] <- "SOC"
allOccupations        <- left_join(originalData, splitSOC, by = "SOC")
#healthCare     <- allOccupations %>% filter(socGroup == 31 | 
#                                            socGroup == 29)




############################## OCCUPATION LISTS BY SOC CODES ##################################################
######## Healthcare
# healthOccupations <- c('29-2041',
#                        '29-2052',
#                        '29-2099',
#                        '29-9099',
#                        '31-1011',
#                        '31-1014',
#                        '31-9091',
#                        '31-9092',
#                        '31-9095',
#                        '31-9097')


######## Manufacturing
# manufacturingOccupations <- c("49-3011",
#                               "53-2011",
#                               "49-3023",
#                               "17-3023",
#                               "49-2094",
#                               "17-3024",
#                               "17-3029",
#                               "49-9021",
#                               "47-3011",
#                               "47-3012",
#                               "47-3013",
#                               "49-9098",
#                               "51-9198",
#                               "17-3026",
#                               "49-9041",
#                               "51-4041",
#                               "49-9071",
#                               "49-9043",
#                               "17-3013",
#                               "17-3027",
#                               "51-4121",
#                               "51-4122")

govLawPublicAdmin <- c("55-9999",
                       "29-2041",
                       "33-2011",
                       "33-2021",
                       "43-5031",
                       "23-2011",
                       "33-3021",
                       "33-9099",
                       "33-9032",
                       "33-3012",
                       "33-3051")





################## OUTPUT 
#govLawPublicAdminData <- growthOutput(govLawPublicAdmin, .0151)
#healthData        <- growthOutput(healthOccupations,        .025)
#manufacturingData <- growthOutput(manufacturingOccupations, .02125)

#healthList <- occupationList(healthOccupations)
#manuList   <- occupationList(manufacturingOccupations)
govList    <- occupationList(govLawPublicAdmin)
colnames(govList)[1] <- "x"


#write.csv(healthData,        file = "healthData.csv")
#write.csv(manufacturingData, file = "manufacturingData.csv")
#write.csv(govLawPublicAdminData, file = "govData.csv")

#write.csv(healthList,    file = "healthOccupationList.csv")
#write.csv(manuList,      file = "manufacturingOccupationList.csv")
write.csv(govList,        file = "govOccupationList.csv")

