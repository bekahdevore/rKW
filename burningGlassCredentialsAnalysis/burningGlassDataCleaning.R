library(dplyr)

## Main 2015
main2015.01 <- read.delim("C:/Users/rdevore/Desktop/mainTextFiles/main2015/main2015-01.txt")
main2015.02 <- read.delim("C:/Users/rdevore/Desktop/mainTextFiles/main2015/main2015-02.txt")
main2015.03 <- read.delim("C:/Users/rdevore/Desktop/mainTextFiles/main2015/main2015-03.txt")
main2015.04 <- read.delim("C:/Users/rdevore/Desktop/mainTextFiles/main2015/main2015-04.txt")
main2015.05 <- read.delim("C:/Users/rdevore/Desktop/mainTextFiles/main2015/main2015-05.txt")
main2015.06 <- read.delim("C:/Users/rdevore/Desktop/mainTextFiles/main2015/main2015-06.txt")
main2015.07 <- read.delim("C:/Users/rdevore/Desktop/mainTextFiles/main2015/main2015-07.txt")
main2015.08 <- read.delim("C:/Users/rdevore/Desktop/mainTextFiles/main2015/main2015-08.txt")
main2015.09 <- read.delim("C:/Users/rdevore/Desktop/mainTextFiles/main2015/main2015-09.txt")
main2015.10 <- read.delim("C:/Users/rdevore/Desktop/mainTextFiles/main2015/main2015-10.txt")
main2015.11 <- read.delim("C:/Users/rdevore/Desktop/mainTextFiles/main2015/main2015-11.txt")
main2015.12 <- read.delim("C:/Users/rdevore/Desktop/mainTextFiles/main2015/main2015-12.txt")

## Main 2016
main2016.01 <- read.delim("C:/Users/rdevore/Desktop/mainTextFiles/main2015/Main 2016-01.txt")
main2016.02 <- read.delim("C:/Users/rdevore/Desktop/mainTextFiles/main2015/Main 2016-02.txt")
main2016.03 <- read.delim("C:/Users/rdevore/Desktop/mainTextFiles/main2015/Main 2016-03.txt")
main2016.04 <- read.delim("C:/Users/rdevore/Desktop/mainTextFiles/main2015/Main 2016-04.txt")
main2016.05 <- read.delim("C:/Users/rdevore/Desktop/mainTextFiles/main2015/Main 2016-05.txt")
main2016.06 <- read.delim("C:/Users/rdevore/Desktop/mainTextFiles/main2015/Main 2016-06.txt")


###############################################################################################
                                        ## FUNCTIONS ##
###############################################################################################
louisvilleFilter <- function(dataFrameName) {
                            dataFrameName %>% 
                                   filter(MSA == 31140)
                     }

main2015.01 <- louisvilleFilter(main2015.01)
main2015.02 <- louisvilleFilter(main2015.02)
main2015.03 <- louisvilleFilter(main2015.03)
main2015.04 <- louisvilleFilter(main2015.04)
main2015.05 <- louisvilleFilter(main2015.05)
main2015.06 <- louisvilleFilter(main2015.06)
