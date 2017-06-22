## FUNCTIONS

## Used when retrieving data for MySql
statement <- function(place) {
  paste("SELECT *", "FROM", place, ";")
}

percent <- function(data){
  data <-  data %>% mutate(per=n/sum(n))
  print(data)
}