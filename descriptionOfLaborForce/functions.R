## FUNCTIONS

percent <- function(data){
  data <-  data %>% mutate(per=n/sum(n))
  print(data)
}

variableWF <- function(variable1, variable2) {
  t <- cleanData %>% group_by_(variable1, variable2)
  t <- count(t, wt = PWGTP)
  t <- spread(t, wf, n)
}


