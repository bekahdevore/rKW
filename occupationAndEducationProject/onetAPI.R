library(httr)
library(xml2)
library(XML)
library(plyr)
library(dplyr)


response <- GET('https://services.onetcenter.org/ws/mnm/careers/17-2051.00/skills',
              authenticate("kentuckianaworks","5529fjb" ))

doc <- xmlInternalTreeParse(content(response, type = 'text'))
xmlTop <- xmlRoot(doc)
xmlTop[[2]][[element]][[text]]
text <- xmlToList(xmlTop)
sapply(text$group$element$text, data.frame)


skills <- ldply(xmlToList(xmlTop[['group']][["element"]][['text']]), data.frame)
skills <- ldply(xmlToList(xmlTop), data.frame)
View(text)
