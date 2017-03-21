#http://stats.idre.ucla.edu/r/modules/coding-for-categorical-variables-in-regression-models/

hsb2 <- read.csv("http://www.ats.ucla.edu/stat/data/hsb2.csv")
hsb2$race.f <- factor(hsb2$race)
is.factor(hsb2$race.f)
hsb2$race.f[1:15]

summary(lm(write ~ race.f, data = hsb2))

hsb2 <- within(hsb2, {
  race.ct <- C(race.f, treatment)
  print(attributes(race.ct))
})

summary(lm(write ~ race.ct, data = hsb2))

a <- contrasts(hsb2$race.f)
a
