setwd("Rstatistics")
getwd()

#read states data
states.data <- readRDS("datasets/states.rds")

#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Examine Data before fitting the models
# Start by examining data to check for problems

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)


## Plot data before fitting models, look for multivariate outliers, non-linear relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data = states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

# what is all states had same percent taking sat
summary(lm(csat ~ expense + percent, data = states.data))

# Examine the model object 
class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

confint(sat.mod)


# Check assumptions for linear regression
# normal distirubiont, homoscedastic, the errors are independednt and
# the relationships are linear
# Investigate by visually plotting your model 

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2))
plot(sat.mod, which = c(1, 2))


# fit another model, adding house and senate as predictors
sat.voting.mod <- lm(csat ~ expense + house + senate, 
                     data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))


## EXERCISE 0
states.en.met <- subset(states.data, select = c("metro", "energy"))
summary(states.en.met)
plot(states.en.met)
cor(states.en.met, use="pairwise")


mod.en.met <- lm(energy ~ metro, data = states.data)
summary(mod.en.met)


states.en.met.pop.wst <- subset(states.data, select = c("energy", "metro", "pop", "waste"))
summary(states.en.met.pop.wst)
plot(states.en.met.pop.wst)
cor(states.en.met.pop.wst, use = "pairwise")
mod.en.met.pop.waste <- lm(energy ~ metro + pop + waste, data = states.data)
summary(mod.en.met.pop.waste)
anova(mod.en.met, mod.en.met.pop.waste)

