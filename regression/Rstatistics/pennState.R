############################################################
### LINEAR REGRESSOIN ###
############################################################

## https://onlinecourses.science.psu.edu/stat485/node/6

# response variable = y ("dependent")
# predictor variable = x ("independent")
# y = B0 + B1*x + e
# B0 = intercept
# B1 = slope
# e = error

# Assumptions about e 
##### the errors are independent and normal with mean 0 and var = s2*


##lm(y ~ x, data = dataHere) creates object of type linear model

library(MASS)
data(mammals)
head(mammals)

model1 <- lm(brain~body, data=mammals)
summary(model1)
op <-  par(mfrow = c(2, 2), mar = c(2,3,1.5,0.5))
plot(model1)
par(op)
par(mar = c(3,3,0.5,0.5), mfrow = c(1,2))
with(mammals, plot(body, brain))
with(mammals, plot(body, brain, log = "xy"))

model2 <- lm(log(brain) ~ log(body), data = mammals)
summary(model2)
op <-  par(mfrow = c(2, 2), mar = c(2,3,1.5,0.5))
plot(model2)

## Compare models
par(op)
op <- par(mfrow = c(1,2), mar = c(2, 3, 2, 1))
plot(density(model1$resid), main = "model1")
plot(density(model2$resid), main = "model2")








