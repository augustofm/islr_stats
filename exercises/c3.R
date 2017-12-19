### QUESTION 8 ----

library(ISLR)
fix(Auto)
attach(Auto)
lm.fit <- lm(mpg ~ horsepower)
summary(lm.fit)
# i) for the fit above, the p-values are almost zero, so the 
# coefficients are significant, showing a relationship
# between the predictor and the response.

# ii)  Rather strong, given that R-squared is about 0.60 
# and that p-values are low.

# iii) It's a negative relationship. Coefficient < 0.

# iv) 
predict(lm(mpg ~ horsepower), data.frame(horsepower=98))
predict(lm(mpg ~ horsepower), data.frame(horsepower=98), interval = "confidence")
predict(lm(mpg ~ horsepower), data.frame(horsepower=98), interval = "prediction")

# b)
par(mfrow=c(1,1))
plot(horsepower, mpg, pch="+")
abline(lm.fit, col="red", lwd=3)
# c~
par(mfrow=c(2,2))
plot(lm.fit)
# by the Residuals vs Fitted plot, a concentration of values
# for larger mpg values shows heteroscedasticity.
#Also, a non-linearity of the relationship is also shown. 



### QUESTION 9 ----
require(data.table)
# a)
pairs(Auto)
names(Auto)
dt <- data.table(Auto)

# b)
cor.matrix <- cor(dt[,1:8])
View(cor.matrix)

lm.fit <- lm(mpg ~ . - name, data = dt)
summary(lm.fit)

# c)
#   i. There is, but much more stronger for only 5 of the predictors
#   ii. displacement, weight, year, origin
#   iii. it suggests that the greater the model year, 
# more miles per gallon the vehicle will require.

# d)
par(mfrow=c(2,2))
plot(lm.fit)
# Yes, it suggests ouliers like those with index 323, 326..
# Yes, observation 14, for instance.

# e)
lm.fit2 <- lm(mpg ~ . - name + 
                I(year^2) +
                year:weight +
                weight:acceleration, data = dt)
summary(lm.fit2)

# f)
lm.fit3 <- lm(mpg ~ . - name - cylinders - origin - acceleration + 
                I(year^2) +
                year:weight +
                log(displacement), data = dt)
summary(lm.fit3)

# In the end, predictors like accelaration, origin and cylinders 
# were much less important than transoforming variables like 
# displacement ad year.

### QUESTION 10 ----
require(ISLR)
require(data.table)
dt <- data.table(Carseats)
dt
summary(dt)

pairs(dt[,.(Sales, Price, Urban, US)])

# a)
lm.fit <- lm(Sales ~ Price+Urban+US, data = dt)
summary(lm.fit)

# b)
# Price is very significative, when it grows, 
# sales are also high, and the relationship might
# be one of consequent/cause.

#When the area is Urban, price also decreases, but
#not significantly, so it might be a poor predictor.

#WHen it's in the US, it also increases.

# c) Sales = -0.055 Prince - 0.0219 Urban + 1.2 US

# d) For Price and US, very low p-values

# e) 
lm.fit2 <- lm(Sales ~ Price + US, data = dt)
summary(lm.fit2)

# f)
anova(lm.fit, lm.fit2)

#The models are very alike, given that the poor
#predictor has been removed. 
#Both of them do not have a good fit considering 
#squared-R.

# g)
#For the Price Coefficient
aux <- predict(lm.fit2, data.frame(Price=c(1),US=as.factor(c("No"))), interval = "confidence")
aux-lm.fit2$coefficients[1]
#For the US Coefficient
aux <- predict(lm.fit2, data.frame(Price=c(0),US=as.factor(c("Yes"))), interval = "confidence")
aux-lm.fit2$coefficients[1]
#For the Intercept Coefficient
aux <- predict(lm.fit2, data.frame(Price=c(0),US=as.factor(c("No"))), interval = "confidence")
aux

# h) 
par(mfrow = c(2,2))
plot(lm.fit2)

# Not that high leverage, but there are some observations which high values, like 368
# with leverage values greater than the average 
# (p+1)/n = 3/400 = 0.0075

### QUESTION 11 ----
set.seed(1)
x <- rnorm(100)
y <- 2*x+rnorm(100)

# a)
lm.fit1 <- lm(y ~ x + 0)
summary(lm.fit1)

# Coefficient estimate B = 1.9939, sd = 0.1065, 
# t-statistic = 18.73, p-value < 2e-16
# Not having the intercept seems already to be a
# good model, given that the null hypothesis is 
# not true and that squared-R is high.

# b) 
lm.fit2 <- lm(x ~ y + 0)
summary(lm.fit2)

# Coefficient estimate B = 0.3911, sd = 0.02089, 
# t-statistic = 18.73, p-value < 2e-16
# The standard error for the B coefficient was lower
# and the squared-R and F-statistics were equal.

# c) They are the inverse, but x onto y provides
# a lower sd for the coefficient
# yet, the coefficients are not inverse. How come?

# d)
# Confirming numerically:
# y -> True value
# y_hat -> Predicted value
y_hat <- predict(lm.fit1, data = x)
names(y_hat)<-NULL
n = length(y_hat)
num <- (n-1)^0.5*sum(x*y_hat)
den <- (sum(x^2)*sum(y^2)-(sum(x*y)^2))^0.5
tstatR <- num/den
tstatR
# which is equal to the one given by the lm()

# e) they are the same because all the elements in the equation
# are independent of the order, they are multiplications

# f)
round(coef(summary(lm(y~x)))[2,"t value"] - coef(summary(lm(x~y)))[2,"t value"],3)==0 #?

### QUESTION 12 ----
