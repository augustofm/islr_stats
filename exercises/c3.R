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

# a) When abs(x) is equal to abs(y), or at least comes from the same
# distribution

# b)
x <- rnorm(100,2)
y <- rnorm(100,0.5)
coef(lm(y~x+0))
coef(lm(x~y+0))

# c)
x <- rnorm(100,2)
y <- -x
coef(lm(y~x+0))
coef(lm(x~y+0))

### QUESTION 13 ----

set.seed(1)

# a)
x <- rnorm(100, 0, 1)

# b)
eps <- rnorm(100, 0, 0.25)

# c)
y <- - 1 + 0.5*x + eps
# Obviously the length of y is also 100
length(y)
lm.fit1 <- lm(y~x)
coef(lm.fit1)

# d)
plot(x, y)
# The plot shows a linear relationship among the data
# with low leverage values and well sparced.

# e) Comparing the coefficients
# The intercept was very close to 1, with 0.9% error
summary(lm(y~x))
#and 0.02425 std. error
# The coefficient was also very close to 0.5, 
# which 0.2693 std. error
plot(x, y, lwd=2, col="blue", pch=4)+abline(lm.fit1)
legend("top", "(x,y)", pch=4, title="Least squared regression")

# g)
lm.fit2 <- lm(y~x+I(x^2))
summary(lm.fit2)
# No evidence that the quadratic term would improv
# the model fit, it's p-value is rather high.

# h)
eps <- rnorm(100, 0, 0.05)
y <- - 1 + 0.5*x + eps
lm.fit2 <- lm(y~x)
summary(lm.fit2)
plot(x, y, lwd=2, col="blue", pch=4)+abline(lm.fit2)
legend("top", "(x,y)", pch=4, title="Least squared regression")
# squared R improved from 0.7542 to 0.9868

# i)
eps <- rnorm(100, 0, 0.5)
y <- - 1 + 0.5*x + eps
lm.fit3 <- lm(y~x)
summary(lm.fit3)
plot(x, y, lwd=2, col="blue", pch=4)+abline(lm.fit3)
legend("top", "(x,y)", pch=4, title="Least squared regression")
# squared R decresead from 0.7542 to 0.4556 with
# more noise in the data

# j) 
#FIT 1
# For the Intercept
aux <- predict(lm.fit1, data.frame(x=c(0)), interval = "confidence")
aux
#For the x Coefficient
aux <- predict(lm.fit1, data.frame(x=c(1)), interval = "confidence")
aux-lm.fit1$coefficients[1]

#FIT 2
# For the Intercept
aux <- predict(lm.fit2, data.frame(x=c(0)), interval = "confidence")
aux
#For the x Coefficient
aux <- predict(lm.fit2, data.frame(x=c(1)), interval = "confidence")
aux-lm.fit2$coefficients[1]

#FIT 3
# For the Intercept
aux <- predict(lm.fit3, data.frame(x=c(0)), interval = "confidence")
aux
#For the x Coefficient
aux <- predict(lm.fit3, data.frame(x=c(1)), interval = "confidence")
aux-lm.fit3$coefficients[1]


### QUESTION 14 ----

# a) 
set.seed(1)
x1 <- runif(100)
x2 <- 0.5*x1+rnorm(100)/10
y <- 2+2*x1+0.3*x2+rnorm(100)
lm.fit <- lm(y~x1+x2)
coef(lm.fit)
plot(x1,x2)

# c) B0 and B1 are very close to the true coefficients
# though B2 is much higher
summary(lm.fit)
# We can see that p-value for B2 is non-significant
# So the null hypothesis can be true

# e) 
lm.fit2 <- lm(y~x1)
summary(lm.fit2)
coef(lm.fit2)
# Yes, we can reject the null Hypothesis

# f)
summary(lm.fit)
summary(lm.fit2)
# the estimate for B1 is much higher when the null hypothesis
# for B2 is true, besides squared-R is higher as well.

# g)
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)

lm.fit_new <- lm(y~x1+x2)
lm.fit2_new <- lm(y~x1)
summary(lm.fit_new)
# Interestingly, the null hypothesis for B0 cannot be rejected 
# any longer
summary(lm.fit2_new)
# squared-R decreases a lot due to this last observation
# for x1=0.1, y should be near 2.2, it is 6 instead.

require(data.table)
dt <- data.table(y, x1, x2)
pairs(dt)
boxplot(dt)
# The new observation is both an outlier and 
# a high leverage point, since it affects too
# much the fit, even though it is not similar 
# to the other points

### QUESTION 15 ----

rm(Boston)
require(ISLR)
dt <- data.table(Boston)

# a)
names(dt)
str(dt)
attach(dt)
coef <- c(NA)
for (i in 2:ncol(dt)){
  cat(paste0("\n\n\nPredictor: ", names(dt)[i]))
  lm.fit.un <- lm(crim~get(names(dt)[i]))
#  print(summary(lm(crim~get(names(dt)[i]))))
  coef <- c(coef, coef(lm.fit.un)[2])
}

# chars seems to be a predictor with no huge effect on the model
lm.fit <- lm(crim~.,dt)
summary(lm.fit)

# b)
# We can reject the null hypothesis for zn, nox, dis, rad, black, lstat, medv 


aux <- coef(lm.fit)
dt2 <- data.table("predictor"=names(aux), "coef_mu"=aux)
dt1 <- data.table("predictor"=names(aux), "coef_un"=coef)
p <- merge(dt1, dt2)
require(ggplot2)
ggplot(na.omit(p), aes(x=coef_un, y=coef_mu))+geom_point(aes(col=predictor))

# the predictor nox seems to be very different 

# d)
for (i in 2:ncol(dt)){
  cat(paste0("\n\n\nPredictor: ", names(dt)[i]))
  lm.fit.un <- lm(crim~get(names(dt)[i])+I(get(names(dt)[i])^2)+I(get(names(dt)[i])^3))
  print(summary(lm.fit.un))
  #coef <- c(coef, coef(lm.fit.un)[2])
}
# The ones with strong non-linearity:
# indus, nox, dis, ptratio, medv
