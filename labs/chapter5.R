#Libraries ----
library(MASS)
library(ISLR)
library(data.table)
library(class)

## 5.3.1 The Validation Set Approach ----

set.seed(1)
train = sample(392,196)

dt <- data.table(Auto)
lm.fit = lm(mpg ~ horsepower,data = Auto,subset = train)

# MSE of the 196 in the validation set
mean((dt[,mpg] - predict(lm.fit, dt))[-train]^2)

lm.fit2 = lm(mpg ~ poly(horsepower,2),data = Auto,subset = train)
mean((dt[,mpg] - predict(lm.fit2, dt))[-train]^2)

lm.fit3 = lm(mpg ~ poly(horsepower,3),data = Auto,subset = train)
mean((dt[,mpg] - predict(lm.fit3, dt))[-train]^2)

# With a different set

set.seed(2)
train = sample(392,196)

dt <- data.table(Auto)
lm.fit = lm(mpg ~ horsepower,data = Auto,subset = train)

# MSE of the 196 in the validation set
mean((dt[,mpg] - predict(lm.fit, dt))[-train]^2)

lm.fit2 = lm(mpg ~ poly(horsepower,2),data = Auto,subset = train)
mean((dt[,mpg] - predict(lm.fit2, dt))[-train]^2)

lm.fit3 = lm(mpg ~ poly(horsepower,3),data = Auto,subset = train)
mean((dt[,mpg] - predict(lm.fit3, dt))[-train]^2)

# A model that predicts mpg using a quadratic function of horsepower
# performs better than a model that involves only a linear function of horsepower

## Leave-One-Out Cross-Valiation ----
dt <- data.table(Auto)
# If "binomial" is not set into glm model, it fits 
# a linear model instead

glm.fit <- glm(mpg ~ horsepower, data = dt)
coef(glm.fit)
lm.fit <- glm(mpg ~ horsepower, data = dt)
coef(lm.fit)

# cv.glm() function is part of the boot library
library(boot)
glm.fit <- glm(mpg ~ horsepower, data = dt)
cv.err <- cv.glm(dt, glm.fit)
# cross validation results
cv.err$delta

cv.error <- rep(0,5)
system.time({
  for (i in 1:length(cv.error)){
    glm.fit <- glm(mpg ~ poly(horsepower, i), data = dt)
    cv.error[i] <- cv.glm(dt, glm.fit)$delta[1]  
  }
})
cv.error

# there is a sharp drop in the estimated test MSE
# between the linear and quadratic fits, 
# but no clear improvement from using higher-orer polynomials

## 5.3.3 k-Fold Cross-Validation ----
set.seed(17)
cv.error.10 <- rep(0,10)
system.time({
    for (i in 1:length(cv.error.10)){
    glm.fit <- glm(mpg ~ poly(horsepower, i), data = dt)
    cv.error.10[i] <- cv.glm(dt, glm.fit, K = 10)$delta[1]  
   }
})
cv.error.10
# 0.641 seg (K=10) x 9.921 seg (K=N)

## 5.3.4 The Bootstrap ----

alpha.fn <- function(data, index){
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y) - cov(X,Y))/(var(X) + var(Y)- 2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T))

# Automatically performing this command many time
# and recording all of the corresponding estimates for alpha
boot(Portfolio, alpha.fn, R=1000)

# Estimating the Accuracy of a Linear Regression Model
boot.fn <- function(data,index)
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
boot.fn(Auto, 1:392)
set.seed(1)
boot.fn(Auto, sample(392,392,replace = T))
boot.fn(Auto, sample(392,392,replace = T))
boot(Auto, boot.fn,1000)
# But the std. error for these coefficients seem to differ
# when we simply run the lm model, due to the assumption
# that the linear model is correct (thus the parameter sigma-squared
# is estimated as so).
summary(lm(mpg ~ horsepower, data = Auto))$coef

# Assessing a quadratic model 
boot.fn <- function(data,index)
  coefficients(lm(mpg ~ horsepower + I(horsepower^2), data = data, subset = index))
set.seed(1)
boot(Auto, boot.fn, 1000)

summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef
# now the standard errors are much more similar due to the non-linear
# nature of the data