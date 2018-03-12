### Conceptual -----

## Question 02 ----
# (c) 
# (0,0), (2,2), (3,8) -> blue
# (-1,1) -> red

# (d)
# It is linear in terms of x1, x1^2, x2, x2^2
# because one can write the boundary equation 
# with linear coefficients multiplying each of 
# these terms

# Question 04 ----

dt <- data.table( X1 = c(3,2,4,1,2,4,4),
                  X2 = c(4,2,4,4,1,3,1),
                  Y = c("Red","Red","Red","Red",
                        "Blue","Blue","Blue"))

plot(dt$X1, dt$X2, col = dt$Y)
# (h) Adding (3, 1) to the Red Class already makes it non-linear 

### Applied ----

library(e1071)
library(data.table)

## QUESTION 04 ----
set.seed(1)
dt <- data.table(x1 = rnorm(150,0,2),
                 x2 = rnorm(150,0,10))

dt[abs(x1) < 2 & abs(x2) <10 , y := 2]
dt[is.na(y), y := 1]
#dt[y == 2,x1 := x2-2/abs(x2)]
#dt[y == 2, ]
dt[, y := as.factor(y)]
# adding some error
dt[, x1 := x1 + rnorm(150, 0, 0.5)]
dt[, x2 := x2 + rnorm(150, 0, 2)]
dt[, y := as.factor(y)]

plot(dt[,.(x1,x2)], col = dt$y)
train <- sample(1:nrow(dt), 0.75*nrow(dt))

# POLYNOMIAL

svm1 <- svm(y ~ .,
            data = dt[train],
            kernel = "polynomial",
            degree = 2,
            cost = 1)
table(predict(svm1, dt[-train]), dt[-train]$y)
par(mfrow = c(1,2))
plot(svm1, dt[-train])
# misclassification 4/50

tune.out <- tune(svm, y ~ .,
                 data = dt[train],
                 kernel = "polynomial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                               degree = c(1, 2, 3, 4, 5)))
table(predict(tune.out$best.model, dt[-train]), dt[-train]$y)
plot(tune.out$best.model, dt[-train])

# RADIAL

svm2 <- svm(y ~ .,
            data = dt[train],
            kernel = "radial",
            gamma = 0.01,
            cost = 100)
table(predict(svm2, dt[-train]), dt[-train]$y)
plot(svm2, dt[-train])
# misclassification 3/50

tune.out <- tune(svm, y ~ .,
                 data = dt[train],
                 kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                               gamma = c(0.5, 1, 2, 3, 4)))
table(predict(tune.out$best.model, dt[-train]), dt[-train]$y)
plot(tune.out$best.model, dt[-train])
# misclassification 4/50
# gamma = 0.01 &  cost = 100 gave the lowest testing error 


# Question 05 ----
#(a)
x1 <- runif(500)-0.5
x2 <- runif(500)-0.5
y <- 1*(x1^2 - x2^2 > 0)
#(b)
plot(x1,x2, col = as.factor(y))
#(c)
dt <- data.table(x1, x2, y = as.factor(y))
lr.fit <- glm(y ~ ., 
        data = dt,
        family = binomial(link = 'logit'))
fitted.results <- predict(lr.fit, dt, type = "response")
pred <- ifelse(fitted.results > 0.5,1,0)
plot(x1,x2, col = as.factor(pred))

#(e)
dt <- data.table(x1, x1s= x1^2, x2, x2s = x2^2, x12 = x1*x2, y = as.factor(y))
lr.fit2 <- glm(y ~ ., 
              data = dt,
              family = binomial)
fitted.results <- predict(lr.fit2, dt, type = "response")
pred <- ifelse(fitted.results > 0.5,1,0)
plot(x1,x2, col = as.factor(pred))
# It already improved

# (g) SVC
dt <- data.table(x1, x2, y = as.factor(y))
svc.fit <- svm(y ~ ., 
              data = dt,
              kernel = "linear")
fitted.results <- predict(svc.fit, dt, type = "response")
plot(x1,x2, col = as.factor(fitted.results))
# POOR

# (h) SVM
dt <- data.table(x1, x2, y = as.factor(y))
svm.fit <- svm(y ~ ., 
               data = dt,
               kernel = "polynomial",
               degree = 2)
fitted.results <- predict(svm.fit, dt, type = "response")
plot(x1,x2, col = as.factor(fitted.results))
# The second degree svm gives a good fit

table(fitted.results, y)
# 26/500 misclassifications

dt <- data.table(x1, x2, y = as.factor(y))
svm.fit <- svm(y ~ ., 
               data = dt,
               kernel = "polynomial",
               degree = 4)
fitted.results <- predict(svm.fit, dt, type = "response")
plot(x1,x2, col = as.factor(fitted.results))
# The fourth degree svm gives an even better fit
# but it can also mean an overfit
table(fitted.results, y)
# 23/500 misclassifications

## QUESTION 06 ----

# (a)
x1 <- rnorm(150, 1)
x2 <- rnorm(150, 1)
dt <- data.table(x1, x2)
dt[x1 < 1, y := 1 ]
dt[is.na(y), y := 2]
dt[, y := as.factor(y)]
dt[, x1 := x1 - rnorm(150, 0, 0.5)]
plot(dt$x1, dt$x2, col = as.factor(dt$y))

# (b)

costs <- data.table(cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000))
# TRAIN
costs[, 
      {
        svmfit <- svm(y ~ ., 
                      data = dt[train],
                      kernel = "linear",
                      cost = cost)
        fitted <- predict(svmfit, dt[train]) 
        a <- table(fitted, dt[train]$y)
        error <- round(100*(a[2,1]+a[1,2])/(nrow(dt[train])),2)
        cat("\nCost:", cost, " \t || Error:", error, "\n")
      },
      by = cost]
# At cost 10, the test error already gets stable.

# TEST
costs[, 
      {
        svmfit <- svm(y ~ ., 
        data = dt[train],
        kernel = "linear",
        cost = cost)
        fitted <- predict(svmfit, dt[-train]) 
        a <- table(fitted, dt[-train]$y)
        error <- round(100*(a[2,1]+a[1,2])/(nrow(dt[-train])),2)
        cat("\nCost:", cost, " \t || Error:", error, "\n")
      },
      by = cost]
# At cost 10, the test error already gets stable.

# For low values for cost, the test dataset performs poorly 
# compared to the training fit
# However, for cost 10, the best value achieved,
# the error decreseas and gets stable.


## QUESTION 07 ----

require(ISLR)
require(data.table)
require(e1071)

dt <- data.table(Auto)
dt[mpg >= median(mpg), y := 1]
dt[is.na(y), y := 0]
dt[, y := as.factor(y)]
pairs(dt)

# SVC
costs <- data.table(cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000))
costs[, 
      {
        svmfit <- svm(y ~ ., 
                      data = dt,
                      kernel = "linear",
                      cost = cost)
        fitted <- predict(svmfit, dt) 
        a <- table(fitted, dt$y)
        error <- round(100*(a[2,1]+a[1,2])/(nrow(dt)),2)
        cat("\nCost:", cost, " \t || Error:", error, "\n")
      },
      by = cost]
# At cost 1, the error already gets small enough and stable, avoiding overfit.

# SVM
grid <- data.table(expand.grid(cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
                    gamma = c(1, 2, 3, 4, 5, 10)))
grid[, 
      {
        svmfit <- svm(y ~ ., 
                      data = dt,
                      kernel = "radial",
                      cost = cost,
                      gamma = gamma)
        fitted <- predict(svmfit, dt) 
        a <- table(fitted, dt$y)
        error <- round(100*(a[2,1]+a[1,2])/(nrow(dt)),2)
        cat("\nCost:", cost, " \t Gamma:", gamma, "\t|| Error:", error, "\n")
      },
      by = .(cost, gamma)]
# At cost 1, the error already gets small enough and stable, avoiding overfit.

grid <- data.table(expand.grid(cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
                               degree = c(1, 2, 3, 4)))
grid[, 
     {
       svmfit <- svm(y ~ ., 
                     data = dt,
                     kernel = "polynomial",
                     cost = cost,
                     degree = degree)
       fitted <- predict(svmfit, dt) 
       a <- table(fitted, dt$y)
       error <- round(100*(a[2,1]+a[1,2])/(nrow(dt)),2)
       cat("\nCost:", cost, " \t Degree:", degree, "\t|| Error:", error, "\n")
     },
     by = .(cost, degree)]

# Best models
# The best models could be calculated 
# only by the function tune

## Question 08 ----
dt <- data.table(OJ)
train <- sample(1:nrow(dt), 800)
summary(dt$Purchase)

svm.fit <- svm(Purchase ~ .,
               data = dt[train],
               kernel = "linear",
               cost = 0.01)
#training error:
table(predict(svm.fit, dt[train]), dt[train]$Purchase)
# 16.75% misclassifications
#testing error:
table(predict(svm.fit, dt[-train]), dt[-train]$Purchase)
# 11.62% misclassifications

range.log <- seq(-2, 1, by = .25)
tune.fit <- tune(svm, 
               Purchase ~ .,
               data = dt,
               kernel = "linear",
               cost = 10^range.log)
tune.fit$best.model

table(predict(tune.fit$best.model, dt[train]), dt[train]$Purchase)
# 17%
table(predict(tune.fit$best.model, dt[-train]), dt[-train]$Purchase)
# 11.08%

# The test error decreased a little

# SVM - Radial

tune.fit <- tune(svm, 
                 Purchase ~ .,
                 data = dt,
                 kernel = "radial",
                 cost = 10^range.log)
table(predict(tune.fit$best.model, dt[train]), dt[train]$Purchase)
# 38.625%
table(predict(tune.fit$best.model, dt[-train]), dt[-train]$Purchase)
# 29.19%

# SVM - Poly
tune.fit <- tune(svm, 
                 Purchase ~ .,
                 data = dt,
                 kernel = "polynomial",
                 degree = 2,
                 cost = 10^range.log)
table(predict(tune.fit$best.model, dt[train]), dt[train]$Purchase)
# 36.68%
table(predict(tune.fit$best.model, dt[-train]), dt[-train]$Purchase)
# 37.03%  

# On this data, the linear approach
# seems to give the best results, with
# a test error of only 11.62%
