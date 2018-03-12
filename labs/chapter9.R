## -----
## 9.6.1 Support Vector Classifier
library(e1071)
library(data.table)

set.seed(1)
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1, 10))
x[y==1,] = x[y==1, ] + 1
plot(x, col = (3-y))

dt <- data.table(x = x, y = as.factor(y))
svmfit <- svm(y ~ ., 
              data = dt,
              kernel = "linear",
              cost = 10,
              scale = FALSE)
plot(svmfit, dt)

# determining the identity of the 
# support vector
svmfit$index
#summary(svmfit)

# if a smaller value for cost is used
svmfit <- svm(y ~ ., 
              data = dt,
              kernel = "linear",
              cost = 0.1,
              scale = FALSE)
plot(svmfit, dt)
svmfit$index

# to auto cross validate
set.seed(1)
tune.out <- tune(svm, 
                 y ~ ., 
                 data = dt,
                 kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)                 
# cost = 0.1 results in the lowest cross-validation error
bestmod <- tune.out$best.model
summary(bestmod)

# test dataset

xtest <- matrix(rnorm(20*2), ncol = 2)
ytest <- sample(c(-1,1), 20, rep = TRUE)
xtest[ytest == 1,] <- xtest[ytest==1, ] + 1
testdata <- data.table(x = xtest,
                       y = as.factor(ytest))
ypred <- predict(bestmod, testdata)
table(predict = ypred, truth = ytest)
# 19/20 correct

# if cost had been chosen to be 0.01
svmfit <- svm(y ~ ., 
              data = dt,
              kernel = "linear",
              cost = 0.01,
              scale = FALSE)
ypred <- predict(svmfit, testdata)
table(predict = ypred, truth = ytest)
# 14/20 correct

# When the two classes are linearly separable:
set.seed(1)
x[y == 1, ] <- x[y == 1,] + 1

# In the book, it says + 0.5, but it does not make it linearly separable
# enough

plot(x, col = (y+5)/2, pch = 19)

dt <- data.table(x = x, y = as.factor(y))
svmfit <- svm(y ~ .,
              data = dt,
              kernel = "linear",
              cost = 100000)
summary(svmfit)
plot(svmfit, dt)

svmfit <- svm(y ~ .,
              data = dt,
              kernel = "linear",
              cost = 1)
summary(svmfit)
plot(svmfit, dt)

## 9.6.1 Support Vector Machine ----

# generate data with a non-linear class boundary
set.seed(1)
require(data.table)
require(e1071)

x <- matrix(rnorm(200*2), ncol = 2)
x[1:100,] <- x[1:100,] + 2
x[101:150,] <- x[101:150,] - 2
y <- c(rep(1,150), rep(2, 50))
dt <- data.table(x, y = as.factor(y))
plot(x, col = y)       

# fitting  the data with a radial kernel

train <- sample(200,100)
svmfit <- svm(y ~ ., 
              data = dt[train],
              kernel = "radial",
              gamma = 1,
              cost = 1)
plot(svmfit, dt[train])
summary(svmfit)

# if cost is higher, then overfitting the train data:
svmfit <- svm(y ~ ., 
              data = dt[train],
              kernel = "radial",
              gamma = 1,
              cost = 1e5)
plot(svmfit, dt[train])
summary(svmfit)

# tuning
set.seed(1)
tune.out <- tune(svm, y ~ .,
                 data = dt[train],
                 kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                               gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)
plot(tune.out)

# to check the test performance of the best model
table(pred = predict(tune.out$best.model, dt[-train]), true = dt[-train]$y)
# 83% accuracy

## 9.6.3 ROC Curves
install.packages("ROCR")
library(ROCR)

rocplot <- function(pred, truth, ...){
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

# to obtain the fitted values from a svm:
svmfit.opt <- svm(y ~ .,
                  data = dt[train],
                  kernel = "radial",
                  gamma = 2,
                  cost = 1, 
                  decision.values = T)
fitted <- attributes(predict(svmfit.opt, dt[train], decision.values = TRUE))$decision.values

# now, to produce the ROC plot

par(mfrow = c(1,2))
rocplot(fitted, dt[train]$y, main = "Testing Data")

# a more flexible fit
svmfit.flex <- svm(y ~ .,
                  data = dt[train],
                  kernel = "radial",
                  gamma = 50,
                  cost = 1, 
                  decision.values = T)
fitted <- attributes(predict(svmfit.flex, dt[train], decision.values = TRUE))$decision.values
rocplot(fitted, dt[train]$y, add = T, col ="red")

# recap that for a single svm 
# running svmfit for a linear model
fitted <- attributes(predict(svmfit, dt[train], decision.values = TRUE))$decision.values
rocplot(fitted, dt[train]$y, add = T, col ="blue")

## 9.6.4 SVM with Multiple Classes

set.seed(1)
x <- rbind(x, matrix(rnorm(50*2), ncol = 2))
y <- c(y, rep(0,50))
dt <- data.table(x, y = as.factor(y))
dt[y == 0, V2 := V2 + 2]
par(mfrow = c(1,1))
plot(x, col = (y+1))

# fitting an SVM to the data 

svmfit <- svm(y ~ .,
              data = dt,
              kernel = "radial",
              cost = 10,
              gamma = 1)
plot(svmfit, dt)

tune.out <- tune(svm, y ~ .,
                 data = dt,
                 kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                               gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)
svm.opt <- svm(y ~ .,
              data = dt,
              kernel = "radial",
              cost = 100,
              gamma = 2)
plot(svm.opt, dt)

# So, the problem with tune is that the cost gets higher,
# the misclassification decreases, but then there is the 
# likelihood of overfit

## 9.6.5 Applications to the Gene Expression Data

# Examining the Khan dataset

require(ISLR)
require(data.table)
require(e1071)

dt <- data.table(x = Khan$xtrain, y = as.factor(Khan$ytrain))
head(dt)

out <- svm(y ~ ., 
           data = dt,
           kernel = "linear",
           cost = 10)
summary(out)

# No missclassification:
table(out$fitted, dt$y)

# Its performance in the test dataset is as follows:

dt.test <- data.table(x = Khan$xtest, y = as.factor(Khan$ytest))
pred.test <- predict(out, dt.test)
table(pred.test, dt.test$y)
# 2 misclassifications 

# Let's see what's the optimun hyperparameters for the train 
tune.out <- tune(svm, y ~ .,
                 data = dt,
                 kernel = "linear",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                               gamma = c(0.5, 1, 2, 3, 4)))
tune.out$best.model
pred.test <- predict(tune.out$best.model, dt.test)
table(pred.test, dt.test$y)
# Again 2 missclassifications 


