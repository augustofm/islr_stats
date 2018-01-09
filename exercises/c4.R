# libraries ----
library(data.table)
library(ggplot2)
library(ISLR)
library(MASS)

## QUESTION 10 ----
dt <- data.table(Weekly)
str(dt)

# a) Do there appear to be any pattern?
pairs(dt[,-c("Year")])
# Direction seems to be very related to today's value recorded,
# however, further patterns might appear only after running a 
# fit model to the data.

# b) Logistic Linear Regression
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = dt,family = binomial)
summary(lda.fit)
# Lag 2 seems to be the only predictor important to the analysis
glm.prob <- data.table("prob" = predict(glm.fit, type = "response"))
glm.prob[prob > 0.5, pred := "Up"]
glm.prob[is.na(pred),pred := "Down"]
table(glm.prob$pred, dt$Direction)
# Overall fraction of correct predictions:
(54+557)/nrow(glm.prob)
# 56.10 %

# d) Only with Lag2
glm.fit <- glm(Direction ~ Lag2, data = dt,family = binomial)
summary(lda.fit)
# Lag 2 seems to be the only predictor important to the analysis
glm.prob <- data.table("prob" = predict(glm.fit, type = "response"))
glm.prob[prob > 0.5, pred := "Up"]
glm.prob[is.na(pred),pred := "Down"]
table(glm.prob$pred, dt$Direction)
# Overall fraction of correct predictions:
(33+579)/nrow(glm.prob)
# 56.19 %
# The response got a bit better only using one predictor

# e) FOR LDA
alt.fit <- lda(Direction ~ Lag2, data = dt)
pred <- predict(alt.fit,dt)
table(pred$class, dt$Direction)
(33+580)/nrow(dt)
# 56.29 %

# f) FOR QDA
alt.fit <- qda(Direction ~ Lag2, data = dt)
pred <- predict(alt.fit,dt)
table(pred$class, dt$Direction)
(0+605)/nrow(dt)
# 55.56 %

# g) FOR KNN
library(class)
set.seed(1)
nrow(dt)
dt.train <- dt[1:900,.(Lag2)]
dt.test <- dt[901:nrow(dt), .(Lag2)]
alt.fit <- knn(dt.train, dt.test, dt[1:900,]$Direction, k = 1)
table(alt.fit, dt[901:nrow(dt),Direction])
(44+51)/nrow(dt[901:nrow(dt),])
# 50.26 %

# h) The best method was the Linear Discriminant Analysis
# The poorest method was the kNN, however QDA predicted 
# no Down on the whole dataset, and predicted correctly 
# all the Up's observed (which is weird).

# i) Experimenting for KNN
library(class)
set.seed(1)
nrow(dt)
dt.train <- dt[1:900,.(Lag2)]
dt.test <- dt[901:nrow(dt), .(Lag2)]
alt.fit <- knn(dt.train, dt.test, dt[1:900,]$Direction, k = 3)
table(alt.fit, dt[901:nrow(dt),Direction])
(34+65)/nrow(dt[901:nrow(dt),])
# 52.38 %

alt.fit <- knn(dt.train, dt.test, dt[1:900,]$Direction, k = 8)
table(alt.fit, dt[901:nrow(dt),Direction])
(41+68)/nrow(dt[901:nrow(dt),])

#A value of K=3 and K=8 gave a much better result


## QUESTION 11 ----

dt <- data.table(Auto)
# a)
dt[mpg > median(mpg), mpg01 := 1]
dt[is.na(mpg01), mpg01 := 0]

# b)
pairs(dt[,-c("name")])
# mpg01 seems to be very correlated with mpg,
# horsepower and acceleration

# c)
dt <- sample(dt)
train <- dt[1:320,]
test <- dt[320:nrow(dt)]

# d)
lda.fit <- lda(mpg01 ~ horsepower+acceleration+mpg,
               data = train)
pred <- predict(lda.fit, test, type="response")
table(pred$class, test$mpg01)
# during the test, the overal error of the model
# is zero. The error might increase if we use less
# observations for training, like 150.

# e)
qda.fit <- qda(mpg01 ~ horsepower+acceleration+mpg,
               data = train)
pred <- predict(qda.fit, test, type="response")
table(pred$class, test$mpg01)
# error is also zero

# and if we use less observations for training:
train <- dt[1:200,]
test <- dt[200:nrow(dt)]

qda.fit <- qda(mpg01 ~ horsepower + acceleration + mpg,
               data = train)
pred <- predict(qda.fit, test, type="response")
table(pred$class, test$mpg01)
# the overall error is 6/392, i.e. 1,53%

# f) 
train <- dt[1:200,]
test <- dt[200:nrow(dt)]

glm.fit <- glm(mpg01 ~ horsepower + acceleration + mpg,
                   data = train, family = binomial)
glm.prob <- data.table("prob" = predict(glm.fit, test,type = "response"))
glm.prob[prob > 0.5, pred := 1]
glm.prob[is.na(pred),pred := 0]
table(glm.prob$pred, test$mpg01)
# error is 1/392, 0.255%

# g)
train <- dt[1:320,.(horsepower, acceleration, mpg, mpg01)]
test <- dt[321:nrow(dt), .(horsepower, acceleration, mpg, mpg01)]


knn.fit <- knn(train, train, train$mpg01, k = 1)
table(knn.fit, train[, mpg01])
#TRAIN ERROR 0/320
knn.fit <- knn(train, test, train$mpg01, k = 1)
table(knn.fit, test[, mpg01])
#TEST ERROR (2+0)/72, i.e 2.78%

knn.fit <- knn(train, train, train$mpg01, k = 3)
table(knn.fit, train[, mpg01])
#TRAIN ERROR 5/320
knn.fit <- knn(train, test, train$mpg01, k = 3)
table(knn.fit, test[, mpg01])
#TEST ERROR (5+0)/72, i.e 6.94%

knn.fit <- knn(train, train, train$mpg01, k = 5)
table(knn.fit, train[, mpg01])
#TRAIN ERROR 5/320
knn.fit <- knn(train, test, train$mpg01, k = 5)
table(knn.fit, test[, mpg01])
#TEST ERROR (6+0)/72, i.e 8.33%
# when K increases, it only increases the train 
# and the test error

## QUESTION 12 ----

# a)
Power <- function()
  print(2^3)

Power()

# b)
Power2 <- function(x, a)
  print(x^a)

Power2(3,8)

# c)
Power2(10,3)
Power2(8,17)
Power2(131,3)

# d)
Power3 <- function(x,a)
  return(x^a)

# e)
Power2 <- function(x, a)
  return(x^a)

x <- 1:10
y <- Power2(x,2)

plot(x, y, col="red", xlab="x",
     ylab="x^2")
plot(x, log(y), col="red", xlab="x",
     ylab="x^2")
plot(log(x), y, col="red", xlab="x",
     ylab="x^2")
plot(log(x), log(y), col="red", xlab="x",
     ylab="x^2")

# f)
PlotPower <- function(x,a)
  plot(x,x^a,col="red",xlab="x",ylab="x^a")
PlotPower(1:10,3)


## QUESTION 13 ----
# Predict whether or not a given suburb
# has a crime rate above or below de median
dt <- data.table(Boston)
str(dt)
dt[crim < median(crim),class:=0] #below
dt[crim >= median(crim),class:=1] #above

# splitting dataset
dt <- sample(dt)
dt.train <- dt[1:420]
dt.test <- dt[421:nrow(dt),]

# logistic regression
glm.fit <- glm(class ~ ., data = dt.train)
summary(glm.fit)
# important variables: nox and ptratio
glm.fit <- glm(class ~ nox + age + rad + medv, data = dt, family = binomial)
glm.prob <- data.table("prob" = predict(glm.fit, dt.train,type = "response"))
glm.prob[prob > 0.5, pred := 1]
glm.prob[is.na(pred),pred := 0]
table(glm.prob$pred, dt.train$class)
# Overall accuracy = 363/420 = 86.42% using all the data for training 
glm.prob <- data.table("prob" = predict(glm.fit, dt.test,type = "response"))
glm.prob[prob > 0.5, pred := 1]
glm.prob[is.na(pred),pred := 0]
table(glm.prob$pred, dt.test$class)
# Overall accuracy = 76/86 = 81.39% using test 

# linear discriminant analysis