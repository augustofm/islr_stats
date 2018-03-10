## -----
library(ISLR)
# install.packages("tree")
library(tree)
dt <- data.table(Carseats)
dt <- na.omit(dt)
dt[, High := as.factor(ifelse(Sales>8, "Yes","No"))]

attach(dt)
tree.carseats <- tree(High ~ .-Sales, dt)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)

set.seed(2)
train <- sample(1:nrow(dt), 200)
dt.test <- dt[-train,]
High.test <- dt.test$High
tree.carseats <- tree(High ~. - Sales, data = dt, subset = train)
tree.pred <- predict(tree.carseats, dt.test, type = "class")
table(tree.pred, High.test)
#Error rate: 57/200 -> 28.5%

set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
# givin error maybe due to data.table

prune.carseats <- prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred <- predict(prune.carseats, dt.test, type = "class")
table(tree.pred, High.test)
#Error rate: 48/200 -> 24.0%

## 8.3.2 Fitting Regression Trees
library(MASS)
set.seed(1)
dt <- data.table(Boston)
train <- sample(1:nrow(dt), 0.5*nrow(dt))
tree.boston <- tree(medv ~., data = dt, subset = train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty = 0)

# If pruned
prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

yhat <- predict(tree.boston, newdata = dt[-train,])
boston.test <- dt[-train, medv]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

## 8.3.3 Bagging and Random Forests
library(randomForest)

set.seed(1)
bag.boston <- randomForest(medv ~ ., 
                           dt,
                           subset = train,
                           mtry = 13,
                           importance = TRUE)
bag.boston

yhat.bag <- predict(bag.boston, dt[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag - boston.test)^2)

bag.boston <- randomForest(medv ~ ., 
                           dt,
                           subset = train,
                           mtry = 13,
                           importance = TRUE,
                           ntree = 25)
yhat.bag <- predict(bag.boston, dt[-train,])
mean((yhat.bag - boston.test)^2)

set.seed(1)
rf.boston <- randomForest(medv ~ ., 
                           dt,
                           subset = train,
                           mtry = 6,
                           importance = T)
yhat.rf <- predict(rf.boston, dt[-train,])
mean((yhat.rf - boston.test)^2)
importance(rf.boston)

# rf performed better then bagging
varImpPlot(rf.boston)

## 8.3.4 Boosting

install.packages("gbm")
library(gbm)
set.seed(1)
boost.boston <- gbm(medv ~ ., data = dt[train,],
                    distribution = "gaussian",
                    n.trees = 5000, 
                    interaction.depth = 4)
summary(boost.boston)
par(mfrow = c(1,2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")

yhat.boost <- predict(boost.boston,
                      newdata = dt[-train,],
                      n.trees = 5000)
mean((yhat.boost-boston.test)^2)

boost.boston <- gbm(medv ~ ., data = dt[train,],
                    distribution = "gaussian",
                    n.trees = 5000, 
                    interaction.depth = 4,
                    shrinkage = 0.2,
                    verbose = F)
yhat.boost <- predict(boost.boston,
                      newdata = dt[-train,],
                      n.trees = 5000)
mean((yhat.boost-boston.test)^2)
#leading to a slightly lower test MSE 
#than Lambda = 0.001 (default)