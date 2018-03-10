### Conceptual -----

## Question 03 ----
require(data.table)
#Gini Index,
#Classification Error
#Entropy
#Simple classification with two classes
#Where pm1 = 1 - pm2

#Gini index 
pm1 <- seq(0,1,0.01)
pm2 <- 1 - pm1
p.dt <- data.table(pm1 = pm1, 
           pm2 = pm2)
p.dt[, G := pm1*(1-pm2)+pm2*(1-pm1)]

#Classification error
p.dt[, E := 1 - max(pm1, pm2), by = pm1]

#Entropy
p.dt[, D := -(pm1*log(pm1)+pm2*log(pm2))]

p.dt.melt <- melt(p.dt, id = c("pm1", "pm2"))
require(ggplot2)
ggplot(p.dt.melt)+
  geom_line(aes(x=pm1, y=value, color=variable))+
  theme_classic()+
  labs(x="Probability of class 1",
       y="Measurement")


## Question 05 ----
# p -> Probability of Class being Red given X.
p <- c(0.1, 0.15, 0.2, 0.2, 0.55,
       0.6, 0.6, 0.65, 0.7, 0.75)
# Approach 1:
#Not RED
length(p[p<0.5])
#RED
length(p[p>=0.5])
# So, the class would be Red
# Approach 2:
mean(p)
# 0.45 is smaller than 0.5,
# so it would not be red, but gren


### Applied ----

## QUESTION 07 ----
require(MASS)
require(data.table)
require(randomForest)

dt <- data.table(Boston)
train <- sample(1:nrow(dt), 0.5*nrow(dt))
boston.test <- dt[-train, medv]

mse <- list()
mse.dt <- data.table(ntree = as.numeric(),
                     mtry = as.numeric(),
                     mse = as.numeric())
for (mtry in c(4, 6, 13)) {
  aux.dt <- data.table(ntree = 1:500)
  aux.dt[, mtry := mtry]
  for (ntree in 1:500){
     rf.boston <- randomForest(medv ~ ., 
                          dt,
                          subset = train,
                          mtry = mtry,
                          ntree = ntree,
                          importance = T)
    yhat.rf <- predict(rf.boston, dt[-train,])
    mse[ntree] <- mean((yhat.rf - boston.test)^2)
  }
  aux.dt[, mse := unlist(mse)]
  mse.dt <- rbind(mse.dt, aux.dt)
}
mse.dt[, mtry := as.factor(mtry)]

require(ggplot2)
ggplot(mse.dt[ntree < 50])+
  geom_line(aes(x = ntree, y = mse, color = mtry))+
  theme_bw()
# m/2 ended up giving the smaller mse

## QUESTION 08 ----
require(ISLR)
require(data.table)
require(randomForest)
require(tree)

dt <- data.table(Carseats)

# (a) Split the data
train <- sample(1:nrow(dt), 0.7*nrow(dt))
# (b) 
tree.model <- tree(Sales ~ ., 
                   data = dt[train])
plot(tree.model)
text(tree.model, pretty = 0)
pred <- predict(tree.model, dt[-train])
test.mse <- mean((pred-dt[-train, Sales])^2)
test.mse


# (c) Cross validation
cv.dt <- cv.tree(tree.model)
# The function above is not working, 
# but after getting the best value for the pruning
# one can try the following
prune.dt <- prune.tree(tree.model, best = 8)
plot(prune.dt)
text(prune.dt, pretty = 0)

pred <- predict(prune.dt, dt[-train])
test.mse <- mean((pred-dt[-train, Sales])^2)
test.mse

# (d) Bagging

mtry <- length(names(dt))-1
bag.model <- randomForest(Sales ~ ., 
                   data = dt[train],
                   mtry = mtry,
                   importance = TRUE)
plot(bag.model)
par(mfrow = c(2,1))
varImpPlot(bag.model)
pred <- predict(bag.model, dt[-train])
test.mse <- mean((pred-dt[-train, Sales])^2)
test.mse


# (e) Random Forest

rf.model <- randomForest(Sales ~ ., 
                         data = dt[train],
                         importance = TRUE,
                         ntree = 250)
plot(rf.model)
varImpPlot(rf.model)
pred <- predict(rf.model, dt[-train])
test.mse <- mean((pred-dt[-train, Sales])^2)
test.mse

# Bagging showed the smallest mse error, of only 1.7467
# When m goes towards p, the random forest error decreases 
# until it becomes the bagging error for the Carseats dataset
# which suggests uncorrelation among the predictors.

## QUESTION 09 ----

dt <- data.table(OJ)
#(b)
train <- sample(1:nrow(dt), 800)

dt.train <- dt[train]
#(c)
tree.model <- tree(Purchase ~ ., 
                   data = as.data.frame(dt.train))
summary(tree.model)
# 7 terminal nodes, 139/800 misclassifications
tree.model
plot(tree.model)
text(tree.model, pretty = 0)

# (e)
test <- data.table(predict(tree.model, newdata = dt[-train]))
test[, Purchase := dt[-train, Purchase]]
test[, pred := ifelse(CH > 0.5, "CH", "MM")]
table(test$pred, test$Purchase)
# error := (13+39)/(270)

cv.model <- cv.tree(tree.model)
plot(cv.model$size, cv.model$dev)

# The tree size with 6 predictors gives the
# smallest error 

prune.dt <- prune.tree(tree.model, best = 6)

# (j) Comparing the training error

train.pred <- data.table(predict(tree.model, newdata = dt[train]))
train.pred[, Purchase := dt[train, Purchase]]
train.pred[, pred := ifelse(CH > 0.5, "CH", "MM")]
table(train.pred$pred, train.pred$Purchase)
# error = 17.375 % 

train.pred <- data.table(predict(prune.dt, newdata = dt[train]))
train.pred[, Purchase := dt[train, Purchase]]
train.pred[, pred := ifelse(CH > 0.5, "CH", "MM")]
table(train.pred$pred, train.pred$Purchase)
# error = 17.375 % 

# (j) Comparing the testing error

test <- data.table(predict(tree.model, newdata = dt[-train]))
test[, Purchase := dt[-train, Purchase]]
test[, pred := ifelse(CH > 0.5, "CH", "MM")]
table(test$pred, test$Purchase)
# error = 19.259

test <- data.table(predict(prune.dt, newdata = dt[-train]))
test[, Purchase := dt[-train, Purchase]]
test[, pred := ifelse(CH > 0.5, "CH", "MM")]
table(test$pred, test$Purchase)
# error = 19.259

# Pruning the tree up to 4 or 5 final nodes does not really 
# change the mse




## QUESTION 10 ----

require(MASS)
require(data.table)
require(randomForest)
require(gbm)

dt <- data.table(Hitters)

# (a)
summary(dt$Salary)
dt <- dt[!is.na(Salary)]
dt[, Salary := log(Salary)]
plot(dt$Salary)

# (b)
train <- dt[1:200,]
test <- dt[-c(1:200),]

mse.dt <- data.table(shrinkage = numeric(),
                     train.mse = numeric(),
                     test.mse = numeric())

for (i in seq(0, 0.2, 0.0005)){
  boost.dt <- gbm(Salary ~ ., data = train,
                    distribution = "gaussian",
                    n.trees = 1000, 
                    interaction.depth = 6,
                    shrinkage = i,
                    verbose = F)
  yhat.boost <- predict(boost.dt,
                      newdata = train,
                      n.trees = 1000)
  train.mse <- mean((yhat.boost-train$Salary)^2)
  yhat.boost <- predict(boost.dt,
                        newdata = test,
                        n.trees = 1000)
  test.mse <- mean((yhat.boost-test$Salary)^2)
  
  aux <- data.table(shrinkage = i, 
                      train.mse = train.mse,
                      test.mse = test.mse)
  mse.dt <- rbind(mse.dt, aux)
  cat("Shrinkage:", i, "\n")
}
par(mfrow = c(2,1))
plot(mse.dt[,.(shrinkage, train.mse)])
plot(mse.dt[,.(shrinkage, test.mse)])
plot(mse.dt[shrinkage < 0.01, .(shrinkage, train.mse)])
# shrinkage of 0.04 seems to be a very good number already.
mse.dt[shrinkage == 0.04]$test.mse

# () Comparing GBM with two other regression algorithms
# linear regression
lm.fit <- lm(Salary ~ ., data = train)
mse.lm <- mean((predict(lm.fit, test)-test$Salary)^2)

require(glmnet)
grid <- 10^seq(10, -2, length = 100)
train[, League := as.numeric(League)]
train[, Division := as.numeric(Division)]
train[, NewLeague := as.numeric(NewLeague)]

x <- as.matrix(train[,-c("Salary")])
y <- train[,Salary]
names(y) <- "y"
lasso.fit <- cv.glmnet(x, y,
                    alpha = 1,
                    lambda = grid,
                    thresh = 1e-12)

# (f) Which variables appear to be the 
# most important predictors in the boosted model?

par(mfrow = c(1,1))
boost.sum <- summary(boost.dt)
plot(boost.sum)
boost.sum <- data.table(boost.sum)
setorder(boost.sum, -rel.inf)
# most important variables
# CAtBat, CHits, CRBI...
head(boost.sum)

# (g) applying bagging
rf.model <- randomForest(Salary ~ ., 
                         data = train,
                         importance = TRUE,
                         ntree = 100)
plot(rf.model)
varImpPlot(rf.model)
pred <- predict(rf.model, test)
mse.bag <- mean((pred-test$Salary)^2)

# In summary
#Boosting test MSE
mse.dt[shrinkage == 0.4]$test.mse
#Linear Regression test MSE
mse.lm
#Bagging test MSE
mse.bag

# Bagging showed the smallest MSE, though boosting 
# is safer for avoiding overfit

## QUESTION 11 ----

dt <- data.table(Caravan)
nrow(dt)

dt.bk <- copy(dt)
dt[, Purchase := ifelse (Purchase == "Yes", 1, 0)]
train <- dt[1:1000,]
test <- dt[1001:nrow(dt),]

# (b) Fit a boosting model with Purchase 
# as the response, ntree = 1000 and lambda = 0.01

boost.model <- gbm(Purchase ~ ., data = train, 
    distribution = "bernoulli",
    n.trees = 1000,
    shrinkage = 0.01,
    verbose = F)

boost.sum <- summary(boost.model)
boost.sum <- data.table(boost.sum)
setorder(boost.sum, -rel.inf)
# Most important variables
head(boost.sum)

# (c)
pred.odds <- predict.gbm(boost.model, 
                         newdata = test, 
                         n.trees = 1000)
test.results <- data.table(Purchase = test$Purchase, 
           pred.odds = pred.odds)
test.results[, prob := plogis(pred.odds)]
test.results[, pred := ifelse(prob>=0.2, "Yes", "No")]
# Confusion matrix
table(test.results$pred, dt.bk[1001:nrow(dt.bk),]$Purchase)
# The fraction of the people predicted
# to make a purchase who do in fact make one is:
100*34/(34+121)
# 21.935%, very bad

require(class)
?knn
pred.knn <- knn(dt[1:1000], dt[1001:nrow(dt)], factor(dt.bk[1:1000,]$Purchase), 
                k = 3)
table(pred.knn, dt.bk[1001:nrow(dt.bk),]$Purchase)
100*10/(62+10)
# 13.88%, very bad, but the chapter on knn showed 
# that for this dataset, if all data is used for 
# training, an accuracy up to 26.7% regarding "Yes"
# could be achieved.

glm.fit <- glm(Purchase ~ ., 
               data = train, 
               family = binomial)
glm.prob <- predict(glm.fit, newdata = test, 
                    type = "response")
glm.pred = rep("No", 1000)
glm.pred[glm.prob > 0.2] <- "Yes"
table(glm.pred, dt.bk[1001:nrow(dt.bk)]$Purchase)
100*58/(58+408)
# 12.44 %
# So, a priori, using only 1000 observations for 
# training, boosting showed to be much superior
# though it predicted Purchases very poorly.

## QUESTION 12 ----

# Reference
# https://www.cgdev.org/publication/dataset-vulnerability-climate-change

dt <- data.table(iris)
summary(dt)
train <- sample(1:nrow(dt), 75)

# Boosting
gbm.fit <- gbm(Species ~ ., 
              data = dt[train],
              distribution = "multinomial",
              n.trees = 1000,
              shrinkage = 0.002,
              verbose = F)
prob <- predict(gbm.fit, 
                dt[-train], 
                n.trees = 1000,
                type = "response")
results <- data.table(data.frame(prob))
names(results) <- c("setosa", "versicolor", "virginica")
results[, max := colnames(.SD)[max.col(.SD, ties.method="first")]]
results[, real := dt[-train]$Species]
table(results$max, results$real)
# missclassification: 1/28, or 1/50

# bagging
require(randomForest)
bag.rf <- randomForest(Species ~ ., 
                   data = dt[train],
                   ntree = 100, 
                   mtry = 4)
pred <- predict(bag.rf, dt[-train])
table(pred, dt[-train, Species])
# missclassification: 3/30, or 3/50

# randomForest
rf <- randomForest(Species ~ ., 
             data = dt[train],
             ntree = 100)
pred <- predict(rf, dt[-train])
table(pred, dt[-train, Species])
# missclassification: 3/30, or 3/50

# linear discriminant analysis
lda.fit <- lda(Species~., 
             data = dt[train,])
pred <- predict(lda.fit, dt[-train])
table(pred$class, dt[-train, Species])
# missclassification: 2/29, or 2/50

# if another dataset were used, with more predictors and observations
# the difference among the models would be easier to distinguish
