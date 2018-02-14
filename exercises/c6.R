## Conceptual

# 1. 
# Best subset. forward stepwise and backward stepwise
# Obtaining p+1 models, containing 0, 1, 2..., p predictors.
# 
# a) Best subset, because the chance of finding models that look
# good on the training data is high, even though they might not 
# have any predictive power on future data.
# b) Forward stepwise and backward stepwise have the smallest 
# testing RSS because in the end it will select the best fit before 
# using all the p number of predictors
# c) i.FALSE ii.TRUE iii.FALSE iv.TRUE v.TRUE

# 2.
# Bias - Variance trade off in R
# http://web.as.uky.edu/statistics/users/pbreheny/603.R

# a(i), b(i), c(i) 
# Sice variance decreases and consequentely, 
# bias increases.

# 3. CHECK
# (a) ii or iii
# (b) i or iv
# (c) iv
# (d) iii
# (e) v



## Applied

## QUESTION 8 ----

require(leaps)
require(data.table)
# a)
x <- rnorm(100)
e <- rnorm(100, sd = 0.1)

# b)
b0 <- 2
b1 <- -2
b2 <- 6
b3 <- 0.5
y <- b0 + b1*x + b2*x^2 + b3*x^3 + e 

dt <- data.table(y,x)

# c)
regfit.full <- regsubsets(y ~ poly(x, 10), dt)
reg.summary <- summary(regfit.full)
names(reg.summary)
plot(reg.summary$rsq)
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS")

plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq")
which.max(reg.summary$adjr2)
points(4,reg.summary$adjr2[4], col="red",cex=2,pch=20)

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp")
which.min(reg.summary$cp)
points(4,reg.summary$cp[4], col="red",cex=2,pch=20)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC")
which.min(reg.summary$bic)
points(4,reg.summary$bic[4], col="red",cex=2,pch=20)

plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

# Wee see that subseting up to the fourth subset x^0:4
# is already enough based on R2, Cp and RSS.

# d)
# FORWARD SELECTION
regfit.fwd <- regsubsets(y ~ poly(x, 10), dt, method = "forward")
# BACKWARD SELECTION
regfit.bwd <- regsubsets(y ~ poly(x, 10), dt, method = "backward")

reg.summary.fwd <- summary(regfit.fwd)
reg.summary.bwd <- summary(regfit.bwd)

par(mfrow = c(1,2))

plot(reg.summary.fwd$rss, xlab = "Number of Variables", ylab = "RSS")
plot(reg.summary.bwd$rss, xlab = "Number of Variables", ylab = "RSS")

coef(regfit.full, 4)
coef(regfit.fwd, 4)
coef(regfit.bwd, 4)

#All the methods gave us the same results

# e) LASSO
install.packages("glmnet")
require(glmnet)

dt.bk <- copy(dt)

for(a in 2:10){
  dt[,c(paste0("x",a)):=x^a]
}

grid <- 10*seq(10,0,length = 100)
train <- sample(1:nrow(dt), nrow(dt)/2)
lasso.mod <- glmnet(as.matrix(dt[train, -c("y")]), dt[train]$y, alpha = 1, lambda = grid)
par(mfrow = c(1,1))
plot(lasso.mod)

set.seed(1)
cv.out <- cv.glmnet(as.matrix(dt[train, -c("y")]), dt[train]$y, alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = as.matrix(dt[-train, -c("y")]))
# The mean squared error is then
mean((lasso.pred-dt[-train, y])^2)

plot.df <- data.table("lambda"=as.numeric(), "mse"=as.numeric())

for (j in 1:length(cv.out$lambda)){
  plot.df <- rbind(plot.df, data.table("lambda" = cv.out$lambda[j], 
                  "mse" = mean((predict(lasso.mod, s = cv.out$lambda[j], newx = as.matrix(dt[-train, -c("y")]))-dt[-train, y])^2)))
}
setkey(plot.df, lambda)
plot(plot.df)

out <- glmnet(as.matrix(dt[,-c("y")]), dt$y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)
lasso.coef
ylasso <- 2 - 1.859*x^1 + 5.938*x^2 - 0.4403*x^3
par(mfrow = c(2,2))
plot(x, y)
plot(x, ylasso)
plot(y-ylasso)
plot(y,ylasso)

# f)

b7 = 1 
e <- rnorm(100, sd = 10)
y <- b0 + b7*x^7+e
plot(x,y)

dt <- data.table(y,x)
regfit.full <- regsubsets(y ~ poly(x, 10), dt)
reg.summary <- summary(regfit.full)
names(reg.summary)
plot(reg.summary$rsq)
par(mfrow = c(2,2))
which.max(reg.summary$adjr2)

dt.bk <- copy(dt)
for(a in 2:10){
  dt[,c(paste0("x",a)):=x^a]
}


cv.out <- cv.glmnet(as.matrix(dt[train, -c("y")]), dt[train]$y, alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
out <- glmnet(as.matrix(dt[,-c("y")]), dt$y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)
lasso.coef

y <- 2.4 + 0.13*x^5 + 0.954*x^7
# If I decrease the error on the measurements, it will perform much better

e <- rnorm(100, sd = 0.1)
y <- b0 + b7*x^7+e
plot(x,y)

dt <- data.table(y,x)
dt.bk <- copy(dt)
for(a in 2:10){
  dt[,c(paste0("x",a)):=x^a]
}

cv.out <- cv.glmnet(as.matrix(dt[train, -c("y")]), dt[train]$y, alpha = 1)
bestlam <- cv.out$lambda.min
out <- glmnet(as.matrix(dt[,-c("y")]), dt$y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)
lasso.coef
y <- 2.088 + 0.9608*x^7

# Different from the ridge regression, the lasso could give us sparse
# coefficients, so that some of them could be zero.

## QUESTION 09 ----
require(ISLR)
dt <- data.table(College)
dt[Private=="Yes", Private2:=1]
dt[Private=="No", Private2:=0]
dt[, Private:=NULL]
dt[, Private:=Private2]
dt[, Private2:=NULL]

# a)
train <- sample(1:nrow(dt), 0.6*nrow(dt))

# b)
lm.fit <- lm(Apps ~ ., dt[train])
#Train error (RMSE):
sqrt(mean((predict(lm.fit, dt[train], type = "response")-dt[train,Apps])^2))
#Test error (RMSE):
mean((predict(lm.fit, dt[-train], type = "response")-dt[-train,Apps])^2)

# c)
set.seed(1)
ridge.mode <- glmnet(as.matrix(dt[train, -c("Apps")]), dt[train]$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mode, s = 4, newx = as.matrix(dt[-train, -c("Apps")]))
cv.out <- cv.glmnet(as.matrix(dt[train, -c("Apps")]), dt[train]$Apps, alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

out <- glmnet(as.matrix(dt[train,-c("Apps")]), dt[train]$Apps, alpha = 0, lambda = bestlam)
ridge.pred <- predict(out, as.matrix(dt[-train, -c("Apps")]))
mean((ridge.pred-dt[-train,Apps])^2)

cv.out <- cv.glmnet(as.matrix(dt[train, -c("Apps")]), dt[train]$Apps, alpha = 1)
bestlam <- cv.out$lambda.min
out <- glmnet(as.matrix(dt[train,-c("Apps")]), dt[train]$Apps, alpha = 1, lambda = bestlam)
lasso.pred <- predict(out, as.matrix(dt[-train, -c("Apps")]))
mean((lasso.pred-dt[-train,Apps])^2)

# PCR
require(pls)
pcr.fit <- pcr(Apps ~ ., data = dt, scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")
# Let's pick 6 components then

pcr.fit <- pcr(Apps ~ ., data = dt, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

pcr.pred <- predict(pcr.fit, dt[-train, -c("Apps")], ncomp = 6)
mean((pcr.pred-dt[-train,Apps])^2)

# PCR MSE -> 4065079
# LM MSE  -> 1749245
# RIDGE   -> 1783026
# LASSO   -> 1825693

