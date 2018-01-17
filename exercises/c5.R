require(data.table)
require(ISLR)

## Question 05 ----

dt <- data.table(Default)

glm.fit <- glm(default ~ income+balance, 
    data = dt,
    family = binomial)

# (b)
dt <- sample(dt)
train <- dt[1:6667]
test <- dt[6668:10000]

glm.fit <- glm(default ~ income+balance, 
               data = train,
               family = binomial)
glm.prob <- data.table("prob" = predict(glm.fit, test, type = "response"))
glm.prob[prob > 0.5, pred := "Yes"]
glm.prob[is.na(pred),pred := "No"]
table(glm.prob$pred, test$default)
# Overall fraction of error in validation dataset:
(12+72)/nrow(glm.prob)
# 2.52%

# c)
train <- dt[1:8000]
test <- dt[8001:10000]

glm.fit <- glm(default ~ income+balance, 
               data = train,
               family = binomial)
glm.prob <- data.table("prob" = predict(glm.fit, test, type = "response"))
glm.prob[prob > 0.5, pred := "Yes"]
glm.prob[is.na(pred),pred := "No"]
table(glm.prob$pred, test$default)
# Overall fraction of error in validation dataset:
(7+43)/nrow(glm.prob)
# 2.5%
# The error decreased a bit, but that is explained by having less variables 
# in the validation set, so that the model already know how to predict too
# many values


# d)

dt[student=="Yes",st:=1]
dt[is.na(st),st:=0]

train <- dt[1:6667]
test <- dt[6668:10000]

glm.fit <- glm(default ~ income+balance+st, 
               data = train,
               family = binomial)
glm.prob <- data.table("prob" = predict(glm.fit, test, type = "response"))
glm.prob[prob > 0.5, pred := "Yes"]
glm.prob[is.na(pred),pred := "No"]
table(glm.prob$pred, test$default)
# Overall fraction of error in validation dataset:
(16+71)/nrow(glm.prob)
# 2.61%
# The error increased a bit, meaning that student might not be that much 
# important when feeding our model

## Question 06 ----

dt <- data.table(Default)

glm.fit <- glm(default ~ income+balance, 
               data = dt,
               family = binomial)
# a)
summary(glm.fit)
#std.error: 
#  income 2.965e-6
#  balance 2.274e-04  

# b)

boot.fn <- function(dt, index)
  return(coef(glm(default ~ income+balance, data = dt[index,], 
      family = binomial)))

dt <- data.table(Default)
nrow(dt)
boot.fn(dt, 1:900)

require(boot)
boot(data = dt, statistic = boot.fn, R = 1000)
# Comparing the two methods, the income had the most different coefficient
# for the different approaches

## QUESTION 07 ----

dt <- data.table(Weekly)

glm.fit <- glm(Direction ~ Lag1 + Lag2, data = dt, family = binomial)
plot(glm.fit)

glm.fit2 <- glm(Direction ~ Lag1 + Lag2, data = dt[-1,], family = binomial)

glm.prob <- data.table("prob" = predict(glm.fit2, dt[1,], type = "response"))
glm.prob[prob > 0.5, pred := "Up"]
glm.prob[is.na(pred),pred := "Down"]
table(glm.prob$pred, dt[1,Direction])
# The prediction says that it is up, when actually it is down 

# d) In a loop
n <- rep(0, nrow(dt))
for (i in 1:nrow(dt)){
  glm.fit <- glm(Direction ~ Lag1 + Lag2, data = dt[-i,], family = binomial)
  glm.prob <- data.table("prob" = predict(glm.fit, dt[i,], type = "response"))
  glm.prob[,pred := ifelse(prob > 0.5, "Up", "Down")]
  if (glm.prob$pred != dt[i,Direction])
    n[i] <- 1
}
#LOOCV Error 
cat(paste0((round(sum(n)*100/length(n),3)),"%"))
# A relatively high error, but the error of predicting all the 
# training dataset is already high, about 44.44%


## QUESTION 07 ----
set.seed(1)
x = rnorm(100)
y = x-2*x^2+rnorm(100)

# a) n is 100 points and p is 1, only one predictor, the x
# b) The relationship seems to be quadratic, from the scatterplot,
# so we expect to have a better fit if we use
# X and the square of X
plot(x,y)
# c) 
set.seed(1)
x = rnorm(100)
dt <- data.table(x,y = x-2*x^2+rnorm(100))
plot(dt)
# i.) Y = B0+B1X+e
cv.glm(dt, glm(y ~ x, data = dt))$delta
# i.) Y = B0+B1X+B2X2+e
cv.glm(dt, glm(y ~ x+I(x^2), data = dt))$delta
# i.) Y = B0+B1X+B2X2+B3X3+e
cv.glm(dt, glm(y ~ poly(x,3), data = dt))$delta
# i.) Y = B0+B1X+B2X2+B3X3+B4X4+e
cv.glm(dt, glm(y ~ poly(x,4), data = dt))$delta
# d)
set.seed(2)
x = rnorm(100)
dt <- data.table(x,y = x-2*x^2+rnorm(100))
plot(dt)
# i.) Y = B0+B1X+e
cv.glm(dt, glm(y ~ x, data = dt))$delta
# i.) Y = B0+B1X+B2X2+e
cv.glm(dt, glm(y ~ x+I(x^2), data = dt))$delta
# i.) Y = B0+B1X+B2X2+B3X3+e
cv.glm(dt, glm(y ~ poly(x,3), data = dt))$delta
# i.) Y = B0+B1X+B2X2+B3X3+B4X4+e
cv.glm(dt, glm(y ~ poly(x,4), data = dt))$delta

# The results changed a bit, since the random values are 
# not coming from the same seed. The scatter plots show the 
# different patterns

