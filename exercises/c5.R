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

glm.fit <- glm(default ~ income+balance, 
               data = dt,
               family = binomial)
summary(glm.fit)
#std.error: 
#  income 2.965e-6
#  balance 2.274e-04  