#Libraries ----
library(MASS)
library(ISLR)
library(data.table)
library(class)

# 4.6.1 - The Stock Market Data ----
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[,-c(9)])
# The only substantial correlation is betweem 
# Year and Volume. 
attach(Smarket)
plot(Volume)

# 4.6.2 - Logistic Regression ----
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
    data = Smarket, family = binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef[,4]

contrasts(Direction)
glm.probs = predict(glm.fit, type = "response")
glm.probs[1:10]
glm.pred = rep("Down",1250)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred, Direction)
mean(glm.pred==Direction)

train <- (Year<2005)
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fit, Smarket.2005, type = "response")
glm.pred = rep("Down",252)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

glm.fit <- glm(Direction~Lag1+Lag2,
               data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fit, Smarket.2005, type = "response")
glm.pred = rep("Down",252)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

require(data.table)
predict(glm.fit, newdata=data.table("Lag1"=c(1.2,1.5), 
                                     "Lag2"=c(1.1,-0.8)), type = "response")

# 4.6.3 - Liner Discriminant Analysis ----

lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
plot(lda.fit)
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class==Direction.2005)

sum(lda.pred$posterior[,1]>=0.5)
sum(lda.pred$posterior[,1]<0.5)

lda.pred$posterior[1:20,1]
lda.class[1:20]

# 4.6.4 - Quadratic Discriminant Analysis ----

qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

# 4.6.5 - K-nearest neighboors ----
library(class)

dt <- data.table(Smarket)
train.X <- dt[train,.(Lag1, Lag2)]
test.X <- dt[!train,.(Lag1, Lag2)]
train.Direction <- dt[train,Direction]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
(83+43)/252

knn.pred <- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005)

# 4.6.6 - An Application to Caravan Insurance Data ----
require(ISLR)
dt <- data.table(Caravan)
dim(dt)
summary(dt[,Purchase])
348/(5474+348)

sdt <- data.table(scale(dt[,-c(86)]))
var(dt[,1])
var(sdt[,1])
var(sdt[,2])

test <- c(1:1000)
train.dt <- sdt[-test,]
test.dt <- sdt[test,]
train.Y <- dt[-test,Purchase] 
test.Y <- dt[test,Purchase] 

set.seed(1)

# K = 1
knn.pred <- knn(train.dt, test.dt, train.Y, k = 1)
mean(knn.pred==test.Y)
#mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred, test.Y)
9/(68+9)
77/(873+50)

# K = 3
knn.pred <- knn(train.dt, test.dt, train.Y, k = 3)
mean(knn.pred==test.Y)
#mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred, test.Y)
5/(21+5)

# K = 5
knn.pred <- knn(train.dt, test.dt, train.Y, k = 5)
mean(knn.pred==test.Y)
#mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred, test.Y)
4/(11+4)

glm.fits <- glm(Purchase ~ ., data = dt, family = binomial,
                 subset=-test)
glm.probs <- predict(glm.fits, dt[test,], type = "response")
glm.pred <- rep("No",1000)
glm.pred[glm.probs>0.5] <- "Yes"
table(glm.pred, test.Y)
glm.pred <- rep("No",1000)
glm.pred[glm.probs>0.25] <- "Yes"
table(glm.pred, test.Y)
