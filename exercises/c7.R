## Conceptual

# 1. 

## Applied

# libraries ----
library(data.table)
library(splines)
library(akima)
library(ISLR)


## QUESTION 06 ----
dt <- data.table(Wage)

train <- sample(1:nrow(dt), 0.8*nrow(dt))

fit1 <- lm(wage ~ poly(age,1), data = dt[train])
fit2 <- lm(wage ~ poly(age,2), data = dt[train])
fit3 <- lm(wage ~ poly(age,3), data = dt[train])
fit4 <- lm(wage ~ poly(age,4), data = dt[train])
fit5 <- lm(wage ~ poly(age,5), data = dt[train])
fit6 <- lm(wage ~ poly(age,6), data = dt[train])
cv.df <- data.table("degree"=as.numeric(), "mse"=as.numeric())
cv.df <- rbind(cv.df, data.table(degree = 1, mse = mean((dt[-train, wage]-predict(fit1, dt[-train]))^2)))
cv.df <- rbind(cv.df, data.table(degree = 2, mse = mean((dt[-train, wage]-predict(fit2, dt[-train]))^2)))
cv.df <- rbind(cv.df, data.table(degree = 3, mse = mean((dt[-train, wage]-predict(fit3, dt[-train]))^2)))
cv.df <- rbind(cv.df, data.table(degree = 4, mse = mean((dt[-train, wage]-predict(fit4, dt[-train]))^2)))
cv.df <- rbind(cv.df, data.table(degree = 5, mse = mean((dt[-train, wage]-predict(fit5, dt[-train]))^2)))
cv.df <- rbind(cv.df, data.table(degree = 6, mse = mean((dt[-train, wage]-predict(fit6, dt[-train]))^2)))

#plot(cv.df)
# Up to the 3rd or 4th degree, the mse does not really change
# With anova:
anova(fit1, fit2, fit3, fit4, fit5, fit6)
# Up to the 4th degree

fit.final <- lm(wage ~ poly(age,4), data = dt)

par(mfrow = c(1,2))
plot(dt[,.(age, wage)], col = "darkgrey")
agelims <- range(dt[,age])
age.grid <- seq(agelims[1],agelims[2])
preds <- predict(fit.final, list(age = age.grid), se = TRUE)
lines(age.grid, preds$fit)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit-2*preds$se.fit)
matlines(age.grid, se.bands, lty = "dashed", col = "blue")

# b) 
table(cut(dt[,age],2))
fit.cut2 <- lm(wage ~ cut(age, 2), data = dt[train])
fit.cut3 <- lm(wage ~ cut(age, 3), data = dt[train])
fit.cut4 <- lm(wage ~ cut(age, 4), data = dt[train])
fit.cut5 <- lm(wage ~ cut(age, 5), data = dt[train])
fit.cut6 <- lm(wage ~ cut(age, 6), data = dt[train])

cv.df <- data.table("cuts"=as.numeric(), "mse"=as.numeric())
cv.df <- rbind(cv.df, data.table(cuts = 2, mse = mean((dt[-train, wage]-predict(fit.cut2, dt[-train]))^2)))
cv.df <- rbind(cv.df, data.table(cuts = 3, mse = mean((dt[-train, wage]-predict(fit.cut3, dt[-train]))^2)))
cv.df <- rbind(cv.df, data.table(cuts = 4, mse = mean((dt[-train, wage]-predict(fit.cut4, dt[-train]))^2)))
cv.df <- rbind(cv.df, data.table(cuts = 5, mse = mean((dt[-train, wage]-predict(fit.cut5, dt[-train]))^2)))
cv.df <- rbind(cv.df, data.table(cuts = 5, mse = mean((dt[-train, wage]-predict(fit.cut5, dt[-train]))^2)))
cv.df <- rbind(cv.df, data.table(cuts = 6, mse = mean((dt[-train, wage]-predict(fit.cut6, dt[-train]))^2)))
#plot(cv.df)
#Optimal number of cuts -> 4

plot(dt[,.(age, wage)], col = "darkgrey")
preds <- predict(fit.cut4, list(age = age.grid), se = TRUE)
lines(age.grid, preds$fit)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit-2*preds$se.fit)
matlines(age.grid, se.bands, lty = "dashed", col = "blue")

## QUESTION 07 ----

str(dt)
plot(dt)

# let's pick some new attributes
sdt <- dt[,.(wage, maritl, race, education, region, jobclass, health, health_ins)]
plot(dt[,.(maritl, wage)])
plot(dt[,.(education, wage)])
plot(dt[,.(region, wage)])
plot(dt[,.(jobclass, wage)])
plot(dt[,.(health, wage)])
plot(dt[,.(health_ins, wage)])

fit <- glm(wage ~ poly(age,4) + maritl + race + education + jobclass + health + health_ins, data = dt)
summary(fit)

# age, maritl, race, education, jobclass, health and health_ins, i.e all seems to be 
# the ones that better describe wage
# let's now do a subset evaluation

sdt$region <- NULL

require(leaps)
sub <- regsubsets(wage ~ ., data = sdt, nvmax = 20)
reg.sub <- summary(sub)
par(mfrow = c(2,2))
plot(reg.sub$rss, 
     xlab = "Number of variables", 
     ylab = "RSS")
which.min(reg.sub$rss)
plot(reg.sub$adjr2, 
     xlab = "Number of variables", 
     ylab = "Adjusted RSq")
which.max(reg.sub$adjr2)
plot(reg.sub$cp, 
     xlab = "Number of variables", 
     ylab = "Cp")
which.min(reg.sub$cp)
plot(reg.sub$bic, 
     xlab = "Number of variables", 
     ylab = "BIC")
which.min(reg.sub$bic)

coef(sub, 7)


#plot(reg.sub, scale = "r2")
#fit.final <- glm(wage ~ poly(age,4) + maritl + race + education + jobclass + health + health_ins, data = dt)

## QUESTION 8 ----

dt <- data.table(Auto)
str(dt)
plot(dt)

# mpg seems to relate to all the predictors but name, so let's first
# model mpg with each cylinders and origin in a linear way and all
# the others with polinomials

summary(lm(mpg ~ poly(displacement, 5), data = dt))
# Using ANOVA to compare nested models 
fit1 <- lm(mpg ~ displacement, data = dt)
fit2 <- lm(mpg ~ poly(displacement,2), data = dt)
fit3 <- lm(mpg ~ poly(displacement,3), data = dt)
fit4 <- lm(mpg ~ poly(displacement,4), data = dt)
fit5 <- lm(mpg ~ poly(displacement,5), data = dt)
anova(fit1, fit2, fit3, fit4, fit5)
#suggesting a two-degree polinomial
summary(lm(mpg ~ poly(horsepower, 5), data = dt))
summary(lm(mpg ~ poly(weight, 5), data = dt))
# but we can clearly see that horsepower and weight are very 
# linear compared to dispacement, so let's use only on attribute
View(cor(dt[,-c("name")]))
summary(lm(mpg ~ poly(acceleration, 5), data = dt))
# since the p-values were very different, let's use a fit with splines
# on this attributes, or a cut
summary(lm(mpg ~ cylinders, data = dt))
summary(lm(mpg ~ origin, data = dt))

# To specify knots with uniform quantiles:
hist(dt$acceleration)
dim(bs(dt$acceleration, df = 6))
attr(bs(dt$acceleration, df = 6), "knots")

acc.grid <- seq(range(dt$acceleration)[1],range(dt$acceleration)[2], 0.1)
# For natural splines
fitaux1 <- lm(mpg ~ ns(acceleration, df = 4), data = dt)
pred <- predict(fitaux1, list(acceleration = acc.grid), se = TRUE)
plot(dt[,.(acceleration, mpg)])
lines(acc.grid, pred$fitaux1, col = "red", lwd = 2)

year.grid <- seq(range(dt$year)[1],range(dt$year)[2], 1)

fitaux2 <- lm(mpg ~ ns(year, df = 4), data = dt)
pred <- predict(fitaux2, list(year = year.grid), se = TRUE)
plot(dt[,.(year, mpg)])
lines(year.grid, pred$fitaux2, col = "red", lwd = 2)

trainIndex <- sample(1:nrow(dt), 0.75*nrow(dt))

fit1 <- lm(mpg ~ poly(displacement, 2) + ns(year, df = 4) + cylinders + origin, data = dt[trainIndex])
pred <- predict(fit1, dt[-trainIndex])
mean((dt[trainIndex, mpg]-predict(fit1, dt[trainIndex]))^2)
mean((dt[-trainIndex, mpg]-pred)^2)


fit2 <- lm(mpg ~ poly(displacement, 2) +  cylinders + origin, data = dt[trainIndex])
pred <- predict(fit2, dt[-trainIndex])
mean((dt[trainIndex, mpg]-predict(fit2, dt[trainIndex]))^2)
mean((dt[-trainIndex, mpg]-pred)^2)

# fit1 seems to be a good fit

# however, a regular fit seemed good already, one could use a lasso approach for
# the polinomial regressions

## QUESTION 09 ----

require(MASS)
dt <- data.table(Boston)[,.(dis, nox)]
dt
# a) 
dis.grid <- seq(range(dt$dis)[1],range(dt$dis)[2], by = 0.1)
 
fit1 <- lm(nox ~ poly(dis, 3), data = dt)
band <- predict(fit1, list(dis = dis.grid), se = TRUE)
tol <- cbind(band$fit + 2*band$se.fit, band$fit - 2*band$se.fit)

plot(dt)
lines(dis.grid, band$fit, col = "blue")
matlines(dis.grid, tol, col = "red", lty = "dashed")
par(mfrow = c(2,5))
for (i in 1:10){
  fit <- lm(nox ~ poly(dis, i), data = dt)
  band <- predict(fit, list(dis = dis.grid), se = TRUE)
  tol <- cbind(band$fit + 2*band$se.fit, band$fit - 2*band$se.fit)
  mse <- mean((dt$nox-predict(fit, dt))^2)
  plot(dt, col="darkgrey")
  lines(dis.grid, band$fit, col = "blue", lwd = 2)
  matlines(dis.grid, tol, col = "red", lty = "dashed", lwd = 2)
  title(paste0("Polynomial fit dis^", i))
  legend(4, 0.8, legend=paste0("mse.error = ", round(mse, 5)),cex=0.75)
}

# c) Selecting the best approach for choosing degree
nrow(dt)
train <- sample(1:nrow(dt), 0.75*nrow(dt))

mse.dt <- data.table("deg" = as.numeric(), "mse" = as.numeric())
for (j in 1:10){
  fit <- lm(nox ~ poly(dis, j), data = dt[train])
  mse <- mean((dt[-train]$nox - predict(fit, dt[-train]))^2)
  mse.dt <- rbind(mse.dt, data.table("deg"=j,"mse"=mse))
}
par(mfrow = c(1,1))
plot(mse.dt)
# So that the cubic polynomial is enough for decreasing the mse.


## 7.8.2 Splines

# bs() by default generates cubic splines
knots <- attr(bs(dt$dis, df = 6), "knots")
fit <- lm(nox ~ bs(dis, knots = knots), data = dt)
pred <- predict(fit, list(dis = dis.grid), se = TRUE)
plot(dt, col = "gray")
lines(dis.grid, pred$fit, lwd = 2)
lines(dis.grid, pred$fit + 2*pred$se.fit, lty = "dashed")
lines(dis.grid, pred$fit - 2*pred$se.fit, lty = "dashed")


## QUESTION 11 ----
x1 <- rnorm(100,4)
x2 <- rnorm(100,25)

# b) Initializing B1
B0 <- 1
B1 <- 2
B2 <- -0.1
# c) Fitting the model Y - B1x1 = B0 + B2x2 + e
y <- B0 + B1*x1 + B2*x2 + rnorm(100,0.25)

df <- data.table("id" = as.numeric(), 
                 "B0" = as.numeric(),
                 "B1" = as.numeric(),
                 "B2" = as.numeric())

b0 <- 1
b1 <- 1
for (i in 1:1000) {
  a <- y - b1*x1
  b2 <- lm(a ~ x2)$coef[2]
  a <- y - b2*x2
  b1 <- lm(a ~ x1)$coef[2]
  b0 <- lm(a ~ x1)$coef[1]
  df <- rbind(df,data.table("id" = i, "B0" = b0, "B1" = b1, "B2" = b2))
}

df.melt <- melt(df, id="id")
names(df.melt) <- c("id","coef","value")

b0 <- lm(y ~ x1 + x2)$coef[1]
b1 <- lm(y ~ x1 + x2)$coef[2]
b2 <- lm(y ~ x1 + x2)$coef[3]

require(ggplot2)
ggplot(df.melt[id <= 11])+
  geom_line(aes(x = id, y = value, color = coef))+
  theme_classic()+
  labs(x = "iteration", y="coefficient")+
  geom_hline(yintercept = b0, size = 0.1)+
  geom_hline(yintercept = b1, size = 0.1)+
  geom_hline(yintercept = b2, size = 0.1)

# Only two iterations were enough to get a good approximation
# for the coefficients, but that's due to model simplicity.

bcoef <- data.table("b1"= 2+rnorm(1,3))
for (z in 1:100) 
  bcoef[,paste0("b",z):=b1^(1/z)+rnorm(1,2)]
b0 <- 4

x <- data.table("x1"= rnorm(100,3))
for (z in 1:100) 
  x[,paste0("x",z):=x1^(1/z)+rnorm(1,0.01)]

y <- sum(as.matrix(bcoef)%*%as.matrix(x))+b0+rnorm(100,0.25)

#y <- apply(x, 1, sum)+rnorm(100,1.3)

blist <- data.table(b = rep(1, 100))
#direct approach
{
  input <- paste0("x$",names(x), collapse = "+")
  f <- paste0("y ~ ", input)
  blist.true <- as.numeric(lm(f)$coef)
  blist.true <- blist.true[-1]
}
# iteration approach
{
  df.total <- data.table("id"=as.numeric(),"error"=as.numeric())

  for (i in 1:100) {
    for (j in 1:100){
      if (!is.na(blist[j])) {    
        a <- y - as.numeric(blist[j])*x[,j,with=FALSE]
        #a[is.na(a)] <- 0
        input <- paste0("x$",names(x[,-j,with=FALSE]), collapse = "+")
        f <- paste0("a$",names(a)," ~ ", input)
        #blist[is.na(b)] <- 0
        blist[-j,] <- lm(f)$coef[-1]
      }
    }
    df <- data.table("id"=i, "error"=mean((as.numeric(blist$b)-blist.true)^2, na.rm = T))
    df.total <- rbind(df.total, df)
    #df <- rbind(df,data.table("id" = i, "B0" = b0, "B1" = b1, "B2" = b2))
  }
}
plot(df.total)
which.min(df.total$error)

# After 3 iterations, it converges, however, the differences between 
# the coefficients is still high, since it is high values.

head(blist)
head(blist.true)
