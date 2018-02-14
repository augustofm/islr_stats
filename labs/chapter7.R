#Libraries ----
library(ISLR)
library(data.table)
library(splines)
library (gam)
library(akima)

## 7.8.1 Polynomial Regression and Step Functions
dt <- data.table(Wage)

fit <- lm(wage ~ poly(age,4), data = dt)
coef(summary(fit))

# But if one wants the coef. for each term
# age^1, age^2...
fit2 <- lm(wage ~ poly(age,4, raw = TRUE), data = dt)
coef(summary(fit2))

agelims <- range(dt[,age])
age.grid <- seq(agelims[1], agelims[2])
pred <- predict(fit2, list(age=age.grid), se = TRUE)
se.bands <- cbind(pred$fit + 2*pred$se.fit, pred$fit-2*pred$se.fit)

#Plotting the data
par(mfrow = c(1,2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(dt[,age], dt[,wage], xlim = agelims, cex = 0.5, col = "darkgrey")
title("Degree -4 Polynomial", outer = T)
lines(age.grid, pred$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# Using ANOVA to compare nested models 
fit1 <- lm(wage ~ age, data = dt)
fit2 <- lm(wage ~ poly(age,2), data = dt)
fit3 <- lm(wage ~ poly(age,3), data = dt)
fit4 <- lm(wage ~ poly(age,4), data = dt)
fit5 <- lm(wage ~ poly(age,5), data = dt)
anova(fit1, fit2, fit3, fit4, fit5)
# either a cubic or a quartic polynomial
# appear to provide a resonable fit to the data
# but lower- or higher-order models are not justified.

coef(summary(fit5))
# yet, anova alows us to have other terms
# in the model as well
fit1 <- lm(wage ~ education + age, data = dt)
fit2 <- lm(wage ~ education + poly(age,2), data = dt)
fit3 <- lm(wage ~ education + poly(age,3), data = dt)
anova(fit1, fit2, fit3)

fit <- glm(I(wage>250) ~ poly(age, 4), data = dt, family = "binomial")
preds <- predict(fit, list(age = age.grid), se = TRUE)
pfit <- exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit <- cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands <- exp(se.bands.logit)/(1+exp(se.bands.logit))

plot(dt[,age], I(dt$wage>250), xlim = agelims, type = "n", ylim = c(0,0.2))
# Rug plot
points(jitter(dt$age), I((dt$wage>250)/5), cex = 0.5, pch = "|", col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# Fitting a step function with cut()

table(cut(dt[,age],4))
fit <- lm(wage ~ cut(age, 4), data = dt)
coef(summary(fit))
# we can set the break points manually by 
# using breaks within the cut function

## 7.8.2 Splines

# bs() by default generates cubic splines
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = dt)
pred <- predict(fit, list(age = age.grid), se = TRUE)
plot(dt[,.(age,wage)], col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2*pred$se.fit, lty = "dashed")
lines(age.grid, pred$fit - 2*pred$se.fit, lty = "dashed")

# To specify knots with uniform quantiles:
dim(bs(dt$age, knots = c(25,40,60)))
dim(bs(dt$age, df = 6))
attr(bs(dt$age, df = 6), "knots")

# For natural splines
fit2 <- lm(wage ~ ns(age, df = 4), data = dt)
pred2 <- predict(fit2, list(age = age.grid), se = TRUE)
lines(age.grid, pred2$fit, col = "red", lwd = 2)

plot(dt[,.(age, wage)], xlim = agelims, cex = 0.5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(dt$age, dt$wage, df = 16)
fit2 <- smooth.spline(dt$age, dt$wage, cv = TRUE)
fit2$df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = 0.8)

# For local regression
plot(dt[,.(age, wage)], xlim = agelims, cex = 0.5, col = "darkgrey")
title("Local Regression")
fit <- loess(wage ~ age, span = 0.2, data = dt)
fit2 <- loess(wage ~ age, span = 0.5, data = dt)
lines(age.grid, predict(fit, data.frame(age = age.grid)),
      col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)),
      col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = 0.8)
# locfit library can also be used for local regression

## 7.8.3 GAMs
gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = dt)

# Now, using smoothing splines rather 
# than natural splines with the package gam
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = dt)
par(mfrow = c(1,3))
plot(gam.m3, se = TRUE, col = "blue")

# Even though gam1 is not of class gam bur rather
# of class lm, we can still use plot.gam() on it.

plot.gam(gam1, se = TRUE, col = "red")

# Let's use ANOVA to check if which model is better
# GAM without year, GAM with linear function of year
# or GAM that uses spline function of year

gam.m1 <- gam(wage ~ s(age, 5) + education, data = dt)
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data = dt)
anova(gam.m1, gam.m2, gam.m3)
# so, we have clear evidence that a non-linear
# term is required for age, but not for year
plot(gam.m2, se = TRUE, col = "darkgrey")
preds <- predict(gam.m2, dt)

gam.lo <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education, 
              data = dt)
plot.gam(gam.lo, se = TRUE, col = "green")

# We can also use the lo() function to create interactions
# before calling the gam() function

gam.lo.i <- gam(wage ~ lo(year, age, span = 0.5) + education,
              data = dt)
require(akima)
plot(gam.lo.i)

# Fitting a logistic regression GAM
gam.lr <- gam(I(wage > 250) ~ year + s(age, 5) + education,
              family = "binomial", data = dt)
par(mfrow = c(1,3))
plot(gam.lr, se = TRUE, col = "green")

table(dt$education, I(dt$wage>250))
# Since there are no high earners in the <HS category:

gam.lr.s <- gam(I(wage > 250) ~ year + s(age, 5) + education,
                family = "binomial", data = dt, subset = dt$education!="1. < HS Grad")
plot(gam.lr.s, se = TRUE, col = "green")
