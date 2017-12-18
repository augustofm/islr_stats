### QUESTION 8 ----

library(ISLR)
fix(Auto)
attach(Auto)
lm.fit <- lm(mpg ~ horsepower)
summary(lm.fit)
# i) for the fit above, the p-values are almost zero, so the 
# coefficients are significant, showing a relationship
# between the predictor and the response.

# ii)  Rather strong, given that R-squared is about 0.60 
# and that p-values are low.

# iii) It's a negative relationship. Coefficient < 0.

# iv) 
predict(lm(mpg ~ horsepower), data.frame(horsepower=98))
predict(lm(mpg ~ horsepower), data.frame(horsepower=98), interval = "confidence")
predict(lm(mpg ~ horsepower), data.frame(horsepower=98), interval = "prediction")

# b)
par(mfrow=c(1,1))
plot(horsepower, mpg, pch="+")
abline(lm.fit, col="red", lwd=3)
# c~
par(mfrow=c(2,2))
plot(lm.fit)
# by the Residuals vs Fitted plot, a concentration of values
# for larger mpg values shows heteroscedasticity.
#Also, a non-linearity of the relationship is also shown. 



### QUESTION 9 ----
require(data.table)
# a)
pairs(Auto)
names(Auto)
dt <- data.table(Auto)

# b)
cor.matrix <- cor(dt[,1:8])
View(cor.matrix)

lm.fit <- lm(mpg ~ . - name, data = dt)
summary(lm.fit)

# c)
#   i. There is, but much more stronger for only 5 of the predictors
#   ii. displacement, weight, year, origin
#   iii. it suggests that the greater the model year, 
# more miles per gallon the vehicle will require.

# d)
par(mfrow=c(2,2))
plot(lm.fit)
# Yes, it suggests ouliers like those with index 323, 326..
# Yes, observation 14, for instance.

# e)
lm.fit2 <- lm(mpg ~ . - name + 
                I(year^2) +
                year:weight +
                weight:acceleration, data = dt)
summary(lm.fit2)

# f)
lm.fit3 <- lm(mpg ~ . - name - cylinders - origin - acceleration + 
                I(year^2) +
                year:weight +
                log(displacement), data = dt)
summary(lm.fit3)

# In the end, predictors like accelaration, origin and cylinders 
# were much less important than transoforming variables like 
# displacement ad year.

### QUESTION 10 ----