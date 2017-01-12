# Chapter 3 Lab: Linear Regression

# 3.6.1 Libraries
library(MASS)
library(ISLR)

# 3.6.2 Simple Linear Regression
fix(Boston)
names(Boston)
# [1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"     "dis"    
# [9] "rad"     "tax"     "ptratio" "black"   "lstat"   "medv" 
lm.fit = lm(medv~lstat, data=Boston)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="confidence")
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="prediction")
# confidence interval and prediction interval are centered around the same point
# but the latter is substantially wider
pdf("medvVSlstat.pdf")
plot(lstat, medv)
abline(lm.fit) # least squares regression line
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3, col="red")
dev.off()
plot(lstat, medv, col="red")
plot(lstat, medv, pch=20)
plot(lstat, medv, pch="+")
plot(1:20, 1:20, pch=1:20) # 20 different symbols
pdf("SimpleRegression.pdf")
par(mfrow=c(2,2))
plot(lm.fit)
dev.off()
par(mfrow=c(1,1))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit)) # which.max returns the index of the largest element

# 3.6.3 Multiple Linear Regression
lm.fit = lm(medv~lstat+age, data=Boston)
summary(lm.fit)
lm.fit = lm(medv~., data=Boston)
summary(lm.fit)
library(car)
vif(lm.fit)
lm.fit1 = lm(medv~.-age, data=Boston)
summary(lm.fit1)
lm.fit1 = update(lm.fit, ~.-age)

# 3.6.4 Interaction Terms
summary(lm(medv~lstat*age, data=Boston))
summary(lm(medv~lstat+age+lstat:age, data=Boston))

# 3.6.5 Non-linear Transformations of the Predictors
lm.fit2 = lm(medv~lstat+I(lstat^2), data=Boston)
summary(lm.fit2)
lm.fit = lm(medv~lstat, data=Boston)
anova(lm.fit, lm.fit2)
pdf("NonLinearTransform.pdf")
par(mfrow=c(2,2))
plot(lm.fit2)
dev.off()
lm.fit5 = lm(medv~poly(lstat,5), data=Boston)
summary(lm.fit5)
summary(lm(medv~log(rm), data=Boston))

# 3.6.6 Qualitative Predictors
fix(Carseats)
names(Carseats)
lm.fit = lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc)

# 3.6.7 Writing Functions
LoadLibraries = function() {
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}
LoadLibraries
LoadLibraries()
