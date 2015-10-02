install.packages("ISLR")

library(MASS)
library(ISLR)

################################### Question - 8 ################################################
### This question involves the Auto Data set.

Auto = read.table("Auto.data", header = T, na.strings = "?")
fix(Auto)
names(Auto)

lm.fit = lm(mpg ~ horsepower, data = Auto)  # mpg is the fuel efficiency of the car
summary(lm.fit)

predict(lm.fit, data.frame(horsepower = (c(98))), interval = "confidence")
predict(lm.fit, data.frame(horsepower = (c(98))), interval = "prediction")

attach(Auto)
plot(horsepower,mpg)
abline(lm.fit, lwd = 3, col = "red")

par(mfrow = c(2,2))
plot(lm.fit)

## a.1. Yes there is a relationship between the predictor (horsepower) and the response (mpg)
## a.2. The relationship is quite strong as the p-value is very very low, so reject the null hypothesis.
## a.3. The relationship is negative as the coefficient is negative (-0.157845)
## a.4. The predicted mpg associated with a horsepower of 98 is 24.47.
##	  The associated 95 % confidence iterval is [23.97, 24.96]
##	  The associated 95 % predicton interval is [14.81, 34.12]
## b.   The least square line has a negative relation. The plot shows a non-linearity in the relation.
## c.   In the residual v/s fitted plot, we find a discernible pattern which tells us about the non-linearlity in the model.
##	  It is no surprise as we find a non-linear pattern between the predictor and the response variables.

################################### Question-9 ############################################

pairs(Auto) # a. scatterplot matrix which includes all the variables of the Auto data set.

cor(Auto[,-9])  # b. matrix of correlations

lm.fit = lm(mpg ~ . - name, data = Auto)  # multiple linear regression with all variables except name as the predictors
summary(lm.fit)

lm.fit2 = update(lm.fit, ~ . - cylinders - horsepower - acceleration) # dropping the predictors which are not statistically significant
summary(lm.fit2)

par(mfrow = c(2,2))
plot(lm.fit2)

summary(lm(mpg~ displacement*weight*year*origin 
- displacement:weight:origin:year - displacement:weight:year - displacement:weight:origin - displacement:year:origin - weight:year:origin
, data = Auto))

lm.fit3 = lm(mpg ~ displacement + log(displacement))
summary(lm.fit3)

par(mfrow = c(2,2))
plot(lm.fit3)

lm.fit2 = lm(mpg ~ displacement, data = Auto)
anova(lm.fit2, lm.fit3)  # it test for the better model

## c.1. The F-statistics is quite high with value of 252.4 and the corresponding p-value is very low, thus the F-value is statistically significant
##	  So, there is at least a relationship between the predictors and the response.
## c.2. Displacement, Weight, Year and Origin are the predictors to have a statistically significant relationship to the response.
## c.3. The year variable and the mpg response is positively corelated, which means newer the car model, better is the mpg (fuel efficiency of the car)
## d.1. In the residual v/s fitted plot, we find a discernible pattern which tells us about the non-linearlity in the model.
## e.1. displacement:weight interactions appear to be statistically significant
## f.1. When I do a log of displacement and regress and plot the rediduals vs fitted we find that there is no discernible pattern in the plot.
##      So, it resembles the log pattern than a linear pattern which we tried earlier.

####################################### Q.10 ##################################
### This question involves the data set Carseats

fix(Carseats)

names(Carseats)

lm.fit = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(lm.fit)

summary(lm(Sales ~ . ,data = Carseats))

lm.fit2 = lm(Sales ~ . - Population - Education - Urban - US, data = Carseats) # smaller model
summary(lm.fit2)

confint(lm.fit2) # 95 % confidence interval for all the coefficients

par(mfrow = c(2,2))
plot(lm.fit2)

## b.1 The price has a negative relation with the Sales and it is also statistically significant with a low p-value.
##     The Urban = 1 if the value is "Yes" else 0 if the value is "No". This is a qualitative variable.
##	 The value is negative,which means that the Sales is low in the Urban area. But it is not statistically significant.
##	 The US = 1 if the value is "Yes" else 0 if the value is "No". This is also a qualitative variable.
##	 The value is positive, which means that the Sales is high if the location is US.
## c.1 Y = B0 + B1X1i(X1 = 1) + B2X2i(X2 = 1) + E
## d.1 We can reject the null hypothesis for the following predictors- CompPrice, Income, Advertising, Price, ShelveLocGood, ShelveLocMedium and Age   	
## f.1 The 2nd regression equation has a greater R-squared:  0.872, so the model fits the data well.

######################################### Q.11 ###################################

set.seed(1)
x = rnorm(100) # predictor
y = 2*x+rnorm(100) # response

lm.fit = lm(y ~ x+0) # linear regression y onto x without an intercept
summary(lm.fit)

lm.fit = lm(x ~ y+0) # linear regression x onto y without an intercept
summary(lm.fit)

## BetaHat is  1.9939 , Stnd. Error is 0.1065 , t-stat is 18.73 and p-value is <2e-16.
## Based on the above result, we reject the Null Hypothesis.
## BetaHat is 0.39111, Stnd. Error is 0.02089 , t-stat is 18.73 and p-value is <2e-16.
## Based on the above result, we reject the Null Hypothesis.
## They have the same t-value and p-value. Both represent the same line.

######################################## Q.13 #######################################
set.seed(1)
x = rnorm(100) # containing 100 observations drawn from a N(0, 1) distribution
eps = rnorm(100, 0, sqrt(0.25)) # zero mean and variance 0.25

y = -1 + 0.5*x + eps

plot(x,y)

lm.fit = lm(y ~ x)
summary(lm.fit)

plot(x,y)
abline(lm.fit, lwd=3, col="red") # regression fit
abline(-1, 0.5, lwd=3, col="green") # true line. They almost overlap.
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3) # data visualization

lm.fit2 = lm(y ~ x + I(x^2))
summary(lm.fit2)

#### Now carryring out the previous steps by modifying the data generation process in such a way that there
#### is less noise in the data. The model remains the same but we reduce the variance of the normal distribution
#### used to generate the error term.

set.seed(1)
x = rnorm(100)
eps1 = rnorm(100, 0, .125)
y = -1 + 0.5*x + eps1
plot(x,y)
lm.fit = lm(y ~ x)
summary(lm.fit)
plot(x,y)
abline(lm.fit, lwd=3, col="red")
abline(-1, 0.5, lwd=3, col="green")
legend(-1, legend = c("model fit", "pop.regression"), col=2:3, lwd=3)

## The length of vector y = 100.
## B0 is -1 and B1 = 0.5
## The response and predictor have a linear relationship, with a positive slope and with some variance.
## The linear regression fits a model close to the true value of the coefficients as was construted.
## B0 = -1.01 and B1 = 0.49947. They both have a very low p-value thus rejecting the null hypothesis.
## The F-value is also quite high i.e. 85.99 and the corresponding p-value is very low.
## So these coefficients are statistically significant.
## In the polynomial regression model, there is an evidence that the model fit has increased over the training data
## It is so, because there is a slight increase in Rsquare and decrease in the RSE.
## However the high p-value of the t-statistics suggests that there isn't any relationship between y and x square.
## By reducing the error, the RSE reduced and R2 increased substantially. This is quite expected as the overall error reduces.


######################################## Q.14 ##########################################################

### This problem focuses on the collinearity problem

set.seed(1)
x1 = runif(100) # runif generates random deviates, runif(n, min = 0, max = 1)
x2 = 0.5*x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)

cor(x1,x2)
plot(x1,x2)

lm.fit = lm(y ~ x1 + x2)
summary(lm.fit)

lm.fit1 = lm(y ~ x1)
summary(lm.fit1)

lm.fit2 = lm(y ~ x2)
summary(lm.fit2)

## Now we obtain another information which is somehow mismeasured.

x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)

cor(x1,x2)
plot(x1,x2)

lm.fit = lm(y ~ x1 + x2)
summary(lm.fit)

lm.fit1 = lm(y ~ x1)
summary(lm.fit1)

lm.fit2 = lm(y ~ x2)
summary(lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit1)

par(mfrow=c(2,2))
plot(lm.fit2)

## Here B0 = 2, B1 = 2 and B2 = 0.3
## After the model fit, B0 = 2.13, B1 = 1.44 and B2 = 1.01. The F statistics is 12.8 and the corresponding p-value is low.
## So, atleast one predictor is associated with the response. We can reject the null hypothesis for B1 but for the rest we
## accept the null hypothesis.
## Yes, we can reject the null hypothesis for the regression coefficient given the p-value for its t-statistic is near zero.
## Yes, we can reject the null hypothesis for the regression coefficient given the p-value for its t-statistic is near zero.
## No, because x1 and x2 have collinearity, it is hard to distinguish their effects when regressed upon together. 
## When they are regressed upon separately, the linear relationship between y and each predictor is indicated more clearly.


################################################## Q. 15 ##################################################

## We will now try to predict per capita crime rate using the other variables in this data set Boston

fix(Boston)
names(Boston)
summary(Boston)

Boston$chas = factor(Boston$chas, labels = c("N","Y"))
summary(Boston)

attach(Boston)

lm.zn = lm(crim ~ zn)
summary(lm.zn) # Yes

par(mfrow=c(2,2))
plot(lm.zn)

lm.indus = lm(crim ~ indus)
summary(lm.indus) # Yes

par(mfrow=c(2,2))
plot(lm.indus)

lm.chas = lm(crim ~ chas)
summary(lm.chas) # No

par(mfrow=c(2,2))
plot(lm.chas)

lm.nox = lm(crim ~ nox)
summary(lm.nox) # Yes

lm.rm = lm(crim ~ rm)
summary(lm.rm) # Yes

lm.age = lm(crim ~ age)
summary(lm.age) # Yes

lm.dis = lm(crim ~ dis)
summary(lm.dis) # Yes

lm.rad = lm(crim ~ rad)
summary(lm.rad) # Yes

lm.tax = lm(crim ~ tax)
summary(lm.tax) # Yes

lm.ptratio = lm(crim ~ ptratio)
summary(lm.ptratio) # Yes

lm.black = lm(crim ~ black)
summary(lm.black) # Yes

lm.lstat = lm(crim ~ lstat)
summary(lm.lstat) # Yes

lm.medv = lm(crim ~ medv)
summary(lm.medv) # Yes

lm.all = lm(crim ~ ., data = Boston)
summary(lm.all)

## We can reject the Null Hypothesis for the following: zn, dis, rad, black and medv

#### Create a plot displaying the univariate regression coefficients
#### from (a) on the x-axis, and the multiple regression coefficients
#### from (b) on the y-axis.

x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])

y = coefficients(lm.all)[2:14]

plot(x, y)

## Coefficient for nox is approximately -10 in univariate model and 31 in multiple regression model.

## Is there evidence of non-linear association between any of the
## predictors and the response? To answer this question, for each
## predictor X, fit a model of the form
## Y = ß0 + ß1X + ß2X2 + ß3X3 + e

lm.zn = lm(crim ~ poly(zn,3))
summary(lm.zn) # 1,2

lm.indus = lm(crim~poly(indus,3))
summary(lm.indus) # 1, 2, 3

## We don't do for the Qualitative predictor, e.g. chas 

lm.nox = lm(crim~poly(nox,3))
summary(lm.nox) # 1, 2, 3

lm.rm = lm(crim~poly(rm,3))
summary(lm.rm) # 1, 2

lm.age = lm(crim~poly(age,3))
summary(lm.age) # 1, 2, 3

lm.dis = lm(crim~poly(dis,3))
summary(lm.dis) # 1, 2, 3

lm.rad = lm(crim~poly(rad,3))
summary(lm.rad) # 1, 2

lm.tax = lm(crim~poly(tax,3))
summary(lm.tax) # 1, 2

lm.ptratio = lm(crim~poly(ptratio,3))
summary(lm.ptratio) # 1, 2, 3

lm.black = lm(crim~poly(black,3))
summary(lm.black) # 1

plot(black,crim)
par(mfrow=c(2,2))
plot(lm.chas)

lm.lstat = lm(crim~poly(lstat,3))
summary(lm.lstat) # 1, 2

lm.medv = lm(crim~poly(medv,3))
summary(lm.medv) # 1, 2, 3



