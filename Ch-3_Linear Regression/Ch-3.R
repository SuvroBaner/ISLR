################## This is the code for Ch-2 #######################

########### Load Libraries ###########

install.packages("ISLR")

library(MASS) # Very large collection of data sets and functions.
library(ISLR) # Data sets associated with this book

fix(Boston) # Boston data set records median house value of 506 neighborhoods around Boston.
names(Boston) # returns the variable names in the data set.

###### We will run a simple linear regression model, lm(y~x,data)
### The response variable is medv (median house value)
### The predictor is lstat (percent of household with low socioeconomic status)

lm.fit = lm(medv ~ lstat, data = Boston)  # running the simple linear regression
lm.fit
summary(lm.fit)  # summary of the regression model

names(lm.fit)  # what all information is there in the model, just returns the names

lm.fit$coefficients # extract all the coefficients

coef(lm.fit) # extract all the coefficients

confint(lm.fit)  # Obtain a confidence interval for the coefficient estimates

predict(lm.fit, data.frame(lstat = (c(5,10,15))), interval = "confidence")  # confidence interval

### Here the 95 % confidence interval associated with a lstat value of 10 is (24.47, 25.63)

predict(lm.fit, data.frame(lstat = (c(5,10,15))), interval = "prediction") # prediction interval

### 95 % prediction interval is (12.83, 37.28)
### Both prediction and confidence interval are centered around the same value i.e. 25.05
### However the prediction interval is quite wide enough compared to the confidence interval
### The confidence interval tells you about the likely location of the true population parameter.
### Prediction intervals must account for both the uncertainty in knowing the value of the population mean, plus data scatter
### So a prediction interval is always wider than a confidence interval. 

attach(Boston) # globally referring it

plot(lstat,medv) # plotting lstat v/s medv
abline(lm.fit) # draw a line, abline(a,b) where a = intercept, b = slope

abline(lm.fit, lwd = 3) # width of the regression line to be increased by the factor of 3
abline(lm.fit, lwd = 3, col = "red")

plot(lstat,medv, col = "red")
plot(lstat,medv, pch = 20)
plot(lstat,medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)

par(mfrow = c(2,2)) # ivides the plotting region into a 2 × 2 grid of panels, help for the next plot
plot(lm.fit) # plots all the four plots pertaining to regression

### Alternatively, we can compute the residuals from a linear regression fit using the residuals() function.

plot(predict(lm.fit), residuals(lm.fit)) # or use below
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit)) # leverage statistics
which.max(hatvalues(lm.fit)) # index of the largest element of a vector, here it says the observation with the highest leverage statistics

###### Multiple linear regression model using least squares, lm(y~x1+x2+x3)

lm.fit = lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

### Boston data set has 13 prdictor variables, we want to use them in our linear regression model

lm.fit = lm(medv ~ . , data = Boston)
summary(lm.fit)

summary(lm.fit)$r.sq  # It gives us the summary of R-squared
summary(lm.fit)$sigma # It gives us the summary of Residual Standard Error (RSE)

### The vif(), part of the car package, can be used to compute variance inflation factors

install.packages("car")
library(car)

vif(lm.fit)

### In the above regression model we see that age has a very high p-value i.e. statistically not significant
### So we may wish to run a regression excluding this predictor.
### The following syntax results in a regression using all predictors except age.

lm.fit1 = lm(medv ~ . - age, data = Boston)
summary(lm.fit1)

### Alternatively an update function can be used

lm.fit2 = update(lm.fit, ~ . - age)
summary(lm.fit2)

###### Interaction term can be used in the regression model by lstat:black tells R to include an 
###### interaction term between lstat and black.
###### The syntax lstat*age means in the regrsssion model use
###### lstat, age and the interaction term lstat:age as predictors.
###### It is a short hand for lstat + age + lstat:age

summary(lm(medv ~ lstat*age, data = Boston))

###### Non-Linear transformations of the predictor

### The lm() can also accomodate the non-linear transformation of the predictors
### Given a predictor X, we can make a predictor X-square using I(X^2)

lm.fit3 = lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(lm.fit3)

### The near zero p-value associated with the quardatic term suggests that it leads to an improved model.
### We use anova() to further qutantify the extent to which the quadratic fit is superior to the linear fit.

lm.fit = lm(medv ~ lstat, data = Boston)
anova(lm.fit, lm.fit3)

### Here anova() performs a hypothesis test comparing the two models.
### The null hypothesis is that the two models fit the data equally well.
### The alternative hypothesis is that the full model is superior.
### Here the F-statistic is 135 and the associated p-value is near-zero.
### This provides very clear evidence that the model containing
### the predictors lstat and lstat2 is far superior to the model that only
### contains the predictor lstat.
### This is not surprising, since earlier we saw
### evidence for non-linearity in the relationship between medv and lstat.

par(mfrow = c(2,2))
plot(lm.fit3)

### We see that when the lstat2 term is included in the model, there is
### little discernible pattern in the residuals.

lm.fit5 = lm(medv ~ poly(lstat,5), data = Boston)  # lstat raised to the 5th power.
summary(lm.fit5)

### Log transformation

names(Boston)
summary(lm(medv ~ log(rm), data = Boston))
lm.fitL = lm(medv ~ log(rm), data = Boston)
par(mfrow = c(2,2))
plot(lm.fitL)


###### Qualitative predictors

### We now examine the Carseats data, which is part of ISLR library. We will attempt to predict sales
### (Child car seats sales) in 400 locations based on a number of predictors.

fix(Carseats)
names(Carseats)
summary(Carseats)

# The Carseats data includes qualitative predictors such as Shelveloc, an indicator
# of the quality of the shelving location—that is, the space within
# a store in which the car seat is displayed—at each location. The predictor
# Shelveloc takes on three possible values, Bad, Medium, and Good.

# Given a qualitative variable such as Shelveloc, R generates dummy variables automatically.

lm.fit = lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

attach(Carseats)
contrasts(ShelveLoc) # returns the coding that R uses for the dummy variables

###### Writing functions which will read in MASS and ISLR libraries

LoadLibraries = function() {
	library(ISLR)
	library(MASS)
	print("The libraries have been loaded")
}

LoadLibraries
LoadLibraries() # calling the function