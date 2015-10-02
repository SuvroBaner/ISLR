################################### Resampling techniques #########################################

######### Validation Set Approach #########	

library(ISLR)
library(boot)

set.seed(1) # to have the reproducibility of the result
names(Auto)
dim(Auto)
attach(Auto)
fix(Auto)

train = sample(dim(Auto)[1], dim(Auto)[1]/2)
# sample() to split the data set into halves by selecting the random set of observatrions.
# The above code is similar to train = sample(392, 196)

lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
# to fit a linear regression using only the obs corresponding to the training set.

# We use predict() to estimate the response of all 392 observations.
# We use mean() to calculate the MSE of the 196 obs in the validation set (test set)

e = mpg - predict(lm.fit, Auto) # the error
mean((e)[-train]^2) # MSE for the validation set.

# It can also be written as-

mean((mpg - predict(lm.fit, Auto))[-train]^2)

# The estimated test MSE for the linear regression fit is 25

lm.fit2 = lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

# The estimated test MSE for the linear regression with quadratic function of horsepower is 21

lm.fit3 = lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

# The estimated test MSE for the linear regression with cubic function of horsepower is 21

## Choose different training set instead, then we will obtain somewhat different errors on the validation set.

set.seed(2)
train = sample(dim(Auto)[1], dim(Auto)[1]/2)
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2) # Estimated test MSE is 23.29

lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2) # Estimated test MSE is 18.9

lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2) # Estimated test MSE is 19.25

## Using this split of the observations into a training set and a validation set, we find that the validation set error rates 
## for the model with linear, quadratic and cubic terms are 23.30, 18.90 and 19.25 respectively
## It is also consistent with the previous samples in the Validation set approach.
## So, we can say that a model that predicts mpg using a quadratic function of horsepower performs better.

predict(lm.fit2, data.frame(horsepower = (c(46,93.5,104.5,230))), interval = "confidence")
predict(lm.fit2, data.frame(horsepower = (c(46,93.5,104.5,230))), interval = "prediction")

######################### Leave-One-Out Cross-Validation (LOOCV) #########################

glm.fit = glm(mpg ~ horsepower, data = Auto) # it alos does a linear regression as family = binomial is not given
coef(glm.fit)

lm.fit = lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)

# Here we'll use the glm() as this helps to compute cv.glm() which is LOOCV. It's part of boot library.

library(boot)

glm.fit = glm(mpg ~ horsepower, data = Auto)
cv.err = cv.glm(Auto, glm.fit)
names(cv.err)
cv.err$delta

# The two numbers in the delta vector contain the cross validation results. In this case the numbers are identical.
# The number represents the estimate for the test error and it is 24.23 which is estimated test MSE

# We can automate it using a for-loop for higher order polynomial functions.

cv.error = rep(0,5)
for (i in 1:5)
{
	glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
	cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

# The value of estimated test MSE for poly 1,2,3,4,5 respectively is 
# 24.23151 19.24821 19.33498 19.42443 19.03321
# We can clearly see that there is a drop of estimated test MSE from liear to quadratic fits but no improvement henceforth.
# So, we can also say that the quadratic function fits the model better.

############################# K - fold cross validation ##############################

set.seed(17)
cv.error.10 = rep(0,10)
for (i in 1:10)
{
	glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
	cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10

# The value of estimated test MSE for poly 1,2,3,4,5 respectively is
# 24.24309 19.24813 19.30470 19.32010 19.08039 18.99459 19.46065 18.89762 19.29570 19.63523
# We can clearly see that there is a drop of estimated test MSE from liear to quadratic fits but no improvement henceforth.
# So, we can also say that the quadratic function fits the model better.
# Note: The computation time is much shorter than LOOCV
# Note: The two numbers associated with delta are k-fold CV estimate (the 1st one) and a bias-corrected (the 2nd one)

######################################### Bootstrap ###############################################

names(Portfolio)
dim(Portfolio)
fix(Portfolio)
attach(Portfolio)

alpha.fn = function(data, index)
{
	X = data$X[index]
	Y = data$Y[index]
	return((var(Y) - cov(X,Y)) / (var(X) + var(Y) - 2*cov(X,Y)))
}
alpha.fn(Portfolio, 1:100)

# Estimating alpha using all 100 observations. The estimated value of alpha is .576

# We construct a new bootstrap data set and estimating value of alpha from the new data set.

set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T))

# The sample() randomly select 100 observations from range 1 to 100 with replacement
# The estimated value of alpha from the bootstrap data set is 0.596
# We can keep doing it several times and recording all of the corresponding estimates of alpha and computing the resulting standard deviation.
# However the boot() automates this approach

boot(Portfolio, alpha.fn, R = 1000)  # i.e. R=1000 bootstrap estimates of alpha

# The estimated value of alpha is 0.576, and that the bootstrap estimate for Standard error is 0.0886

#################### Estimating the Accuracy of a Linear Regression Model using Bootstrap ################

boot.fn = function(data, index)
{
	lm.fit = lm(mpg ~ horsepower, data = data, subset = index)
	return(coef(lm.fit))
}

boot.fn(Auto, 1:392)

# The coefficient of intercept is 39.9358610 and the horsepower is -0.1578447

set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))

# The coefficient of intercept is 38.7387134 and the horsepower is  -0.1481952

boot.fn(Auto, sample(392, 392, replace = T))

# The coefficient of intercept is 40.0383086 and the horsepower is  -0.1596104

boot(Auto, boot.fn, 1000)

# Teh coef of intercept is 39.9358610 with standard error of 0.860007896
# The coef of horsepower is -0.1578447 with standard error of 0.007404467

## Now lets verify this with our usual regresison model

summary(lm(mpg ~ horsepower, data = Auto))$coef

# Teh coef of intercept is 39.9358610 with standard error of 0.717498656
# The coef of horsepower is -0.1578447 with standard error of 0.006445501

# We can see that the standard error are different. Which is more correct.
# Bootstrap gives more accurate estimate of standrad error as it is independent of many assumptions which lm() has.

# Lets do it for the quadratic function model-

boot.fn2 = function(data, index)
{
	lm.fit2 = lm(mpg ~ horsepower + I(horsepower^2), data = data, subset = index)
	return(coef(lm.fit2))
}
set.seed(1)
boot(Auto, boot.fn2, 1000)


#Bootstrap Statistics :
#        original        bias     std. error
#t1* 56.900099702  6.098115e-03 2.0944855842
#t2* -0.466189630 -1.777108e-04 0.0334123802
#t3*  0.001230536  1.324315e-06 0.0001208339






