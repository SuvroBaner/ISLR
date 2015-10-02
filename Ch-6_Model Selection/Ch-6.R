############################## Ch-6 #####################################

################### Best Subset Selection ###############

install.packages("ISLR")
install.packages("leaps")

library(ISLR)

fix(Hitters)
names(Hitters)
dim(Hitters)

sum(is.na(Hitters$Salary))  # gives the total count of salary which are NA i.e. 59 i n this case.

Hitters = na.omit(Hitters) # removes all of the rows that have missing values in any variable

dim(Hitters)
sum(is.na(Hitters)) # it is 0 now.

library(leaps) # contains regsubsets()

# regsubsets() performs best subset selection by identifying the best model that contains a given number of predictors
# where best is quatified using RSS.

regfit.full = regsubsets(Salary ~ . , Hitters)
summary(regfit.full)

# An astrick indicates that a given variable is included in the corresponding model
# This output indicates the best two-variable model contains Hits & CRBI
# Hits : Number of Hits in 1986
# CRBI : Number of runs batted in during his career
# By default, regsubsets() only reports results up to the best eight-variable model.
# But nvmax, option can be used in order to return as many variables as are desired.
# Here we fit upto a 19-variable model.

regfit.full = regsubsets(Salary ~ . , data = Hitters, nvmax = 19)
reg.summary = summary(regfit.full)

names(reg.summary)

#  "which"  "rsq"    "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj"  

reg.summary$rsq

# We see that R-squared statistic increases from 32% when only one variable is included in the model, to almost 55%
# when all variables are included. As, expected R-sq statistic increases monotonically as more variables are included.

# Plotting RSS, Adjusted R-sq, Cp, and BIC for all of the models at once will help up decide which model to select.
# Note: type = "l" option tells R to connect the plotted points with lines.

par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq", type = "l")
which.max(reg.summary$adjr2)
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col="red", cex=2, pch=20)

# The points() command works like the plot() command, except that it puts points on a plot that has already been created, instead of creating a new plot.
# which.max() can be used to identify the location of the maximum point of vector.
# We've just plotted a red dot to indicate the model with the largest adjusted R-sq statistics.

# In a similar manner we can plot the Cp and BIC statistics, and indicate the model with the smallest statistic using which.min()

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], col = "red", cex = 2, pch=20)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col = "red", cex = 2, pch = 20)

# The regsubsets() has a built-in plot() to do the aforementioned steps.

plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

# The top row of each plot contains a black square for each variable selected according to the optimal model associated with that statistic.
# For instance, we see that several methods share a BIC close to -150. However, the model with the lowest BIC
# is the six-variable model that contains only
# AtBat : Number of times at bat in 1986
# Hits  : Number of Hits in 1986
# Walks : Number of walks in 1986
# CRBI  : Number of runs batted in during his career
# DivisionW : Players division at the end of 1986, whether it is E or W
# PutOuts : Number of PutOuts in 1986

coef(regfit.full, 6)

# (Intercept)        AtBat         Hits        Walks         CRBI    DivisionW 
#  91.5117981   -1.8685892    7.6043976    3.6976468    0.6430169 -122.9515338 
#     PutOuts 
#   0.2643076 

###################### Forward and Backward Stepwise Selection #########################

regfit.fwd = regsubsets(Salary ~ . , data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd = regsubsets(Salary ~ . , data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)

##################### Choosing among models using the Validation Set Approach and Cross-Validation ###########

# Validation set and Cross-Validation are the direct methods to estimate the test error rate unlike AIC, BIC etc. which are indirect methods
# We use only the training observations to estimate the test error rate.
# If the full data set is used to perform the best subset selection step, the validation set errors
# and cross-validation errors that we obtain will not be accurate estimates of the test-error.

set.seed(1)
train = sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)  # sampling is with replacement
test = (!train)

# Now, we apply regsubsets() to the training set in order to perform the best subset selection.

regfit.best = regsubsets(Salary ~ . , data = Hitters[train,], nvmax = 19)

# We now compute validation set error for the best model of each model size.
# We first make a model of matrix from the test data

test.mat = model.matrix(Salary ~ . , data = Hitters[test,])

# Now we run a loop, and for each size i, we extract the coeffients from the regedit.best for the best model of that size
# multiply them into the appropriate columns of the test model matrix to form the predictions, and compute the test MSE.

val.errors = rep(NA, 19)
for(i in 1:19)
{
	coefi = coef(regfit.best, id=i)
	pred = test.mat[, names(coefi)]%*%coefi
	val.errors[i] = mean((Hitters$Salary[test] - pred)^2)
}

val.errors
which.min(val.errors)
coef(regfit.best, 10)

plot(val.errors, xlab = "Number of Predictors", ylab = "RSS", type = "l")
points(which.min(val.errors), val.errors[which.min(val.errors)], col="red", cex=2, pch=20)

# This was bit tedious as there are no predict() for regsubsets(). So, instead we can write our own function.

predict.regsubsets = function(object, newdata, id,...)
{
	form = as.formula(object$call[[2]])
	mat = model.matrix(form, newdata)
	coefi = coef(object, id=id)
	xvars = names(coefi)
	mat[, xvars]%*%coefi
}

# Finally, we perform best subset selection on the full data set, and select best which.min(val.errors) = 10 variable model.
# It is important that we make use of the full data set in order to obtain more accurate coefficient estimates.

regfit.best = regsubsets(Salary ~ . , data = Hitters, nvmax = 19)
coef(regfit.best, 10)

# We now try to choose among the model of fifferent sizes using cross-validation approach. We must perform best-subset
# selection within each of the k training sets.

# First we create a vector that allocates each observation to one of k = 10 folds, and we'll create a matrix in which we'll store this result.

k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace = TRUE)
cv.errors = matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

# Now we write a loop for that performs cross validation. In the jth fold, the elements of folds that equal j are in the test set
# and the remainder are in the training set. We make our predictions for each model size, compute the test errors
# on the appropriate subset, and store them in the appropraite slot in the matrix cv.errors

for(j in 1:k)
{
	best.fit = regsubsets(Salary ~ . , data = Hitters[folds != j,], nvmax = 19)
	for(i in 1:19)
	{
		pred = predict(best.fit, Hitters[folds == j,], id = i)
		cv.errors[j,i] = mean((Hitters$Salary[folds == j] - pred)^2)
	}
}

# This has given us a 10x19 matrix, of which the (i,j) th element corresponds to the test MSE for the ith cross-validation
# fold for the best j-variable model. We use apply() to average over the columns of this matrix in order to obtain a vector
# for which the jth element is the cross-validation error for the jth variable model.

mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors

par(mfrow = c(1,1))
plot(mean.cv.errors, type = 'b')

# We see that the cross-validation selects an 11-variable model. We now perform best subset selection on the full data set 
# in order to obtain the 11-variable model.

reg.best = regsubsets(Salary ~ . , data = Hitters, nvmax = 19)
coef(reg.best, 11)

############################### Ridge Regression ##########################################

install.packages("glmnet")
library(glmnet)  # contains ridge-regression and lasso functions

x = model.matrix(Salary ~ . , Hitters)[,-1]  # excluding Salary which is the dependent variable
y = Hitters$Salary

# 'x' is a matrix corresponding to 19 predictors and it also automatically trasnforms any qualitative variables in dummy variables.
# as glmnet() can only take numerical and quatitative inputs.


# If alpha = 0 then it is a ridge-regression model, and if alpha = 1 then a lasso model is fit

grid = 10^seq(10, -2, length = 100)
ridge.mod = glmnet(x, y, alpha=0, lambda=grid)

# Here we have chosen a function over a grid of values ranging from lambda = 10^10 to 10^-2, essentially covering
# full range of scenarios from the null model containing only the intercept, to the least squares fit.
# By default, glmnet() standardizes the variables so that they are on the same scale.
# To turn off this default setting, we use an argument called standardize=FLASE

# Associated with each value of lambda is a vector of ridge regression coefficients, stored in a matrix that
# can be accessed by coef() . In this case it is a 20 x 100 matrix with 20 rows (one for each predictor, plus an intercept)
# and 100 columns one for each value of lambda (length = 100)
			
dim(coef(ridge.mod))

# We expect the coefficient estimates to be much smaller, in terms of l2 norm, when a large value of lambda is used
# as compared to when a small value of lambda is sused.

ridge.mod$lambda[50] # lambda is 11498
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))  # 6.36

# In contrast, here are coefficients when lambda = 705, along with the l2 morm.
# Note, the much larger l2 norm of the coefficients associated with this smaller value of lambda.

ridge.mod$lambda[60]  # lambda is 705
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))  # 57.11

# We can use predict() to obtain the ridge regression coefs for a new value of lambda say 50

predict(ridge.mod, s=50, type = "coefficients")[1:20,]

# We now split the samples into a training set and a test set in order to estimate the test error of the ridge-regression and the lasso
# We'll use a different method, which works equally well as the previous method.

set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]

# Next we fit a ridge regression model on the training set, and evaluate its MSE on the test set, using lambda = 4

ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred = predict(ridge.mod, s=4, newx = x[test,])
mean((ridge.pred - y.test)^2)

# The test MSE is 101037

# Instead, if we had simply fit a model with just an intercept, we would have predicted each test observation using
# the mean of the training observations. In that case we could compute the test set MSE like this:

mean((mean(y[train]) - y.test)^2)   # test set MSE is 193253

# We could also get the same result by fitting a ridge regression model with a very large value of lambda
# 1e10 means 10^10

ridge.pred = predict(ridge.mod, s = 1e10, newx = x[test,])
mean((ridge.pred - y.test)^2)  # test set MSE is 193253

# So fitting a ridge regression model with lambda = 4 leads to a much lower test MSE than fitting a model with just an intercept.

# Let's see if there is any benefit to performing ridge regression with lambda = 4 instead of just performing least squares regression.
# Least squares is simply ridge regression with lambda = 0

ridge.pred = predict(ridge.mod, s=0, newx = x[test,], exact = T)
mean((ridge.pred - y.test)^2)  # test MSE is 114783.1

lm(y ~ x, subset = train)
predict(ridge.mod, s=0, exact = T, type = "coefficients")[1:20,]

# Instead of arbitrarily choosing lambda = 4, it would be better to use cross-validation to choose the tunning parameter, lambda
# We can do this using a buit-in cross-validation function, cv.glmnet(), it does a 10 fold cross-validation.

set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam

# Therefore, we see that the value of lambda that results in the smallest cross-validation error is 212.
# The test MSE associated with this value of lambda is-

ridge.pred = predict(ridge.mod, s=bestlam, newx = x[test,])
mean((ridge.pred - y.test)^2)

# The test MSE is 96015.51 which is again an improvement by choosing lambda = 4

# Finally we refit our ridge regression model on the full data set, using the value of lambda chosen by cross-validation.
# and examine the coefficient estimates.

out = glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20,]

# As expected, none of the coefficients are zero i.e. ridge regression does not perform variable selection.

#################################### The Lasso ###############################################

lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

# Here, based on the value of tuning parameter, some of the coefficients will be exactly equal to 0.
# We now perform cross-validation and compute the associated test error.

set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s=bestlam, newx = x[test,])
mean((lasso.pred - y.test)^2)

# The test set MSE is 100743
# This is substantially lower than the test set MSE of the null model and of least squares, and very similar to the test set MSE of ridge regression with lambda chosen by cross-validation.

# However, the lasso has a substantial advantage over ridge regression in that the resulting coefficient estimates are sparse.
# Here we see that 12 of the 19 coefficient estimates are exactly zero. So, the lasso model with lambda chosen by cross-validation
# contains only 7 variables.

out = glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef = predict(out, type = "coefficients", s=bestlam)[1:20,]
lasso.coef

################################### Principal Component Regressions #################################

# pcr() is part of pls library. We now apply PCR to the Hitters data, in order to predict Salary.

install.packages("pls")

library(pls)

fix(Hitters)

set.seed(2)

pcr.fit = pcr(Salary ~ . , data = Hitters, scale = TRUE, validation = "CV")
summary(pcr.fit)

# scale = TRUE : standardizing each predictors
# validation = "CV"  : caused pcr() to compute the 10-fold cross-validation error for each possible value of M, the number of principal componenets used.

# It reports the CV score for each possible number of components starting from M = 0
# It also reports root mean squared error
# To obtain MSE , square the root mean squared error

validationplot(pcr.fit, val.type = "MSEP")
# plot cross-validation scores, MSEP plots the cross-validation MSE.

# From the plot, the smallest cross-validation error occurs when M = 16 components are used.
# It is barely any dimension reduction from M = 19.
# We can also see that at M = 1, the cross validation error is about the same as M = 16.
# This suggests that a model that uses just a small number of componenets might suffice.

# The summary also includes the % of variance explained. Here, when M = 1 (i.e. first component) captures 38.31 % of all variance, or information in the predictors.
# In contrast, M = 6 contains 88.63 % of all the information.

#### We now perform PCR on the training data and evaluate its test set performance.

set.seed(1)
pcr.fit = pcr(Salary ~ . , data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

# Now, we can see that the lowest-cross validation error occurs when M = 7 components are used.
# Let's now compute the test MSE.

pcr.pred = predict(pcr.fit, x[test,], ncomp = 7)
mean((pcr.pred - y.test)^2)

# The test MSE is 96556, which is competitive with the results obtained from ridge-regression and lasso.
# But, the way this PCR is implemented it does not do a variable selection so hard to interpret.

# Finally we fit PCR on the full data set with M = 7, the number of componenets identified using the cross-validation.

pcr.fit = pcr(y ~ x, scale = TRUE, ncomp = 7)
summary(pcr.fit)


#################################### Partial Least Squares ##################################

# plsr() is a part of "pls" library.

set.seed(1)
pls.fit = plsr(Salary ~ . , data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)

validationplot(pls.fit, val.type = "MSEP")

# The lowest cross-validation error occurs when M = 2 partial least squares directions are used.
# Let's evaluate the corresponding test set MSE.

pls.pred  = predict(pls.fit, x[test,], ncomp = 2)
mean((pls.pred - y.test)^2)

# The test MSE is 101417 which is comparable to PCR but slightly higher.

# Finally we perform PLS using the full data-set, using M = 2, the number of components identified by cross-validation.

pls.fit = plsr(Salary ~ . , data = Hitters, scale = TRUE, ncomp = 2)
summary(pls.fit)

# Notice, that the percentage of variance in Salary that the two-component PLS fit explains, 
# 46.40 % is almost as much as that explained using the final seven-component model PCR fit, 46.69 %

# This is because PCR only attempts to maximize the amount of variance explained in the predictors.
# While PLS searches for directions that explain variance in both the predictors and the response.

