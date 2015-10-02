######################## Lab Ch-9 ###################

install.packages("e1071")  # Support Vector Classifier and Support Vector Machines
install.packages("LiblineaR") # useful for very large linear problems.

library("e1071")
library("LiblineaR")

# Let's use the svm() to fit the support vector classifier for a given value of the cost parameter. 
# Generate the observations, which belong to two classes.

set.seed(1)
x = matrix(rnorm(20*2), ncol = 2)  # standard normal distribution
y = c(rep(-1, 10), rep(1, 10))   # two classes
x[y==1, ]=x[y==1, ] +1  # the last 10 obs, as they are TRUE from y==1 boolean expresssion, add 1 to each of these numbers.

plot(x, col = (3 - y)) 
# col = 2 : signifies "red"
# col = 4 : signifies "blue"

# This is a fancy way to plotting, 
# for y = 1, col = 3 - 1 = 2 i.e. red
# for y = -1, col = 3 - (-1) = 4 i.e. blue

# So, we can clearly see that the classes are not linearly separable.

# Now we fit the Support Vector Classifier. In order for the svm() to perform classification (as opposed to SVM-based regression),
# we must encode the response as a factor variable.

dat = data.frame(x=x, y=as.factor(y))

svmfit = svm(y ~ . , data = dat, kernel = "linear", cost = 10, scale = FALSE)

# Here, for Support Vector Classifier the argument kernel = "linear" is used.
# The cost argument allows us to specify the cost of a violation to the margin.
# The interpretation is just the opposite to the budget which we had studied in the book.

# When the cost argument is small, then the margins will be wide and many support vectors will be on the margin or will violate the margin.
# When the cost argument is large, then the margins will be narrow and there will be few vectors on the margin or violating the margin.
# The argument scale = FALSE tells svm() not to scale each feature to have a mean zero or standard deviation as one.

plot(svmfit, dat)  # these two parameters are the output of the svm() and the plot is reversed in terms of the axis.

# Here we are trying to classify the pairs of (x1, x2) based on the class they belong to.

# The region of feature space that will be assigned to the -1 class is shown in the light blue, and the region that will be 
# assigned to the +1 clkass is shown in purple.
# The support vectors are plotted as crosses and the remaining observations are plotted as circles.

# Here, the support vectors are those which are:
# a) on the margin
# b) crosses the margin on the wrong side but doesn't cross the hyperplane
# c) crosses the hyperplane on the wrong side.


# Here just one observation is misclassified. We can see it in the plot. Look, there is a cross of different color (purple)
# in the light blue region. This means that the observation is misclassified.
# If you check the data you would find that the observation is the 14th observation (x1, x2, y): (-1.2146999  0.94619496  1)

# the way in which the plotting function is implemented in this library the decision boundary looks somewhat jagged in the plot.
# although the kernel = "linear"

svmfit  # Number of Support Vectors:  7

svmfit$index  # the positions of the support vectors.

# 1  2  5  7 14 16 17

summary(svmfit)

# It also tells us that there are 7 support vectors, four in one class and three in the other i.e. (4, 3) --- (-1, 1)

# Let's use the smaller value of cost, cost = 0.1

# Smaller value of cost means the margin is widen, more support vector classifier and in turn the variance decreases with some increase in bias.

svmfit = svm(y ~ . , data = dat, kernel = "linear", cost = 0.1, scale = FALSE)
plot(svmfit, dat)
svmfit$index  # Now that at smaller value of the cost parameter is being used, we obtain a larger number of support vectors because the margin is now wider.

summary(svmfit)  # 16 support vector classifiers. (8, 8)


# Unfortunately the svm() does not explicitly output the coefficients of the linear decision boundary obtained 
# when the support vector classifier is fit, nor does it output the width of the margin.


# e1071 library includes a built-in function, tune(), to perform cross-validation. By default it does a 10-fold cross validation.

# Now, we want to compare different results of Support Vector Classifier using a range of values of the cost parameter.
# We'll use the cross-validation tune() which is also a part of e1071 library.
# We pass the modeling type as "svm"


set.seed(1)
tune.out = tune(svm, y ~ . , data = dat, kernel = "linear", range = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

#Parameter tuning of ‘svm’:

#- sampling method: 10-fold cross validation 

#- best parameters:
# cost
#  0.1

#- best performance: 0.1 

#- Detailed performance results:
#   cost error dispersion
#1 1e-03  0.70  0.4216370
#2 1e-02  0.70  0.4216370
#3 1e-01  0.10  0.2108185
#4 1e+00  0.15  0.2415229
#5 5e+00  0.15  0.2415229
#6 1e+01  0.15  0.2415229
#7 1e+02  0.15  0.2415229

# We see that cost = 0.1 results in the lowest cross-validation error rate. The tune() stores the best model obtained,
# which can be accessed as follows.

names(tune.out)

bestmod = tune.out$best.model
summary(bestmod)

# The output is as follows-

#Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  linear 
#       cost:  0.1 
#      gamma:  0.5 
#
#Number of Support Vectors:  16
#
# ( 8 8 )
#
#
#
#Number of Classes:  2 
#
#Levels: 
# -1 1


# Now lets predict by first generating the test data.

xtest = matrix(rnorm(20*2), ncol=2)
ytest = sample(c(-1, 1), 20, rep = TRUE)   # note I am sampling it to keep it random.
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat = data.frame(x = xtest, y=as.factor(ytest))

# Now we predict the class labels of the test observations. Here we use the best model obtained through cross-validation in order to make predictions.

ypred = predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)
mean(ypred == testdat$y)

#       truth
#predict -1  1
#     -1 11  1
#     1   0  8

# Thus, with this value of cost, 19 of the test observations are correctly classified (that's remarkable)
# So, the correct prediction rate is 95 %.

# What if we had instead used cost = 0.01 ?

svmfit = svm(y~., data = dat, kernel = "linear", cost = .01, scale = FALSE)
ypred = predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y)
mean(ypred == testdat$y)

# In this case one additional observaion is misclassified, i.e the correct classification error rate it 90%

# Now condider a situation in which the two classes are linearly separable. Then we can find a separable hyperplane using svm().
# First we further separate the two classes in our simulated data so that they are linearly separable.

x[y==1,]=x[y==1,] + 0.5
plot(x, col = (y+5)/2, pch = 19)

# Now the observations are just barely linearly separable. We fit the support vector classifier and plot the resulting hyperplane
# using a very large value of cost, so that no observations are misclassified (fitting the data hard, in other words low bias)

dat = data.frame(x=x, y=as.factor(y))
svmfit = svm(y ~ . , data = dat, kernel = "linear", cost = 1e5)
summary(svmfit)

# There are only 3 support vector classifiers..

plot(svmfit, dat)

# No traininig errors were made and only three support vectors were used. The margin is very narrow and looks like the model will perform very poorly on the test data.
# We now use a very small value of the cost.

svmfit = svm(y~., data = dat, kernel = "linear", cost = 1)
summary(svmfit)

# There are now 7 support vectors.

plot(svmfit, dat)

# Using cost = 1, we misclassify 1 training observation, but we also obtain a much wider margin and make use of seven support vectors.
# It seems like that this model will perform better on test data that the model with cost = 1e5

############## Support Vector Machine ###################

# In order to fit an SVM using a non-linear kernel, we once again use svm().
# To fit an SVM with a polynomial kernel we use kernel = "polynomial"
# To fit an SVM with a radial kernel we use kernel = "radial"


# We first generate some data with a non-linear class boundary.

set.seed(1)
x = matrix(rnorm(200*2), ncol = 2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150), rep(2,50))
dat = data.frame(x=x, y=as.factor(y))

# The dataframe looks like-

#	x.1		x.2		y
#149 -3.28630053 -1.1967167839 1
#150 -3.64060553 -0.1125255367 1
#151  0.45018710  1.4738811811 2
#152 -0.01855983  0.6772684923 2

plot(x, col = y) # the class boundary is indeed non-linear
# 1 : black
# 2 : red

# The data is randomly split into training and testing groups. 
# We then fit the training data using the svm() with a radial kernel and gamma = 1
# gamma : parameter needed for all kernels except linear (default: 1/(data dimension))

train = sample(200, 100)
svmfit = svm(y ~ . ,data = dat[train,], kernel = "radial", gamma = 1, cost = 1)

summary(svmfit)

# There are 37 support vector classifiers. (17 20) -- (1 2)

plot(svmfit, dat[train,])

# The plot shows that the resulting SVM has a decidedly non-linear boundary.
# We can see from the plot that there are fair number of training errors in this SVM fit.
# If we increase the value of cost, we can reduce the number of training errors.
# However, this comes at the price of a more irregular decision boundary that seems to be at risk of overfitting the data.

svmfit = svm(y ~ . , data = dat[train,], kernel = "radial", gamma = 1, cost = 1e5)

summary(svmfit)  
# there are now 26 support vectors (fewer) as with increase in cost the margin has shrunk.

plot(svmfit, dat[train,])

# We can perform cross-validation using tune() to select the best choice of gamma and cost for an SVM with a radial kernel.

set.seed(1)
tune.out = tune(svm, y ~ . , data = dat[train,], kernel = "radial",
		    ranges = list(cost = c(0.1, 1, 10, 1000),
		    gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)


# Therefore, the best choice of parameters involves cost=1 and gamma = 2

names(tune.out)

bestmod = tune.out$best.model
summary(bestmod)  # there are 43 support vectors

ypred = predict(bestmod, newx = dat[-train, ])

table(predict = ypred, truth = dat[-train, "y"])

mean(ypred == dat[-train, "y"])


# So, using SVM the correct classification rate is 61 % i.e. 39 % of the test observations are misclassified.

################################# ROC Curves ################################

install.packages("ROCR")
library(ROCR)


# Let's first write a short function, to plot an ROC curve
# given a vector containing a numerical score for each observation, pred,
# and a vector containing the class label for each observation, truth.

rocplot = function(pred, truth, ...)
{
	predob = prediction(pred, truth)
	perf = performance(predob, "tpr", "fpr")
	plot(perf ,...)
}

# SVMs and support vector classifiers output class labels for each observation.
# However it is also possible to obtain fitted values for each observation, which are the numerical scores used
# to obtain the class labels.
# Foir instance, in the case of a support vector classifier, the fitted value of an observation X = (X1, X2, ..., Xp)^T
# takes the form B0hat + B1hatX1 + B2hatX2 + ... + BphatXp, for SVM with a non-linear kernel it uses a different function explained in the book.
# In essence, the sign of the fitted value determines on which side of the decision boundary the observation lies.

# So, the relationship between the fitted value and the class prediction for an observation is as follows- 
# if the fitted value exceeds zero then the observation is assigned to one class.
# if the fitted value is less than zero, then it is assigned to the other class.

# So, in order to obtain the fitted values for a given SVM model fit, we use decision.values = TRUE when fitting the svm().
# Then the predict() will output the fitted values.

# First we'll only do for the training data.

svmfit.opt = svm(y ~ . , data = dat[train, ], kernel = "radial", gamma = 2, cost = 1, decision.values = T)

fitted = attributes(predict(svmfit.opt, dat[train, ], decision.values = TRUE))$decision.values

# Now we can produce the ROC plot.

par(mfrow = c(1, 2))
rocplot(fitted, dat[train, "y"], main = "Test Data")

# By increasing gamma we can produce a more flexible plot

svmfit.flex = svm(y ~ . , data = dat[train, ], kernel = "radial", gamma = 50, cost = 1, decision.values = T)
fitted = attributes(predict(svmfit.flex, dat[train, ], decision.values = TRUE))$decision.values
rocplot(fitted, dat[train, "y"], add = T, col = "red")  # add = T, adds to the same plot.

# Now let's do for the test data.

fitted = attributes(predict(svmfit.opt, dat[-train, ], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], main = "Test Data")

fitted = attributes(predict(svmfit.flex, dat[-train, ], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], add = T, col = "red")



####################### SVM with multiple classes ########################

# If the response is a factor containing more than two levels, then the svm() will perform multi-class classification
# using the one-versus-one approach. 

# Let's explore that setting here by generating a third class of observations.

set.seed(1)
x = rbind(x, matrix(rnorm(50*2), ncol = 2))
y = c(y, rep(0, 50))
x[y==0, 2] = x[y==0, 2] + 2
dat = data.frame(x=x, y = as.factor(y))
par(mfrow = c(1,1))
plot(x, col=(y+1))

# Fitting the SVM to the data

svmfit = svm(y ~ . , data = dat, kernel = "radial", cost = 10, gamma = 1)

summary(svmfit) # there are 105 support vectors

plot(svmfit, dat)

###################################################################

# Note: The e1071 library can also be used to perform support vector regression, if the response vector that is passed
# in to svm() is numerical rather than a factor.

###########################################################