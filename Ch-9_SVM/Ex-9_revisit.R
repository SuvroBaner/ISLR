##################################### Q 4 ################################################

install.packages("e1071")
library(e1071)

# Generate a simulated two-class data set with 100 observations and 
# two features in which there is a visible but non-linear separation between the two classes.


# Let's create a random generated dataset which lies along the parabola y = 3*x^2 + 4.

set.seed(11)

x = rnorm(100)
y = 3*x^2 + 4 + rnorm(100)  # it is a parabola

train = sample(100, 50)

# Trying to separate the two classes-

y[train] = y[train] + 3
y[-train] = y[-train] - 3

par(mfrow = c(1, 2))
plot(x[train], y[train], pch = "+", lwd = 4, col = "red", ylim = c(-8, 24), xlab = "X train", ylab = "Y train")
plot(x[-train], y[-train], pch = "o", lwd = 4, col = "blue", xlab = "X test", ylab = "Y test")

# The above plot clearly shows the non-linear separation.

# We now create both train and test dataframes by taking half of positive and negative classes 
# and creating a new z vector of 0 and 1 for classes.

set.seed(1)
z = rep(0, 100)
z[train] = 1

# Take 25 observations each from train and -train

final.train = c(sample(train, 25), sample(setdiff(1:100, train), 25))  # setdiff : performs set difference

data.train = data.frame(x = x[final.train], y = y[final.train], z = as.factor(z[final.train]))
data.test = data.frame(x = x[-final.train], y = y[-final.train], z = as.factor(z[-final.train]))

#### In this setting, show that a support vector machine with a polynomial kernel or a radial kernel 
#### will outperform a support vector classifier (i.e. a linear kernel)on the training data.

svm.linear = svm(z ~ . , data = data.train, kernel = "linear", cost = 10)
summary(svm.linear)

# It has 16 support vectors.

plot(svm.linear, data.train)
# It can be also seen that roughly 3 observations have been wrongly classified.
# It shows the linear boundary.

names(svm.linear)
table(svm.linear$fitted, data.train$z)
# the confusion matrix shows that there are in total 4 observations that are misclassified.

svm.linear.pred = predict(svm.linear, data.train)  # this is similar to above , just another way of doint it.
table(svm.linear.pred, data.train$z)

# Let's train an SVM with polynomial kernel

svm.poly = svm(z ~ . , data = data.train, kernel = "polynomial", cost = 10)
summary(svm.poly)

# Now you have 25 support vectors.

plot(svm.poly, data.train)  # non-linear boundary.

table(svm.poly$fitted, data.train$z)

# Here 5 observations are misclassified.

# Now train using the radial kernel.

svm.radial = svm(z ~ . , data = data.train, kernel = "radial", cost = 10)
summary(svm.radial)

# Now it uses 11 support vectors.

table(svm.radial$fitted, data.train$z)

# The classifier perfectly classifies the train data.

plot(svm.radial, data.train)

# Now let's employ these models to the test dataset.

plot(svm.linear, data.test)
plot(svm.poly, data.test)
plot(svm.radial, data.test)

svm.linear.pred.test = predict(svm.linear, data.test)
table(svm.linear.pred.test, data.test$z)  # 4 obs are misclassified 

svm.poly.pred.test = predict(svm.poly, data.test)
table(svm.poly.pred.test, data.test$z)  # 9 obs are misclassified

svm.radial.pred.test = predict(svm.radial, data.test)
table(svm.radial.pred.test, data.test$z) # 3 obs are misclassified

# Final Result : Radial basis kernel is the best and has 3 test misclassification error.

################################# Q. 5 ##############################################

# We have seen that we can fit an SVM with a non-linear kernel in order to perform classification using a 
# non-linear decision boundary. We will now see that we can also obtain a non-linear decision boundary by
# performing logistic regression using non-linear transformations of the features.

# Generate a data set with n = 500 and p = 2, such that the observations belong to two classes with a quadratic decision
# boundary between them.

set.seed(10)

# Here two predictors are x1 and x2.

x1 = runif(500) - 0.5  # does a uniform distribution.
x2 = runif(500) - 0.5

# Here y is the response variable.

y = 1*(x1^2 - x2^2 > 0)  
# the expresion inside the bracket is the boolean indexing (True and False). It is further multiplied by 1 to make it either 0 or 1.

# Plot the observations, colored according to their class labels. You should display x1 on x-axis and x2 on y-axis

# i.e. first plot the two predictors x1 and x2 which belong to one class i.e. class "0"

plot(x1[y==0], x2[y==0], col = "red", xlab = "x1", ylab = "x2", pch = "+")

# now plot the x1 and x2 which belong to the other class i.e. class "1" on the previous plot.

points(x1[y==1], x2[y==1], col = "blue", pch = 4)

# The above plot clearly shows the non-linear boundary.

# Now fit a logistic regression model to the data, using x1 and x2 as predictors.

lm.fit = glm(y ~ x1 + x2, family = binomial)
summary(lm.fit)

# Both the predictors in the above model are insignificant in predicting y.

# Apply this model to the training data in order to obtain a predicted class for each training observation. 
# Plot the observations, colored according to the predicted class labels. The decision boundary should be linear.

data = data.frame(x1 = x1, x2 = x2, y = y)
lm.prob = predict(lm.fit, data, type = "response")
lm.pred = ifelse(lm.prob > 0.52, 1, 0)

data.positive = data[lm.pred == 1, ]
data.negative = data[lm.pred == 0, ]

plot(data.positive$x1, data.positive$x2, col = "blue", xlab = "x1", ylab = "x2", pch = "+")
points(data.negative$x1, data.negative$x2, col = "red", pch = 4)

# With the given model and a probability threshold of 0.5, most of the points are classified to single class 
# and no decision boundary can be shown.
# Hence we shift the probability threshold to 0.52 to show a meaningful decision boundary. 
# This boundary is linear as seen in the figure.

# Now fit a logistic regression model to the data using non-linear functions of x1 and x2 as predictors.
# Let them be x1^2, x1*x2 etc to capture the true decision boundary.

lm.fit = glm(y ~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2), data = data, family = binomial)

lm.prob = predict(lm.fit, data, type = "response")

lm.pred = ifelse(lm.prob > 0.5, 1, 0)

data.positive = data[lm.pred == 1, ]
data.negative = data[lm.pred == 0, ]

plot(data.positive$x1, data.positive$x2, col = "blue", xlab = "x1", ylab = "x2", pch = "+")
points(data.negative$x1, data.negative$x2, col = "red", pch = 4)

# The non-linear boundary closely resembles the true decision boundary. 

# Now fit a support vector classifier to the data with x1 and x2 as predictors.
# Obtain a class prediction for each training observation. Plot the observations, colored according to the predicted class labels.

svm.fit = svm(as.factor(y) ~ x1 + x2, data, kernel = "linear", cost = 0.1)
svm.pred = predict(svm.fit, data)

data.positive = data[svm.pred == 1, ]
data.negative = data[svm.pred == 0, ]

plot(data.positive$x1, data.positive$x2, col = "blue", xlab = "x1", ylab = "x2", pch = "+")
points(data.negative$x1, data.negative$x2, col = "red", pch = 4)

# A linear kernel, even with low cost fails to find non-linear decision boundary and classifies 
# most of the points to a single class.

# Fit a SVM using a non-linear kernel to the data. Obtain a class prediction for each training observation.
# Plot the observations, colored according to the predicted class labels.

svm.fit = svm(as.factor(y) ~ x1 + x2, data, gamma = 1)
svm.pred = predict(svm.fit, data)

data.positive = data[svm.pred == 1, ]
data.negative = data[svm.pred == 0, ]

plot(data.positive$x1, data.positive$x2, col = "blue", xlab = "x1", ylab = "x2", pch = "+")
points(data.negative$x1, data.negative$x2, col = "red", pch = 4)

# Again, the non-linear decision boundary on predicted labels closely resembles the true decision boundary.

# This experiment enforces the idea that SVMs with non-linear kernel are extremely powerful in finding non-linear boundary. 
# Both, logistic regression with non-interactions and SVMs with linear kernels fail to find the decision boundary. 
# Adding interaction terms to logistic regression seems to give them same power as radial-basis kernels. 
# However, there is some manual efforts and tuning involved in picking right interaction terms. 
# This effort can become prohibitive with large number of features. 
# Radial basis kernels, on the other hand, only require tuning of one parameter - gamma - 
# which can be easily done using cross-validation.

################################## Q. 6 #####################################

# During our study it was claimed that in the case of data that is just barely linearly separable, a support
# vector classifier with a small value of cost (i.e wider margin) that misclassifies couple of training observations
# may perform better on the test data than one with a huge value of cost (i.e. narrower margin) that does not
# misclassify any training observations.
# let's now investgate this claim.

# Generate two-class data with p = 2 in such a way that the classes are just barely linearly separable.

# So, we will randomly generate 1000 points and scatter them across line y = x with wide margin
# We'll also create noisy points along the line 5x - 4y - 50 = 0.
# These points make the classes barely separable and also shift the maximum margin classifier.

set.seed(12)

# Class One

x.one = runif(500, 0, 90)  # total count is 500, min = 0  and max = 90 with uniform distribution.
y.one = runif(500, x.one + 10, 100)
x.one.noise = runif(50, 20, 80)
y.one.noise = 5/4 * (x.one.noise - 10) + 0.1

# Class Zero

x.zero = runif(500, 10, 100)
y.zero = runif(500, 0, x.zero - 10)
x.zero.noise = runif(50, 20, 80)
y.zero.noise = 5/4 * (x.zero.noise - 10) - 0.1

# Combine all

class.one = seq(1, 550)
x = c(x.one, x.one.noise, x.zero, x.zero.noise)
y = c(y.one, y.one.noise, y.zero, y.zero.noise)

# Plot

plot(x[class.one], y[class.one], col = "blue", pch = "+", ylim = c(0, 100))
points(x[-class.one], y[-class.one], col = "red", pch = 4)

# The plot shows that classes are barely separable. The noisy points create a fictitious boundary 5x-4y-50=0

### Compute the cross-validation error rates for support vector classifiers with a range of cost values.

z = rep(0, 1100)
z[class.one] = 1
data = data.frame(x = x, y = y, z = z)

tune.out = tune(svm, as.factor(z) ~ . , data = data, kernel = "linear", 
	ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 10000)))

summary(tune.out)

# Parameter tuning of ‘svm’:

# - sampling method: 10-fold cross validation 

# - best parameters:
#  cost
# 10000
#
#- best performance: 0 
#
#- Detailed performance results:
#   cost      error dispersion
#1 1e-02 0.05818182 0.02275756
#2 1e-01 0.04545455 0.01916532
#3 1e+00 0.04545455 0.01916532
#4 5e+00 0.04545455 0.01916532
#5 1e+01 0.04727273 0.01858147
#6 1e+02 0.05090909 0.02235041
#7 1e+04 0.00000000 0.00000000

names(tune.out)
names(tune.out$performances)

data.frame(cost = tune.out$performances$cost, misclass = tune.out$performances$error * 1100)

#  cost misclass
#1 1e-02       64
#2 1e-01       50
#3 1e+00       50
#4 5e+00       50
#5 1e+01       52
#6 1e+02       56
#7 1e+04        0

# The table above shows train-misclassification error for all costs. 
# A cost of 10000 seems to classify all points correctly. This also corresponds to a cross-validation error of 0.

# We now generate a random test-set of same size. This test-set satisfies the true decision boundary x=y.

set.seed(111)
x.test = runif(1000, 0, 100)
class.one = sample(1000, 500)
y.test = rep(NA, 1000)
# Set y > x for class.one
for (i in class.one) {
    y.test[i] = runif(1, x.test[i], 100)
}
# set y < x for class.zero
for (i in setdiff(1:1000, class.one)) {
    y.test[i] = runif(1, 0, x.test[i])
}
plot(x.test[class.one], y.test[class.one], col = "blue", pch = "+")
points(x.test[-class.one], y.test[-class.one], col = "red", pch = 4)

# We now make same predictions using all linear svms with all costs used in previous part.

set.seed(30012)
z.test = rep(0, 1000)
z.test[class.one] = 1
all.costs = c(0.01, 0.1, 1, 5, 10, 100, 1000, 10000)
test.errors = rep(NA, 8)
data.test = data.frame(x = x.test, y = y.test, z = z.test)
for (i in 1:length(all.costs)) {
    svm.fit = svm(as.factor(z) ~ ., data = data, kernel = "linear", cost = all.costs[i])
    svm.predict = predict(svm.fit, data.test)
    test.errors[i] = sum(svm.predict != data.test$z)
}

data.frame(cost = all.costs, `test misclass` = test.errors)

#cost test.misclass
#1 1e-02            49
#2 1e-01            12
#3 1e+00             2
#4 5e+00             1
#5 1e+01             3
#6 1e+02           157
#7 1e+03           177
#8 1e+04           177

# cost=5 seems to be performing better on test data, making the least number of classification errors. 
# This is much smaller than optimal value of 10000 for training data.

# We again see an overfitting phenomenon for linear kernel. 
# A large cost tries to fit correctly classify noisy-points and hence overfits the train data. 
# A small cost, however, makes a few errors on the noisy test points and performs better on test data.