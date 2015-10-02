############################# Lab Decision Trees ###################################

####### Fitting Classification Trees ##############

install.packages("ISLR")
install.packages("tree")

library(tree)  # to construct classification and regression trees
library(ISLR)

# We first use the Classification trees to analyze the Carseats data set. In this data, Sales is a continuous variable,
# and so we begin by recording it as a binary variable.

fix(Carseats)
attach(Carseats)
names(Carseats)
summary(Carseats)
dim(Carseats)

# We use ifelse() function to create a variable, called High, which takes on a value of "Yes"
# if the Sales variable exceeds 8 (i.e. Mean) and take on a value of "No" otherwise.

High = ifelse(Sales <= 8, "No", "Yes")
Carseats = data.frame(Carseats, High)

# We now use the tree() to fit a classification tree in order to predict High using all variables but Sales.

tree.carseats = tree(High ~ . -Sales, Carseats)
summary(tree.carseats)

#Variables actually used in tree construction:
#[1] "ShelveLoc"   "Price"       "Income"      "CompPrice"   "Population" 
#[6] "Advertising" "Age"         "US"         
#Number of terminal nodes:  27 
#Residual mean deviance:  0.4575 = 170.7 / 373 
#Misclassification error rate: 0.09 = 36 / 400 

## The training error rate is 9%
## Deviance = 45.75 %. A small deviance indicates a tree that provides a good fit to the training data.
## The residual mean deviance is the deviance divided by n - |T0|, whis in this case is 400 - 27 = 373

# The following code shows the tree graphically-

plot(tree.carseats)
text(tree.carseats, pretty = 0)

# The most important indicator of Sales appears to be shelving location, since the first branch differentiates
# Good locations from Bad and Medium locations.

tree.carseats
# It prints output corresponding to each branch of the tree. Lets take an example from these branches.
# Price < 92.5 46  56.530 Yes ( 0.30435 0.69565 )
# The split criterion : Price < 92.5
# The number of obs in that branch : 46
# The deviance : 56.53%
# The overall prediction of the branch (Yes/No) : Yes
# The fraction of observations in that branch that take on values of Yes and No : ( 0.30435 0.69565 )
# Branches that lead to terminal nodes are indicated using asterisks.

# In order to evaluate the performance of a classification tree on these data, we must estimate the test error 
# rather than simply computing the traininig error.
# So, we split the observations into a training set and a test set, build the tree using the training set
# and evaluate its performance on the test set.

set.seed(2)
train = sample(1:nrow(Carseats), dim(Carseats)[1]/2)
Carseats.test = Carseats[-train,]
High.test = High[-train]

tree.carseats = tree(High ~ . - Sales, data = Carseats, subset = train)
tree.pred = predict(tree.carseats, Carseats.test, type = "class")  # type = "class" instructs R to return the actual class prediction.
table(tree.pred, High.test)
mean(tree.pred == High.test)

# This approach leads to correct predictions for around 71.5 % of the locations in the test data set.

### Now we consider whether prunning the tree might lead to improved results.
# cv.tree() : performs cross-validation in order to determine the optimal level of tree complexity;
# cost complexity prunning is used in order to select a sequence of trees for consideration.
# FUN = prune.misclass ; we use this to indicate that we want the classification error rate to guide the 
# cross-validation and prunning process, rather than the default of prunning which is the deviance.
# The cv.tree function reports the number of terminal nodes of each tree considered (size) as well as the
# corresponding error rate and the value of the cost-complexity parameter used (k) i.e. the alpha which we had studied.

set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats

#$size
#[1] 19 17 14 13  9  7  3  2  1

#$dev
#[1] 55 55 53 52 50 56 69 65 80

#$k
#[1]       -Inf  0.0000000  0.6666667  1.0000000  1.7500000  2.0000000  4.2500000
#[8]  5.0000000 23.0000000

## Note, dev is the cross-validation error rate.
## Here, a tree with 9 terminal nodes results in the lowest cross-validation error rate, with 50 cross-validation errors.
## The value of k in 2 which has the lowest cross-validation error rate.

# We plot the error rate as a function of both size and k.

par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

# We now apply the prune.misclass() in order to prune the tree to obtain the nine-node tree.

prune.carseats = prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

# How well does this pruned tree perform on the test data set? Once again, we apply the predict().

tree.pred = predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
mean(tree.pred == High.test)

## Now 77 % of the test observations are correctly classified.
## So, not only has the prunnig process produced more interpretable tree, but it has also improved the classification accuracy.

# If we increase the value of best, we obtain a larger prunned tree with lower classification accuracy.

prune.carseats = prune.misclass(tree.carseats, best = 15)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred = predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
mean(tree.pred == High.test)

# The classification accuracy is now 74 %.

###################################### Fitting Regression Trees ##############################

# Here we fit a regression tree to the Boston data set. First we create a training set, and fit the tree to the training data.

library(MASS)
names(Boston)

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv ~ . , data = Boston, subset = train)
summary(tree.boston) 

# Variables actually used in tree construction:
#[1] "lstat" "rm"    "dis" 

# In the context of a regression tree, the deviance is simply the sum of squared for the tree.
# Here the deviance is 12.65

plot(tree.boston)
text(tree.boston, pretty = 0)

## The variable lstat measures the percentage of individuals with lower socioeconomic status.
## The tree indicates that lower values of lstat corresponds to more expensive houses.
## The tree predicts a median house price of $ 46,400 for larger homes in suburbs in which the residents 
## have high socioeconomic status (rm >= 7.437 and lstat < 9.715)

# Now we use cv.tree() function to see whether pruning the tree will improve the performance.

cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b') 

prune.boston = prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

# In keeping with the cross-validation results, we use the unpruned tree to make predictions on the test set.

yhat = predict(tree.boston, newdata = Boston[-train,])
boston.test = Boston[-train, "medv"]   # same as Boston[-train,]$medv
plot(yhat, boston.test)
abline(0, 1)
mean((yhat - boston.test)^2)
 
# In other words, the test set MSE associated with the regression tree is 25.05
# The square root of the MSE is therefore around 5.005, indicating that this model leads to test predictions
# that are within around $ 5,005 of the true median home value of the suburb.


######################## Bagging and Random Forests ###########################

install.packages("randomForest")
library(randomForest)

# Bagging process:

set.seed(1)
bag.boston = randomForest(medv ~ ., data = Boston, subset = train, mtry = 13, importance = TRUE)
bag.boston

# The argument mtry = 13, indicates that all 13 predictors should be considered for each split of the tree

# How well does this bagged model perform on the test set?

yhat.bag = predict(bag.boston, newdata = Boston[-train, ])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag - boston.test)^2)

# The test set MSE associated with the bagged regression tree is 12.8, almost half that obtained using an 
# optimally-prunned single tree.

# We could change the number of trees grown by randomForest() using the ntree argument.

bag.boston = randomForest(medv ~ . , data = Boston, subset = train, mtry = 13, ntree = 25)
yhat.bag = predict(bag.boston, newdata = Boston[-train, ])
mean((yhat.bag - boston.test)^2)

# Random Forests:

# For Random forest we use a smaller value of mtry argument. By default randomForest() uses p/3 variables
# when building a random forest of regression trees, and sqrt(p) variables when building a random forest of classification trees.

# Here we use, mtry = 6

set.seed(1)
rf.boston = randomForest(medv ~ . , data = Boston, subset = train, mtry = 6, importance = TRUE)
yhat.rf = predict(rf.boston, newdata = Boston[-train, ])
mean((yhat.rf - boston.test)^2)

# The test MSE is 11.31, this indicates that random forests yielded an improvement over the bagging in this case.

importance(rf.boston) # using importance(), we can view the importance of each variable.

# Two measures of variable importance are reported. 
# The former is based upon the mean decrease of accuracy in predictions on the out of bag samples when a given variable is excluded from the model.
# The later is a measure of the total decrease in node impurity that results from splits over that variable, averaged over all trees.
# In the case of regression trees, the node impurity is measured by the training RSS, and for classification trees by deviance.
# This plot can be using below.

varImpPlot(rf.boston)

## The results indicate that across all of the trees considered in the random forest, the wealth level of the community (lstat)
## and the house size (rm) are by far the two most important variables.

############################ Boosting #######################\

# Here we use the gbm package, and within it the gbm() to fit boosted regression trees to the Boston data set.
# We run gbm() with the option, distribution = "gaussian" since this is a regression problem.
# we would use distribution = "bernoulli" if it's a classification problem.
# The argument n.trees = 5000 indicates that we want 5000 trees, and the option 
# interaction.depth = 4 limits the depth of each tree.

install.packages("gbm")
library(gbm)

set.seed(1)
boost.boston = gbm(medv ~ . , data = Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(boost.boston)  # it reports the relative influence statistics.

# By far, lstat and rm are the most important variables.
# We can also produce partial dependence plots for these two variables. These plots illustrate
# the marginal effect of the selected variables on the response after integrating out the other variables.
# In this case, as we might expect, median house prices are increasing with rm and decreasing with lstat.

par(mfrow = c(1, 2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")

# We now use boosted model to predict medv on the test data set.

yhat.boost = predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - boston.test)^2)

# The test MSE obtained is 11.8, similar to the test MSE for random forests and superior to that for bagging.
# If we want to, we can perform boosting with different value of the shrinkage parameter lambda

boost.boston = gbm(medv ~ . , data = Boston[train, ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)
yhat.boost = predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - boston.test)^2)

# Here the test MSE is 11.51 which is slightly better with lambda = 0.2 than the default value of lambda which is .001
