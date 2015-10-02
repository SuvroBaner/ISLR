############################################ Ch- 8 ####################################################
########################## Q. 7 ###################################

install.packages("randomForest")

library(randomForest)
library(MASS)

set.seed(100)

#### Setting up the data ###

train = sample(dim(Boston)[1], dim(Boston)[1]/2)
x.train = Boston[train, -14]  # removing the response variable "medv"
x.test = Boston[-train, -14]
y.train = Boston[train, 14]
y.test = Boston[-train, 14]

### Setting different values of predictors ###

p = dim(Boston)[2] - 1
p.2 = p/2
p.3 = p/3
p.sq = sqrt(p)

### Executing the randomForest model fitting on the training data ###

rf.boston.p = randomForest(x.train, y.train, xtest = x.test, ytest = y.test, mtry = p, ntree = 500)  # it gives us the test error rate as well in one shot, which predict() does separately.
rf.boston.p.2 = randomForest(x.train, y.train, xtest = x.test, ytest = y.test, mtry = p.2, ntree = 500)
rf.boston.p.3 = randomForest(x.train, y.train, xtest = x.test, ytest = y.test, mtry = p.3, ntree = 500)
rf.boston.p.sq = randomForest(x.train, y.train, xtest = x.test, ytest = y.test, try = p.sq, ntree = 500)

### Data Visualization of these models ###

plot(1:500, rf.boston.p$test$mse, col = "green", type = "l", xlab = "Number of Trees", ylab = "Test MSE", ylim = c(10, 19))
lines(1:500, rf.boston.p.2$test$mse, col = "red", type = "l")
lines(1:500, rf.boston.p.3$test$mse, col = "blue", type = "l")
lines(1:500, rf.boston.p.sq$test$mse, col = "darkgrey", type = "l")
legend("topright", c("m=p", "m=p/2", "m=p/3", "m=sqrt(p)"), col = c("green", "red", "blue", "darkgrey"), cex = 1, lty = 1)

# The plot shows that test MSE for single tree is quite high around 18.
# It is reduced by adding more trees to the model and stabilized around 300 to 400 trees.
# Test MSE for including all variables at split i.e. m = p (special case of Random Forest also called as Bagging) is slightly higher at 11
# as compared to subset of predictors where the test MSE is around 10.

########################### Q. 8 ######################################

# We'll seek to predict Sales using Regression trees and related approaches, treating the response as a quantitative variable.

install.packages("ISLR")
install.packages("tree")

library(tree)
library(ISLR)

names(Carseats)
attach(Carseats)

train = sample(dim(Carseats)[1], dim(Carseats)[1]/2)
Carseats.train = Carseats[train, ]
Carseats.test = Carseats[-train, ]

#### Fitting a regression tree to a training set 

tree.carseats = tree(Sales ~ . , data = Carseats.train)
summary(tree.carseats)

# Variables actually used in tree construction:
# [1] "ShelveLoc"   "Price"       "Age"         "CompPrice"   "Income"     
# [6] "Advertising" "Education"  
# Number of terminal nodes:  18
# Residual mean deviance:  2.116 = 385.1 / 182 

plot(tree.carseats)
text(tree.carseats, pretty = 0)

pred.carseats = predict(tree.carseats, Carseats.test)
mean((pred.carseats - Carseats.test$Sales)^2)

# The test MSE is 4.15.
# The square root of MSE is about 2 indicating that this model leads to test predictions that are within 2,000 
# of the true value of the Sales (units sold at each location)

#### Using cross-validation in order to determine the optimal level of tree complexity.

cv.carseats = cv.tree(tree.carseats, FUN = prune.tree)

names(cv.carseats)

par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = 'b')
plot(cv.carseats$k, cv.carseats$dev, type = 'b')

# The best size of the terminal node is 10 which is also the optimal level of tree complexity.

pruned.carseats = prune.tree(tree.carseats, best = 10)
par(mfrow = c(1, 1))
plot(pruned.carseats)
text(pruned.carseats, pretty = 0)

pred.pruned.carseats = predict(pruned.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.pruned.carseats)^2)

# The test MSE is 5.16. In this case pruning has increased the test MSE.

#### Use Bagging approach to analyze the data.

set.seed(101)
bag.carseats = randomForest(Sales ~ . , data = Carseats.train, mtry = 10, ntree = 500, importance = T)
bag.pred = predict(bag.carseats, Carseats.test)
mean((bag.pred - Carseats.test$Sales)^2)

# The test MSE is 3.12. Bagging has improved the test MSE.

importance(bag.carseats)
varImpPlot(bag.carseats)

# We see that Price, ShelveLoc and Age are the three most important predictors of Sale.

### Use Random Forests to analyze the data

rf.carseats = randomForest(Sales ~ . , data = Carseats.train, mtry = 5, ntree = 500, importance = TRUE)
rf.pred = predict(rf.carseats, Carseats.test)
mean((Carseats.test$Sales - rf.pred)^2)

# The test MSE is 3.05 which has a lower test MSE than Bagging.

importance(rf.carseats)
varImpPlot(bag.carseats)
# We see that Price, ShelveLoc and Age are the three most important predictors of Sale.

############################################ Q.9 ###########################################

########## Orange Juice Data ##########

install.packages("ISLR")
install.packages("tree")

library(ISLR)
library(tree)

dim(OJ)
fix(OJ)
names(OJ)
summary(OJ)
attach(OJ)

###### Setting the data

set.seed(1)
train = sample(nrow(OJ), 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]

###### Fitting a tree to the training data

tree.OJ = tree(Purchase ~ ., data = OJ, subset = train)
summary(tree.OJ)

#Classification tree:
#tree(formula = Purchase ~ ., data = OJ, subset = train)
#Variables actually used in tree construction:
#[1] "LoyalCH"       "PriceDiff"     "SpecialCH"     "ListPriceDiff"
#Number of terminal nodes:  8 
#Residual mean deviance:  0.7305 = 578.6 / 792 
#Misclassification error rate: 0.165 = 132 / 800 

### The tree has 4 variables [1] "LoyalCH"       "PriceDiff"     "SpecialCH"     "ListPriceDiff"
### It has 8 terminal nodes
### The training error rate (misclassification error rate) for the tree is 16.5 %

tree.OJ

### Lets pick the node (10) : 10) PriceDiff < 0.195 83   91.66 MM ( 0.24096 0.75904 ) 
### The split criterion is PriceDiff < 0.195, the number of observation in this branch is 83
### The deviance is 91.66 % in this region. The overall prediction of this branch is purchase of MM (Minute Maid)
### About 24.1 % points in this node have CH as value of Purchase and remaining 75.9 % as MM.

plot(tree.OJ)
text(tree.OJ, pretty = 0)

### LoyalCH is the most important variable of the tree, in fact the top three nodes are LoyalCH.
### If the LoyalCH < .26, the tree predicts MM. If the LoyalCH > .76, the tree predicts CH.
### For intermediate values of LoyalCH, purchase also depends on PriceDiff and SpecialCH.

###### Predict the response on the test data

OJ.pred = predict(tree.OJ, OJ.test, type = "class")
table(OJ.test$Purchase, OJ.pred)
mean(OJ.test$Purchase != OJ.pred)

### The test error rate is 22.6 %

###### Use cross-validation approach on the training set to determine the optimal tree size.

cv.OJ = cv.tree(tree.OJ, FUN = prune.tree)  # guided by deviance which is by default.
names(cv.OJ)
cv.OJ

plot(cv.OJ$size, cv.OJ$dev, type = "b", xlab = "Tree Size", ylab = "Deviance")

### The tree size of 7 gives the lowest cross-validation error. So, the optimal tree size is 7

###### Produce a prunned tree corresponding to the optimal tree size obtained by cross validation.

prune.OJ = prune.tree(tree.OJ, best = 7)
summary(prune.OJ)

### The misclassification error rate of the prunned tree i.e. the training error rate is 16.5 % i.e. exactly the same as the unprunned tree.

###### Compare the test error rate of the prunned and unprunned trees.

OJ.prun.pred = predict(prune.OJ, OJ.test, type = "class")
table(OJ.test$Purchase, OJ.prun.pred)
mean(OJ.test$Purchase != OJ.prun.pred)

### The test error rate of the prunned tree is 22.6 % which is exactly same as the test error rate of the unpruned tree.

#################################### Q. 10 ##########################################

###### Boosting to predict the salary in the Hitters data set.

install.packages("ISLR")
install.packages("gbm")

library(ISLR)
library(gbm)

fix(Hitters)
names(Hitters)
dim(Hitters)
attach(Hitters)
summary(Hitters)

sum(is.na(Hitters$Salary)) # there are 59 records where salary is 'NA'
Hitters = Hitters[-which(is.na(Hitters$Salary)), ] # omiting those

Hitters$Salary = log(Hitters$Salary)  # log transforming the salary

train = 1:200  # training set with first 200 obs.
Hitters.train = Hitters[train, ]
Hitters.test = Hitters[-train, ]

### Perform boosting on the training data set.

set.seed(102)
pow = seq(-10, -0.1, by = 0.2)
lambda = 10^pow
length.lambda = length(lambda)
train.errors = rep(NA, length.lambda)
test.errors = rep(NA, length.lambda)

for (i in 1:length.lambda)
{
	boost.Hitters = gbm(Salary ~ . , data = Hitters.train, distribution = "gaussian", n.trees = 1000, shrinkage = lambda[i])
	train.pred = predict(boost.Hitters, Hitters.train, n.trees = 1000)
	test.pred = predict(boost.Hitters, Hitters.test, n.trees = 1000)
	train.errors[i] = mean((Hitters.train$Salary - train.pred)^2)
	test.errors[i] = mean((Hitters.test$Salary - test.pred)^2)
}
# a shrinkage parameter applied to each tree in the expansion. Also known as the learning rate or step-size reduction.

### Plotting training set MSE and test set MSE with respect to the shrinkage parameters.

par(mfrow = c(1, 2))
plot(lambda, train.errors, type = "b", xlab = "Shrinkage", ylab = "Train MSE", col = "green", pch = 20)
plot(lambda, test.errors, type = "b", xlab = "Shrinkage", ylab = "Test MSE", col = "blue", pch = 20)

min(test.errors)
lambda[which.min(test.errors)]

# Minimum test MSE is 0.26 or 26 % when the value of lambda is 0.04

###### Compare this boosting method with other regression techniques

lm.fit = lm(Salary ~ . , data = Hitters, subset = train)
lm.pred = predict(lm.fit, Hitters.test)
mean((Hitters.test$Salary - lm.pred)^2)

# The test set MSE using linear regression is 49.18 %

install.packages("glmnet")

library(glmnet)

set.seed(104)
x = model.matrix(Salary ~ . , data = Hitters.train)
y = Hitters.train$Salary
x.test = model.matrix(Salary ~ . , data = Hitters.test)
lasso.fit = glmnet(x, y, alpha = 1)
lasso.pred = predict(lasso.fit, s = 0.01, newx = x.test)
mean((Hitters.test$Salary - lasso.pred)^2)

# The test set MSE using lasso is 47 %
# Both Lasso and Linear Regression have a higher test set MSE compared to Boosting.

###### In the boosted model lets set the shrinkage parameter which has the lowest test set MSE.

boost.best = gbm(Salary ~ . , data = Hitters.train, distribution = "gaussian", n.trees = 1000, shrinkage = lambda[which.min(test.errors)])
summary(boost.best)

### The most important predictors in the boosted model is CAtBat, CRuns , CHits , CWalks etc.

###### Apply Bagging to the training set

install.packages("randomForest")
library(randomForest)

set.seed(105)
rf.Hitters = randomForest(Salary ~ . , data = Hitters.train, ntree = 500, mtry = 19)
rf.pred = predict(rf.Hitters, Hitters.test)
mean((Hitters.test$Salary - rf.pred)^2)

# The test set MSE using Bagging is 23.02 % which is slightly lower than test set MSE for boosting.

######################################## Q. 11 ############################################

######## Caravan: The Insurance Company (TIC) Benchmark ########

# The data contains 5822 real customer records. Each record consists of 86 variables, containing
#sociodemographic data (variables 1-43) and product ownership (variables 44-86). The sociodemographic
#data is derived from zip codes. All customers living in areas with the same zip code have
#the same sociodemographic attributes. Variable 86 (Purchase) indicates whether the customer purchased
#a caravan insurance policy.
#Predictor information is at : http://www.liacs.nl/~putten/library/cc2000/data.html	

install.packages("ISLR")
install.packages("gbm")

library(ISLR)
library(gbm)

names(Caravan)
fix(Caravan)
attach(Caravan)
dim(Caravan)
summary(Caravan$Purchase)
# Note: 348 people have purchased the Caravan insurance policy i.e. about 6% of the people from the data set.

# Fit a boosting model to the training set. 
# Remember it is a classification problem, so the distribution is "bernoulli"
# It needs the response variable to be in {0, 1}

train = 1:1000
Caravan$Purchase = ifelse(Caravan$Purchase == "Yes", 1, 0)
Caravan.train = Caravan[train, ]
Caravan.test = Caravan[-train, ]

set.seed(1)
boost.Caravan = gbm(Purchase ~ . , data = Caravan.train, n.trees = 1000, shrinkage = 0.01, distribution = "bernoulli")
summary(boost.Caravan)  # relative influence statistics.

# The following are the most important predictors in sequence.
# PPERSAUT : Contribution Car Policies
# MKOOPKLA : Purchasing power class
# MOPLHOOG : High level education
# MBERMIDD : Middle management (Occupation)
# PBRAND   : Contribution fire policies
# MGODGE   : No Relegion
# ABRAND   : Number of fire policies
# MINK3045 : Income 30-45.000

###### Use the boosting model to predict the response on the test data.
###### Predict that a person will make a purchase if the estimated probability of the purchase is greater than 20%.

boost.prob = predict(boost.Caravan, Caravan.test, n.trees = 1000, type = "response")
boost.pred = ifelse(boost.prob > 0.2, 1, 0)
table(Caravan.test$Purchase, boost.pred)  # the confusion matrix is below

#   boost.pred
#       0    1
#  0 4410  123
#  1  256   33

# Of 156 people predicted to make the purchase 33 people actually ended up purchasing that i.e. 21.15 %

##### Compare the result with the logistic regression.

glm.Caravan = glm(Purchase ~ . , data = Caravan.train, family = binomial)
glm.prob = predict(glm.Caravan, Caravan.test, type = "response")
glm.pred = ifelse(glm.prob > 0.2, 1, 0)
table(Caravan.test$Purchase, glm.pred)

# Of 408 people predicted to make the purchase 58 people actually ended up purchasing that i.e. 14.21 %
# Boosting process has a better result than the logistic regression.

##################################### Q. 12 ##########################################
###### Data set: Weekly, Weekly percentage returns for the S&P 500 stock index between 1990 and 2010.

set.seed(1)
summary(Weekly)
attach(Weekly)
names(Weekly)

train = sample(nrow(Weekly), 2/3 * nrow(Weekly))
test = -train

###### Logistic regression

glm.fit = glm(Direction ~ . - Year - Today , data = Weekly, subset = train, family = binomial)
glm.probs = predict(glm.fit, newdata = Weekly[test,], type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Weekly$Direction[test])
mean(glm.pred != Weekly$Direction[test])

# The test MSE is 49.03, this model is as good as random guessing.

###### Boosting

Weekly$BinomialDirection = ifelse(Weekly$Direction == "Up", 1, 0)
boost.Weekly = gbm(BinomialDirection ~ . - Year - Today - Direction, data = Weekly[train, ], distribution = "bernoulli", n.trees = 5000)
yhat.prob = predict(boost.Weekly, newdata = Weekly[test, ], n.trees = 5000)
yhat.pred = rep(0, length(yhat.prob))
yhat.pred[yhat.prob > 0.5] = 1
table(yhat.pred, Weekly$BinomialDirection[test])
mean(yhat.pred != Weekly$BinomialDirection[test])

# The test MSE is 47.93 % which is slightly better than Logistic regression.

###### Bagging

install.packages("randomForest")
library(randomForest)

Weekly = Weekly[, !(names(Weekly) %in% c("BinomialDirection"))]
set.seed(10)
bag.Weekly = randomForest(Direction ~ . - Year - Today , data = Weekly, subset = train, mtry = 6)
yhat.bag = predict(bag.Weekly, newdata = Weekly[test, ])
table(yhat.bag, Weekly$Direction[test])
mean(yhat.bag != Weekly$Direction[test])

# The test MSE is 48.76 %

###### Random Forests
rf.Weekly = randomForest(Direction ~ . - Year - Today, data = Weekly, subset = train, mtry = 2)
yhat.bag = predict(rf.weekly, newdata = Weekly[test, ])
table(yhat.bag, Weekly$Direction[test])
mean(yhat.bag != Weekly$Direction[test])

# Th test MSE is 48.76 %