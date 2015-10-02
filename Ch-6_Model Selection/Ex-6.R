########################### Q.8 ###################################

install.packages("ISLR")
install.packages("leaps")
install.packages("glmnet")
install.packages("pls")


library(ISLR)
library(leaps)
library(glmnet)
library(pls)
library(MASS)

# Generate a predictor X of length n = 100
# Generate a noise vector Epsilon of length n = 100

set.seed(1)
X = rnorm(100)
Eps = rnorm(100)

# Generate a response vector Y of length n = 100, according to model Y = B0 + B1X + B2X^2 + B3X^3 + Eps

B0 = 3
B1 = 2
B2 = -3
B3 = 0.3

Y = B0 + B1*X + B2*X^2 + B3*X^3 + Eps

# Use regsubsets() to perform the best subset selection in order to choose the best model containing the predictors X, X^2..., X^10

data.full = data.frame(y = Y, x = X)
mod.full = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10)  # raw = T, use raw and not orthogonal polynomials.
mod.summary = summary(mod.full)
names(mod.summary)
which.min(mod.summary$cp)     # No. of variables are 3
which.min(mod.summary$bic)    # No. of variables are 3
which.max(mod.summary$adjr2)  # No. of variables are 3

par(mfrow = c(2, 2))
plot(mod.summary$cp, xlab = "Number of Variables", ylab = "CP", type = "l")
points(which.min(mod.summary$cp), mod.summary$cp[which.min(mod.summary$cp)], col = "red", cex = 2, pch = 20)

plot(mod.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(which.min(mod.summary$bic), mod.summary$bic[which.min(mod.summary$bic)], col = "red", cex = 2, pch = 20)

plot(mod.summary$adjr2, xlab = "Number of Variables", ylab = "AdjR2", type = "l")
points(which.max(mod.summary$adjr2), mod.summary$adjr2[which.max(mod.summary$adjr2)], col = "red", cex = 2, pch = 20)

# Lets also use the in-built plot function of regsubsets-

plot(mod.full, scale = "Cp")
plot(mod.full, scale = "bic")
plot(mod.full, scale = "adjr2")

# They also give you the same results.

# So, the best model obtained according to Cp, BIC and AdjR2 is 3 variables model.
# Report the coefficients of the best model obtained.

coef(mod.full, 3)

#(Intercept)      poly(x, 10, raw = T)1     poly(x, 10, raw = T)2    poly(x, 10, raw = T)7 
#3.07627412            2.35623596           -3.16514887               0.01046843 

# Note the variables are 1, 2, and 7

# Repeat the above process with forward stepwise selection and backward stepwise slection

mod.fwd = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10, method = "forward")
fwd.summary = summary(mod.fwd)

mod.bwd = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10, method = "backward")
bwd.summary = summary(mod.bwd)

par(mfrow = c(3, 2))
plot(fwd.summary$cp, xlab = "Number of Variables", ylab = "CP", main = "Forward Selection", type = "l")
points(which.min(fwd.summary$cp), fwd.summary$cp[which.min(fwd.summary$cp)], col = "red", cex = 2, pch = 20)
plot(fwd.summary$bic, xlab = "Number of Variables", ylab = "BIC", main = "Forward Selection", type = "l")
points(which.min(fwd.summary$bic), fwd.summary$bic[which.min(fwd.summary$bic)], col = "red", cex = 2, pch = 20)
plot(fwd.summary$adjr2, xlab = "Number of Variables", ylab = "AdjR2", main = "Forward Selection", type = "l")
points(which.max(fwd.summary$adjr2), fwd.summary$adjr2[which.max(fwd.summary$adjr2)], col = "red", cex = 2, pch =20)

plot(bwd.summary$cp, xlab = "Number of Variables", ylab = "CP", main = "Backward Selection", type = "l")
points(which.min(bwd.summary$cp), bwd.summary$cp[which.min(bwd.summary$cp)], col = "red", cex = 2, pch = 20)
plot(bwd.summary$bic, xlab = "Number of Variables", ylab = "BIC", main = "Backward Selection", type = "l")
points(which.min(bwd.summary$bic), bwd.summary$bic[which.min(bwd.summary$bic)], col = "red", cex = 2, pch = 20)
plot(bwd.summary$adjr2, xlab = "Number of Variables", ylab = "AdjR2", main = "Backward Selection", type = "l")
points(which.max(bwd.summary$adjr2), bwd.summary$adjr2[which.max(bwd.summary$adjr2)], col = "red", cex = 2, pch =20)

### Forward Selection ###
# Cp : 3 variables model
# AdjR2 : 3 variables model
# BIC : 3 variables model

### Backward Selection ###
# Cp : 3 variables model
# AdjR2 : 4 variables model
# BIC : 3 variables model

coef(mod.fwd, 3)

#(Intercept)      poly(x, 10, raw = T)1   poly(x, 10, raw = T)2   poly(x, 10, raw = T)7 
# 3.07627412       2.35623596             -3.16514887               0.01046843

coef(mod.bwd, 3)

#(Intercept)      poly(x, 10, raw = T)1    poly(x, 10, raw = T)2   poly(x, 10, raw = T)9 
#  3.078881355     2.419817953              -3.177235617             0.001870457

coef(mod.bwd, 4)

#(Intercept)    poly(x, 10, raw = T)1   poly(x, 10, raw = T)2  poly(x, 10, raw = T)4   poly(x, 10, raw = T)5 
# 3.12902640      2.27105667             -3.32284363            0.04320229              0.05388957 


### Use Lasso model to the simulated data using X, X^2 .., X^10 as predictors.

xmat = model.matrix(y ~ poly(x, 10, raw = T), data = data.full)[, -1]  # excluding the dependent variable

set.seed(1)
mod.lasso = cv.glmnet(xmat, Y, alpha = 1)
names(mod.lasso)
bestlambda = mod.lasso$lambda.min
bestlambda
plot(mod.lasso)

# Using cross-validation the optimal value of lambda is 0.008208395

# Now fit the model on the entire data set using the best value of lambda

best.model = glmnet(xmat, Y, alpha = 1)
predict(best.model, s = bestlambda, type = "coefficients")

# Lasso picks the following predictors in the best models-

#(Intercept)             3.0857752439
#poly(x, 10, raw = T)1   2.2819916853
#poly(x, 10, raw = T)2  -3.1851476386
#poly(x, 10, raw = T)3   .           
#poly(x, 10, raw = T)4   .           
#poly(x, 10, raw = T)5   0.0411013951
#poly(x, 10, raw = T)6   .           
#poly(x, 10, raw = T)7   0.0023168677
#poly(x, 10, raw = T)8   0.0001211269
#poly(x, 10, raw = T)9   .           
#poly(x, 10, raw = T)10  0.0000821956

### Now let's change the model to Y = B0 + B7*X^7 + epsilon ##

B7= 7
Y = B0 + B7*X^7 + Eps

data.full = data.frame(y = Y, x = X)
mod.full = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10)
mod.summary = summary(mod.full)
which.min(mod.summary$cp)    # variable is 2
which.min(mod.summary$bic)   # variable is 1 
which.max(mod.summary$adjr2) # variable is 4

coef(mod.full, 1)

#(Intercept)      poly(x, 10, raw = T)7 
#   2.95894               7.00077 

coef(mod.full, 2)

#(Intercept)     poly(x, 10, raw = T)2    poly(x, 10, raw = T)7 
#  3.0704904            -0.1417084             7.0015552 

coef(mod.full, 4)

#(Intercept)   poly(x, 10, raw = T)1  poly(x, 10, raw = T)2   poly(x, 10, raw = T)3   poly(x, 10, raw = T)7 
# 3.0762524      0.2914016            -0.1617671               -0.2526527               7.0091338 

### Here , BIC picks the most accurate 1 - variable model with matching coefficients, Other criteria pick additional variables.

xmat = model.matrix(y ~ poly(x, 10, raw = T), data = data.full)[,-1]
mod.lasso = cv.glmnet(xmat, Y, alpha = 1)
best.lambda = mod.lasso$lambda.min
best.lambda  # The value of best lambda is 12.37

best.model = glmnet(xmat, Y, alpha = 1)
predict(best.model, s = best.lambda, type = "coefficients")

#(Intercept)            3.820215
#poly(x, 10, raw = T)1  .       
#poly(x, 10, raw = T)2  .       
#poly(x, 10, raw = T)3  .       
#poly(x, 10, raw = T)4  .       
#poly(x, 10, raw = T)5  .       
#poly(x, 10, raw = T)6  .       
#poly(x, 10, raw = T)7  6.796694
#poly(x, 10, raw = T)8  .       
#poly(x, 10, raw = T)9  .       
#poly(x, 10, raw = T)10 .    

# Lasso also picks the best 1-variable model. The intercept is also quite similar. 

########################### Q - 9 #################################################

### Predict the number of applications received using the other variables in the College data set.

fix(College)
names(College)
dim(College)
attach(College)

sum(is.na(College))

### Split the dataset into training and test set.

train = sample(1: dim(College)[1], dim(College)[1]/2)
test = -train
College.train = College[train, ]
College.test = College[test, ]
Apps.test = College.test$Apps
Apps.train = College.train$Apps

lm.fit = lm(Apps ~ . , data = College, subset = train)
lm.pred = predict(lm.fit, College.test, type = "response")
mean((Apps.test - lm.pred)^2)

# The test Residual Sum of Squared (i.e. Test RSS) is 1132563

### Fit the ridge regression model on the training set, with lambda chosen by cross-validation.

train.mat = model.matrix(Apps ~ . , data = College.train)
test.mat = model.matrix(Apps ~ . , data = College.test)

set.seed(1)

grid = 10^seq(10, -2, length = 100))

ridge.mod = cv.glmnet(train.mat, Apps.train, alpha = 0, lambda = grid, thresh = 1e-12)
plot(ridge.mod)
bestlam = ridge.mod$lambda.min
bestlam

# So, the value of lambda is 342 when the cross validation error is minimum.

ridge.pred = predict(ridge.mod, s = bestlam, newx = test.mat)
mean((Apps.test - ridge.pred)^2)

# The test error obtained i.e. the RSS is 1438359 which is slightly higher than OLS (Ordinary Least Squares)

### Fit the lasso regression model on the training set, with lambda chosen bt cross-validation

set.seed(2)

lasso.mod = cv.glmnet(train.mat, Apps.train, alpha = 1, lambda = grid, thresh = 1e-12)
plot(lasso.grid)
bestlam = lasso.mod$lambda.min
bestlam

# So, the value of lambda is 12.33 when the cross validation error is minimum.

lasso.pred = predict(lasso.mod, s = bestlam, newx = test.mat)
mean((Apps.test - lasso.pred)^2)

# The test error obtained i.e. the RSS is 1286731 which is about the same as the OLS (Ordinary Least Squares)

coef(lasso.mod)

# The statistically significant coefficients as per lasso fit are as follows-
#PrivateYes  -315.41089384
#Accept         1.26203603
#Enroll         0.07292179
#Top10perc     22.35351255
#Expend         0.04762121

summary(lm.fit)

# The statistically significant coefficients as per OLS fit are as follows-
#PrivateYes  -805.42256
#Accept         1.30965
#Top10perc     48.37332
#Top25perc    -12.84845
#Outstate      -0.09953
#Room.Board     0.25851
#Expend         0.08645
#Grad.Rate     14.63967

### The model as per lasso has fewer predictors which seem better having almost the same RSS compared to OLS.

### Fit a PCR (Principal Componenet Regresison) model on the training data set, with M chosen by cross validation

set.seed(3)

pcr.fit = pcr(Apps ~ . , data = College, subset = train, scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP") # mean squared error of prediction (MSEP)
# The lowest MSEP is when M (number of principal components) are 16 which is barely a dimension reduction.
# But at 5, it is also quite minimal, so let's take M = 5

pcr.pred = predict(pcr.fit, College.test, ncomp = 5)
mean((Apps.test - pcr.pred)^2)

# Test RSS for PCR is about 3105447 whihc is quite high. Note, if you change the ncomp to 16 then the test RSS would decrease but
# in that case PCR has got so extra meaning as it wouldn't be any dimension reduction, we could very well use the earlier models.

### Fit a PLS model on the training set, with M chosen by cross-validation.

set.seed(4)
pls.fit = plsr(Apps ~ . , data = College, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)

validationplot(pls.fit, val.type = "MSEP")

# The lowest MSEP is when M = 6

pls.pred = predict(pls.fit, College.test, ncomp = 6)
mean((Apps.test - pls.pred)^2)

# With M = 6, the test RSS is 1491985 which is quite decent. It does well compared to PCR as it does a supervised learning.

### How accurately we can predict the numbers of college applications received and what's the difference in all these models.

test.avg = mean(Apps.test)
lm.test.r2 = 1 - mean((Apps.test - lm.pred)^2) / mean((Apps.test - test.avg)^2)
ridge.test.r2 = 1 - mean((Apps.test - ridge.pred)^2) / mean((Apps.test - test.avg)^2)
lasso.test.r2 = 1 - mean((Apps.test - lasso.pred)^2) / mean((Apps.test - test.avg)^2)
pcr.test.r2 = 1 - mean((Apps.test - pcr.pred)^2) / mean((Apps.test - test.avg)^2)
pls.plot.r2 = 1 - mean((Apps.test - pls.pred)^2) / mean((Apps.test - test.avg)^2)

barplot(c(lm.test.r2, ridge.test.r2, lasso.test.r2, pcr.test.r2, pls.test.r2), col="red", names.arg=c("OLS", "Ridge", "Lasso", "PCR", "PLS"), main="Test R-squared")

## Test R2 for all the models except PCR is about 0.9 i.e. 90 % of the varaibility of the test set is explained by the model.
## All the models except PCR predicts the College application with a high accuracy.

############################ Q. 10 #######################################################

# Generate a dataset with p = 20 features, n = 1000 observations, and an associated quantitative response vector
# generated according to the model Y = XB + eps, where B has some elements exactly equal to zero.

set.seed(1)

p = 20
n = 1000
x = matrix(rnorm(n * p), n, p)
B = rnorm(p)
B[3] = 0
B[4] = 0
B[9] = 0
B[10] = 0
B[19] = 0
eps = rnorm(p)

y = x %*% B + eps  # matrix multiplication.

train = sample(seq(n), 100, replace = FALSE)  # training set contains 100 obs
y.train = y[train, ]
y.test = y[-train, ]
x.train = x[train, ]
x.test = x[-train, ]

# Perform best subset selection on the training set

regfit.train = regsubsets(y ~ . , data = data.frame(x = x.train, y = y.train), nvmax = p)

# Plot the training set MSE associated with the best model of each size.

val.errors = rep(NA, p)
x_cols = colnames(x, do.NULL = FALSE, prefix = "x.")

for(i in 1:p)
{
	coefi = coef(regfit.train, id = i)
	pred = as.matrix(x.train[, x_cols %in% names(coefi)]) %*% coefi[names(coefi) %in% x_cols]
	val.errors[i] = mean((y.train - pred)^2)
}
plot(val.errors, xlab = "Number of predictors", ylab = "Training MSE", pch = 19, type = "b")
which.min(val.errors)  # 20

# Plot the test set MSE associated with the best model of each size-

val.errors = rep(NA, p)

for(i in 1:p)
{
	coefi = coef(regfit.train, id = i)
	pred = as.matrix(x.test[, x_cols %in% names(coefi)]) %*% coefi[names(coefi) %in% x_cols]
	val.errors[i] = mean((y.test - pred)^2)
}
plot(val.errors, ylab = "Test MSE", pch = 19, type = "b")
which.min(val.errors) # 9
coef(regfit.train, which.min(val.errors))

### So, the 9 parameter model has the smallest test MSE.
#(Intercept)         x.2         x.8        x.11        x.12        x.13 
#  0.1702579  -1.0457233   1.4062346  -0.5665442   0.8884424   0.4286907 
#       x.14        x.15        x.17        x.20 
#  0.4037272   0.7683277   1.2228999   1.0341927

#B[3] = 0
#B[4] = 0
#B[9] = 0
#B[10] = 0
#B[19] = 0

# The result has caught all the zeros of the true model. Here 3, 4, 9, 10, 19 are not present in the 9 parameter model which we found right now.

####################################### Q 11. ###########################################

##### Prdict per capita crime rate in the Boston Data set ###########################

fix(Boston)
dim(Boston)
attach(Boston)
names(Boston)
sum(is.na(Boston))

### Best Subset selection method, and checking for Cp, BIC, AdjR2 to minimize the test error ###

regfit.full = regsubsets(crim ~ . , data = Boston, nvmax = 13)
reg.summary = summary(regfit.full)

names(reg.summary)

par(mfrow = c(2, 2))

plot(reg.summary$rss, ylab = "RSS", type = "l")

plot(reg.summary$adjr2, ylab = "Adjusted Rsq", type = "l")
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col = "red", cex = 2, pch = 20)

plot(reg.summary$cp, ylab = "Cp", type = "l")
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], col = "red", cex = 2, pch = 20)

plot(reg.summary$bic, ylab = "BIC", type = "l")
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col = "red", cex = 2, pch = 20)

# As per AdjR2 it is a 9 variable model
# As per Cp it is a 8 variable model
# As per BIC it is a 3 variable model

# Now let's use the in-built plot functionality of regsubsets()

par(mfrow = c(2, 2))
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic") 

# As per r2 : all 13 predictors are statistically significant
# As per adjr2 : zn, indus, nox, dis, rad, ptratio, black, lstat , medv  (9 predictors)
# As per Cp : zn, nox, dis, rad, ptratio, black, lstat , medv  (8 predictors)
# As per BIC : rad, black, lstat  (3 predictors)

coef(regfit.full, 3)

#### Let's use forward and backward stepwise selection using subset apprraoch ###

regfit.fwd = regsubsets(crim ~ . , data = Boston, nvmax = 13, method = "forward")

regfit.bwd = regsubsets(crim ~ . , data = Boston, nvmax = 13, method = "backward")

par(mfrow = c(2, 2))
plot(regfit.fwd, scale = "r2")
plot(regfit.fwd, scale = "adjr2")
plot(regfit.fwd, scale = "Cp")
plot(regfit.fwd, scale = "bic")

# The results are identical using Forward stepwise selection

par(mfrow = c(2, 2))
plot(regfit.bwd, scale = "r2")
plot(regfit.bwd, scale = "adjr2")
plot(regfit.bwd, scale = "Cp")
plot(regfit.bwd, scale = "bic")

# Using Backward selection r2, adjr2 and Cp are identical. But BIC gives a different result of best 4 predictor model
# zn, dis, rad, medv  ( which is quite interesting, we'll investigate it further)

#### Now, perform Best Subset selection and choose the best model using Validation set and Cross-validation approach (directly estimate test MSE)###

### Best Subset selection and estimating test MSE using Validation set approach

set.seed(1)

train = sample(c(TRUE, FALSE), nrow(Boston), rep = TRUE)
test = (!train)

regfit.train = regsubsets(crim ~ . , data = Boston[train,], nvmax = 13)

test.mat = model.matrix(crim ~ . , data = Boston[test,])

val.errors = rep(NA, 13)  # validation set errors

for(i in 1:13)
{
	coefi = coef(regfit.train, id = i)
	pred = test.mat[, names(coefi)]%*% coefi
	val.errors[i] = mean((crim[test] - pred)^2)
}

val.errors
which.min(val.errors)
plot(val.errors, ylab = "RSS", type = "l")
points(which.min(val.errors), val.errors[which.min(val.errors)], col = "red", cex = 2, pch = 20)

# The best subset model with validation set approach is 2 predictor models : rad lstat

# Now lets do it for the entire data set to get a better estimate of the coeficients

regfit.best = regsubsets(crim ~ . , data = Boston, nvmax = 13)
coef(regfit.best, which.min(val.errors))

(Intercept)         rad       lstat 
 -4.3814053   0.5228128   0.2372846 


### Best Subset selection and estimating test MSE using k - fold cross validation approach

k = 10
set.seed(2)
folds = sample(1:k, nrow(Boston), replace = TRUE)
cv.errors = matrix(NA, k, 13, dimnames = list(NULL, paste(1:13)))

# the elements of folds that equal j are in the test set
for(j in 1:k)
{
	train.fit = regsubsets(crim ~ . , data = Boston[folds != j,], nvmax = 13)
	for(i in 1:13)
	{
		pred = predict(train.fit, Boston[folds == j,], id = i)
		cv.errors[j, i] = mean((crim[folds == j] - pred)^2)
	}	
}

# This is a 10 x 13 matrix, of which the (i, j) th element corresponds to the test MSE for the ith cross-validation fold for the best j-th model.

mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors

par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")
which.min(mean.cv.errors)

# Although the minimum value is 12 where MSE is 42.68571
# The value of index = 9 where MSE is 42.72009 which is very close.
# So, we will take the optimum model to be 9 variable model

# Let's run on the entire data set with this model specifiction and find the estimated coefficients.

reg.best = regsubsets(crim ~ . , data = Boston, nvmax = 9)
coef(reg.best, 9)
#Intercept)            zn         indus           nox           dis 
#19.124636156   0.042788127  -0.099385948 -10.466490364  -1.002597606 
#         rad       ptratio         black         lstat          medv 
# 0.539503547  -0.270835584  -0.008003761   0.117805932  -0.180593877 


## This is an interesting result and it also coincides with the AdjR2 result for estimating the test MSE.
## We'll move on and do other genere of model fits.

### Lets's do a ridge-regression on this data set 

set.seed(1)

train.mat = model.matrix(crim ~ . ,data = Boston[train,])
test.mat = model.matrix(crim ~ . , data = Boston[test,])
crim.train = Boston[train,]$crim
crim.test = Boston[test,]$crim

grid = 10^seq(10, -2, length = 100)

ridge.mod = cv.glmnet(train.mat, crim.train, alpha = 0, lambda = grid, thresh = 1e-12)
plot(ridge.mod)
bestlam = ridge.mod$lambda.min
bestlam

ridge.pred = predict(ridge.mod, s = bestlam, newx = test.mat)
mean((crim.test - ridge.pred)^2)

x.mat = model.matrix(crim ~ . , data = Boston)
y.mat = Boston$crim

ridge.best = glmnet(x.mat, y.mat, alpha = 0, lambda = bestlam)
coef(ridge.best)

#(Intercept)  9.455718029
#(Intercept)  .          
#zn           0.033587825
#indus       -0.082350022
#chas        -0.737483117
#nox         -5.640774955
#rm           0.338830981
#age          0.001957069
#dis         -0.716746627
#rad          0.430595489
#tax          0.003119667
#ptratio     -0.142911948
#black       -0.008440088
#lstat        0.142079579
#medv        -0.142270339

### Let's do a Lasso 

lasso.mod = cv.glmnet(train.mat, crim.train, alpha = 1, lambda = grid, thresh = 1e-12)
plot(lasso.mod)
bestlam = lasso.mod$lambda.min
bestlam

lasso.pred = predict(lasso.mod, s = bestlam, newx = test.mat)
mean((crim.test - lasso.pred)^2)

lasso.best = glmnet(x.mat, y.mat, alpha = 1, lambda = bestlam)
coef(lasso.best)

#(Intercept)  0.983690160
#(Intercept)  .          
#zn           .          
#indus        .          
#chas         .          
#nox          .          
#rm           .          
#age          .          
#dis         -0.035172876
#rad          0.458547206
#tax          .          
#ptratio      .          
#black       -0.005869646
#lstat        0.126149001
#medv        -0.049623596

### Now let's try Principal Component Regression 

set.seed(4)
pcr.fit = pcr(crim ~ . , data = Boston, subset = train, scale = TRUE, validation = "CV")
summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")

# The lowest cross validation error occurs when M is about 8.
# Let's compute the test MSE.

pcr.pred = predict(pcr.fit, Boston[test,], ncomp = 8)
mean((pcr.pred - crim.test)^2)

pcr.best = pcr(crim ~ . , data = Boston, scale = TRUE, ncomp = 8)
summary(pcr.fit)


