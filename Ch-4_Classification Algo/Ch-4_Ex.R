#################################### Q10 ###############################################

install.packages("ISLR")

library(ISLR)
library(MASS)
library(class)

# dataset containing 1,089 weekly returns for 21 years from the begining of 1990 to the end of 2010
fix(Weekly)
names(Weekly)
dim(Weekly)
attach(Weekly)

summary(Weekly)
cor(Weekly[,-9])
plot(Volume)
pairs(Weekly)

# There is a tight correlation b/w years and volume which do make sense. Thus there is only relation b/w Year & Volume.

glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(glm.fit)

# Lag2 is statistically significant.

contrasts(Direction)
glm.probs = predict(glm.fit, type = "response")
glm.probs[1:5]

glm.pred = rep("Down", nrow(Weekly))
glm.pred[glm.probs > 0.5] = "Up"
glm.pred[1:5]

table(glm.pred, Direction)
mean(glm.pred != Direction)

# Overall error rate of the model is 43.89 % which is better than random guessing with prob of 50 %
# The logistic regression is right 92 % of the days when goes go "Up", which is awesome.
# But it did poorly when market came down with a prediction probability of 11.11 %

train = (Year < 2009)
dim(Weekly[train,])

Weekly.2009_10 = Weekly[!train,]
dim(Weekly.2009_10)

Direction.2009_10 = Direction[!train]

glm.fit = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
summary(glm.fit)

glm.probs = predict(glm.fit, Weekly.2009_10, type = "response")

glm.pred = rep("Down", nrow(Weekly.2009_10))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2009_10)
mean(glm.pred != Direction.2009_10)

# Now the test set error rate is 37.5 % which is better.
# The logistic regression is right 92 % of the days when goes go "Up", which is awesome.
# But it did poorly when market came down with a prediction probability of 20.9 % although better than the previous step.


lda.fit = lda(Direction ~ Lag2, data = Weekly, subset = train)
lda.fit
plot(lda.fit)

lda.pred = predict(lda.fit, Weekly.2009_10)
names(lda.pred)
lda.class = lda.pred$class
lda.posterior = lda.pred$posterior
lda.x = lda.pred$x
table(lda.class, Direction.2009_10)
mean(lda.class != Direction.2009_10)

# The test error rate is 37.5 % which is same as Logistic regression model. But it did badly in predicting market going down.

qda.fit = qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.fit

qda.pred = predict(qda.fit, Weekly.2009_10)
qda.class = qda.pred$class
table(qda.class, Direction.2009_10)
mean(qda.class != Direction.2009_10)

# The test error rate is 42 % which is lower than the LDA and Logistic model.
# It got 100% correct in predicting the market going "Up"
# But it missed 100% of the time when market went down. This is truely extreme.


train.X = as.matrix(Lag2[train])
test.X = as.matrix(Lag2[!train])
train.Direction = Direction[train]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2009_10)
mean(knn.pred != Direction.2009_10)

# The test error rate is 50%
# The correct prediction of market going up and down is also about 50%.

# So, the lda and logistic regression did well compared to other models.


######################################### Q-11 ###################################################

# To predict whether a given car gets high or low gas mileage

names(Auto)
fix(Auto)
summary(Auto)
pairs(Auto)
dim(Auto)
attach(Auto)

Auto.median = median(mpg)
mpg01 = rep(0, nrow(Auto))
mpg01[mpg > Auto.median] = 1
Auto = data.frame(Auto, mpg01)

cor(Auto[,-9])
pairs(Auto)
boxplot(cylinders ~ mpg01)

# mpg is related with cylinders, displacement, horsepower, weight, acceleration and year

train = (year %% 2 == 0) # if the year is even
test = !train
Auto.train = Auto[train,]
Auto.test = Auto[!train,]
mpg01.test = mpg01[test]

lda.fit = lda(mpg01 ~ cylinders + displacement + horsepower + weight + acceleration + year, data = Auto, subset = train)
lda.fit

lda.pred = predict(lda.fit, Auto.test)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class, mpg01.test)
mean(lda.class != mpg01.test)

# The LDA test error rate is 10.43 %

qda.fit = qda(mpg01 ~ cylinders + displacement + horsepower + weight + acceleration + year, data = Auto, subset = train)
qda.fit

qda.pred = predict(qda.fit, Auto.test)
qda.class = qda.pred$class
table(qda.class, mpg01.test)
mean(qda.class != mpg01.test)

# The QDA test error rate is 13.18 %

glm.fit = glm(mpg01 ~ cylinders + displacement + horsepower + weight + acceleration + year, data = Auto, family = binomial, subset = train)

glm.probs = predict(glm.fit, Auto.test, type = "response")

glm.pred = rep(0, nrow(Auto.test))
glm.pred[(glm.probs > 0.5)] = 1
table(glm.pred, mpg01.test)
mean(glm.pred != mpg01.test)

# The Logistic model test error rate is 11 %

train.X = cbind(cylinders, displacement, horsepower, weight, acceleration, year)[train,]
test.X = cbind(cylinders, displacement, horsepower, weight, acceleration, year)[test,]
mpg01.train = mpg01[train]

set.seed(1)
knn.pred = knn(train.X, test.X, mpg01.train, k=1)
table(knn.pred, mpg01.test)
mean(knn.pred != mpg01.test)

# Using knn(), with k=1 the test error rate is 15.38 %

knn.pred = knn(train.X, test.X, mpg01.train, k=10)
table(knn.pred, mpg01.test)
mean(knn.pred != mpg01.test)

# Using knn(), with k=10 the test error rate is 16.48 %

knn.pred = knn(train.X, test.X, mpg01.train, k=100)
table(knn.pred, mpg01.test)
mean(knn.pred != mpg01.test)

# Using knn(), with k=100 the test error rate is 14.28 %, with 100 nearest neighbors

###################################### Q12 #####################################################

Power = function()
{
	2^3
}
print(Power())

Power2 = function(x, a)
{
	x^a
}
print(Power2(3,2))
print(Power2(10,3))

Power3 = function(x, a)
{
	result = x^a
	return(result)
}

x = 1:10
plot(x, Power3(x, 2), xlab = "Values of x", ylab = "Values of f(x)", main = "Function plot")

plot(x, Power3(x, 2), log="xy", xlab = "Log of x", ylab = "Log of y", main = "Function plot") # using log for x & y

plot(x, Power3(x, 2), log="x", xlab = "Log of x", ylab = "y", main = "Function plot")

plot(x, Power3(x, 2), log="y", xlab = "x", ylab = "Log of y", main = "Function plot")

Plotpower = function(x, a)
{
	plot(x, Power3(x, a))
}
Plotpower(1:10, 3)

############################################## Q13 ##############################################

names(Boston)
summary(Boston)
fix(Boston)
attach(Boston)

crim01 = rep(0, length(crim))
crim01[(crim > median(crim))] = 1
Boston = data.frame(Boston, crim01)

train = 1:(dim(Boston)[1]/2)
test = (dim(Boston)[1]/2 + 1):dim(Boston)[1]

# To check the distribution of crim01 in both train and test data set.

sum(Boston[train,][15] == 1)
sum(Boston[train,][15] == 0)
sum(Boston[test,][15] == 1)
sum(Boston[test,][15] == 0)

Boston.train = Boston[train,]
Boston.test = Boston[test,]
crim01.test = crim01[test]

### Logistic regression ###

glm.fit = glm(crim01 ~ . -crim, data = Boston, family = binomial, subset = train)
summary(glm.fit)
contrasts(as.factor(crim01))

glm.probs = predict(glm.fit, Boston.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
table(glm.pred, crim01.test)
mean(glm.pred != crim01.test)

# So, the test error rate is only 18.2%

### LDA ###

lda.fit = lda(crim01 ~ . -crim, data = Boston, subset = train)
lda.fit

lda.pred = predict(lda.fit, Boston.test)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class, crim01.test)
mean(lda.class != crim01.test)

# So, the test error rate is only 13.4%

### KNN() ###

train.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv)[train,]
test.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv)[test,]
train.crim01 = crim01[train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.crim01, k=1)
table(knn.pred, crim01.test)
mean(knn.pred != crim01.test)

# The test error rate is 45.8 % with k=1 and that is poor.

knn.pred = knn(train.X, test.X, train.crim01, k=10)
mean(knn.pred != crim01.test)

# The test error rate is now better 11.5% with k=10






