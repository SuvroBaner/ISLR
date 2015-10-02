install.packages("ISLR")
library(ISLR)


names(Smarket)  # data set included in the ISLR package.

## The data set consists of percentage return for the S&P 500 stock index over 1,250 days from the beginning
## of 2001 until end of 2005. For each date, we have recorded the percentage returns for each of the five 
## previous trading days, Lag1 through Lag5.
## We have also recorded Volume (the number of shares traded on the previous day, in billions)
## Today ( the percentage return of the date in question)
## Direction (whether the market was up or down on this date).

fix(Smarket)
dim(Smarket)
summary(Smarket)

cor(Smarket)  # gives an error as "Direction" is not numeric

cor(Smarket[,-9]) # taking out the 9th variable which is the Direction from the list

## This gives the pairwise correlations among the predictors in the data set
## There appears to be little correlation between today's returns and previous days' returns
## This is being seen from the correlations between the lag variables and today's returns which are close to 0
## The only substantial correlation is between Year and Volume, Lets plot and see the relation they have.

attach(Smarket)
plot(Volume)

######################################### Logistic Regression #########################################

## Lets fit a logistic regression model in order to predict "Direction" using "Lag1" through "Lag5" and "Volume"
## The glm() function fits the generalized linear models, a class of models which includes Logistic Regression.
## "family = binomial" specifies R to run a logistic regression.

glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fit)

## The lowest p-value is for Lag1 although it is still quite high compared to 95 % CI. The value is 0.145
## The negative coefficient for this predictor suggests that if the market had a positive return yesterday
## then it is less likely to go up today. However we reject this theory as the coef is not statistically significant.

coef(glm.fit)  # to get the coeffcients the fitted model.
summary(glm.fit)$coef  # the coef and other details in a matrix format
summary(glm.fit)$coef[,4]  # all rows and the 4th column from the matrix, i.e. the p-values

## The predict() can be used to predict the probability that the market will go up, given values of the predictors
## The type = "response" option tells R to output probabilities of the form Pr(Y=1|X),as opposed to the other information such as logit

contrasts(Direction) # R has created a dummy variable with a 1 for "Up" and 0 for "Down"

## So, the values of probabilities obtained below are for the market going "Up" which is Pr(Y=1|X)

glm.probs = predict(glm.fit, type = "response")
glm.probs[1:5]  # printing the first 5 probabilities i.e. the 1st 5 trading days of 2001 for the direction of the market to go up on that specific day

## Let's convert these predicted probabilities into class labels, Up or Down

glm.pred = rep("Down", 1250)  # creates a vector of 1250 Down elements
glm.pred[glm.probs > 0.5] = "Up"  # makes "Up" for the prob which is above 0.5 or 50 %
glm.pred[1:5] # shows the predictions for the first 5 trading days.

table(glm.pred, Direction) # This creates a confusion matrix, note "Direction" is the true value.

## Given the estimated Directions using the logistic model the above function prints a confusion matrix
## in order to determine how many observations were correctly or incorrectly classified.

## The diagonal elements of the confusion matrix indicate the correct predictions, 
## while the off-diagonals represent the incorrect predictions.

## Hence our model correctly predicted that the market would go up on 507 days and that it would go down 
## on 145 days. So, total of 652 correct predictions.

(145+507)/1250  # 0.5216, i.e. Logistic regression correctly predicted the movement of the market 52.2 % of the time.
mean(glm.pred == Direction)  # This is used to calculate the fraction of days for which the prediction was correct.

## Here the training error rate is 100 - 52.2 % = 47.8 %. It might underestimate the test error rate.

## In order to better assess the accuracy of the logistic regression model in this setting, we can fit the model
## using part of the data, and then examine how well it predicts the held out data.
## This will yield a more realistic error rate, in the sense that in practice we'll be interested in our 
## model's performance not on the data that we used to fit the model, but rather on the days in the future
## for which the market's movements are unknown.

## We'll create a vector corresponding to the observations from 2001 through 2004.
## We'll then use this vector to create a held out data set of observations from 2005.

train = (Year < 2005)  # the object train is the vector of 1,250 elements and boolean based on the condition.
dim(Smarket[train,])  # submatrix of the stock market data set, corresponding only to the dates before 2005, since those have TRUE values in the train vector
Smarket.2005 = Smarket[!train,]  # submatrix where the train is FALSE i.e. for the year 2005
dim(Smarket.2005)  # only have 252 observations i.e. with dates 2005.
Direction.2005 = Direction[!train]  # real values of direction


## Now we use the logistic model for the 1st set of data which is before 2005 and predict the directions.

glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)

## Obtained predicted probabilities of the stock market going up for each days in our test set, i.e. for the year 2005.
glm.probs = predict(glm.fit, Smarket.2005, type = "response")

## Note, the training was performed using only the dates before 2005, and testing was performed using only the dates in 2005

glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(!glm.pred == Direction.2005)  # test set error rate, it is 52 %

## The test set error rate is 52 % which is even worse than plain guessing with 50 % chance.
## This is quite understandable that using previous days returns to predict future market performance is not an option.

## Lets refit the model by removing the predictors which have a very high p-values.

glm.fit = glm(Direction	~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fit, Smarket.2005, type = "response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(!glm.pred == Direction.2005)  # test set error rate is now 44 %

## Suppose we want to predict the returns associated with particular values of Lag1 and Lag2

predict(glm.fit, newdata = data.frame(Lag1 = c(1.2,1.5), Lag2 = c(1.1,-0.8)), type = "response")

################################## Linear Discriminant Analysis ######################################

library(MASS)  # lda() is a part of this library
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit

## The LDA output indicates that pihat1 = 0.492 and pihat2 = 0.508 (this is the prior probability that a randomly chosen observation comes from the kth class), in other words, 49.2 % of the training observations
## correspond to days during which the market went down. 
## It also provides the group means, these are the average of each predictor within each class, and are used by LDA as estimates of mu k.
## These suggest that there is a tendency for the previous 2 day's returns to be negative on days when the market increases, 
## and a tendency for the previous 2 day's returns to be positive on days when market declines.

## The coefficients of linear discriminants output provides the linear combination of Lag1 and Lag2 that are used to form the LDA decision rule.
## In other words, these are the multipliers of the elements of X = x in the Baye's classifier equation.
## If -0.642*Lag1 - 0.513*Lag2 is large, then the LDA classifier will predict a market increase, and if small, then the LDA classifier will predict a market decline.

plot(lda.fit) # It produces plots of the linear discriminants obtained by computing -0.642*Lag1 - 0.513*Lag2 for each of the training observations.

## The predict() returns a list with three elements. The first element, class, contains LDA's predictions about the movement of the market.
## The second element, posterior, is a matrix whose kth column contains the posterior probability that the corresponding observation belongs to the kth class.
## X contains the linear discriminants

lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class == Direction.2005)  # LDA and logistic regression predictions are almost identical.

## Applying 50 % threshold to the posterior probabilities allows us to recreate  the predictions contained in lda.pred$class

sum(lda.pred$posterior[,1] >= .5)
sum(lda.pred$posterior[,1] < 0.5)

## Notice that the posterior probability output by the model corresponds to the probability that the market will decrease

lda.pred$posterior[1:20,1]
lda.class[1:20]

## If we wanted to use a posterior probability threshold other than 50 % in order to make predictions, then we could easily do so.
## For instance, suppose that we wish to predict a market decrease only if we are very certain that the market will indeed decrease 
## on that day - say, is the posterior probability is at least 90%

sum(lda.pred$posterior[,1] > .9)
# No days in 2005 meet that threshold! In fact, the greatest posterior probability of decrease in all of 2005 was 52.02 %

######################################## Quadratic Discriminant Analysis ###########################

qda.fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit

qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

### So, qda predictions are accurate 60% of the time, even though 2005 data was not used to fit the model. This level of accuracy is quite impressive for the stock market data.
## This suggests that the quadratic form assumed by QDA may capture the true relationship more accurately than the linear forms assumed by LDA and Logistic regression.

######################################## K-nearest neighbors #####################################

## knn() function is part of the Class library. The function requires 4 inputs-
# a) A matrix containing the predictors associated with the training data, labeled train.X below
# b) A matrix containing the predictors associated with the data for which we wish to make predictions, labeled test.X below.
# c) A vector containing the class labels for the training observations, labeled train.Direction
# d) A value of K, the number of nearest neighbor to be used by the classifier.

library(class)
train.X = cbind(Lag1, Lag2)[train,]  # cbind stands for column bind, which binds Lag1 and Lag2 variables together.
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]

## Now, knn() can be used to predict the market's movement for the dates in 2005.
##  We set a random seed before we apply knn() because if several observations are tied as nearest neighbors, then R will randomly break tie.
## Therefore, a seed must be set in order to ensure reproducibility of the results.

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

## The result using K=1 are not very good since only 50% of the observations are correctly predicted.
## Let's repeat with k=3.

knn.pred = knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

## The results have improved slightly. But increasing k further would not improve the model fit.
## It is clear that the QDA analysis is the best model fit for the data.
