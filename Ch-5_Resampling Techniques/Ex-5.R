############################################# Ch-5 ##################################################

#################### Q. 5 #####################

install.packages("ISLR")

library(ISLR)

fix(Default)
dim(Default)
names(Default)
attach(Default)
summary(Default)
pairs(Default)

set.seed(1)
glm.fit = glm(default ~ balance + income, data = Default, family = binomial)
summary(glm.fit)

FiveB = function()
{
	train = sample(dim(Default)[1], dim(Default)[1]/2)
	test = Default[-train,]
	Default.test = test$default
	glm.fit = glm(default ~ balance + income, data = Default, subset = train, family = binomial)
	glm.probs = predict(glm.fit, test, type = "response")
	glm.pred = rep('No', nrow(test))
	glm.pred[glm.probs > 0.5] = "Yes"
	return(mean(glm.pred != Default.test))
}
FiveB()

# The test error rate from the validation set is 2.68 %
# Invoke the function for few times i.e. run the validation set approach on various samples and thus got the average test error rate as 2.6 %

FiveD = function()
{
	train = sample(dim(Default)[1], dim(Default)[1]/2)
	test = Default[-train,]
	Default.test = test$default
	glm.fit = glm(default ~ income + balance + student, data = Default, subset = train, family = binomial)
	glm.probs = predict(glm.fit, test, type = "response")
	glm.pred = rep('No', nrow(test))
	glm.pred[glm.probs > 0.5] = "Yes"
	return(mean(glm.pred != Default.test))
}
FiveD()

# The test error rate fom the validation set is also 2.6 % even after adding dummy variable student in the regression.

############################### Q. 6 ##################################

library(boot)
set.seed(1)

glm.fit = glm(default ~ income + balance, data = Default, family = binomial)
summary(glm.fit)

# The SE(income) is 4.985e-06 and SE(balance) is 2.274e-04

boot.fn = function(data, index)
{
	glm.fit = glm(default ~ income + balance, data = data, family = binomial, subset = index)
	return(coef(glm.fit))
}
boot.fn(Default, dim(Default)[1])

# The coefs are exactly same as they are essentially the same thing.

boot(Default, boot.fn, 50)

# Using bootstrap, SE(income) is 4.861065e-06 and SE(balance) is 2.392097e-04 which are similar to the earlier result.

###################################### Q. 7 #########################################

summary(Weekly)

glm.fit = glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial)
summary(glm.fit)

glm.fit = glm(Direction ~ Lag1 + Lag2, data = Weekly[-1,], family = binomial)
summary(glm.fit)

glm.probs = predict(glm.fit, Weekly[1,], type = "response") # this is LOOCV
glm.pred = "Down"
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Weekly[1,]$Direction)
mean(glm.pred != Weekly[1,]$Direction)

# The prediction is "Up" but true was "Down", so that test error rate is 100 %

count = rep(0, dim(Weekly)[1])

for (i in 1:dim(Weekly)[1])
{
	glm.fit = glm(Direction ~ Lag1 + Lag2, data = Weekly[-i,], family = binomial)
	glm.probs = predict(glm.fit, Weekly[i,], type = "response")
	glm.pred[glm.probs > 0.5] = "Up"
	if (glm.pred != Weekly[i,]$Direction)
		count[i] = 1
}
sum(count) 

# The total number of wrong predictions are 484

mean(count)

# So, the test error rate is 44.44 %

##################################### Q. 8 ##########################################

set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - 2*x^2 + rnorm(100)

# Here, n = 100 and p = 2  i.e. y = x - 2*x^2 + e

plot(x, y)  # quadratic plot, x from -2 to 2 and y from -8 to -2

library(boot)

Data = data.frame(x, y)  # single data set containing x and y
set.seed(1)

for (i in 1:4)
{
	glm.fit = glm(y ~ poly(x, i), data = Data)  # Note this is a linear model
	cv.error[i] = cv.glm(Data, glm.fit)$delta[1]
}
cv.error

set.seed(10)

for (i in 1:4)
{
	glm.fit = glm(y ~ poly(x, i), data = Data)  # Note this is a linear model
	cv.error[i] = cv.glm(Data, glm.fit)$delta[1]
}
cv.error

# The quadratic polynomial has the lowest test error rate i.e. 1.08

summary(glm.fit)

# The same thing is seen when you see the p-value for the quadratic predictor, which is <2e-16, that makes it statistically significant.

################################## Q. 9 ################################################

library(MASS)
names(Boston)
attach(Boston)
summary(Boston)

set.seed(1)
medv.mean = mean(medv)
medv.mean

# The estimate for population mean of medv is 22.53

medv.err = sd(medv)/sqrt(length(medv))
medv.err

# The estimate of standard error is 0.41

boot.fn = function(data, index)
{
	return(mean(data[index]))
}
bstrap = boot(medv, boot.fn, 1000)
bstrap

# The standard error using bootstrap is also 0.41

t.test(medv)

# 95 percent confidence interval: 21.72953 23.33608

c(bstrap$t0 - 2*0.4119, bstrap$t0 + 2 * 0.4119)

# 21.70901 23.35661
