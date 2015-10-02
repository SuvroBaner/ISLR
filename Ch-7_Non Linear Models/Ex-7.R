################################ Ex - 7 ###############################################

####################### Q.6 ########################

install.packages("ISLR")
install.packages("boot")
install.packages("gam")
install.packages("glmnet")

library("ISLR")
library("boot")
library("gam")
library("glmnet")
library("splines")
library("MASS")

# Perform polynomial regression to predict wage using age. Use cross-validation to select the optimal degree d
# for the polynomial. What degree was chosen, and how does this compare to the results of hypothesis testing using ANOVA
# Make a plot of the resulting polynomial fit to the data.

# Load Wage dataset. Keep an array of all cros-validation errors. We are performing k-fold cross validation with k = 10

set.seed(1)
attach(Wage)

all.deltas = rep(NA, 10)

for(i in 1:10)
{
	glm.fit = glm(wage ~ poly(age, i), data = Wage)
	all.deltas[i] = cv.glm(Wage, glm.fit, K = 10)$delta[2]
}

plot(1:10, all.deltas, xlab = "Degrees", ylab = "CV.Error", type = "l", pch = 20, lwd = 2, ylim = c(1590, 1700))
min.point = min(all.deltas)
sd.points = sd(all.deltas)
abline(h = min.point + 0.2 * sd.points, col = "red", lty = "dashed")
abline(h = min.point - 0.2 * sd.points, col = "red", lty = "dashed" )
legend("topright", "0.2-standard deviation lines", lty = "dashed", col = "red")

## The cv-plot with standard deviation lines show that d = 3 is the smallest degree giving reasonably small cross-validation error

# We now find the best degree using ANOVA

fit.1 = lm(wage ~ poly(age, 1), data = Wage)
fit.2 = lm(wage ~ poly(age, 2), data = Wage)
fit.3 = lm(wage ~ poly(age, 3), data = Wage)
fit.4 = lm(wage ~ poly(age, 4), data = Wage)
fit.5 = lm(wage ~ poly(age, 5), data = Wage)
fit.6 = lm(wage ~ poly(age, 6), data = Wage)
fit.7 = lm(wage ~ poly(age, 7), data = Wage)
fit.8 = lm(wage ~ poly(age, 8), data = Wage)
fit.9 = lm(wage ~ poly(age, 9), data = Wage)
fit.10 = lm(wage ~ poly(age, 10), data = Wage)

anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7, fit.8, fit.9, fit.10)

# We see that all polynomials above degree 3 are statistically insignificant

# We now plot the polynomial prediction on the data.

plot(wage ~ age, data = Wage, col = "darkgrey")
agelims = range(Wage$age)
age.grid = seq(from = agelims[1], to = agelims[2])
lm.fit = lm(wage ~ poly(age, 3), data = Wage)
lm.pred = predict(lm.fit, data.frame(age = age.grid))
lines(age.grid, lm.pred, col = "blue", lwd = 2)

# Fit a step function to predict wage using age, and perform cross-validation to choose the optimal number of cuts
# Make a plot of the fit obtained.


# We use cut points up to 10

all.cvs = rep(NA, 10)
for(i in 2:10)
{
	Wage$age.cut = cut(Wage$age, i)
	lm.fit = glm(wage ~ age.cut, data = Wage)
	all.cvs[i] = cv.glm(Wage, lm.fit, K = 10)$delta[2]
}
plot(2:10, all.cvs[-1], xlab = "Number of cuts", ylab = "CV error", type = "l", pch = 20, lwd = 2)

# The cross validation shows that test error is minimum for k = 8 cuts.

# We now train the entire data with step function using 8 cuts and plot it.

lm.fit = glm(wage ~ cut(age, 8), data = Wage)
agelims = range(Wage$age)
age.grid = seq(from = agelims[1], to = agelims[2])
lm.pred = predict(lm.fit, data.frame(age = age.grid))
plot(wage ~ age, data = Wage, col = "darkgrey")
lines(age.grid, lm.pred , col = "red", lwd = 2)

############################# Q.7 ###########################################
# The wage dat set contains a number of other features not explored in this chapter, such as marital status (maritl),
# job class (jobclass), and others. Explore the relationships between some of these other predictors and wage, and use
# non-linear fitting techniques in order to fit flexible models to the data. Create plots of the results obtained,
# and write a summary of your finding.

set.seed(1)
summary(Wage$maritl)
summary(Wage$jobclass)

par(mfrow = c(1, 2))
plot(Wage$maritl, Wage$wage)
plot(Wage$jobclass, Wage$wage)

# It appears a married couple makes more money on average than other groups.
# I also appears that informational jobs are higher-wage than industrial jobs on average.

### Polynomial Step functions ###

fit = lm(wage ~ maritl,  data = Wage)
deviance(fit)   # 4858941

fit = lm(wage ~ jobclass, data = Wage)
deviance(fit)   # 4998547

fit = lm(wage ~ maritl + jobclass, data = Wage)
deviance(fit)   # 4654752

###### Unable to fit Splines on categorical variables 

### GAMs ###

fit = gam(wage ~ maritl + jobclass + s(age, 4), data = Wage)
deviance(fit)  # 4476501

### maritl and jobclass do add atatistically significant improvements to the previously discussed models.

################### Q.8 #################################

# Fit some of the non-linear models investigated in this chapter, to the Auto data set. Is there evidence for
# non-linear relationships in this data set. Create some informative plots to justify your answer.

set.seed(1)
pairs(Auto)
names(Auto)

# mpg appears inversly proportional to cylinders, displacement, horsepower and weight.

### Polynomial ###

rss = rep(NA, 10)
fits = list()

for (d in 1:10)
{
	fits[[d]] = lm(mpg ~ poly(displacement, d), data = Auto)
	rss[d] = deviance(fits[[d]])
}
rss

# The RSS are-
#[1] 8378.822 7412.263 7392.322 7391.722 7380.838 7270.746 7089.716 6917.401
#[9] 6737.801 6610.190

anova(fits[[1]], fits[[2]], fits[[3]], fits[[4]])

# Training RSS decreases over time. Quadratic polynomic sufficient from ANOVA- perspective

cv.errs = rep(NA, 15)
for(d in 1:15)
{
	fit = glm(mpg ~ poly(displacement, d), data = Auto)
	cv.errs[d] = cv.glm(Auto, fit, K = 10)$delta[2]
}
which.min(cv.errs)  # 11

cv.errs

#[1] 21.49936 19.06198 19.09875 19.41447 19.59539 19.17719 18.79682 18.19984
#[9] 17.81664 17.74728 17.68688 17.89916 18.19771 18.02290 17.91096


# Very interestingly, cross-validation selected 10-degree polynomial.

### Step Functions ###

cv.errs = rep(NA, 10)

for(c in 2:10)
{
	Auto$dis.cut = cut(Auto$displacement, c)
	fit = glm(mpg ~ dis.cut, data = Auto)
	cv.errs[c] = cv.glm(Auto, fit, K = 10)$delta[2]
}
which.min(cv.errs)  # 11

cv.errs

# [1] 21.49936 36.40932 24.42122 24.22566 22.46100 22.00411 21.42273 19.79627
# [9] 18.15626 19.08299 17.68688 17.89916 18.19771 18.02290 17.91096

### Splines ###

cv.errs = rep(NA, 10)
for(df in 3:10)
{
	fit = glm(mpg ~ ns(displacement, df = df), data = Auto)
	cv.errs[df] = cv.glm(Auto, fit, K = 10)$delta[2]
}
which.min(cv.errs)  # 10

cv.errs

#  [1]       NA       NA 19.26921 19.37686 18.95506 18.43710 17.97605 18.02670
# [9] 17.63094 17.45331

############# GAM ################

fit = gam(mpg ~ s(displacement, 4) + s(horsepower, 4), data = Auto)
summary(fit)


######################################### Q 9 ########################################

# Using the variable dis (the weighted mean of distances to five Boston employment centers), and 
# nox (nitrogen oxides concentration in parts per 10 million) from Boston Data set.
# We'll treat dis as the predictor and nox as the response.

set.seed(2)
attach(Boston)

lm.fit = lm(nox ~ poly(dis, 3), data = Boston)
summary(lm.fit)

dislim = range(dis)
dis.grid = seq(from = dislim[1], to = dislim[2], by = 0.1)
lm.pred = predict(lm.fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, lm.pred, col = "red", lwd = 2) 

# Summary shows that all polynomial terms are significant while predicting nox using dis.
# Plot shows a smooth curve fitting the data fairly well.

### We plot the polynomials of degrees 1 to 10 and save the train RSS.

all.rss = rep(NA, 10)
for (i in 1:10)
{
	lm.fit = lm(nox ~ poly(dis, i), data = Boston)
	all.rss[i] = sum(lm.fit$residuals^2)
}
all.rss

#[1] 2.768563 2.035262 1.934107 1.932981 1.915290 1.878257 1.849484 1.835630
#[9] 1.833331 1.832171

# As expected the train RSS monotonically decreases with degree of polynomial

### We use a 10-fold cross-valuidation to pick the best polynomial degree

all.deltas = rep(NA, 10)
for(i in 1:10)
{
	glm.fit = glm(nox ~ poly(dis, i), data = Boston)
	all.deltas[i] = cv.glm(Boston, glm.fit, K = 10)$delta[2]
}

plot(1:10, all.deltas, xlab = "Degree", ylab = "CV error", type = "l", pch = 20, lwd = 2)

# A 10-fold CV shows that the CV error reduces as we increase degree from 1 to 3, stay almost constant till degree 5,
# and starts increasing for higher degrees.  We pick 4 as the best polynomial degree.

### We see that dis has limits of about 1 and 13 respectively. We split this range in roughly 4 intervals and establish
### knots at [4, 7, 11]. Note: bs() in R expects either df or knots argument. If both are specified then knots are ignored.

sp.fit = lm(nox ~ bs(dis, df = 4, knots = c(4, 7, 11)), data = Boston)
summary(sp.fit)

sp.pred = predict(sp.fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, sp.pred, col = "red", lwd = 2)

# The summary shows that all terms in the spline fit are significant. Plot shows that the spline fits data well
# except at the extreme values of dis, when dis > 10

### We firt regression splines with dfs between 3 and 16

all.cv = rep(NA, 16)
for(i in 3:16)
{
	lm.fit = lm(nox ~ bs(dis, df = i), data = Boston)
	all.cv[i] = sum(lm.fit$residuals^2)
}
all.cv[-c(1,2)]

#[1] 1.934107 1.922775 1.840173 1.833966 1.829884 1.816995 1.825653 1.792535
#[9] 1.796992 1.788999 1.782350 1.781838 1.782798 1.783546

# Train RSS monotonically decreases till df=14 and then slightly increases for df=15 and df=16.

# Finally, we use a 10-fold cross validation to find best df. We try all integer values of df between 3 and 16.

all.cv = rep(NA, 16)
for (i in 3:16) {
    lm.fit = glm(nox ~ bs(dis, df = i), data = Boston)
    all.cv[i] = cv.glm(Boston, lm.fit, K = 10)$delta[2]
}

plot(3:16, all.cv[-c(1, 2)], lwd = 2, type = "l", xlab = "df", ylab = "CV error")

# CV error is more jumpy in this case, but attains minimum at df=10. We pick 10 as the optimal degrees of freedom.
