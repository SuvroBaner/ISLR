################################ Ch - 7 (Moving beyond linearity #########################

install.packages("ISLR")
install.packages("gam")
install.packages("akima")

library("ISLR")
library("splines")
library("gam")
library("akima")


attach(Wage)
fix(Wage)
dim(Wage)
names(Wage)

################## Polynomial Regression and Step Functions ######################

fit = lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))

# It allows to predict Wage using a 4th-degree polynomial in age. It returns a matrix whose columns are a basis of 
# orthogonal polynomials, which essentially means that each column is a linear combination of the variables 
# age, age^2, age^3 and age^4

fit2 = lm(wage ~ poly(age, 4, raw = TRUE), data = Wage)
coef(summary(fit2))

# Using raw = TRUE returns these variables (age, age^2, age^3 and age^4)directly and not a linear combination.

fit2a = lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage)
coef(summary(fit2a))  # the coefs are similar to 'fit2'

# Yet another way of expression.

fit2b = lm(wage ~ cbind(age, age^2, age^3, age^4), data = Wage)
coef(summary(fit2b))

# We now create a grid of values for age at which we want predictions, and then call the generic predict() specifying that we want the standard deviation as well.

agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
preds = predict(fit, newdata = list(age = age.grid), se = TRUE)
names(preds)
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)  # 95 % confidence interval

preds2 = predict(fit2, newdata = list(age = age.grid), se = TRUE)
se.bands2 = cbind(preds2$fit + 2*preds2$se.fit, preds2$fit - 2*preds2$se.fit)


# Now we plot the data and add the fit from the degree-4 polynomial

par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0)) # mar and oma help us to control the margins of the plot
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree-4 polynomial", outer = T)

lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
lines(age.grid, preds2$fit, lwd = 2, col = "green")
matlines(age.grid, se.bands, lwd = 1, col = "green", lty = 3)

# Note : The first prediction was based on the orthogonal parameters estimate and the second one was RAW.
# But despite two different representations, the prediction is same for the both.

# We now fit models ranging from linear to a degree-5 polynomial and seek to determine the simplest model which is sufficient 
# to explain the relationship between wage and age.
# We, use anova() in order to test the null hypothesis that a model M1 is sufficient to explain the data against
# the altrenative hypothesis that a more complex model M2 is required.

# In order to use ANOVA, M1 and M2 must be a nestely compare their simplier model to the more complex model.
# model, the predictors in M1 must be a subset of predictors M2.

# In this case, we fit five different models and sequential
fit.1 = lm(wage ~ age, data = Wage)
fit.2 = lm(wage ~ poly(age, 2), data =  Wage)
fit.3 = lm(wage ~ poly(age, 3), data = Wage)
fit.4 = lm(wage ~ poly(age, 4), data = Wage)
fit.5 = lm(wage ~ poly(age, 5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# From the p-value of F-stats a quadratic or cubic fit is a resonable fit to the data.

# Let's do the same thing using the lm() p-values where all these terms are present.
coef(summary(fit.5)) # Note the f-value is the square of t-values.

### As an alternative to Hypothesis test and ANOVA, we could choose the polynomial degree using cross-validation ###

install.packages("boot")
library(boot)

cv.errors = rep(0, 5)

for (i in 1:5)
{
  glm.fit = glm(wage ~ poly(age, i), data = Wage)
  cv.errors[i] = cv.glm(Wage, glm.fit, K = 10)$delta[2]
}

print(cv.errors)

# 1676.300 1600.564 1595.911 1594.306 1594.616  # so, degree 2 or 3 would be accurate

#### Let's take a task of predicting whether an individual earns more that $ 250,000 per year.

# This is a classification problem so, we fit a polynomial logistic regression model.

fit = glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial)

# Note, we use the wrapper I() to create a binary response variable on the fly. The expression wage > 250 evaluates
# to a logical variable containing TRUEs and FALSEs, where TRUEs to 1 and FALSEs to 0
# This is done a bit differently from what we had learned in Ch-3 where we worked with the probability separately to classify.

preds = predict(fit, newdata = list(age = age.grid), se = T)

# Note, we get the predictions for the logit, which is the default predition type for glm() called type = "link"

pfit = exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit = cbind(preds$fit + 2*preds$se.fit , preds$fit - 2*preds$se.fit)
se.bands = exp(se.bands.logit) / (1 + exp(se.bands.logit))

plot(age, I(wage > 250), xlim = agelims, type = "n", ylim = c(0, .2))
points(jitter(age), I((wage > 250)/5), cex = .5, pch = "|", col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)


# We have drawn the age values corresponding to the observations with age values above 250 as gray marks on the top
# of the plot, and those with age values below 250 are shown as gray marks on the bottom of the plot.
# We use the jitter() function to jitter the age values a bit so that observations with the same age value do not cover each other up.
# This is called a rug plot.

####################### Let's now fit a step function. #######################
table(cut(age, 4))

#(17.9,33.5]   (33.5,49]   (49,64.5] (64.5,80.1] 
#        750        1399         779          72

# Here cut() automatically picked the cutpoints at 33.5, 49 and 64.5 years of age.
# They are the ordered categorical variable. lm() creates a set of dummy variables for use in the regression.

fit = lm(wage ~ cut(age, 4), data = Wage)
coef(summary(fit))

#                        Estimate Std. Error   t value     Pr(>|t|)
#(Intercept)            94.158392   1.476069 63.789970 0.000000e+00
#cut(age, 4)(33.5,49]   24.053491   1.829431 13.148074 1.982315e-38
#cut(age, 4)(49,64.5]   23.664559   2.067958 11.443444 1.040750e-29
#cut(age, 4)(64.5,80.1]  7.640592   4.987424  1.531972 1.256350e-01

# The age<33.5 is left out, so the intercept coefficient of $94,160 can be interpreted as the average salary for those under 35 years of age.
# The other coefficients can be interpreted as the average additional salary for those in the other age group.

####################################### Splines ############################

fit = lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)

# The bs() generates the entire matrix of basis functions for splines with the specified set of knots.
# By default, cubic splines are produced.

# Here we have prespecified knots at ages 25, 40 and 60. This produces a spline with six basis functions.
# A cubic spline with 3 knots has 7 degrees of freedom; these degrees of freedom are used up by an intercept,
# plus six basis functions.

dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df=6))
attr(bs(age, df=6), "knots")

# In this case R chooses knots at ages 33.8, 42 and 51 which corresponds to 25th , 50th and 75th percentiles of age.

## Let's now fit the natural spline, with 4 degrees of freedom.

fit2 = lm(wage ~ ns(age, df = 4), data = Wage)

attr(ns(age, df = 4), "knots")  # R creates knots based on the degrees of freedom as below
# 25%   50%   75% 
# 33.75 42.00 51.00


pred2 = predict(fit2, newdata = list(age = age.grid), se = T)

plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
lines(age.grid, pred2$fit, col = "red", lwd = 2)
title("Smoothing Spline")

fit = smooth.spline(age, wage, df = 16)
fit2 = smooth.spline(age, wage, cv = TRUE)
fit2$df

lines(fit, col = "red", lwd = 2)
lines("topright", legend = c("16 DF", "6.8 DF"), col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

# To, perform local regression.

plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression")
fit = loess(wage ~ age, span = .2, data = Wage)
fit2 = loess(wage ~ age, span = .5, data = Wage)
lines(age.grid, predict(fit, data.frame(age = age.grid)), col = "red", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"), col = c("red", "blue"), lty = 1, lwd = 2, cex = 0.8)


####################### General Additive Model (GAM) ##########################

# We now fit a GAM to predict wage using natural spline functions of year and age, treating education as a qualitative predictor.

gam1 = lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)  # df = 4 etc.

# Let us do it using a smoothing splines, we'll use the gam library in R.
# The s(), is used to indicate that we would like to use a smoothing spline.

gam.m3 = gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
par(mfrow = c(1, 3))
plot(gam.m3, se = TRUE, col = "blue")

plot.gam(gam1, se = TRUE, col = "red")

gam.m1 = gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 = gam(wage ~ year + s(age, 5) + education, data = Wage)
anova(gam.m1, gam.m2, gam.m3, test = "F")

# There is a compelling evidence that a GAM with a linear function of year is better than a GAM that does not
# include year at all. The p-value is 0.0001447
# However, there is no evidence that a non-linear function of year is needed (p-value : 0.3485661)
# Based on the result of this ANOVA, M2 is preferred.

summary(gam.m3)

# The p-values of year and age correspond to a null hypothesis of a linear relationship v/s the alternative relationship.
# The large p-value for year, reinforces our conclusion from the ANOVA test that a linear function is adequate for this term.
# However, there is a very clear evidence that a non-linear term is required for age.


preds = predict(gam.m2, newdata = Wage)
gam.lo.i = gam(wage ~ lo(year, age, span = 0.5) + education, data = Wage)  # lo() creates an interaction.

plot(gam.lo.i)

# To fit a logistic GAM.

gam.lr = gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage)
par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = "green")

# It is easy to see that there are no high earners in the <HS category.

table(education, I(wage > 250))

# Hence we fit the logistic regression GAM using all but this category. This provides more sensible results.

gam.lr.s = gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage, subset = (education != "1. <HS Grad"))
plot(gam.lr.s, se = T, col = "green")




