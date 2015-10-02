x <- c(1,3,2,5) # c - concatenate, and stores a vector named x.
x # print x
y = c(9,8,7,6) # another vector with a different syntax.
y # print y
length(x) # length of x
length(y) #length of y
x+y # addition
ls() # list of all the objects saved so far
rm(x,y) # delete objects
ls() # if there is no objects ls() gives character(0)
rm(list=ls()) # remove all objects at once.
x = matrix(data=c(1,2,3,4), nrow=2, ncol=2) # create a matrix of numbers, by default it is by column
x = matrix(c(2,3,4,5),2,2) # same matrix with a different syntax
matrix(c(11,12,13,14),2,2,byrow=TRUE) # matrix creation by row, no assignment here
x = matrix(c(4,9,16,25),2,2)
x
sqrt(x) # computes square root
x = rnorm(50) # generates a vector of random normal variables, with first argument 'n' the sample size 
y = x + rnorm(50, mean = 50, sd = .1) # by default rnorm creates standard normal variables with mean = 0 and standard deviation = 1
cor(x,y) # correlation of x & y
set.seed(1303) # this helps to generate the same set of random variables. The argument is arbitrary.
rnorm(50)
set.seed(3)
y=rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)
x = rnorm(100)
y = rnorm(100)
plot(x,y) # scatterplot between x & y
plot(x,y,xlab="This is the x-axis",ylab="This is the y-axis",main="Plot of x vs y")
# pdf("Figure.pdf") # save in a file
# jpeg("Figure.jpeg")
plot(x,y,col="green")
# dev.off() # done creating the plot
x = seq(1,10) # creates a sequence of numbers
x = 1:10 # alternative way of writing a sequence
x = seq(-pi,pi, length = 50)
y = x
f = outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)  # creates contours
fa = (f-t(f))/2
image(x,y,fa) # colour coded contours, z-value controls the colour
persp(x,y,fa) # 3d image
persp(x,y,fa, theta = 30, phi = 20) # 3d plot but theta and phi control the angeles
a = matrix(1:16,4,4)
a
a[2,3] # 2nd row and 3rd column
a[c(1,3),c(2,4)]
a[1:3,2:4]
a[1:2,]
a[,1:2]
a[1,]
a[-c(1,3),] # exclude those with minus
dim(a) # dimension of the matrix
Auto = read.table("Auto.data") # primary way to load external data into R, here stores in the object called auto
fix(Auto) # view the imported file in an excel format
Auto = read.table("Auto.data", header = T, na.strings = "?")
# While loading the data it omits the header as they are the variable names
# It also says if R sees "?" in any data it should be treated as missing element of a data matrix */
fix(Auto)
Auto = read.csv("Auto.csv", header = T, na.strings = "?") # to load csv
dim(Auto) # 397 observations or rows and 9 columns or variables
Auto[1:4,] # 4 rows and all columns

sum(is.na(Auto)) # get 5 observations which are NA.

Auto[!complete.cases(Auto), ]  # the rows which has NA as values.

# we can omit rows which having missing data.
Auto = na.omit(Auto)
dim(Auto)
names(Auto) # to check the variables name
write.table() # export data


plot(Auto$cylinders, Auto$mpg) # plot two variables from the Auto object
attach(Auto) # referencing the object globally
plot(cylinders,mpg) # now object name is not required

# As the cylinder variable is stored as a numeric vector, SAS TREATS IT AS QUANTITATIVE
# However there are since very small number of values of cylinder
# so, it can be made a qualitative variable.

cylinders = as.factor(cylinders) # converts Quantitative to Qualitative variable

# If the x-variable is categorial (qualitative), then the plot by default gives a box plot
plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T, horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, horizontal=T, xlab="Cylinders", ylab="MPG")

hist(mpg)
hist(mpg, col=2)
hist(mpg, col=2, breaks=15)

pairs(Auto) # Creates a scatterplot matrix, i.e. scatterplot for every pair of variables for any given data set
pairs(~mpg + displacement + horsepower + weight + acceleration, Auto)
 
plot(horsepower, mpg)
identify(horsepower, mpg, name) # to name the datapoints in the plot

summary(Auto) # provides a numerical summary of each variable in the data set
summary(mpg) # summary for a single variable

savehistory()
loadhistory()

q()