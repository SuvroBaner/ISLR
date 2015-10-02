######################## R data structure ###########################

###################### Vectors #########################

# Data on three medical patients

subject_name = c("John Doe", "Jane Doe", "Steve Graves")   # character vector
temperature = c(98.1, 98.6, 101.4)   # numeric vector
flu_status = c(FALSE, FALSE, TRUE)   # logical vector

# The vectors are inherently ordered

subject_name[2]
temperature[2:3]
temperature[-2]
temperature[c(TRUE, TRUE, FALSE)]

####################### Factors #########################

# In the medical data set which we are building, we might use a factor to represent gender, i.e. FEMALE and MALE
# They are stored as a categorical variables.

gender = factor(c("MALE", "FEMALE", "MALE"))
gender

# Lets add another factor as blood type. These are for the three patients. But also notice that an additional vector
# with levels is created which represents all possible blood types.

blood = factor(c("O", "AB", "A"), levels = c("A", "B", "AB", "O"))

##################### Lists #############################

# It is an ordered datastructure and can have different data types.
# The above vectors can be converted to a single list.

subject1 = list(fullname = subject_name[1],
		    temperature = temperature[1],
		    flu_status = flu_status[1],
		    gender = gender[1],
		    blood = blood[1])

subject1
subject1[5]  # gives us the 'blood' type
subject1$blood
subject1[c("gender", "flu_status")]

###################### DataFrame ###############################

# It has both rows and columns of data. It is a list of vectors or factors.
# It is two dimension unlike vectors, factors and list and so it displays like a matrix.

pt_data = data.frame(subject_name, temperature, flu_status, gender, blood, stringsAsFactors = FALSE)

# stringsAsFactors = False, ensures that the  vectors are not automatically converted to factors.

names(pt_data)
pt_data$subject_name
pt_data[c("temperature", "flu_status")]
pt_data[2:3]
pt_data[1, 1]
pt_data[c(1, 1), c(2, 1)]
pt_data[, 1]
pt_data[1, ]
pt_data[,]

################# Matrix & arrays #########################
# A matrix represents a two-dimensional table with rows and columns of data.

m = matrix(c('a', 'b', 'c', 'd'), nrow = 2)
m
m = matrix(c('a', 'b', 'c', 'd'), ncol = 2)
m
m = matrix(c('a', 'b', 'c', 'd', 'e', 'f'), nrow = 2)
m
m = matrix(c('a', 'b', 'c', 'd', 'e', 'f'), ncol = 2)
m
# All the above matrices are first loaded by column.

m[1,1]
m[3, 2]
m[1, ]

###### Managing data with R #####

save(m, file = "mydata.RData")  # saving a file
load("mydata.RData")  # loads a file

save.image() # your entire session is copied and it recreates when you start R again.

## Importing and saving data from csv files 

ps_data = read.csv("pt_data.csv", stringsAsFactors = FALSE) # by default header = TRUE, which means the Header exists in the csv.

# read.csv() is a special case of read.table()

# To save a dataframe to a CSV file

write.csv(pt_data, file = "new_pt_data.csv")

### Importing data from SQL databases.
# ODBC is a standard protocol for connecting to databases regardless of the OS or DBMS.

install.packages("RODBC")  # connecting to databases.
library(RODBC)

my_db = odbcConnect("my_dsn")  # got to create your DSN (Data Source Name)
my_db = odbcConnect("my_dsn", uid = "my_username" pwd = "my_password")

# Now, we have an open database connection.

patient_query = "select * from patient_data where alive = 1"
patient_data = sqlQuery(channel = mydb, query = patient_query, stringsAsFactors = FALSE)

odbcClose(mydb)  # this will close mydb connection.


### Exploring and understanding the data ###

# Let's explore the following data set for the used cars. This contains the actual data about used cars recently
# advertised for sale on a popular U.S website.

usedcars = read.csv("usedcars.csv", stringsAsFactors = FALSE)

# str() provides a method for displaying the structure of a data frame or other R data structures.

str(usedcars) # basic outline of your data dictionary.

# There are 150 obs and 6 variables (features)

summary(usedcars$year)
summary(usedcars[c("price", "mileage")])
summary(usedcars)

##### Measuring Central Tendency #####

mean(c(36000, 44000, 56000))
median(c(36000, 44000, 56000))

# Since the mean is more sensitive to extreme values than the median, the fact that the mean is much higher 
# than the median for mileage, might lead us to suspect that there are some used cars in the dataset with 
# extremely high mileage values. 

### Measuring spread - quartiles ###

# Spread of the data : How tightly or loosely the values are spaced can be found using the five-number summary
# Minimum
# First Quartile, or Q1
# Median, or Q2
# Third Quartile, or Q3
# Maximum

range(usedcars$price)
diff(range(usedcars$price))  # difference function gives the range of the data.

# The difference between Q1 and Q3 is of particular interest because it itself is a simple measure of spread.
# This difference is called Interquartile Range (IQR)

IQR(usedcars$price)  # this is the middle 50% of the data.

# The quantile() provides a robust tool for identifying quantiles for a set of values.

quantile(usedcars$price)  # it exactly shows the summary statistics.

# We can mentinon our own cut-points
quantile(usedcars$price, probs = c(0.01, 0.99))

quantile(usedcars$price, seq(from = 0, to = 1, by = .20))

# The spread is a normal distribution. The middle 50 % is tight and 25 % on each side is well dispersed.

############### Visualizing the numeric variables - boxplot ##################

par(mfrow = c(1, 2))
boxplot(usedcars$price, main = "Boxplot of Used Car Prices", ylab = "Price ($)")
boxplot(usedcars$mileage, main = "Boxplot of Used Car Prices", ylab = "Odometer (mi.)")

# Illustration of the Box plot-
# It depicts the five-number summary values using horizontal lines.
# The horizontal lines forming the box, represents Q1, Q2 (median) and Q3 when reading the plot from botton-to-top.
# The minimum and the maximum are illustrated using the whiskers that extend below and above the box.
# However it is convention to only allow the whiskers to extend to a minimum or maximum of 1.5 times the IQR below Q1 and above Q3.
# Any values which go beyond this threshold are called outliers and denoted as circles and dots.
# E.g : for the mileage boxplot and we see that there are many outliers above the Q3 which is also reflective why the mean is higher than the median.

############ Visualizing numeric variables - histograms ####################

par(mfrow = c(1,2))
hist(usedcars$price, main = "Histogram of Used Car Prices", xlab = "Price ($)")
hist(usedcars$mileage, main = "Histogram of Used Car Mileage", xlab = "Odometer (mi.)")

# The height of the histogram indicates the count or frequency, falling within each of the equally-sized bins partinioning the values.
# The vertical lines that separate the bars, as labeled on the horozontal axis, indicate the start and end points of the range of the values for the bin.
# For price histogram, each of the 10 bars spans an interval of $2,000, begining at $2,000 and and ending at $22,000
# The tallest bar at the center covers the range of $ 12,000 to $ 14,000 and has a frequency of 50.
# Since we know that our data includes 150 cars , so one-third of all cars are priced from $12,000 to $14,000
# Nearly 90 cars, i.e more than half are priced b/w $12,000 and $16,000

# The mileage histogram includes 8 bars indicating bins of 20,000 miles each, beginning at 0 and ending at 160,000 miles.
# Here the tallest bar is not in the center of the data, but left hand side of the diagram.
# The tallest bar has 70 cars with odomoter reading from 20,000 to 40,000 miles.

# The Price histogram is normally distrubuted with almost no skewness, however the Mileage histogram has a skewness on the right.

# An uniform distribution, has all bars of approximately same height in the histogram.

var(usedcars$price)  # variance
sd(usedcars$price)   # standard deviation

# i.e. the sd for usedcars price is 3122 i.e. on average each value differs from the mean by $ 3122

############ Exploring categorical variable ##############

# in this data set there are 3 categorical variables "model", "color", "transmission". 
# Additionally we will also make "year" as categorical.

# We exmine the categorial data using table() instead of summary()

table(usedcars$year)
table(usedcars$model)
table(usedcars$color)
table(usedcars$transmission)

model_table = table(usedcars$model)
prop.table(model_table)  # it gives the proportion of the "model", e.g. 52% cars are of "SE" type

color_table = table(usedcars$color)
color_pct = prop.table(color_table) * 100
round(color_pct, digits = 1)

############ Visualizing relationships ##############
### Scatterplots

par(mfrow = c(1,1))
plot(x = usedcars$mileage, y = usedcars$price,
	main = "Scatterplot of Price vs. Mileage",
	xlab = "Used car odometer (mi.)",
	ylab = "Used car price ($)")

# The value of price tend to be lower as the values of mileage increase, implied that 
# advertised prices are lower with higher mileage for the used cars.

### Two-way cross tabulations (or contingency table)

# What's the relationship b/w model and color using cross tab.

install.packages("gmodels")  # two-way tables
library(gmodels)

# Task, whether or not, the car's color is conservative. Towards the end we'll divide the nine colors into two groups:
# the first group will include the conservative colors like Black, Gray, Silver, and White;
# second group will include Blue, Gold, Green, Red and Yellow.

# We will create a dummy variable, indicating whether or not the car's color is conservative by our definition.
# It's value will be 1 if true, 0 otherwise.

usedcars$conservative = usedcars$color %in% c("Black", "Gray", "Silver", "White")

# %in% : in operator returns TRUE or FALSE for each value in the vector on the left-hand side of the operator,
# depending whether or not the value is found in vector on the right-hand side. 
# It translates as "is the used car color in the set of black, gray, silver, and white?"

table(usedcars$conservative) # i.e 2/3 rd cars have conservative colors.

# Now, let's look at a cross-tabulation to see how the proportion of conservative colored cars varies by model.
# Since, we're assuming that the model of car dictates the choice of color, we'll treat conservative as the dependent (y) variable.

CrossTable(x = usedcars$model, y = usedcars$conservative)

# The data states that 65% of the SE cars are colored conservatively
# 70% of the SEL cars are colored conservatively
# 65 % of the SES cars are colored conservatively.

# The Chi-square values refer to the cell's contribution in the Pearson's Chi-squared test for independence between two variables.
# This test measures how likely it is that the difference in cell counts in the table is due to chance alone.
# If the probability is very low, it provides strong evidence that the two variables are associated.

CrossTable(x = usedcars$model, y = usedcars$conservative, chisq = TRUE)

# this case, the probability is 93 %, suggesting that it is very likely that the variations in cell count are due to 
# chance alone, and not due to a true association between model and color.
