####################################### Lab-1 , Q-8 onwards in the Applied section

###################### Q 8.

# Read the data from an external file and display
college = read.csv("College.csv", header = T, na.strings = "?")
fix(college)

#The first column is the name of the universities, we don't want R to treat it as data

rownames(college) = college[,1]
fix(college)

#You should see that there is now a row.names column with the
#name of each university recorded. This means that R has given
#each row a name corresponding to the appropriate university. R
#will not try to perform calculations on the row names.

# However, we still need to eliminate the first column in the data where the
# names are stored.

college = college[,-1]
fix(college)

summary(college)

attach(college) # global object instantiation
plot(Outstate,Private, col = "red")

# Create a new qualitative variable called Elite

Elite = rep("No",nrow(college)) # repeating "No" for the number of row times in college

# We are going to divide the universities in two groups based on whether or not
# the proportion of students coming from the top ten percent of their high school classes
# exceeds 50 %.

Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite) # converting into a qualitative variable
college = data.frame(college, Elite)  # adding the qualitative variable Elite into the college matrix
fix(college)

summary(college)
plot(Elite,Outstate, col="red", xlab = "Elite", ylab = "Outstate")

################### Q 9.

Auto = read.table("Auto.data", header = T, na.strings = "?")
fix(Auto)
summary(Auto)
range(Auto[,1], na.rm = T) # all na's have to be omitted
range(Auto[,2], na.rm = T)

################## Q 10.

# This is to do with the Boston Housing data set

library(MASS)




