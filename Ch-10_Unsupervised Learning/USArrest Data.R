############################## Unsupervised Learning #############################

################### Principal Component Analysis ########################

# 'USArrests' data-set is a part of R base package

names(USArrests)
dim(USArrests)

states = row.names(USArrests)

summary(USArrests)

# The same thing could be done using apply()

apply(USArrests, 2, mean) 
# E.g., for a matrix 1 indicates rows, 2 indicates columns, c(1, 2) indicates rows and columns. 

# We can see that the variables have vastly different means.

# Here, there are on an average three times as many rapes as murders,
# and more than eight times as many assaults as rapes.

# Let's also examine the variances of these four variables

apply(USArrests, 2, var)

# The variables also have vastly different variances.

# Example, UrbanPop variable measures the percentage of the population in each state living in an urban area,
# which is not a comparable number to the number of rapes in each state per 100,000 individuals.

# If we fail to scale the variables before performing PCA, then most of the principal components that we observed 
# would be driven by the Assault variable, since it has by far the largest mean and variance.

# Thus, it is important to standardize the variables to have mean zero and standard deviation one before 
# performing the PCA.

# We'll use prcomp() to perform the principal component analysis. By default it centers the variables to have mean zero.
# By using the option scale = TRUE, we scale the variables to have standard deviation one.

pr.out = prcomp(USArrests, scale = TRUE)

names(pr.out)

# "sdev"     "rotation" "center"   "scale"    "x" 

# "center" and "scale" components correspond to the means and standard deviations of the variables that were used for scaling prior to implementing PCA.

pr.out$center  # mean

#Murder  Assault UrbanPop     Rape 
#   7.788  170.760   65.540   21.232

pr.out$scale  # standard deviation

# Murder   Assault  UrbanPop      Rape 
# 4.355510 83.337661 14.474763  9.366385

# The rotation matrix provides the principal component loadings.
# These loading vectors represent the direction in the feature space along which the data vary the most.

pr.out$rotation

#                PC1        PC2        PC3         PC4
#Murder   -0.5358995  0.4181809 -0.3412327  0.64922780
#Assault  -0.5831836  0.1879856 -0.2681484 -0.74340748
#UrbanPop -0.2781909 -0.8728062 -0.3780158  0.13387773
#Rape     -0.5434321 -0.1673186  0.8177779  0.08902432

# We see that there are 4 distinct principal components. This is to be expected because there are in general
# min(n - 1, p) informative principal components in a data set with n observations and p variables.

# Using the prcomp() function, we do not need to explicitly multiply the data by the principal component loading vectors
# in order to obtain the principal component score vectors.
# Rather 50 x 4 matrix X has its columns the principal component score vectors. That is, the kth column is the kth
# principal component score vector.

dim(pr.out$x)
# 50  4

# Let's plot the first two principal components as follows-

biplot(pr.out, scale = 0)

# The scale = 0 argument to biplot() ensures that the arrows are scaled to represent the loadings;
# other values for scale give slightly different biplots with different interpretations.

# To have the mirror image of it.

pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x

biplot(pr.out, scale = 0)

# prcomp() also outputs the standard deviation of each principal components

pr.out$sdev

# [1] 1.5748783 0.9948694 0.5971291 0.4164494

# The variance explained by each principal component is obtained by squaring-

pr.var = pr.out$sdev^2

# [1] 2.4802416 0.9897652 0.3565632 0.1734301

# To compute the proportion of variance explained by each principal component 

pve = pr.var / sum(pr.var)

# [1] 0.62006039 0.24744129 0.08914080 0.04335752

# We see that the first principal component explains 62% of the variance in the data,
# The next principal component explains 24.7 % of the variance in the data and so on.

# We can plot the PVE explaines by each component, as well as the cumulative PVE as follows-

plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0, 1), type = "b")

plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim = c(0, 1), type = 'b')

############### Hierarchical clustering ##################

# Without scaling

set.seed(3)

hc.complete = hclust(dist(USArrests), method = "complete")
plot(hc.complete)

cutree(hc.complete, 3)

table(cutree(hc.complete, 3))

#1  2  3 
#16 14 20 

# With scaling

dsc = scale(USArrests)

hc.s.complete = hclust(dist(dsc), method = "complete")
plot(hc.s.complete)

cutree(hc.s.complete, 3)

table(cutree(hc.s.complete, 3))

#1  2  3 
#8 11 31 

table(cutree(hc.s.complete, 3), cutree(hc.complete, 3))

#   1  2  3
#1  6  2  0
#2  9  2  0
#3  1 10 20

# Here in my opinion all the variables are not similar, for example UrbanPop is different from other three.
# So, it's better to scale the variables to have zero mean and standard deviation one before performing the clustering.

############# K-means clustering ###############

set.seed(5)

dsc = scale(USArrests)

apply(dsc, 2, var)  # variance is 1 so scaled.
apply(dsc, 2, mean) # the mean is insignificant, i.e. zero

km.out = kmeans(dsc, 3, nstart = 20)

names(km.out)

km.out$cluster

plot(dsc, col = (km.out$cluster + 1), main = "K-Means Clustering Results with K = 3",
     xlab = "", ylab = "", pch = 20, cex = 2)

