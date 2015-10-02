############################## Unsupervised Learning #############################

################### Principal Component Analysis ########################

# 'USArrests' data-set is a part of R base package

fix(USArrests)
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


####################################### K - means clustering ####################################3

# The function kmeans() performs K-means clustering in R.
# We begin with a simple simulated example in which there truly are two clusters in the data.
# The first 25 observations have a mean shift relative to the next 25 observations.

set.seed(2)
x = matrix(rnorm(50 * 2), ncol = 2)

# Mean shift of the first 25 observations
x[1:25, 1] = x[1:25, 1] + 3
x[1:25, 2] = x[1:25, 2] - 4


# We now perform K-means clustering with K = 2

km.out = kmeans(x, 2, nstart = 20)

names(km.out)

km.out$cluster # The cluster assignments of the 50 observations are contained here.

#[1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1
#[39] 1 1 1 1 1 1 1 1 1 1 1 1

# The k-means clustering perfectly separated the observations into two clusters even though we did not supply any
# group information to kmeans().

# Let's plot the data with each observation colored according to its cluster assignment

plot(x, col = (km.out$cluster + 1), main = "K-Means Clustering Results with K = 2",
			xlab = "", ylab = "", pch = 20, cex = 2)

# We could easily plot because here there are two variables  i.e. two-dimension.
# If there were more than two variables then we could instead perform PCA and plot the first two principal componeents score vectors.

# In real data, we do not know the true number of clusters.
# Let's perform the k-means clustering on this data using K = 3.

set.seed(4)
km.out = kmeans(x, 3, nstart = 20)
km.out

#K-means clustering with 3 clusters of sizes 10, 23, 17
#
#Cluster means:
#        [,1]        [,2]
#1  2.3001545 -2.69622023
#2 -0.3820397 -0.08740753
#3  3.7789567 -4.56200798
#
#Clustering vector:
# [1] 3 1 3 1 3 3 3 1 3 1 3 1 3 1 3 1 3 3 3 3 3 1 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2
#[39] 2 2 2 2 2 1 2 1 2 2 2 2
#
#Within cluster sum of squares by cluster:
#[1] 19.56137 52.67700 25.74089
# (between_SS / total_SS =  79.3 %)
#
#Available components:
#
#[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
#[6] "betweenss"    "size"         "iter"         "ifault" 



### 'nstart' argument allows R with multiple initial cluster assignments.
# If a nstart greater than 1 is used, the k-means clustering will be performed using multiple random assignments 
# of the cluster before the iterating over all the clusters, and the kmeans() will report only the best results.
# Let's do it with nstart = 1 and 20 and compare.

set.seed(3)
km.out = kmeans(x, 3, nstart = 1)
km.out$tot.withinss
# 104.3319

km.out = kmeans(x, 3, nstart = 20)
km.out$tot.withinss
# 97.97


# km.out$tot.withinss : is the total within-cluster sum of squares, which we seek to minimize by performing K-means clustering.
# The individual within-cluster sum-of-squares are contained in the vector km.out$withinss

# We strongly recommend always running K-means clustering with a large value of nstart, such as 20 or 50,
# since otherwise an undesirable local optimum may be obtained.

# When performing K-means clustering, in addition to using multiple initial cluster assignments,
# it is also important to set a random seed using set.seed(). This way, the initial cluster assignments in Step 1 can be 
# replicated, and the K-means output will be fully reproducible.

############################## Hierarchical Clustering ##########################
# Below we'll do hierarchical clustering and plot the dendogram using complete, single, and average linkage clustering
# with Euclidean distance as the dissimilarity measure.

# a) We begin by clustering observations using complete linkage.
# dist() function is used to compute 50 x 50 inter-observation Euclidean distance matrix.

hc.complete = hclust(dist(x), method = "complete")

# We can also use the other linkage

hc.average = hclust(dist(x), method = "average")
hc.single = hclust(dist(x), method = "single")

# Now let's plot the dendograms.

par(mfrow = c(1, 3))
plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", cex = .9)
plot(hc.average, main = "Average Linkage", xlab = "", sub = "", cex = .9)
plot(hc.single, main = "Single Linkage", xlab = "", sub = "", cex = .9)

# To determine the cluster labels for each observation associated with a 
# given cut of the dendogram, we do the following-

cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)

# For this data, complete and average linkage generally separate the observations into their correct groups.
# However single linkage identifies one point as belonging to its own cluster.

# A more sensible answer is obtained when four clusters are selected, although there are still two singletons.

cutree(hc.single, 4)

# To scale variables before performing hierarchical clustering of the observations, we use scale()

xsc = scale(x)
plot(hclust(dist(xsc), method = "complete"), main = "Hierarchical Clustering with Scaled Features")

# Correlation based distances can be computed using as.dist(), which converts an arbitrary
# square symmetric matrix into a form that hclust() recognizes as a distance matrix.
# However, this only makes sense for data with at least three features since the 
# absolute correlation between any two observations with measurements on two features
# is always 1. Hence, we'll cluster a three-dimensional data set.

x = matrix(rnorm(30*3), ncol = 3)
dd = as.dist(1 - cor(t(x)))
plot(hclust(dd, method = "complete"), main = "Complete Linkage with Correlation-Based Distance", xlab = "", sub = "")
