# Generate a simulated data set with 20 observations in each of three classes
# i.e. 60 observations in total and 50 variables.

set.seed(1)

x = matrix(rnorm(20 * 3 * 50, mean = 0, sd = 0.001), ncol = 50)  # 60 observations of 50 variables.

x[1:20, 2] = 1
x[21:40, 1] = 2
x[21:40, 2] = 2
x[41:60, 1] = 1

# Here we do the mean shift only for two features, i.e. to separate the three classes amongst two dimensions.

pca.out = prcomp(x)
summary(pca.out)

Importance of components:
                         PC1    PC2     PC3     PC4      PC5     PC6      PC7      PC8      PC9     PC10
Standard deviation     1.008 0.5821 0.00188 0.00183 0.001689 0.00168 0.001622 0.001563 0.001523 0.001437
Proportion of Variance 0.750 0.2500 0.00000 0.00000 0.000000 0.00000 0.000000 0.000000 0.000000 0.000000
Cumulative Proportion  0.750 1.0000 0.99997 0.99997 0.999970 0.99997 0.999970 0.999980 0.999980 0.999980

# Plot the first two principal components score vectors

pca.out$x[, 1:2]

plot(pca.out$x[, 1:2], col = 2:4, xlab = "Z1", ylab = "Z2", pch = 19) # all three classes.

km.out = kmeans(x, 3, nstart = 20)
table(km.out$cluster, c(rep(1, 20), rep(2, 20), rep(3, 20)))

km.out = kmeans(x, 2, nstart=20)
km.out$cluster

# K-means clustering with K = 3 on the first two principal componenet score vectors, rather than on the raw data.

km.out = kmeans(pca.out$x[,1:2], 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

#   1  2  3
#1 20  0  0
#2  0 20  0
#3  0  0 20

# perfect match.
