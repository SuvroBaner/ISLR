############################### NCI60 Data ##############################

# Description: 
# National Cancer Institute (NCI): The NCI coordinates the U.S. National Cancer Program and conducts and 
# supports research, training, health information dissemination, and other activities related to the causes, prevention, diagnosis, and 
# reatment of cancer
# NCI microarray data, contains expression levels on 6830 genes on 64 cancer cell lines.
# Cancer type is also recorded.

# Data format:
# 'data' is a 64 by 6830 matrix of the expression values while
# 'labs' is a vector listing the cancer types for the 64 cell lines.

# Unsupervised techniques are often used in the analysis of genomic data.
# In particular PCA and Hierarchical clustering are popular tools.

install.packages("ISLR")
library(ISLR)

names(NCI60)

nci.labs = NCI60$labs
nci.data = NCI60$data

# Each cell line is labeled with a cancer type.
# Note: We do not make use of the Cancer Types in PCA nor in Clustering method as these are unsupervised learning.
# But after performing PCA and clustering we'll check to see the extent to which these cancer types
# agree with the results of these unsupervised techniques.

dim(nci.data)

# So, the data has 64 rows and 6,830 columns.

# We begin by examining the cancer types for the cell lines.

length(nci.labs)  # the vector length is 64, i.e. as many as cancer cell lines.

table(nci.labs)

#BREAST         CNS       COLON K562A-repro K562B-repro    LEUKEMIA MCF7A-repro 
#7           5           7           1           1           6           1 
#MCF7D-repro    MELANOMA       NSCLC     OVARIAN    PROSTATE       RENAL     UNKNOWN 
#1           8           9           6           2           9           1 

# Here we see the distribution of the cancer types for 64 cancel cell lines.

####################################### Principal Component Analysis ########################################

# We'll first perform the Principal Component Analysis on the data after scaling the variables (genes)
# to have standard deviation one.
# By default prcomp() centers the variables to have mean of zero. By providing scale = TRUE
# we scale the variables to have a standard deviation of one.

pr.out = prcomp(nci.data, scale = TRUE)
names(pr.out)

dim(pr.out$rotation)
# 6830   64

# [1] "sdev"     "rotation" "center"   "scale"    "x"

# We now plot the first few principal component score vectors, in order to visualize the data.
# If you remember, the mechanism of the pricipal component score vectors is that-
# the loading vectors and the observations are matrix multiplied.
# We get n x min{n - 1, p} matrix
dim(pr.out$x) # it is a 64 x 64 matrix which is also the score vectors of the principal component.

# The observations (cell lines) corresponsing to a given cancer type will be plotted in the same color, so that we can see to what extent
# the observations within a cancer type are similar to each other.

# We'll first create a simple function that assigns a distinct color to each element of a numeric vector. The function will be used to assign
# a color to each of the 64 cell lines, based on the cancer type to which it corresponds.

Cols = function(vec)
{
  cols = rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

# rainbow(): It takes as its argument a positive integer, and returns a vector containing that number of distinct colors.
# Let's now plot the principal component score vectors.

par(mfrow = c(1, 2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")
plot(pr.out$x[, c(1, 3)], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z3")

# First, we are plotting the 1st and 2nd principal components.
# Then we are plotting the 1st and 3rd principal components.

# It is the projections of the NCI60 cancer cell lines onto the first three principal components.
# On the whole, observations belonging to a single cancer type tend to lie near each other in this low-dimensional space.
# This indicates that cell lines from the same cancer type tend to have pretty similar gene expression levels.

# It should not have been possible to visualize this data with using a dimension reduction method, such as PCA,
# since based on the full data set there are 6,830 taken 2 at a times scatterplot, 
# none of which would have been particularly informative.

# Now let's obtain a summary of the proportion of variance explained (PVE) for the first few principal components.

summary(pr.out)  # the output shows all the 64 principal components.
# A few of them are shown below-

#Importance of components:
#                         PC1      PC2      PC3      PC4      PC5      PC6      PC7      PC8
#Standard deviation     27.8535 21.48136 19.82046 17.03256 15.97181 15.72108 14.47145 13.54427
#Proportion of Variance  0.1136  0.06756  0.05752  0.04248  0.03735  0.03619  0.03066  0.02686
#Cumulative Proportion   0.1136  0.18115  0.23867  0.28115  0.31850  0.35468  0.38534  0.41220

plot(pr.out)  # it also shows the variance explained.
# The height of each bar in the bar plot is given by squaring the corresponding element of pr.out$sdev (standard deviation explained by the principal components)

# Let's do the scree plot (PVE of each principal component)

pve = 100 * (pr.out$sdev^2 / sum(pr.out$sdev^2))

par(mfrow = c(1, 2))

plot(pve, type = "o", ylab = "PVE", xlab = "Principal Component", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE", xlab = "Principal Component", col = "brown3")

# We see from the cumulative PVE plot, that together the first 7 principal components explain around 40% of the vardiance in the data.
# Although it is not a huge amount of the variance. 
# However looking at the scree plot, we see that while each of the first 7 principal components
# explain a substantial amount of variance, there is a marked decrease in the variance explained by further principal components.
# That is, there is an elbow in the plot after approximately the seventh principal comonent.
# This suggests thare may be little benefit to examining more than seven or so principal components.
# Although examining even 7 principal components may be difficult :)

#################### Clustering the observations of the NCI60 data ######################

######## Hierarchical Clustering #############

# Although unsupervised, but our goal is to find out whether or not the observations cluster into distinct types of cancer.
# Note, it is unsupervised learning so, there is no response which is supervising here (i.e. cancer type)

# Let's standardize the variables to have mean zero and standard deviation one.
# This step is optional, we do only if we want each gene to be on the same scale.

sd.data = scale(nci.data)

# We will now cluster the observations using "complete", "single" and "average" linkage.
# Euclidean distance is used as the dissimilarity measure.

par(mfrow = c(1, 3))

data.dist = dist(sd.data)  # to compute the pairwise Euclidean distances.

plot(hclust(data.dist), labels = nci.labs, main = "Complete Linkage", xlab = "", sub = "", ylab = "")

plot(hclust(data.dist, method = "average"), labels = nci.labs, main = "Average Linkage", xlab = "", sub = "", ylab = "")

plot(hclust(data.dist, method = "single"), labels = nci.labs, main = "Single Linkage", xlab = "", sub = "", ylab = "")

# Complete and Average linkage tend to yield evenly sized clusters whereas single linkage tends to yield extended clusters 
# to which the single leaves are fused one by one.

# Here, the average and complete linkage has more balanced clusters and so they are prefered to single linkage.

# Let's use the "Complete" linkage to do the further analysis of the dendrogram.

# We can cut the dendrogram at the height that will yield a particular number of clusters, say 4.

hc.out = hclust(dist(sd.data))  # by default the linkage method is "Complete"
hc.clusters = cutree(hc.out, 4)

table(hc.clusters, nci.labs)

table(nci.labs)

# hc.clusters BREAST CNS COLON K562A-repro K562B-repro LEUKEMIA MCF7A-repro MCF7D-repro MELANOMA NSCLC OVARIAN PROSTATE RENAL UNKNOWN
# 1                2   3     2           0           0        0           0           0        8     8       6        2     8       1
# 2                3   2     0           0           0        0           0           0        0     1       0        0     1       0
# 3                0   0     0           1           1        6           0           0        0     0       0        0     0       0
# 4                2   0     5           0           0        0           1           1        0     0       0        0     0       0

# We can clearly see some patterns. I am comparing and contrasting the hc.clusters (which our model has provided) and the true cancer types
# There are total 6 cases of LEUKEMIA and all of the 6 cases have been clustered together in cluster 3. Same can be said of many other cancer types.

# Again say for BREAST cancer type, the true number is seven and they have been clustered across 1, 2, and 4 clusters (i.e. spread out over three different clusters.)
# i.e. like BREAST cancer cell lines with few others are spread out over multiple clusters.

# Let's plot the dendrogram that produces these four clusters-

par(mfrow = c(1, 1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red")

# The argument h = 139 plots a horizontal line at height 139 on the dendrogram, this is the height that results in four distinct clusters.
# In total there are 64 leaves which is the count of oibservations.
# There are 4 clusters from where the dendrogram is horizontallu cut.

cutree(hc.out, 4)  # just to verify that we have 4 clusters and how the all the observations are clustered.
#V1  V2  V3  V4
#1   1   1   1

# Here the first four observations which are {Breast, Breast, CNS, CNS} are clustered in the same cluster which is numbered as 1.
# Note, the observations are the cell lines with 6830 different gene expressions.
# So, it is possible for them to be clustered the way they did (unsupervised way)

# Let's print out the summary of the clustering result.

hc.out

################## K-means clustering ######################

# Lets perform the k-means clustering on NCI60 data set.

set.seed(2)
km.out = kmeans(sd.data, 4, nstart = 20)  # sd.data is the scaled data matrix of 64 x 6830 shape

names(km.out)

km.clusters = km.out$cluster
km.clusters


table(km.clusters, hc.clusters)

#              hc.clusters
#km.clusters  1  2  3  4
#          1 11  0  0  9
#          2  0  0  8  0
#          3  9  0  0  0
#          4 20  7  0  0

# We see that four clusters obtained using hierarchical clustering and k-means clustering are somewhat different.
# Note, cluster-2 in K-means clustering is identical to cluster 3 in hierarchical clustering.
# However other clusters differ.
# e.g. Cluster 4 in k-means clustering contains a portion of the observations assigned to cluster 1 by hierarchical clustering.


# Rather than performing hierarchical clustering on the entire data matrix, we can simply perform 
# hierarchical clustering on the first few principal component score vectors.

hc.out = hclust(dist(pr.out$x[, 1:5]))

plot(hc.out, labels = nci.labs, main = "Hier. Clust. on First Five Score Vectors")

cutree(hc.out, 4)

table(cutree(hc.out, 4), nci.labs)

# The result is not so good, so we'll consider the PCA as teh denoising step in this entire learning process.
# Note : Even you can do the k-means clustering using the first few principal components.