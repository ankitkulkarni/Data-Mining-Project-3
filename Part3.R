#----------------------------------------------------------------------------


#Part3

#K-Means Clustering

#Dataset2

#download and install package "cluster", "rgl", "fpc"
#load the libraries "cluster", "rgl", "fpc"
library(cluster)
library(rgl)
library(fpc)

#load data set from the working directory
data2 <- read.csv("dataset2.csv")

#removing serial number column (column 1) and cluster labels (column 6)
data22 <- data2[, 2:5]

#plotting the given data
plot3d(data22)

#using K-MEANS and plotting the 3D graph as required
cluster5 <- kmeans(data22, centers = 2)
plotcluster(data22, cluster5$cluster)
points(cluster5$centers[,c("x", "y", "z", "w")], col=2:5)
plot3d(data22[c("x", "y", "z", "w")], col=cluster5$cluster)

#purity function to calculate the accuracy of clustering performed by function above
purity <- function(clusters, classes)
{
sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

#this gives the accuracy of the clustering performed on a scale from 0 to 1
purity(cluster5$cluster, data2[, 6])


#------------------------------------------------------------------------------