#----------------------------------------------------------------------------


#Part1

#Distance Based Clustering

#Dataset1

#download and install package "cluster", "rgl", "fpc"
#load the libraries "cluster", "rgl", "fpc"
library(cluster)
library(rgl)
library(fpc)

#load data set from the working directory
data1 <- read.csv("dataset1.csv")

#removing cluster labels
data11 <- data1[, 1:3]

#plotting the given data
plot3d(data11, col=data1[, 4])

#using K-MEANS and plotting the 3D graph as required
cluster1 <- kmeans(data11, centers = 8)
plotcluster(data11, cluster1$cluster)
points(cluster1$centers[,c("x", "y", "z")], col=1:3)
plot3d(data11[c("x", "y", "z")], col=cluster1$cluster)

#purity function to compare it with the distance based clustering function created in part 2
purity <- function(clusters, classes)
{
sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

#this gives the accuracy of the clustering performed on a scale from 0 to 1
purity(cluster1$cluster, data1[, 4])


#------------------------------------------------------------------------------


#Density Based Clustering

#Dataset1

#download and install package "rgl", "cluster", "fpc"
#load the libraries "rgl", "cluster", "fpc"
library(cluster)
library(rgl)
library(fpc)

#load data set from the working directory
data1 <- read.csv("dataset1.csv")

#removing cluster labels
data11 <- data1[, 1:3]

#plotting the given data
plot3d(data11, col=data1[, 4])

#using DBSCAN and plotting the 3D graph as required
cluster2 <- dbscan(data11, eps = 5, MinPts = 100, showplot = 1)
plotcluster(data11, cluster2$cluster)
points(cluster2$centers[,c("x", "y", "z")], col=1:3)
plot3d(data11[c("x", "y","z")], col=cluster2$cluster+1)


#------------------------------------------------------------------------------


#Graph Based Clustering

#Dataset1

#download and install package "MCL", "fields", "rgl", "fpc", "cluster"
#load the libraries "MCL", "fields", "rgl", "fpc", "cluster"
library(MCL)
library(fields)
library(rgl)
library(fpc)
library(cluster)

#load data set from the working directory
data1 <- read.csv("dataset1.csv")

#removing cluster labels
data11 <- data1[, 1:3]

#plotting the given data
plot3d(data11, col=data1[, 4])

#to convert the data into matrix form
am <- as.matrix(data11)

#to compute distance matrix among all pairs
distanceCalculation <- rdist(am)

#to assign logical values to the distance matrix
distanceCalculationLogical <- distanceCalculation < 2

#using Markov Cluster Algorithm and plotting the 3D graph as required
cluster3 <- mcl(distanceCalculationLogical, addLoops=TRUE, ESM=TRUE)
plot3d(data11, col=(cluster3$Cluster+1))


#------------------------------------------------------------------------------
