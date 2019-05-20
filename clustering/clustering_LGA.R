######### AT2 Data Analysis Project - Regression Model #########

##### Clear Workspace
rm(list=ls())

##### Set working directory
setwd("D:/OneDrive - UTS/36103/AT2B/Files for R Script")

##### Load libraries
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(scales)
library(ggrepel)
library(readxl)
library(corrplot)
library(cluster)

#Clustering Algo to find similar LGAs
unified_new <- read.csv("clustering-input.csv")
unif <- unified_new
unif <- unif[-20,]
rownames(unif) <- unif[,1]
unif[,1:3] <- NULL


# function to find cosine distance
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

# calculate cosine distance
cs <- cosineSim(as.matrix(unif))
# calculate cosine distance
cd <- 1-cs

#---------------------------------------------------------------------------------
#run hierarchical clustering using cosine distance
groups <- hclust(cd,method="ward.D")
#plot, use hang to ensure that labels fall below tree
plot(groups, hang=-1)
#cut into 5 subtrees.
rect.hclust(groups,5)
hclusters_cosine <- cutree(groups,5)
write.csv(hclusters_cosine,"hclusters_cosine.csv")


#---------------------------------------------------------------------------------
#create 5 clusters
kfit <- kmeans(cd, 5, nstart=100)

#plot the clusters
clusplot(as.matrix(cd), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#write clusters to csv file
write.csv(kfit$cluster,file="KMClustGroups2_cosine.csv")


# loop to find optimal number of clusters for K-Mean clustering
wss <- 2:(nrow(unif)-1)
for (i in 2:(nrow(unif)-1)) wss[i] <- sum(kmeans(cd,centers=i,nstart=25)$withinss)
plot(2:(nrow(unif)-1), wss[2:(nrow(unif)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


