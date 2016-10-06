##############################################
# CMPT 459 Programming Assignment 1
# Ivy Tse
##############################################

#***** QUESTION 1 *****#
library(cluster)
wine <- read.csv("wine.csv") 

#normalize all columns except the ID column
wine_norm <- scale(wine[, 2:14]) 

#***** QUESTION 2 *****#
kmeans_clusters <- kmeans(wine_norm, 3) # 3 cluster solution

#plot clusters by "Alcohol" and "Phenols"
plot(wine[c("Alcohol", "Phenols")], col = kmeans_clusters$cluster)
dsm <- daisy(wine_norm)# disimilarity distance  
sil <- silhouette(kmeans_clusters$cluster, dsm)
sm <- summary(sil)
sm$avg.width #the average silhouette width is 0.2848589 when k = 3
plot(sil)

#***** QUESTION 3 *****#

# Use k means to find clusters from 2 to 10
km2 <- kmeans(wine_norm, 2) # 2 cluster solution
km4 <- kmeans(wine_norm, 4) # 4 cluster solution
km5 <- kmeans(wine_norm, 5) # 5 cluster solution
km6 <- kmeans(wine_norm, 6) # 6 cluster solution
km7 <- kmeans(wine_norm, 7) # 7 cluster solution
km8 <- kmeans(wine_norm, 8) # 8 cluster solution
km9 <- kmeans(wine_norm, 9) # 9 cluster solution
km10 <- kmeans(wine_norm, 10) # 10 cluster solution

#Find Silhouette Score for clusters of k = 2:10
sil2 <- silhouette(km2$cluster, dsm) # k = 2
sil4 <- silhouette(km4$cluster, dsm) # k = 4
sil5 <- silhouette(km5$cluster, dsm) # k = 5
sil6 <- silhouette(km6$cluster, dsm) # k = 6
sil7 <- silhouette(km7$cluster, dsm) # k = 7
sil8 <- silhouette(km8$cluster, dsm) # k = 8
sil9 <- silhouette(km9$cluster, dsm) # k = 9
sil10 <- silhouette(km10$cluster, dsm) # k = 10

# Get Summary from silhouette functions for k = 2:10
sm2 <- summary(sil2) # k = 2
sm4 <- summary(sil4) # k = 4
sm5 <- summary(sil5) # k = 5
sm6 <- summary(sil6) # k = 6
sm7 <- summary(sil7) # k = 7
sm8 <- summary(sil8) # k = 8
sm9 <- summary(sil9) # k = 9
sm10 <- summary(sil10) # k = 10

# Display average silhouette scores from k = 2:10
sm2$avg.width # k = 2, average silhouette width = 0.259317
sm$avg.width # k = 3, average silhouette width = 0.2848589
sm4$avg.width # k = 4, average silhouette width = 0.258447
sm5$avg.width # k = 5, average silhouette width = 0.183585
sm6$avg.width # k = 6, average silhouette width = 0.1891821
sm7$avg.width # k = 7, average silhouette width = 0.1290138
sm8$avg.width # k = 8, average silhouette width = 0.1447175
sm9$avg.width # k = 9, average silhouette width = 0.1309074
sm10$avg.width # k = 10, average silhouette width = 0.1309341

# Based on all the silhouette scores when k = 2 to 10, the largest 
# silhouette score is 0.2848589 when k = 3 (when there are 3 clusters).

#***** QUESTION 4 *****#
d <- dist(wine_norm, method = "euclidean")
hcluster_complete <- hclust(d, method = "complete") # complete linkage
hcluster_average <- hclust(d, method = "average") # average linkage
hcluster_single <- hclust(d, method = "single") # single linkage

# Cut dendrograms into 3 clusters
cut_complete <- cutree(hcluster_complete, 3)
cut_average <- cutree(hcluster_average, 3)
cut_single <- cutree(hcluster_single, 3)

hsil_complete <- silhouette(cut_complete, dsm)
hsil_average <- silhouette(cut_average, dsm)
hsil_single <- silhouette(cut_single, dsm)

hsum_complete <- summary(hsil_complete)
hsum_average <- summary(hsil_average)
hsum_single <- summary(hsil_single)

hsum_complete$avg.width # average silhouette width = 0.2037869
hsum_average$avg.width # average silhouette width = 0.1575253
hsum_single$avg.width # average silhouette with = 0.182738

#***** QUESTION 5 *****#
plot(hcluster_complete)
dev.new()
plot(hcluster_average)
dev.new()
plot(hcluster_single)

#***** QUESTION 6 *****#
# See "report1.pdf" question 6

