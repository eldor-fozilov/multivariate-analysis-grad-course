######################
## Cluster Analysis ##
######################
##### 1. Hierarchical Clustering #####
## reference: https://uc-r.github.io/hc_clustering

## Required packages
#install.packages("cluster", dependencies = T)
#install.packages("factoextra", dependencies = T)
#install.packages("dplyr", dependencies = T)
#install.packages("purrr", dependencies = T)

library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dplyr)
library(purrr)


##########################
#### Data preparation ####
## read US arrest data: statistics in arrests per 100,000 residents for assault, murder, and rape in each of the 50 US states in 1973
df <- USArrests
df <- na.omit(df) # remove missing values (if exist)
str(df)
dim(df)
head(df)
View(df)

## standardization/ scale function default: centering + scaling
df <- scale(df)
head(df)

##################################
#### Hierarchical Clustering #####
## agglomerative hierarchical clustering - hclust (stats package) / agnes (cluster package)
## divisive hierarchical clustering - diana (cluster package)

## Distance: Dissimilarity matrix
d <- dist(df, method = "euclidean")

## Hierarchical clustering using five methods 
hc1 <- hclust(d, method = "single" ) # single linkage
hc2 <- hclust(d, method = "complete" ) # complete linkage
hc3 <- hclust(d, method = "average" ) # average linkage
hc4 <- hclust(d, method = "centroid" ) # centroid linkage
hc5 <- hclust(d, method = "ward.D2" ) # Ward's minimum variance- "ward.D2"!!!

## Plot the dendrograms
par(mfrow=c(2,3))
plot(hc1, cex = 0.7, hang = -1, main="Single Linkage")
plot(hc2, cex = 0.7, hang = -1, main="Complete Linkage")
plot(hc3, cex = 0.7, hang = -1, main="Average Linkage")
plot(hc4, cex = 0.7, hang = -1, main="Centroid Linkage")
plot(hc5, cex = 0.7, hang = -1, main="Ward's Method")

## Which method to choose? 
#1. Practical(and rough) guideline
#https://stats.stackexchange.com/questions/195456/how-to-select-a-clustering-method-how-to-validate-a-cluster-solution-to-warran/195481#195481

#2. Use "agnes" function to calculate Agglomerative coefficients 
#"Agglomerative coefficient" measures the amount of clustering structure found (values closer to 1 suggest strong clustering structure)

# Compute with agnes
hc5a <- agnes(df, method = "ward")

# Agglomerative coefficient
hc5a$ac

# methods to assess: 4 methods
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute agglomerativecoefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

map_dbl(m, ac)
# ward is the best according to agglomerative coefficients

## hclust/agnes comparison: exactly the same results
par(mfrow=c(1,2))
pltree(hc5a, cex = 0.7, hang = -1, main="Dendrogram of agnes-ward")
plot(hc5, cex = 0.7, hang = -1, main="Dendrogram of hclust-Ward")

## Determining the optimal number of clusters 
# 1. Elbow method: total within-cluster sum of square is minimized
fviz_nbclust(df, FUN = hcut, method = "wss")
# 2. Average Silhouette method: the average silhouette approach measures the quality of a clustering. That is, it determines how well each object lies within its cluster. A high average silhouette width indicates a good clustering. 
fviz_nbclust(df, hcut, method = "silhouette")
# 3. Gap statistic method:  The gap statistic compares the total intra-cluster variation for different values of k with their expected values under null reference distribution of the data (i.e. a distribution with no obvious clustering).
gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

## Cut tree into 4 groups by Ward's method(hc5)
sub_grp <- cutree(hc5, k = 4)
sub_grp

## Number of members in each cluster
table(sub_grp)

## Dendrogram with a border
plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)

## Descriptive stat of each cluster
USArrests %>% 
  mutate(cluster = sub_grp)  %>% 
  head()

USArrests %>% 
  mutate(cluster = sub_grp)  %>% 
    group_by(cluster) %>%
      summarise_all("mean")

## Scatter plot
# four defined clusters from hc5[ward's method]
fviz_cluster(list(data = df, cluster = sub_grp)) # Dim1=PC1, Dim2=PC2

#### cf: Divisive Hierarchical Clustering - diana 
# compute divisive hierarchical clustering
hc6 <- diana(df)

# Divise coefficient; amount of clustering structure found
hc6$dc

# plot dendrogram
pltree(hc6, cex = 0.6, hang = -1, main = "Dendrogram of diana")
rect.hclust(hc6, k = 4, border = 2:5)

cutree(as.hclust(hc6), k = 4)

###############################################################################################
##### 2. K-means clustering #####
## Reference: https://uc-r.github.io/kmeans_clustering
## Required packages
#install.packages("fpc",dependencies=T)

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
library(purrr)
library(fpc) # pseudo-F 

##########################
#### Data preparation ####
## read US arrest data: statistics in arrests per 100,000 residents for assault, murder, and rape in each of the 50 US states in 1973
df <- USArrests
df <- na.omit(df) # remove missing values (if exist)
str(df)
dim(df)
head(df)
View(df)

## standardization/ scale function default: centering + scaling
df <- scale(df)
head(df)

### Determining the optimal number of clusters 
## 1. Elbow method: total within-cluster sum of square is minimized

#####
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

####
## Alternatively use fviz_nbclust funtion 
#fviz_nbclust(df, FUN = kmeans, method = "wss")

### 2. Average Silhouette method: the average silhouette approach measures the quality of a clustering. That is, it determines how well each object lies within its cluster. A high average silhouette width indicates a good clustering. 

####
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

####
## Alternatively use fviz_nbclust funtion
#fviz_nbclust(df, kmeans, method = "silhouette")

### 3. Gap statistic method:  The gap statistic compares the total intra-cluster variation for different values of k with their expected values under null reference distribution of the data (i.e. a distribution with no obvious clustering). The estimate of the optimal clusters will be the value that maximizes the gap statistic. This means that the clustering structure is far away from the uniform distribution of points.
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
print(gap_stat, method = "firstmax")

####
##Alternatively use fviz_gap_stat funtion
#fviz_gap_stat(gap_stat)

# 4. Pseudo-F [Calinski and Harabasz (1974)] 
sub_grp2 <- kmeans(df, 2, nstart = 25)$cluster
sub_grp3 <- kmeans(df, 3, nstart = 25)$cluster
sub_grp4 <- kmeans(df, 4, nstart = 25)$cluster
sub_grp5 <- kmeans(df, 5, nstart = 25)$cluster
sub_grp6 <- kmeans(df, 6, nstart = 25)$cluster
sub_grp7 <- kmeans(df, 7, nstart = 25)$cluster
sub_grp8 <- kmeans(df, 8, nstart = 25)$cluster
sub_grp9 <- kmeans(df, 9, nstart = 25)$cluster

round(calinhara(df,sub_grp2),digits=2)
round(calinhara(df,sub_grp3),digits=2)
round(calinhara(df,sub_grp4),digits=2)
round(calinhara(df,sub_grp5),digits=2)
round(calinhara(df,sub_grp6),digits=2)
round(calinhara(df,sub_grp7),digits=2)
round(calinhara(df,sub_grp8),digits=2)
round(calinhara(df,sub_grp9),digits=2)

# Compute k-means clustering with k = 4
set.seed(123)
final <- kmeans(df, 4, nstart = 25)
print(final) # standardized centroids

## Descriptive stat of each cluster
USArrests %>%
  mutate(Cluster = final$cluster) %>%
    group_by(Cluster) %>%
     summarise_all("mean")

## cf: Compare with hierarchical clustering result (4 clusters by Ward's method(hc5))
USArrests %>% 
  mutate(cluster = sub_grp)  %>% 
  group_by(cluster) %>%
  summarise_all("mean")
