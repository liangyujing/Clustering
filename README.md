rm(list=ls())

# Set your working dir as the current dir
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
my_data <- read.csv(file ="R code/regression-assignments 1+ 2/ExperienceSampling_Group7.csv", head=T,sep=";") 

## Standardizing the data:
mydata <- scale(my_data) 

## 1.Assessing Clustering Tendency
##Before applying any clustering method on your data, 
##it’s important to evaluate whether the data sets contains meaningful clusters (i.e.: non-random structures) or not. 

## 1.1 Visual inspection of the data
library("factoextra")
# Plot faithful data set
fviz_pca_ind(prcomp(mydata), title = "PCA - Iris data", 
             habillage = mydata$PA,  palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")

#Plot the random df
fviz_pca_ind(prcomp(mydata), title = "PCA - Random data", 
             geom = "point", ggtheme = theme_classic())

##visualize the results
library(factoextra)
set.seed(123)

#K-means on iris dataset
km.res1 <- kmeans(mydata, 3)
fviz_cluster(list(data = mydata, cluster = km.res1$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())


#K-means on the random dataset
km.res2 <- kmeans(random_df, 3)
fviz_cluster(list(data = random_df, cluster = km.res2$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

#Hierarchical clustering on the random dataset
fviz_dend(hclust(dist(random_df)), k = 3, k_colors = "jco",  
          as.ggplot = TRUE, show_labels = FALSE)


## 1.2 Statistical methods
library(factoextra)

#Compute Hopkins statistic for iris dataset
res <- get_clust_tendency(mydata, n = nrow(mydata)-1, graph = FALSE)
res$hopkins_stat


#Compute Hopkins statistic for a random dataset
res <- get_clust_tendency(random_df, n = nrow(random_df)-1,
                          graph = FALSE)
res$hopkins_stat

##It can be seen that the iris data set is highly clusterable (the H value = 0.82 which is far above the threshold 0.5). 
##However the random_df data set is not clusterable (H = 0.46)


##Visual methods
res.dist <- get_dist(mydata, stand = TRUE, method = "pearson")
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))



## 2.三种方法：图示Determining the optimal number of clusters
##fviz_nbclust() function: Elbow, Silhouhette and Gap statistic methods
# 2.1 Elbow method
fviz_nbclust(mydata, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# 2.2 Silhouette method
fviz_nbclust(mydata, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# 2.3 Gap statistic
#nboot = 50 to keep the function speedy. 
#recommended value: nboot= 500 for your analysis.
#Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(mydata, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

##Elbow method: 4 clusters solution suggested
##Silhouette method: 2 clusters solution suggested
##Gap statistic method: 4 clusters solution suggested

##The disadvantage of elbow and average silhouette methods is that, 
##they measure a global clustering characteristic only. 
##A more sophisticated method is to use the gap statistic which provides 
##a statistical procedure to formalize the elbow/silhouette heuristic in order to estimate the optimal number of clusters.


## 2.4 NbClust() function: 30 indices for choosing the best number of clusters
library(NbClust)
NbClust(data = mydata, diss = NULL, distance = NULL,
        min.nc = 2, max.nc = 15, method = kmeans)


NbClust(data = NULL, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, 
        method = NULL, index = "all", alphaBeale = 0.1)


##min.nc, max.nc: minimal and maximal number of clusters
##method: The cluster analysis method to be used including “ward.D”, “ward.D2”, “single”, “complete”, “average”, “kmeans” 

## 2.5 hierarchical clustering 
##is a tree-based representation of the objects, which is also known as dendrogram

##R code to compute and visualize hierarchical clustering:
res.hc <- hclust(dist(mydata),  method = "ward.D2")     #Computation
fviz_dend(res.hc, cex = 0.5, k = 4, palette = "jco")    #Visualization

## heatmap
##is another way to visualize hierarchical clustering. 
##data values are transformed to color scale. 
##columns are samples and rows are variables. 

library(pheatmap)
pheatmap(t(mydata), cutree_cols = 4)



## 3. Cluster Validation Statistics: Must Know Methods  
#https://www.datanovia.com/en/lessons/cluster-validation-statistics-must-know-methods/
library(factoextra)
library(fpc)
library(NbClust)

# Standardize
df <- scale(my_data)

# 图
eclust(df, FUNcluster = "kmeans", hc_metric = "euclidean")


## 3.1 To compute a partitioning clustering, such as k-means clustering with k = 4, type this:
# K-means clustering
km.res <- eclust(df, "kmeans", k = 4, nstart = 25, graph = FALSE)
# Visualize k-means clusters
fviz_cluster(km.res, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())


## 3.2 To compute a hierarchical clustering, use this:
# Hierarchical clustering
hc.res <- eclust(df, "hclust", k = 3, hc_metric = "euclidean", 
                   hc_method = "ward.D2", graph = FALSE)

# Visualize dendrograms
fviz_dend(hc.res, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)

## 3.4 Validation statistics
library(fpc)
# Statistics for k-means clustering
km_stats <- cluster.stats(dist(df),km.res$cluster)
# Dun index
km_stats$dunn
##display all statistics
km_stats   


## 3.5External clustering validation
#Does the K-means clustering matches with the true structure of the data?
table(df$PA, km.res$cluster)

# quantify the agreement between PA and k-means clusters
library("fpc")
# Compute cluster stats
PA <- as.numeric(mydata$PA)
clust_stats <- cluster.stats(d = dist(df), 
                             PA, km.res$cluster)
# Corrected Rand index
clust_stats$corrected.rand


library(fpc)
# Statistics for k-means clustering
km_stats <- cluster.stats(dist(df),  km.res$cluster)
# Dun index
km_stats$dunn

# VI
clust_stats$vi


## 4 Choosing the Best Clustering Algorithms 
##https://www.datanovia.com/en/lessons/choosing-the-best-clustering-algorithms/
clValid(obj, nClust, clMethods = "hierarchical", 
        validation = "stability", maxitems = 600,
        metric = "euclidean", method = "average")

## 4.1 
library(clValid)
# Iris data set:
# - Remove Species column and scale
df <- scale(mydata)

# Compute clValid
clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(df, nClust = 2:6, 
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)


## 4.2 The stability measures can be computed as follow:
# Stability measures
clmethods <- c("hierarchical","kmeans","pam")
stab <- clValid(df, nClust = 2:6, clMethods = clmethods, 
                validation = "stability")
# Display only optimal Scores
optimalScores(stab)

## 5 Computing P-value for Hierarchical Clustering 
##https://www.datanovia.com/en/lessons/computing-p-value-for-hierarchical-clustering/

