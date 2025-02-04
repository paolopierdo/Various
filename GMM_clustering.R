# ex 1 ####
library(readr)
library(mclust)
library(cluster)
library(factoextra)
library(ggplot2)
library(smacof)

glass <- as.matrix(read_table("C:/Users/pierd/OneDrive/Desktop/PAOLO/UNIBO/5° ANNO/MODERN STATISTICS_&_BIG_DATA/datasets/glass.txt"))

set.seed(1234)
mglass <- Mclust(glass,G=1:25)
summary(mglass)
summary(mglass$BIC)
# best with VEV 4

plot(mglass)
# for sure this method is advising us to choose a VEV method, and the best one is eith 4

rainbow = rainbow(10)
sglass = scale(glass)
sprglass <- princomp(sglass)
colors = c("red","blue","green","black","purple","brown","yellow","pink","lightblue")
plot(sprglass$scores,col=colors[mglass$classification],lwd=2.5)


# kmeans ###
set.seed(1234)
km = kmeans(sglass,centers = 4,iter.max = 100,nstart = 100)
table(km$cluster)
adjustedRandIndex(km$cluster,mglass$classification)
# 0.26, quite different clusterings

k_values = 2:20
silhouette_avg <- numeric(length(2:20))
for (i in 1:length(k_values)) {
  k <- k_values[i]
  set.seed(123456)
  kmeans_result <- kmeans(sglass, centers = k, nstart = 50)
  silhouette_scores <- silhouette(kmeans_result$cluster, dist(sglass))
  silhouette_avg[i] <- mean(silhouette_scores[, 3])
}
(best_k <- k_values[which.max(silhouette_avg)])
# silhouette tends to favour small k
plot(k_values, silhouette_avg, type="b", xlab="Numero di Cluster (k)", 
     ylab="Silhouette Media", main="Valutazione di k usando la Silhouette")
silhouette_avg
# excluding 2 which is maybe too low, also 4, 9 or 11 seems good

set.seed(1234)
km4 = kmeans(sglass,centers = 4,iter.max = 100,nstart = 100)
table(km4$cluster)
adjustedRandIndex(km4$cluster,mglass$classification)
# 0.26, even worse

set.seed(1111)
km9 = kmeans(sglass,centers = 9,iter.max = 100,nstart = 100)
table(km9$cluster)
adjustedRandIndex(km9$cluster,mglass$classification)
# 0.397, better

set.seed(1234)
km11 = kmeans(sglass,centers = 11,iter.max = 100,nstart = 100)
table(km11$cluster)
adjustedRandIndex(km11$cluster,mglass$classification)
# 0.348, a bit worse

#principal component plot with kmeans k = 9
plot(sprglass$scores,col=colors[km9$cluster],lwd=2.6)

# try with agglomerative
distances = as.dist(daisy(sglass))
silhouette_avg <- numeric(length(2:20))
for (i in 2:20) {
  result <- hclust(distances,method = "complete")
  clusters = cutree(result,i)
  silhouette_scores <- silhouette(clusters, distances)
  silhouette_avg[i-1] <- mean(silhouette_scores[, 3])
}
(best_k <- k_values[which.max(silhouette_avg)])
# silhouette tends to favour small k
plot(2:20, silhouette_avg, type="b", xlab="Numero di Cluster (k)", 
     ylab="Silhouette Media", main="Valutazione di k usando la Silhouette")
silhouette_avg

# complete with 5
complete5 = hclust(dist(sglass),method="complete")
clusters5 = cutree(complete5,5)
adjustedRandIndex(clusters5,mglass$classification)
table(clusters5)
# if in the complete we have this problem, with average or single
# linkage would be even worse. I'll exclude the agglomerative
# it's the same taking 11 clusters, which was my 2nd chance with complete method

# mds
colors = c("red","blue","green","black","purple","brown","yellow","pink","lightblue")

Mmds = mds(delta = dist(sglass),ndim = 2)
plot(Mmds$conf[, 1], Mmds$conf[, 2], type = "n", xlim = c(-1.25, 1.25), 
     xlab = "Dimension 1", ylab = "Dimension 2", main="9-means MDS")
points(Mmds$conf[, 1], Mmds$conf[, 2], col = colors[km9$cluster], pch = 19, lwd = 1.5)


plot(Mmds$conf[, 1], Mmds$conf[, 2], type = "n", xlim = c(-1.25, 1.25), 
     xlab = "Dimension 1", ylab = "Dimension 2", main="Mclust MDS, 4 mixtures")
points(Mmds$conf[, 1], Mmds$conf[, 2], col = colors[mglass$classification], pch = 19, lwd = 1.5)

adjustedRandIndex(km9$cluster,mglass$classification)

# cluster means 
# Grafico per 9-means
medie_cluster <- aggregate(sglass, by=list(cluster=km9$cluster), FUN=mean)
colnames(medie_cluster)[1] <- "Cluster"
print(medie_cluster)
cluster_labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4",
                    "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8","Cluster 9")
medie_cluster_t <- t(medie_cluster[, -1])

par(mar=c(5, 4, 6, 2))  
barplot(medie_cluster_t, beside=TRUE, col=colors,
        legend.text = FALSE,  
        ylab="Means", 
        main="Cluster means: k-means (k = 9)",
        names.arg = cluster_labels, 
        las=2, 
        border="white", ylim=c(-3,11))
grid(nx = NA, ny = NULL)
legend("top", legend=colnames(medie_cluster)[-1], 
       fill=colors, horiz=TRUE, bty="n")


# grafico per 4 mixtures
cluster_labels2 = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")
medie_cluster <- aggregate(sglass, by=list(cluster=mglass$classification), FUN=mean)
colnames(medie_cluster)[1] <- "Cluster"
print(medie_cluster)

medie_cluster_t <- t(medie_cluster[, -1])
par(mar=c(5, 4, 6, 2))  
barplot(medie_cluster_t, beside=TRUE, col=colors,
        legend.text = FALSE,  
        ylab="Means", 
        main="Cluster means: Mclust (k = 4)",
        names.arg = cluster_labels2, 
        las=2, 
        border="white", ylim=c(-2,5))
grid(nx = NA, ny = NULL)
legend("top", legend=colnames(medie_cluster)[-1], 
       fill=colors, horiz=TRUE, bty="n")

# so we have those 2 different clusterings
# i want to understand if the clusters could be spherical
# and with the same variances and orientation.
# If it's the case, the mixture model EII isn't so bad 
# compare to our VEV 4 (even better) and kmeans could be more appropriate

mglassEII <- Mclust(glass,G=1:25,modelNames="EII")
# Information available in output object, e.g.
mglassEII$classification
# Clustering vector
mglassEII$parameters$variance
# Estmated parameters
mglassEII$BIC
# with -2653 of BIC we are so far by the 4069 of the VEV 4
# so the VEV 4 is far better than EEI 13
summary(mglassEII)
# something interesting
# looking at the table of the model EII 12,
# there is a similarity with the agglomerative clustering, complete method:
# there a big cluster with 50% of the observations
# and the other half splitted into 11 clusters.
table(clusters5) 
table(HIERARCHICAL = clusters5,
      GMM = mglassEII$classification) # we can see clearly some similarities
# even if 
adjustedRandIndex(clusters5,mglassEII$classification) #0,162
adjustedRandIndex(km9$cluster,mglassEII$classification) #0.559
# good positive correlation with kmeans


# high correlation between this clustering (EII 12) with the previous one
# 9-means and similarities with the agglomerative one, complete with 5.
# Those 3 clusters seems to share something. All of them are quite
# different to my selected clustering (VEV 4). The kmeans with 
# k=9 could have been a good choice but we saw that 
# the clusters aren't spherical and with the same volume
# in my opinion in this case the gaussian mixture models
# could be more appropriate than the kmeans one.
# I think it's appropriate the GMM also because:
# GMMs can handle cluster overlaps well. Instead of assigning each point to a single cluster like K-means, GMMs assign a probability of belonging to each cluster for every point. This is useful when the boundaries between clusters are not clear, and there are transition zones between one cluster and another.
# K-means forces each point to belong exclusively to one cluster and calculates the distance from centroids using the Euclidean distance measure. This approach is not ideal if the clusters have different shapes or variances in different directions. GMMs offer greater flexibility compared to K-means, as they consider not only the centroid of the cluster but also the shape and orientation of the cluster through the covariance matrix.
# GMMs are more flexible, handling different cluster shapes.

# GMMs assume that all data points are generated from a mixture of a finite number of Gaussian distributions
# so the problems could arise if our data aren't generated from a finite number of gaussian distributions
# sometimes could be better methods like DBSCAN in order to use
# a density based cluster method. Moreover, if we know
# our data are of the same spherical shape, we can use kmeans as well

# in the end i don't like, in general, to have clusters with 2,4,5 units each
# and kmeans (with different k) produced that

# INTERPRET THE CLUSTERS I CHOOSE (VEV4) 
cluster_labels2 = c("CL 1,\nn=94", "CL 2, \nn=75", "CL 3, \nn=9", "CL 4, \nn=36")
medie_cluster <- aggregate(sglass, by=list(cluster=mglass$classification), FUN=mean)
colnames(medie_cluster)[1] <- "Cluster"
print(medie_cluster)

medie_cluster_t <- t(medie_cluster[, -1])
par(mar=c(5, 4, 6, 2))  
barplot(medie_cluster_t, beside=TRUE, col=colors,
        legend.text = FALSE,  
        ylab="Means", 
        main="Cluster means: Mclust (k = 4)",
        names.arg = cluster_labels2, 
        las=2, 
        border="white", ylim=c(-2,2.2))
grid(nx = NA, ny = NULL)
legend("top", legend=colnames(medie_cluster)[-1], 
       fill=colors, horiz=TRUE, bty="n")
# CLUSTER 1: indice di rifrazione positivo. Unico cluster
# ad avere questo valore insieme a quellE del calcio positivi!!
# Inoltre anche il ferro risulta positivo
# CLUSTER 2: E' il cluster col magnesio più alto, per il 
# resto nulla di particolare
# CLUSTER 3: magnesio positivo anche qui, ma in aggiunta 
# (rispetto al 2), abbiamo un elevatissimo valore del ferro
# CLUSTER 4: Very low value of Magnesio ma alte componenti di 
# Sodio, Alluminio e Bario
# Silicio e Potassio sembrano non discriminare molto


# FIZV VISUALIZATION 
mglass$classification 
fviz_cluster(mglass,
             geom = "point", 
             ellipse.type = "convex", 
             palette = "jco",         
             ggtheme = theme_minimal(), 
             main = "Gaussian mixture models Clustering (4 distributions)")
# from this plot we can clearly see the different shapes 
# fviz within his function make PCA using prcomp
# so the axis are the 1st 2 components explaining
# a bit more than 50 % of the variability, which is 
# quite low 

# MDS
colors2 = c("blue","yellow","grey","red")
plot(Mmds$conf[, 1], Mmds$conf[, 2], type = "n", xlim = c(-1.25, 1.25), 
     xlab = "Dimension 1", ylab = "Dimension 2", main="Mclust MDS, 4 mixtures")
points(Mmds$conf[, 1], Mmds$conf[, 2], col = colors2[mglass$classification], pch = 19, lwd = 1.5)

# 2 PCA graph
plot(sprglass$scores,col=colors2[mglass$classification],lwd=3,
     main = "Mclust PCA, 4 mixtures")

# look at the data
summary(glass)
round(diag(var(glass)),6)

par(mfrow = c(3, 3),
    mar = c(2, 2, 2, 2),  
    oma = c(1, 1, 1, 1)) 

for (i in 1:ncol(glass)){
  boxplot(glass[,i],main=paste("variable",i,colnames(glass)[i]))
}
for (i in 1:ncol(glass)){
  plot(density(glass[,i]),main=paste("variable",i,colnames(glass)[i]))
}

# ex 3 DBSCAN ####
library(fpc)
library(dbscan) 

glass<- read.table("glass.txt", header=T) 
sglass<- scale(glass) 
sprglass <- princomp(sglass) 

kNNdistplot(sglass, k = 4) # k chosen randomly 
abline(h=1.74, col="red") 

#try to figure out how clusterings change, moving the parameters
db <- dbscan(sglass, eps = 1.74, minPts = 5) 
plot(sprglass$scores,col=(db$cluster+1), pch=16, main="Eps=1.74 MinPts=5") 

par(mfrow=c(1,3))
db <- dbscan(sglass, eps = 2.74, minPts = 5) 
plot(sprglass$scores,col=(db$cluster+1), pch=16, main="Eps=2.74 MinPts=5") 
db <- dbscan(sglass, eps = 1.74, minPts = 5) 
plot(sprglass$scores,col=(db$cluster+1), pch=16, main="Eps=1.74 MinPts=5") 
db <- dbscan(sglass, eps = 0.74, minPts = 5) 
plot(sprglass$scores,col=(db$cluster+1), pch=16, main="Eps=0.74 MinPts=5") 


db <- dbscan(sglass, eps = 1.74, minPts = 12) 
plot(sprglass$scores,col=(db$cluster+1), pch=16, main="Eps=1.74 MinPts=12") 
db <- dbscan(sglass, eps = 1.74, minPts = 6) 
plot(sprglass$scores,col=(db$cluster+1), pch=16, main="Eps=1.74 MinPts=6") 
db <- dbscan(sglass, eps = 1.74, minPts = 3) 
plot(sprglass$scores,col=(db$cluster+1), pch=16, main="Eps=1.74 MinPts=3") 

## all in 1 plot
par(mfrow=c(2,3))
db <- dbscan(sglass, eps = 2.74, minPts = 5) 
plot(sprglass$scores,col=(db$cluster+1), pch=16, main="Eps=2.74 MinPts=5") 
db <- dbscan(sglass, eps = 1.74, minPts = 5) 
plot(sprglass$scores,col=(db$cluster+1), pch=16, main="Eps=1.74 MinPts=5") 
db <- dbscan(sglass, eps = 0.74, minPts = 5) 
plot(sprglass$scores,col=(db$cluster+1), pch=16, main="Eps=0.74 MinPts=5") 
db <- dbscan(sglass, eps = 1.74, minPts = 12) 
plot(sprglass$scores,col=(db$cluster+1), pch=16, main="Eps=1.74 MinPts=12") 
db <- dbscan(sglass, eps = 1.74, minPts = 6) 
plot(sprglass$scores,col=(db$cluster+1), pch=16, main="Eps=1.74 MinPts=6") 
db <- dbscan(sglass, eps = 1.74, minPts = 3) 
plot(sprglass$scores,col=(db$cluster+1), pch=16, main="Eps=1.74 MinPts=3") 
