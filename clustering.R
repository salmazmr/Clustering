#Import library
library(cluster)    
library(factoextra)
library(ggplot2)
library(dplyr)
library(clusterSim)
library(ppclust)
library(clValid)
#Import data
data = read.csv("C:/Users/ASUS/Desktop/data-kemiskinan-provinsi.csv",row.names="Kabupaten.Kota")
head(data)
#Menangani missing value
data = na.omit(data)
head(data)
#Melihat deskripsi data
str(data)
summary(data)
#Standarisasi data
datafix=scale(data)
head(datafix)
#Menentukan K optimal dengan metode Elbow dan Silhouette
fviz_nbclust(datafix, kmeans, method = "wss") 
fviz_nbclust(datafix, kmeans, method = "silhouette")
#----KMeans-----
km_res = kmeans(datafix, centers=3)
km_res
#----CMeans-----
fcm_res = fanny(x = datafix, k = 3)
fcm_res

fviz_cluster(km_res, data=datafix, repel = T, ellipse.type = "euclid", star.plot = T, ggtheme = theme_minimal())
fviz_cluster(fcm_res, data=datafix, repel = T, ellipse.type = "euclid", star.plot = T, ggtheme = theme_minimal())
#Davies Bouldins Index
d <-dist(datafix)
dbi_kmean = index.DB(datafix, km_res$cluster, d, centrotypes = 'centroids')
dbi_cmean = index.DB(datafix, fcm_res$cluster, d, centrotypes = 'centroids')
cat("Davies Bouldins KMeans= ",dbi_kmean$DB)
cat("Davies Bouldins CMeans= ",dbi_cmean$DB)
#Dunn Index
dunn_km <- dunn(clusters = km_res$cluster, Data = datafix)
dunn_fcm <- dunn(clusters = fcm_res$cluster, Data = datafix)
cat("Dunn KMeans= ",dunn_km)
cat("Dunn CMeans= ",dunn_fcm)
#Calinski-Harabasz Index
chi_kmean = index.G1(datafix, km_res$cluster, d, centrotypes = 'centroids')
chi_cmean = index.G1(datafix, fcm_res$cluster, d, centrotypes = 'centroids')
cat("CH KMeans= ",chi_kmean)
cat("CH CMeans= ",chi_cmean)
