library(openxlsx)
setwd("C:/Users/tejas/Desktop/DS-2 (EXAM)")

data <- read.xlsx("Clustering.xlsx", sheet = 1)

#Creating Distance Matrix
dist_matrix <- dist(data, method = "euclidean")

#Applying Agglomerative algorithm
clust <- hclust(dist_matrix, method = "single")
plot(clust, cex = 0.6, hang = -1)  
