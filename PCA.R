library(calibrate)
library(FactoMineR)
data_iris<- iris
head(data_iris)
colSums(is.na(data_iris))
data_iris_1<- data_iris[,-5]

# Scale the data
# standardize <- function(x) {(x - mean(x))}
# data_iris.scaled <- apply(data_iris_1,2,function(x) (x-mean(x)))

# Find Eigen values & Eigen Vector of covariance matrix
cor<- cor(data_iris_1)
cov <- cov(data_iris_1)
eigen <- eigen(cov)
rownames(eigen$vectors) <- c("Sepal.length","Sepal.width","petal.length","petal.width")
colnames(eigen$vectors) <- c("PC1","PC2","PC3","PC4")
eigen

# Note that the sum of the eigen values equals the total variance of the data
sum(eigen$values)
var(data_iris_1[,1]) + var(data_iris_1[,2])+ var(data_iris_1[,3])+ var(data_iris_1[,4])

# The Eigen vectors are the principal components. We see to what extent each variable contributes
loadings <- eigen$vectors
pca_model <- prcomp(data_iris_1, scale. = T)                # pca_model<- PCA(data_iris_1)
////////////////////PCA(data_iris_1)$eig //////////////////////////
attributes(pca_model)
summary(pca_model)
pca_model$rotation

pca.var <- pca_model$sdev^2
pro_varx <- pca.var/sum(pca.var)
#This shows that first principal component explains 73% variance. 
#Second component explains 23% variance. Together


# Visualization
biplot(pca_model,scale = 0) 

# Scree Plot
plot(cumsum(pro_varx), 
     xlab = "Principle Component",
     ylab = "Cummulative Proportion of variance explained",
     type = "b")


# Predict
pca_pred<- predict(pca_model)