setwd("F://R//files")
wine_data <- read.csv("wine.csv")
View(wine_data)

#PCA
cor(wine_data)

model_pca <- princomp(wine_data[,-1], cor=T, scores = T, covmat = NULL)

plot(model_pca)
biplot(model_pca)

#takking first three pca's

mydata <- cbind(wine_data, model_pca$scores[,1:3])
View(mydata)

#distance matrix

d <- dist(mydata[,c(15,16,17)])

#building model
model_clust <- hclust(d, method = "complete")

plot(model_clust, hang = -1)

groups <- cutree(model_clust, k = 3)

b <- rect.hclust(model_clust, k = 3, border = "red")

table(as.matrix(groups))

acc <- table(as.matrix(groups), mydata[,1])
sum(diag(acc))/sum(acc) #accuracy is 71%

#screw plot for k-means clustering

wss <- (nrow(mydata[,c(15,16,17)]-1)*sum(apply(mydata[,c(15,16,17)],2,var)))
for(i in 2:8)
  wss[i] <- sum(kmeans(mydata[,c(15,16,17)], centers = i)$withinss)
plot(wss, type = "b")
#kmeans model
library(animation)
km_model <- kmeans.ani(mydata[,c(15,16,17)], 3)

km_model$cluster
library(gmodels)
CrossTable(km_model$cluster, mydata[,1]) #95% same compared to the raw dataset
