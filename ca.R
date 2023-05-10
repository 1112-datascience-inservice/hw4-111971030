# library(ca)
# data(iris)
# 
# 
# # 轉換 iris 資料集中 Species 欄位為數值變數，1: setosa, 2: versicolor, 3: virginica
# iris$Species <- as.numeric(iris$Species)
# iris_ca <- iris[, 1:4]
# 
# # 使用 k-means 進行因子分析
# set.seed(888)
# iris_ca_kmean <- kmeans(iris_ca, center=3, nstart=20)
# 
# # 產生 CA 圖表
# ca_plot <- ca(iris_ca_kmean$centers, row.w = iris_ca_kmean$size, graph = TRUE )
# plot(ca_plot,  arrows=c(TRUE, FALSE))
# 
# 
# install.packages(c("FactoMineR", "factoextra","gplots"))
library("FactoMineR")
library("factoextra")
library("ggplot2")
data(iris)
iris$Species <- as.numeric(iris$Species)
set.seed(101)
irisCluster <- kmeans(iris[,1:4], center=3, nstart=20)
table(irisCluster$cluster, iris$Species)

res.ca <- CA(irisCluster, graph = FALSE)
fviz_ca_row(res.ca, repel = FALSE)
