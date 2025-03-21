library(ggplot2)
data("iris")
iris_data <- iris[, -5]

set.seed(58)

kmeans_result <- kmeans(iris_data, centers = 3, nstart = 25)
print(kmeans_result)

iris$cluster <- as.factor(kmeans_result$cluster)

table(iris$Species, iris$cluster)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(size = 3) + 
  ggtitle("K-means Clustering") + 
  theme_minimal()