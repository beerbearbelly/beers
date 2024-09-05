data(faithful)
head(faithful)
summary(faithful)
faithful_scaled <- scale(faithful)
wss <- (nrow(faithful_scaled)-1)*sum(apply(faithful_scaled,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(faithful_scaled, centers=i)$withinss)


par(mar = c(8, 8, 8, 8))

plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within sum of squares",
     main="Elbow Method",
     pch=19, frame=FALSE, col="blue",
     cex.lab=3,  # 调整 xlab 和 ylab 的字体大小
     cex.main=3)   # 调整 main 的字体大小
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
abline(v=3, col="red", lty=2)
abline(v=2, col='red', lty=2)
legend("topright", legend=c("WSS"), col="blue", pch=19, bty="n")

set.seed(99) 
kmeans_result <- kmeans(faithful_scaled, centers=3)

kmeans_result$cluster
kmeans_result$centers

# install.packages("ggplot2")
library(ggplot2)

faithful$cluster <- as.factor(kmeans_result$cluster)
cluster_colors <- c("1" = "red", "2" = "blue", "3" = "green")

ggplot(faithful, aes(x=eruptions, y=waiting, color=cluster)) + 
  geom_point(size=3) + 
  stat_ellipse(aes(group=cluster), type="norm", linetype=2) + 
  scale_color_manual(values = cluster_colors, guide = FALSE) +
  labs(title="K-means K=3 Clustering of Old Faithful Data", 
       x="Eruption Duration", 
       y="Waiting Time") +
  theme_minimal() +
  theme(
    text = element_text(size = 36) 
  )
