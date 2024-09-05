# install.packages('mclust')
# install.packages('ggplot2')

library('mclust')
library('ggplot2')

file_path <- "~/Desktop/Dissertation/blob.csv"
df <- read.csv(file_path)
df_original <- df
df <- subset(df, select = -color)

gmm <- Mclust(df)
summary(gmm)
cluster_label <- gmm$classification
df$cluster <- as.factor(cluster_label)

ggplot(df, aes(x = x, y = y, color = cluster))+
  geom_point()+
  labs(title = 'GMM clustering of blob dataset',
      x = 'x',
      y = 'y')+
  theme_minimal()

means <- gmm$parameters$mean
sigmas <- gmm$parameters$variance$sigma
proportions <- gmm$parameters$pro

dmvnorm <- function(x, mean, sigma) {
  k <- length(mean)
  det_sigma <- det(sigma)
  inv_sigma <- solve(sigma)
  coef <- 1 / ((2 * pi)^(k / 2) * sqrt(det_sigma))
  exp_term <- exp(-0.5 * t(x - mean) %*% inv_sigma %*% (x - mean))
  return(coef * exp_term)
}

x <- seq(min(df[,1]), max(df[,1]), length.out = 100)
y <- seq(min(df[,2]), max(df[,2]), length.out = 100)
grid <- expand.grid(x = x, y = y)
densities <- matrix(0, nrow = nrow(grid), ncol = gmm$G)
for (k in 1:gmm$G) {
  for (i in 1:nrow(grid)) {
    densities[i, k] <- dmvnorm(t(grid[i,]), as.matrix(means[,k]), sigmas[,,k])
  }
}

density_df <- data.frame(grid, densities = rowSums(densities * proportions))
centers_df <- data.frame(t(means), cluster = 1:ncol(means))
colnames(centers_df) <- c("x", "y", "cluster")

ggplot(df, aes(x = x, y = y)) +
  geom_contour_filled(data = density_df, aes(x = x, y = y, z = densities)) +
  geom_point(data = centers_df, aes(x = x, y = y), color = "red", size = 3, shape = 16) +  
  labs(title = "GMM Result of Blob Dataset with Mixture Gaussian Distribution",
       x = "",
       y = "") +
  theme_minimal() +
  scale_color_discrete(name = "Cluster")+
  theme(
    plot.title = element_text(size = 26),       
    axis.title.x = element_text(size = 26),     
    axis.title.y = element_text(size = 26),     
    axis.text.x = element_text(size = 26),      
    axis.text.y = element_text(size = 26),
    legend.position = "none")

