# install.packages('mclust')
# install.packages('ggplot2')

library('mclust')
library('ggplot2')

file_path <- "~/Downloads/chrome.csv"
df <- read.csv(file_path)
df_original <- df
custom_colors <- c("red", "blue", "green", "orange")
ggplot(df_original, aes(x = x, y = y, color = as.factor(color)))+
  geom_point()+
  scale_color_manual(values = custom_colors) +
  labs(title = 'GMM clustering of chrome dataset',
       x = '',
       y = '')+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 26),       
    axis.title.x = element_text(size = 26),     
    axis.title.y = element_text(size = 26),     
    axis.text.x = element_text(size = 26),      
    axis.text.y = element_text(size = 26),
    legend.position = "none")

df <- subset(df, select = -color)

num_repeats <- 10  
best_gmm <- NULL
best_loglik <- -Inf 

for (i in 1:num_repeats) {
  gmm <- Mclust(df)
  
  if (gmm$loglik > best_loglik) {
    best_gmm <- gmm
    best_loglik <- gmm$loglik
  }
}

cluster_label <- best_gmm$classification
df$cluster <- as.factor(cluster_label)

ggplot(df, aes(x = x, y = y, color = cluster)) +
  geom_point() +
  labs(title = 'GMM Clustering with Best Log-Likelihood',
       x = '',
       y = '') +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 26),       
    axis.title.x = element_text(size = 26),     
    axis.title.y = element_text(size = 26),     
    axis.text.x = element_text(size = 26),      
    axis.text.y = element_text(size = 26),
    legend.position = "none"
  )

means <- best_gmm$parameters$mean
sigmas <- best_gmm$parameters$variance$sigma
proportions <- best_gmm$parameters$pro

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
densities <- matrix(0, nrow = nrow(grid), ncol = best_gmm$G)
for (k in 1:best_gmm$G) {
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

ggplot(df, aes(x = x, y = y)) +
  geom_tile(data = density_df, aes(x = x, y = y, fill = densities)) +  
  scale_fill_viridis_c(option = "magma") +
  geom_point(data = df, aes(x = x, y = y), color = "white", size = 1.5, alpha = 0.4) +
  geom_point(data = centers_df, aes(x = x, y = y), color = "red", size = 3, shape = 16) +
  labs(title = "GMM Result with Mixture Gaussian Distribution",
       x = "",
       y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 26),       
    axis.title.x = element_text(size = 26),     
    axis.title.y = element_text(size = 26),     
    axis.text.x = element_text(size = 26),      
    axis.text.y = element_text(size = 26),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA)   
  )








