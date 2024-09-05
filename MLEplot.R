set.seed(42)
n_samples <- 1000
weights <- c(0.5, 0.5)
means <- c(-5, 5)
std_devs <- c(1, 3)

data <- c(rnorm(n_samples * weights[1], means[1], std_devs[1]),
          rnorm(n_samples * weights[2], means[2], std_devs[2]))
log_likelihood <- function(params, data) {
  mean1 <- params[1]
  mean2 <- params[2]
  std1 <- std_devs[1]
  std2 <- std_devs[2]
  w1 <- weights[1]
  w2 <- weights[2]
  
  ll <- log(w1 * dnorm(data, mean1, std1) + w2 * dnorm(data, mean2, std2))
  return(-sum(ll))
}
initial_means <- c(-1, 1)  # 初始猜测的均值
result <- optim(initial_means, log_likelihood, data = data)
mle_means <- result$par

print(paste("MLE for means:", paste(mle_means, collapse = ", ")))
library(ggplot2)
library(reshape2)

mean1_range <- seq(-10, 10, length.out = 400)
mean2_range <- seq(-10, 10, length.out = 400)
likelihood_values <- matrix(0, nrow = 400, ncol = 400)
for (i in 1:length(mean1_range)) {
  for (j in 1:length(mean2_range)) {
    likelihood_values[i, j] <- log_likelihood(c(mean1_range[i], mean2_range[j]), data)
  }
}

likelihood_values <- melt(likelihood_values)
likelihood_values$Var1 <- mean1_range[likelihood_values$Var1]
likelihood_values$Var2 <- mean2_range[likelihood_values$Var2]

min_index <- which(likelihood_values$value == min(likelihood_values$value), arr.ind = TRUE)

# 绘制图像并标记最小值位置
ggplot(likelihood_values, aes(x = Var1, y = Var2, z = value)) +
  geom_tile(aes(fill = value)) +
  geom_contour(color = "white") +
  geom_point(data = likelihood_values[min_index,], aes(x = Var1, y = Var2), color = "red", shape = 16, size = 5) +  # 标记全局最小值位置
  scale_fill_viridis_c() +
  labs(title = "MLE of Means for Gaussian Mixture Model",
       x = "Mean 1", y = "Mean 2", fill = "-LL") +
  theme_minimal()+
  theme(
    text = element_text(size = 32) 
  )

