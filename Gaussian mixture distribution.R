# Load necessary libraries
library(mixtools)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Define mixture distribution parameters
mu <- c(-2, 2)       # Means of the components
sigma <- c(1, 1)    # Standard deviations of the components
lambda <- c(0.7, 0.3)  # Mixing proportions (weights)

# Generate sample data from the mixture distribution
n <- 200  # Sample size
mix_data <- rnormmix(n, lambda = lambda, mu = mu, sigma = sigma)

# Fit Gaussian mixture model to the data
fit <- normalmixEM(mix_data, k = 2)

# Extract fitted parameters from the model
mu_fit <- fit$mu
sigma_fit <- sqrt(fit$sigma)
lambda_fit <- fit$lambda

# Generate density estimates for plotting
x_range <- seq(min(mix_data), max(mix_data), length.out = 1000)
density_est <- sapply(x_range, function(x) {
  sum(lambda_fit * dnorm(x, mean = mu_fit, sd = sigma_fit))
})

# Create data frame for plotting
density_df <- data.frame(x = x_range, y = density_est)
sample_df <- data.frame(x = mix_data)
# Plot the density estimate of the Gaussian mixture distribution
ggplot() +
  stat_function(fun = function(x) lambda[1] * dnorm(x, mu[1], sigma[1]) + lambda[2] * dnorm(x, mu[2], sigma[2]),
                color = "blue", size = 1) +
  geom_point(data = sample_df, aes(x = x, y = 0), color = "red", size = 2, alpha = 0.5) +
  labs(x = "X", y = "Density") +
  theme_minimal()+
  theme(
    text = element_text(size = 32) 
  )

