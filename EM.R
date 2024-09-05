library(MASS)

# E-step
e_step <- function(data, parameters) {
  k <- parameters$k
  mu <- parameters$mu
  sigma <- parameters$sigma
  pi <- parameters$pi
  n <- nrow(data)
  d <- ncol(data)
  
  gamma <- matrix(0, n, k)  # 初始化后验概率矩阵 4086*4
  for (i in 1:k) {x
    gamma[, i] <- pi[i] * dmvnorm(data, mean = mu[i, ], sigma = sigma[,, i])
  }
  
  gamma <- gamma / rowSums(gamma) # standardize
  return(gamma)
}

# M-step
m_step <- function(data, posterior_probabilities) {
  n <- nrow(data)
  d <- ncol(data)
  k <- ncol(posterior_probabilities)
  
  Nk <- colSums(posterior_probabilities)
  
  pi <- Nk / n
  
  mu <- matrix(0, k, d)
  for (i in 1:k) {
    mu[i, ] <- colSums(data * posterior_probabilities[, i]) / Nk[i] # update mean vector
  }
  
  sigma <- array(0, dim = c(d, d, k))
  for (i in 1:k) {
    diff <- sweep(data, 2, mu[i, ]) # 2 means from the direction of column (data-mu[i,])
    sigma[,, i] <- t(diff) %*% (posterior_probabilities[, i] * diff) / Nk[i]
  }
  
  return(list(pi = pi, mu = mu, sigma = sigma))
}

# EM算法迭代过程
em_algorithm <- function(data, initial_parameters) {
  parameters <- initial_parameters
  improvement <- TRUE
  tol <- parameters$tol
  max_itr <- parameters$max_itr
  itr <- 0
  
  while (improvement && itr < max_itr) {
    posterior_probabilities <- e_step(data, parameters)# E-step
    new_parameters <- m_step(data, posterior_probabilities)# M-step
    improvement <- any(abs(unlist(new_parameters) - unlist(parameters)) > tol)# check convergence
    if (improvement) {
      parameters <- new_parameters
    }
    itr <- itr + 1
  }
  
  return(parameters)
}

# initial parameters and dataset
file_path <- "~/Desktop/Dissertation/blob.csv"
data <- read.csv(file_path)
data <- subset(data, select = -color)

initial_parameters <- list(
  k = 4,
  mu = data[sample(1:nrow(data), 4), ],
  sigma = array(rep(diag(ncol(data)), 4), dim = c(ncol(data), ncol(data), 4)),
  pi = rep(1/4, 4),
  max_itr = 100,
  tol = 1e-6
)

# EM algorithm
optimized_parameters <- em_algorithm(data, initial_parameters)

# print result
print(optimized_parameters)

