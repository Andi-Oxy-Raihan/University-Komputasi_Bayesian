####Latihan 1
#1.
set.seed(123)

# Fungsi target f(x) = 1/sqrt(2*pi) * exp(-x^2/2)
f <- function(x) (1/sqrt(2*pi)) * exp(-x^2 / 2)

# Jumlah simulasi
N <- 100000

# Sampling dari distribusi Uniform(-3, 3)
x <- runif(N, -3, 3)

# Estimasi integral
integral_mc <- mean(f(x)) * (3 - (-3))  # (b-a) * mean(f(x))

# Hasil
cat("Estimasi Integral (Monte Carlo Biasa):", integral_mc, "\n")


#2.
set.seed(456)

# Fungsi target f(x) = 1/sqrt(2*pi) * exp(-x^2/2)
f <- function(x) (1/sqrt(2*pi)) * exp(-x^2 / 2)

# Metropolis-Hastings Algorithm
mcmc_sampler <- function(n_iter = 10000, theta_init = 0, sd_prop = 1.0) {
  theta <- numeric(n_iter)
  theta[1] <- theta_init
  
  for (t in 2:n_iter) {
    # Proposal step: Gaussian proposal centered around current theta
    candidate <- rnorm(1, mean = theta[t-1], sd = sd_prop)
    
    # Acceptance probability
    alpha <- min(1, f(candidate) / f(theta[t-1]))
    
    # Accept or reject the candidate
    if (runif(1) < alpha) {
      theta[t] <- candidate
    } else {
      theta[t] <- theta[t-1]
    }
  }
  
  return(theta)
}

# Sampling using Metropolis-Hastings
samples <- mcmc_sampler(n_iter = 10000, theta_init = 0, sd_prop = 0.5)

# Estimasi integral dengan rata-rata sampel
integral_mcmc <- mean(f(samples))

# Hasil
cat("Estimasi Integral (MCMC):", integral_mcmc, "\n")

plot(samples, type='l')

#Latihan 2
1. 
set.seed(123)

# Fungsi target: distribusi normal standar
f <- function(x) (1/sqrt(2*pi)) * exp(-x^2 / 2)

# Metropolis-Hastings
mh_sampler <- function(n_iter = 10000, theta_init = 0, sd_prop = 1.0) {
  theta <- numeric(n_iter)
  theta[1] <- theta_init
  
  for (t in 2:n_iter) {
    # Proposal step: Gaussian proposal
    cand <- rnorm(1, mean = theta[t-1], sd = sd_prop)
    # Acceptance ratio
    alpha <- min(1, f(cand) / f(theta[t-1]))
    if (runif(1) < alpha) {
      theta[t] <- cand
    } else {
      theta[t] <- theta[t-1]
    }
  }
  
  return(theta)
}

# Sampling with Metropolis-Hastings
samples <- mh_sampler(n_iter = 10000)

# Plotting results
par(mfrow = c(1,2))
plot(samples, type = "l", main = "Trace Plot (MH)")
hist(samples, probability = TRUE, main = "Histogram of Samples (MH)")
curve(f(x), add = TRUE, col = "red", lwd = 2)

#2.
set.seed(456)

# Gibbs Sampling for Bivariate Normal (rho = 0.8)
gibbs_sampler <- function(n_iter = 10000, rho = 0.8) {
  x <- numeric(n_iter)
  y <- numeric(n_iter)
  
  # Initial values
  x[1] <- 0
  y[1] <- 0
  
  for (t in 2:n_iter) {
    # Update x given y
    x[t] <- rnorm(1, mean = rho * y[t-1], sd = sqrt(1 - rho^2))
    # Update y given x
    y[t] <- rnorm(1, mean = rho * x[t], sd = sqrt(1 - rho^2))
  }
  
  return(list(x = x, y = y))
}

# Sampling with Gibbs Sampling
gibbs_samples <- gibbs_sampler(n_iter = 10000, rho = 0.8)

# Plotting results
par(mfrow = c(1,2))
plot(gibbs_samples$x, gibbs_samples$y, pch = 16, cex = 0.5, main = "Scatter Plot (Gibbs)")
hist(gibbs_samples$x, probability = TRUE, main = "Histogram of X (Gibbs)")
hist(gibbs_samples$y, probability = TRUE, main = "Histogram of Y (Gibbs)")


#Latihan 3.
#1.
set.seed(123)

# Fungsi distribusi target: Normal Standar
pi_func <- function(x) {
  (1/sqrt(2*pi)) * exp(-x^2 / 2)
}

# Metropolis-Hastings Algorithm
mh_sampler <- function(n_iter = 10000, theta_init = 0, sd_prop = 1.0) {
  theta <- numeric(n_iter)
  theta[1] <- theta_init
  
  for (t in 2:n_iter) {
    cand <- rnorm(1, mean = theta[t-1], sd = sd_prop)
    alpha <- min(1, pi_func(cand) / pi_func(theta[t-1]))
    if (runif(1) < alpha) {
      theta[t] <- cand
    } else {
      theta[t] <- theta[t-1]
    }
  }
  
  return(theta)
}

# Sampling with Metropolis-Hastings
samples <- mh_sampler(n_iter = 10000)

# Estimasi rata-rata
mean_estimate <- mean(samples)
cat("Estimasi Rata-rata (Metropolis-Hastings):", mean_estimate, "\n")

#2.
# Plot Trace Plot untuk melihat konvergensi rantai
plot(samples, type = "l", main = "Trace Plot Metropolis-Hastings", 
     xlab = "Iteration", ylab = expression(theta))

#3.
# Plot Histogram untuk distribusi sampel
hist(samples, probability = TRUE, main = "Histogram dari Sampel (Metropolis-Hastings)", 
     xlab = expression(theta), col = "lightblue", breaks = 50)
curve(dnorm(x, mean = 0, sd = 1), add = TRUE, col = "red", lwd = 2)

# Plot ACF untuk memeriksa autocorrelation
acf(samples, main = "Autocorrelation Function (ACF)")


#4.
# Proposal Uniform
mh_sampler_uniform <- function(n_iter = 10000, theta_init = 0, a = -1, b = 1) {
  theta <- numeric(n_iter)
  theta[1] <- theta_init
  
  for (t in 2:n_iter) {
    cand <- runif(1, min = a, max = b)
    alpha <- min(1, pi_func(cand) / pi_func(theta[t-1]))
    if (runif(1) < alpha) {
      theta[t] <- cand
    } else {
      theta[t] <- theta[t-1]
    }
  }
  
  return(theta)
}

# Sampling dengan Proposal Uniform
samples_uniform <- mh_sampler_uniform(n_iter = 10000)


####Latihan 4
#1.
set.seed(123)

# Gibbs Sampling for Bivariate Normal
gibbs_sampler <- function(n_iter = 10000, rho = 0.8) {
  x <- numeric(n_iter)
  y <- numeric(n_iter)
  
  # Initial values
  x[1] <- 0
  y[1] <- 0
  
  for (t in 2:n_iter) {
    # Update x given y
    x[t] <- rnorm(1, mean = rho * y[t-1], sd = sqrt(1 - rho^2))
    # Update y given x
    y[t] <- rnorm(1, mean = rho * x[t], sd = sqrt(1 - rho^2))
  }
  
  return(list(x = x, y = y))
}

# Sampling with Gibbs Sampling
gibbs_samples <- gibbs_sampler(n_iter = 10000, rho = 0.8)

# Plotting results
par(mfrow = c(1,2))
plot(gibbs_samples$x, gibbs_samples$y, pch = 16, cex = 0.5, main = "Scatter Plot (Gibbs)")
hist(gibbs_samples$x, probability = TRUE, main = "Histogram of X (Gibbs)")
hist(gibbs_samples$y, probability = TRUE, main = "Histogram of Y (Gibbs)")

#2.
# Plot Trace Plot untuk melihat konvergensi rantai
par(mfrow = c(2,1))
plot(gibbs_samples$x, type = "l", main = "Trace Plot X (Gibbs)", xlab = "Iteration", ylab = "X")
plot(gibbs_samples$y, type = "l", main = "Trace Plot Y (Gibbs)", xlab = "Iteration", ylab = "Y")
par(mfrow = c(1,1))

#3.
# Plot Histogram untuk distribusi sampel
par(mfrow = c(1,2))
hist(gibbs_samples$x, probability = TRUE, main = "Histogram of X (Gibbs)", col = "lightblue", breaks = 50)
hist(gibbs_samples$y, probability = TRUE, main = "Histogram of Y (Gibbs)", col = "lightgreen", breaks = 50)

# Plot ACF untuk memeriksa autocorrelation
acf(gibbs_samples$x, main = "ACF X (Gibbs)")
acf(gibbs_samples$y, main = "ACF Y (Gibbs)")
par(mfrow = c(1,1))

#4.
# Sampling dengan beberapa nilai rho
gibbs_samples_rho_0.5 <- gibbs_sampler(n_iter = 10000, rho = 0.5)
gibbs_samples_rho_0.9 <- gibbs_sampler(n_iter = 10000, rho = 0.9)

# Plotting hasil untuk rho = 0.5
par(mfrow = c(1,2))
plot(gibbs_samples_rho_0.5$x, gibbs_samples_rho_0.5$y, pch = 16, cex = 0.5, main = "Scatter Plot (rho=0.5)")
hist(gibbs_samples_rho_0.5$x, probability = TRUE, main = "Histogram of X (rho=0.5)")
par(mfrow = c(1,1))

# Plotting hasil untuk rho = 0.9
par(mfrow = c(1,2))
plot(gibbs_samples_rho_0.9$x, gibbs_samples_rho_0.9$y, pch = 16, cex = 0.5, main = "Scatter Plot (rho=0.9)")
hist(gibbs_samples_rho_0.9$x, probability = TRUE, main = "Histogram of X (rho=0.9)")
par(mfrow = c(1,1))

