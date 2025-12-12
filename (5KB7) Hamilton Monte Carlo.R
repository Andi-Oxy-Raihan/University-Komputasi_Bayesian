####Latihan 1

#1.
# Install jika belum ada
#install.packages("coda")
#install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)
library(coda)
library(rstan)

#2.
log_posterior <- function(theta){
  # log posterior (Gaussian)
  return(dnorm(theta, mean=0, sd=1, log=TRUE))
}

#3.
set.seed(123)
n_iter <- 5000
theta <- numeric(n_iter)
theta[1] <- 0
sigma_prop <- 0.5 # step size proposal

for (i in 2:n_iter) {
  theta_prop <- rnorm(1, mean=theta[i-1], sd=sigma_prop)
  alpha <- exp(log_posterior(theta_prop) - log_posterior(theta[i-1]))
  if (runif(1) < alpha) {
    theta[i] <- theta_prop
  } else {
    theta[i] <- theta[i-1]
  }
}

theta #Nilai Kinetik

# Plot hasil
plot(theta, type='l', main="Trace Plot: Metropolis", ylab="Theta", xlab="Iteration")

hist(theta, breaks=50, main="Histogram: Metropolis", xlab="Theta")
acf(theta, main="ACF: Metropolis")

#4.

# Model stan untuk Gaussian 1-dim
stan_code <- "
data {
  int<lower=0> N; // dummy
}
parameters {
  real theta;
}
model {
  theta ~ normal(0,1);
}
"

stan_data <- list(N = 1) # data dummy
fit <- stan(model_code = stan_code, data = stan_data, iter = 10000, chains = 1)

# Ekstrak hasil sampling
theta_hmc <- extract(fit)$theta

# Plot hasil
plot(theta_hmc, type='l', main="Trace Plot: HMC", ylab="Theta", xlab="Iteration")
hist(theta_hmc, breaks=50, main="Histogram: HMC", xlab="Theta")
acf(theta_hmc, main="ACF: HMC")



####Latihan 2
#1.
set.seed(123)

# Fungsi log posterior (Gaussian 1D)
log_post <- function(q) {
  return(dnorm(q, mean=0, sd=1, log=TRUE))
}

grad_log_post <- function(q) {
  # turunan log posterior
  return(-(q-0)/1^2) # - (q-mu)/sigma^2
}

# HMC parameters
n_iter <- 5000
epsilon <- 0.1   # step size
L <- 10          # leapfrog steps
theta <- numeric(n_iter)
theta[1] <- 0

for (i in 2:n_iter) {
  q <- theta[i-1]
  p <- rnorm(1,0,1)  # inisialisasi momentum
  
  q_new <- q
  p_new <- p
  
  # Leapfrog integrator
  p_new <- p_new - epsilon/2 * grad_log_post(q_new)
  for (l in 1:L) {
    q_new <- q_new + epsilon * p_new
    if (l != L) {
      p_new <- p_new - epsilon * grad_log_post(q_new)
    }
  }
  p_new <- p_new - epsilon/2 * grad_log_post(q_new)
  
  # Metropolis accept/reject
  H_old <- -log_post(q) + 0.5*p^2
  H_new <- -log_post(q_new) + 0.5*p_new^2
  if (runif(1) < exp(H_old - H_new)) {
    theta[i] <- q_new
  } else {
    theta[i] <- q
  }
}

# Visualisasi
par(mfrow=c(1,3))
plot(theta, type='l', main="Trace Plot HMC", ylab="Theta", xlab="Iteration")
hist(theta, breaks=50, main="Histogram HMC", xlab="Theta")
acf(theta, main="Autocorrelation HMC")
