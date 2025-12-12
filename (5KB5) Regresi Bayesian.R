####Latihan 1
#1.
set.seed(123)
n <- 100
x <- rnorm(n, mean=5, sd=2)
beta0 <- 2
beta1 <- 3
sigma <- 1
y <- beta0 + beta1*x + rnorm(n, mean=0, sd=sigma)

#2.
fit_ls = lm(y~x)
summary(fit_ls)

#3.
log_likelihood <- function(params) {
  beta0 <- params[1]
  beta1 <- params[2]
  sigma <- params[3]
  -sum(dnorm(y, mean=beta0 + beta1*x, sd=sigma, log=TRUE)) # negatif untuk minimisasi
}

# initial guess
init <- c(0,0,1)
log_likelihood = log_likelihood(init)
log_likelihood

fit_mle <- optim(init, log_likelihood, method="L-BFGS-B", lower=c(-Inf,-Inf,0.0001))
fit_mle$par

#4.
plot(x, y, main="Regresi Linear (MLE)", pch=19)
abline(a=fit_mle$par[1], b=fit_mle$par[2], col="red", lwd=2)


####Latihan 2
#1.
set.seed(123)
n <- 100
x <- rnorm(n, mean=5, sd=2)
beta0 <- 2
beta1 <- 3
sigma <- 1
y <- beta0 + beta1*x + rnorm(n, mean=0, sd=sigma)

#2.
# install.packages("rstanarm") # jika belum terinstall
library(rstanarm)

# Model Bayesian Linear Regression
fit_bayes <- stan_glm(y ~ x, data = data.frame(x, y),
                      prior = normal(0, 10),      # prior untuk beta
                      prior_intercept = normal(0, 10),
                      prior_aux = exponential(1), # prior untuk sigma
                      chains = 4, iter = 10000)
summary(fit_bayes)

#3.
library(bayesplot)

posterior <- as.matrix(fit_bayes)
mcmc_areas(posterior, pars = c("x", "(Intercept)"))

#4.
# Trace plot
mcmc_trace(posterior, pars = c("x", "(Intercept)"))

# Autocorrelation plot
mcmc_acf(posterior, pars = c("x", "(Intercept)"))

#Cek Parameter
fit_bayes$stan_summary