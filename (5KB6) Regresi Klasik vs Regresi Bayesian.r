data <- marmot_dataset

# Variabel
x1 <- data$food_per_day
x2 <- data$wheel_hours
y  <- data$marmot_size

#1. OLS biasa
fit_ls <- lm(y ~ x1 + x2)
summary(fit_ls)

par(mfrow=c(1,2))

plot(x1, y,
     xlab = "Food per day",
     ylab = "Marmot Size",
     main = "Hubungan X1 vs Y",
     pch = 19, col = "steelblue")
abline(lm(y ~ x1), col="red", lwd=2)

plot(x2, y,
     xlab = "Wheel hours",
     ylab = "Marmot Size",
     main = "Hubungan X2 vs Y",
     pch = 19, col = "darkgreen")
abline(lm(y ~ x2), col="red", lwd=2)

#===============================================================================

#Regresi Bayesian
#2. MLE
log_likelihood <- function(params) {
  beta0 <- params[1]
  beta1 <- params[2]
  beta2 <- params[3]
  sigma <- params[4]
  
  mu <- beta0 + beta1*x1 + beta2*x2
  -sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))  # negatif untuk minimisasi
}

# initial guess
init <- c(0,0,0,1)

log_likelihood(init)

fit_mle <- optim(init,
                 log_likelihood,
                 method = "L-BFGS-B",
                 lower = c(-Inf, -Inf, -Inf, 0.0001))

fit_mle$par  # hasil MLE

#3. Plot OLS / MLE (pairwise scatter)
par(mfrow=c(1,2))
plot(x1, y, pch=19, main="Regresi MLE: x1 vs y",
     xlab="food_per_day", ylab="marmot_size")
abline(a=fit_mle$par[1], b=fit_mle$par[2], col="red", lwd=2)

plot(x2, y, pch=19, main="Regresi MLE: x2 vs y",
     xlab="wheel_hours", ylab="marmot_size")
abline(a=fit_mle$par[1], b=fit_mle$par[3], col="blue", lwd=2)

#2. MLE
log_likelihood <- function(params) {
  beta0 <- params[1]
  beta1 <- params[2]
  beta2 <- params[3]
  sigma <- params[4]
  
  mu <- beta0 + beta1*x1 + beta2*x2
  -sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))  # negatif untuk minimisasi
}

# initial guess
init <- c(0,0,0,1)

log_likelihood(init)

fit_mle <- optim(init,
                 log_likelihood,
                 method = "L-BFGS-B",
                 lower = c(-Inf, -Inf, -Inf, 0.0001))

fit_mle$par  # hasil MLE

#3. Plot OLS / MLE (pairwise scatter)
par(mfrow=c(1,2))
plot(x1, y, pch=19, main="Regresi MLE: x1 vs y",
     xlab="food_per_day", ylab="marmot_size")
abline(a=fit_mle$par[1], b=fit_mle$par[2], col="red", lwd=2)

plot(x2, y, pch=19, main="Regresi MLE: x2 vs y",
     xlab="wheel_hours", ylab="marmot_size")
abline(a=fit_mle$par[1], b=fit_mle$par[3], col="blue", lwd=2)

#===============================================================================

#2. Model Bayesian
library(rstanarm)

df <- data.frame(y=y, x1=x1, x2=x2)

fit_bayes <- stan_glm(
  y ~ x1 + x2,
  data = df,
  prior = normal(0, 10),             # prior beta
  prior_intercept = normal(0, 10),   # prior intercept
  prior_aux = exponential(1),        # prior sigma
  chains = 4, iter = 10000, cores = 4
)

summary(fit_bayes)

library(bayesplot)

posterior <- as.matrix(fit_bayes)

# Traceplot parameter utama
mcmc_trace(
  posterior,
  pars = c("(Intercept)", "x1", "x2"),
  n_warmup = 2000
)

mcmc_trace(
  posterior,
  pars = c("sigma"),
  n_warmup = 2000
)
