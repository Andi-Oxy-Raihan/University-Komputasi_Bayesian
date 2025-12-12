####Latihan 1
#1.
set.seed(123)
n <- 50
x1 <- rnorm(n, 5, 2)
x2 <- rnorm(n, 3, 1)
x3 <- rnorm(n, 2, 1)
y <- 1 + 2*x1 + 0.5*x2 + rnorm(n, 0, 1)

data <- data.frame(y, x1, x2, x3)
head(data)

#2.
# Model 1: y ~ x1
m1 <- lm(y ~ x1, data = data)

# Model 2: y ~ x1 + x2
m2 <- lm(y ~ x1 + x2, data = data)

# Model 3: y ~ x1 + x2 + x3
m3 <- lm(y ~ x1 + x2 + x3, data = data)

# Model 4: y ~ x2 + x3
m4 <- lm(y ~ x2 + x3, data = data)

# Model 4: y ~ x1 + x3
m5 <- lm(y ~ x1 + x3, data = data)

#3.
# Hitung AIC
aic_values <- c(AIC(m1), AIC(m2), AIC(m3), AIC(m4), AIC(m5))

# Approximate marginal likelihood
ml <- exp(-0.5 * aic_values)
ml

#4.
# Misal prior sama untuk semua model
prior <- rep(1/3, 5)

# Posterior model probability
pmp <- (ml * prior) / sum(ml * prior)
pmp

#5.
barplot(pmp, names.arg = c("M1", "M2", "M3", "M4","M5"),
        col = "skyblue", main = "Posterior Model Probability",
        ylab = "P(M|D)")


####Latihan 2
#1
set.seed(123)
n <- 50
x1 <- rnorm(n, mean=5, sd=2)
x2 <- rnorm(n, mean=10, sd=3)
x3 <- rnorm(n, mean=0, sd=1)
y  <- 2 + 1.5*x1 - 0.8*x2 + rnorm(n, mean=0, sd=1)
data <- data.frame(y, x1, x2, x3)


#2
# Install package jika belum ada
install.packages("BMA")
install.packages("ggplot2")
library(BMA)
library(ggplot2)

# Bayesian Model Averaging dengan prior BIC
bma_model <- bicreg(x = data[,c("x1","x2","x3")], y = data$y, strict = FALSE) #Disesuikan sama variabel
summary(bma_model)


r#3
# Lihat PMP setiap model
bma_model$postprob


#4.
# Prediksi gabungan BMA
y_bma <- predict(bma_model, newdata = data[,c("x1","x2","x3")])
head(y_bma$mean)  # Prediksi gabungan
#jika posterior probability > 50, maka variabel tersebut signifikan
# BIC: Semakin rendah, Semakin bagus

#5
# Buat data frame untuk visualisasi
model_names <- paste0("M", 1:length(bma_model$postprob))
pmp_df <- data.frame(Model = model_names, PMP = bma_model$postprob)

# Plot
ggplot(pmp_df, aes(x=Model, y=PMP)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal() +
  labs(title="Posterior Model Probability (PMP)", y="PMP", x="Model")

