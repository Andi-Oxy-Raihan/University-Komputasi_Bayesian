data <- house

library(car)      # VIF
library(lmtest)   # BP test & Durbin-Watson

# Variabel
x1 <- data$Square_Footage
x2 <- data$Num_Bedrooms
x3 <- data$Num_Bathrooms
x4 <- data$Year_Built
x5 <- data$Neighborhood_Quality
y  <- data$House_Price

#1. OLS biasa
fit_ls <- lm(y ~ x1 + x2 + x3 + x4 + x5)
summary(fit_ls)

#Uji Lain
diagnostic_results <- list(
  VIF = vif(fit_ls),
  Shapiro_Wilk = shapiro.test(residuals(fit_ls)),
  Breusch_Pagan = bptest(fit_ls),
  Durbin_Watson = dwtest(fit_ls)
)

diagnostic_results

#===============================================================================
library(BMA)
library(ggplot2)

bma_model <- bicreg(
  x = data[, c("Square_Footage", "Num_Bedrooms", "Num_Bathrooms", "Year_Built", "Neighborhood_Quality")],
  y = data$House_Price,
  strict = FALSE
)
summary(bma_model)

#Lihat PMP setiap model
bma_model$postprob

#4.
# Prediksi gabungan BMA
y_bma <- predict(
  bma_model,
  newdata = data[, c("Hours.Studied", "Hours.Studied", "Extracurricular.Activities", "Sleep.Hours", "Sample.Question.Papers.Practiced")])
head(y_bma$mean)   # Prediksi gabungan BMA

#jika posterior probability > 50, maka variabel tersebut signifikan
# BIC: Semakin rendah, Semakin bagus

bma_model$probne0

#5
model_names <- paste0("M", 1:length(bma_model$postprob))

pmp_df <- data.frame(
  Model = model_names,
  PMP = bma_model$postprob
)

ggplot(pmp_df, aes(x = Model, y = PMP)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Posterior Model Probability (PMP)",
    y = "Posterior Model Probability",
    x = "Model"
  )

