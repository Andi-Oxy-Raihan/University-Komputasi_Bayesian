#Nomor 1

# Distribusi Geometrik
set.seed(123)

# Parameter
p <- 0.3      # peluang sukses
n <- 1000     # jumlah sampel

# Generate data acak
data_geom <- rgeom(n, prob = p)

# Histogram frekuensi
hist(data_geom, breaks = 20, col = "skyblue", main = "Distribusi Geometrik (p=0.3)",
     xlab = "Jumlah kegagalan sebelum sukses", ylab = "Frekuensi")

# Hitung rata-rata & varians sampel
mean_sample <- mean(data_geom)
var_sample <- var(data_geom)

# Nilai teoritis
mean_theory <- (1 - p) / p
var_theory <- (1 - p) / (p^2)

# Tampilkan hasil
cat("Rata-rata sampel   =", mean_sample, "\n")
cat("Rata-rata teori    =", mean_theory, "\n")
cat("Varians sampel     =", var_sample, "\n")
cat("Varians teori      =", var_theory, "\n")


#--------------------------------------------------------------------------------------------
#Nomor 2

#Distribusi Kontinu
set.seed(123)

# Parameter
a <- 0; b <- 10
n <- 1000

# Generate data
data_unif <- runif(n, min = a, max = b)

# Histogram + overlay densitas teoritis
hist(data_unif, probability = TRUE, col = "lightgreen",
     main = "Distribusi Uniform(0,10)", xlab = "X", ylab = "Densitas")
curve(dunif(x, min=a, max=b), col = "red", lwd = 2, add = TRUE)

# Hitung rata-rata & varians
mean_sample <- mean(data_unif)
var_sample <- var(data_unif)

# Teori
mean_theory <- (a + b) / 2
var_theory <- (b - a)^2 / 12

cat("Uniform\n")
cat("Mean sampel   =", mean_sample, " | Teori =", mean_theory, "\n")
cat("Var sampel    =", var_sample, " | Teori =", var_theory, "\n\n")

#--------------------------------------------------------------------------------------------
#Nomor 3

#Distribusi Gamma

set.seed(123)

# Parameter
alpha <- 2   # shape
beta <- 3    # scale
n <- 1000

# Generate data
data_gamma <- rgamma(n, shape = alpha, scale = beta)

# Histogram + overlay densitas
hist(data_gamma, probability = TRUE, col = "lightblue",
     main = "Distribusi Gamma(α=2, β=3)", xlab = "X", ylab = "Densitas")
curve(dgamma(x, shape = alpha, scale = beta), col = "red", lwd = 2, add = TRUE)

# Hitung rata-rata & varians
mean_sample <- mean(data_gamma)
var_sample <- var(data_gamma)

# Teori
mean_theory <- alpha * beta
var_theory <- alpha * (beta^2)

cat("Gamma\n")
cat("Mean sampel   =", mean_sample, " | Teori =", mean_theory, "\n")
cat("Var sampel    =", var_sample, " | Teori =", var_theory, "\n\n")


#Nomor 4
#Distribusi Chi-Square
set.seed(123)

# Parameter
n <- 1000
k_small <- 2   # derajat kebebasan kecil
k_large <- 20  # derajat kebebasan besar

# Generate data
data_chi_small <- rchisq(n, df = k_small)
data_chi_large <- rchisq(n, df = k_large)

# Histogram untuk k kecil
hist(data_chi_small, probability = TRUE, col = "orange",
     main = paste("Chi-Square (df=", k_small, ")", sep=""),
     xlab = "X", ylab = "Densitas")
curve(dchisq(x, df = k_small), col = "red", lwd = 2, add = TRUE)

# Histogram untuk k besar
hist(data_chi_large, probability = TRUE, col = "pink",
     main = paste("Chi-Square (df=", k_large, ")", sep=""),
     xlab = "X", ylab = "Densitas")
curve(dchisq(x, df = k_large), col = "blue", lwd = 2, add = TRUE)

#Nomor 5
#Distribusi Negatif Binomial

set.seed(123)

# Parameter
r <- 5     # jumlah sukses
p <- 0.4   # peluang sukses
n <- 1000

# Generate data
data_nb <- rnbinom(n, size = r, prob = p)

# Histogram
hist(data_nb, breaks = 20, col = "purple", main = "Distribusi Negatif Binomial",
     xlab = "Jumlah kegagalan", ylab = "Frekuensi")

# Hitung rata-rata & varians
mean_sample <- mean(data_nb)
var_sample <- var(data_nb)

# Teori
mean_theory <- r * (1 - p) / p
var_theory <- r * (1 - p) / (p^2)

cat("Negatif Binomial\n")
cat("Mean sampel   =", mean_sample, " | Teori =", mean_theory, "\n")
cat("Var sampel    =", var_sample, " | Teori =", var_theory, "\n\n")