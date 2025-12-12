#Latihan 1
# 1. Hitunglah peluang marginal dan bersyarat dari table distribusi berikut.

# Membuat tabel frekuensi dua variabel
# Misal variabel A = Cuaca (Cerah, Hujan)
# Variabel B = Aktivitas (Olahraga, Tidak Olahraga)
data_matrix <- matrix(c(30, 10, 20, 40),
                      nrow = 2,
                      byrow = TRUE)
rownames(data_matrix) <- c("Cerah", "Hujan")
colnames(data_matrix) <- c("Olahraga", "Tidak_Olahraga")

data_matrix

# Hitung total data
total_data <- sum(data_matrix)

# Peluang marginal untuk Cuaca
peluang_marginal_cuaca <- rowSums(data_matrix) / total_data
peluang_marginal_cuaca

# Peluang marginal untuk Aktivitas
peluang_marginal_aktivitas <- colSums(data_matrix) / total_data
peluang_marginal_aktivitas

# Peluang bersyarat: P(Olahraga | Cerah)
P_olahraga_dengan_cerah <- data_matrix["Cerah", "Olahraga"] / sum(data_matrix["Cerah", ])
P_olahraga_dengan_cerah

#--------------------------------------------------------------------------------------------

# 2. Menggunakan data acak untuk menghitung peluang

# Simulasi 2
# Simulasi data 100 orang
set.seed(42) # Tambahan set.seed untuk hasil yang dapat direproduksi
cuaca <- sample(c("Cerah", "Hujan"), size = 100, replace = TRUE, prob = c(0.6, 0.4))
aktivitas <- sample(c("Olahraga", "Tidak_Olahraga"), size = 100, replace = TRUE, 
                    prob = c(0.5, 0.5))

# Buat tabel kontingensi
tabel <- table(cuaca, aktivitas)
tabel

# Hitung peluang marginal
prop.table(tabel, margin = 1) # Peluang bersyarat berdasarkan baris (Cuaca)

# Peluang bersyarat P(Olahraga | Hujan)
P_olahraga_hujan <- tabel["Hujan", "Olahraga"] / sum(tabel["Hujan", ])
P_olahraga_hujan

#--------------------------------------------------------------------------------------------

# Latihan 2

# 2. Menggunakan data acak untuk menghitung peluang

# Simulasi 2
# Simulasi data 100 orang
set.seed(42) # Tambahan set.seed untuk hasil yang dapat direproduksi
cuaca <- sample(c("Cerah", "Hujan"), size = 100, replace = TRUE, prob = c(0.6, 0.4))
aktivitas <- sample(c("Olahraga", "Tidak_Olahraga"), size = 100, replace = TRUE, 
                    prob = c(0.5, 0.5))

# Buat tabel kontingensi
tabel <- table(cuaca, aktivitas)
tabel

# Hitung peluang marginal
prop.table(tabel, margin = 1) # Peluang bersyarat berdasarkan baris (Cuaca)

# Peluang bersyarat P(Olahraga | Hujan)
P_olahraga_hujan <- tabel["Hujan", "Olahraga"] / sum(tabel["Hujan", ])
P_olahraga_hujan

# 1. Distribusi Binomial
# Parameter
n <- 10 # jumlah percobaan
p <- 0.3 # peluang sukses
x <- 0:n # Membuat rentang nilai x
# Probabilitas
prob_binom <- dbinom(x, size = n, prob = p)
# Visualisasi
barplot(prob_binom, names.arg = x,
        main = "Distribusi Binomial (n=10, p=0.3)",
        xlab = "Jumlah Sukses", ylab = "Probabilitas")

# 2. Distribusi Poisson
# Parameter
lambda <- 4
# Rentang nilai
x <- 0:10
prob_pois <- dpois(x, lambda)
# Visualisasi
barplot(prob_pois, names.arg = x,
        main = "Distribusi Poisson (lambda=4)",
        xlab = "Jumlah Kejadian", ylab = "Probabilitas")

# 3. Distribusi Normal
# Parameter
mean_val <- 0
sd_val <- 1
# Rentang nilai
x <- seq(-4, 4, length=100)
y <- dnorm(x, mean = mean_val, sd = sd_val)
# Visualisasi
plot(x, y, type = "l", col = "blue", lwd = 2,
     main = "Distribusi Normal Standar",
     xlab = "X", ylab = "Densitas")

# 4. Distribusi Eksponensial
# Parameter
lambda <- 1
# Rentang nilai
x <- seq(0, 5, length=100)
y <- dexp(x, rate = lambda)
# Visualisasi
plot(x, y, type = "l", col = "red", lwd = 2,
     main = "Distribusi Eksponensial (lambda = 1)",
     xlab = "X", ylab = "Densitas")