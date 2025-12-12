# ================================
# Simulasi Monte Carlo (versi manual)
# ================================

set.seed(123)

# 1️⃣ Jumlah simulasi
N <- 1000000   # <<< GANTI DI SINI >>> contoh: 500000, 600000, 700000, ..., 1000000

# 2️⃣ Nilai burn-in (jumlah awal yang diabaikan)
burnin <- 100000   # <<< GANTI DI SINI >>> contoh: 10, 1000, 10000, 100000

# 3️⃣ Sampling dari Uniform(0,1)
x <- runif(N, min = 0, max = 1)

# 4️⃣ Estimasi integral
I_est <- mean(x^2)
cat("Estimasi integral (Monte Carlo) =", I_est, "\n")

# 5️⃣ Estimasi kumulatif untuk konvergensi
est <- cumsum(x^2) / (1:N)

# 6️⃣ Abaikan burn-in
est_burn <- est[(burnin + 1):N]

# 7️⃣ Plot hasil konvergensi
plot(est_burn, type = "l", col = "blue",
     xlab = "Jumlah Sampel (setelah burn-in)",
     ylab = "Estimasi Integral",
     main = paste("Konvergensi Monte Carlo\nN =", format(N, big.mark = ","),
                  "| Burn-in =", format(burnin, big.mark = ",")))

# Garis nilai teoritis 1/3
abline(h = 1/3, col = "red", lty = 2, lwd = 2)

# Tambahkan legenda
legend("topright",
       legend = c("Estimasi Monte Carlo", "Nilai Teoritis (1/3)"),
       col = c("blue", "red"), lty = c(1, 2), bty = "n")
