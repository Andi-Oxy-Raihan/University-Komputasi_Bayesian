#Contoh Sederhana Teorema Bayes

#prior
prior_sakit <- 0.01
prior_sehat <- 1 - prior_sakit

#likelihood'
sensitivitas <- 0.90  #P (Posititf | Sakit)
spesifisitas <- 0.95  #P (Negatif | Sehat)
p_positif_diberi_sehat <- 1 - spesifisitas

#Total Evidence
p_positif <- (sensitivitas*prior_sakit) +
  (p_positif_diberi_sehat*prior_sehat)

#Posterior: P(Sakit | Positif)
posterior <- (sensitivitas*prior_sakit)/p_positif

cat("Peluang seorang benar-benar sakit jika hasilnya positif", posterior, "\n")