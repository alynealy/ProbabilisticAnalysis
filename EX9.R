###==================================================================
### Exercitiul 9 - Aproximare normala si agregare 
###==================================================================

# Alegem o zi cu trafic mare (ex: ziua 330 - Black Friday 50% off)
# CLT functioneaza bine cand 'n' (nr useri) e mare
AgregDetails = new.env()

AgregDetails$target_day <- 333
AgregDetails$target_year <- 2019
AgregDetails$n_sim <- 100
AgregDetails$vec <- c()


###==================================================================
### SIMULARE AGREGARI ZILNICE
###==================================================================

# Vrem sa obtinem un vector de 5000 de valori
# Fiecare valoare reprezinta "Timpul TOTAL de procesare" intr-o zi de tip Black Friday

# Functie helper 
sim_day_aggregate <- function(n) {
  
  # Cati useri vin azi? (V. a. Kd)
  k_users <- rActiveUsers(n, day = AgregDetails$target_day, year = AgregDetails$target_year)
  
  # Cat dureaza fiecare? (V. a. T)
  # rRequestTime e vectorizat, deci ii dam direct k_users
  
  vapply(k_users, function(users){
    sum(rRequestTime(users))
  }, numeric(1))
}


regenerateAgregValues <- function() {
  AgregDetails$vec <- sim_day_aggregate(AgregDetails$n_sim)
}

generateNormalAgreg <- function() {
  ###==================================================================
  ### COMPARATIE CU NORMALA
  ###==================================================================
  
  # Ca sa desenam curba Normala peste histograma, ne trebuie parametrii ei (mu si sigma)
  # Ii estimam din datele simulate 
  mu_est <- mean(AgregDetails$vec)
  sd_est <- sd(AgregDetails$vec)
  
  
  # --- Histograma vs Densitate Normala ---
  hist(AgregDetails$vec, 
       prob = TRUE, 
       breaks = 50, 
       col = "lightblue", 
       border = "white",
       main = paste("Agregat Ziua", AgregDetails$target_day, "(Trafic Mare)"),
       xlab = "Timp Total Procesare (ms)",
       ylab = "Densitate")
  
  # Generam puncte pt curba teoretica normala
  x_vals <- seq(min(AgregDetails$vec), max(AgregDetails$vec), length.out = 200)
  y_vals <- dnorm(x_vals, mean = mu_est, sd = sd_est)
  
  # Suprapunem linia rosie
  lines(x_vals, y_vals, col = "red", lwd = 3)
  legend("topright", legend=c("Empiric", "Normal Fit"), col=c("lightblue", "red"), lwd=2, bty="n")
}

generateNormalAgregLine <- function() {
  # --- QQ-Plot ---
  # Daca punctele stau pe diagonala, e Normala
  qqnorm(AgregDetails$vec, main = "QQ Plot - Verificare Normalitate")
  qqline(AgregDetails$vec, col = "red", lwd = 2)  
}


###==================================================================
### ANALIZA PENTRU TRAFIC MIC 
###==================================================================
# o zi proasta (trafic foarte mic) 
# sau fortam un N mic manual sa vedem diferenta

#test_low_traffic <- replicate(n_sim, {
#  # Fortam doar 3 useri (nu mai luam din KDetails)
#  sum(rRequestTime(3)) 
#})

# Desenam histograma pt trafic mic
# nu e clopot perfect
#dev.new()
#par(mfrow=c(1,1))
#hist(test_low_traffic, prob=TRUE, breaks=50, col="orange", border="white",
#     main = "Agregat pentru Trafic Mid (3 useri)",
#     xlab = "Timp Total (ms)")
#curve(dnorm(x, mean=mean(test_low_traffic), sd=sd(test_low_traffic)), 
#      add=TRUE, col="red", lwd=2)

###==================================================================
### INTERPRETARE
###==================================================================

#cat("\n=== REZULTATE ===\n")
#cat("Media agregatului (ziua", target_day, "):", round(mu_est, 2), "ms\n")
#cat("Deviatia standard:", round(sd_est, 2), "ms\n")

#cat("\n CONCLUZIE:\n")
#cat("   Pentru trafic MARE (ex: Black Friday, >100 useri/zi): Histograma se suprapune\n")
#cat("   aproape perfect cu curba rosie (Normala). Aproximarea este EXCELENTA.\n")

#cat("\n Pentru trafic MIC (ex: <10 useri): Histograma este asimetrica (coada spre dreapta).\n")
#cat("   Aproximarea normala este SLABA. Ar trebui folosita o distributie Gamma sau Logn.\n")