NTDetails <- new.env()

NTDetails$n_sim <- 1000

regenerateNT <- function() {
  NTDetails$rep_N <- rRetry(NTDetails$n_sim)
  NTDetails$rep_T <- rRequestTimeWithNTries(NTDetails$rep_N, 1)
  NTDetails$df_val <- data.frame(
    N = NTDetails$rep_N,
    T = NTDetails$rep_T
  )
  
  #b
  NTDetails$stats <- data.frame(
    mean_N = mean(NTDetails$rep_N),
    mean_T = mean(NTDetails$rep_T),
    var_N  = var(NTDetails$rep_N),
    var_T  = var(NTDetails$rep_T),
    covariance = cov(NTDetails$rep_N, NTDetails$rep_T),
    correlation = cor(NTDetails$rep_N, NTDetails$rep_T)
  )
}

regenerateNT()
#a
NTDetails$gen_graph <- function() {
  ggplot(NTDetails$df_val, aes(x = N, y = T)) +
    geom_jitter(alpha = 0.25, width = 0.15, color = "lightblue") +
    labs(
      title = "Reprezentarea bidimensionala a variabilei (N, T)",
      x = "N – numar total de incercari",
      y = "T – timpul total al cererii"
    ) +
    theme_minimal()
}


#c
#Coeficientul de corelatie dintre variabilele aleatoareN si T este pozitiv, 
#ceea ce indica faptul ca un numar mai mare de retry-uri conduce, in medie, la o latenta totala mai mare.
#Aceasta relatie este explicata de structura modelului, deoarece timpul total T
#este obtinut prin insumarea timpilor de raspuns si a perioadelor de backoff asociate fiecarei incercari.
#Totusi, corelatia nu este perfecta, deoarece atat timpii de raspuns, cat si mecanismul de backoff sunt aleatoare, 
#ceea ce face ca pentru aceeasi valoare a lui N latenta totala sa poata varia semnificativ.
#ex 5