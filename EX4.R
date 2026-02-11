NFDetails <- new.env()

NFDetails$n_sim <- 1000

regenerateNF <- function () {
  #a
  NFDetails$rep_N <- rRetry(NFDetails$n_sim)
  print(NFDetails$rep_N)
  NFDetails$rep_F <- sapply(NFDetails$rep_N, function(n) {
    if (n < NDetails$NMAX)
      n - 1
    else 
      n - sample(c(0, 1), 1, prob = c(probSucc, probEsec))
  })
  
  NFDetails$p_table <- prop.table(
    table(NFDetails$rep_N, NFDetails$rep_F)
  )
  
  #b
  NFDetails$dist_N <- rowSums(NFDetails$p_table)
  NFDetails$dist_F <- colSums(NFDetails$p_table)
}

#c
NFDetails$test_independence <- function(tolerance = 1e-6) {
  P_indep <- outer(NFDetails$dist_N, NFDetails$dist_F)
  
  max(abs(NFDetails$p_table - P_indep)) < tolerance
}

#indep_result <- NFDetails$test_independence()
#d
NFDetails$gen_heatmap <- function() {
  df <- as.data.frame(NFDetails$p_table)
  colnames(df) <- c("N", "F", "prob")
  
  ggplot(df, aes(x = F, y = N, fill = prob)) +
    geom_tile() +
    scale_fill_viridis_c() +
    labs(
      title = "Distributia comuna empirica a variabilei (N, F)",
      x = "F – numar de esecuri",
      y = "N – numar total de incercari",
      fill = "Probabilitate"
    ) +
    theme_minimal()
}