sim_T_independence <- function() {
  as.numeric(rRequestTime(1))
}

sim_T_dependence <- function() {
  
  N <- rRetry(1)
  
  penalty_vector <- 1 + 0.5 * (0:(N - 1))
  
  response_times <- sapply(penalty_vector, function(penalty) {
    
    penalized_mean <- SDetails$mean * penalty
    
    if (SDetails$distribution == "exp") {
      rexp(1, rate = 1 / penalized_mean)
    } else {
      rnorm(1, mean = penalized_mean, sd = SDetails$std_variation)
    }
  })
  
  backoff_time <- if (N > 1)
    sum(backoff(1:(N - 1)))
  else
    0
  
  sum(response_times) + backoff_time
}

exercise_7 <- function(nsim = 5000) {
  
  #a
  T_indep <- replicate(nsim, sim_T_independence())
  T_dep   <- replicate(nsim, sim_T_dependence())
  
  #b Medii si variante
  stats <- data.frame(
    Scenariu = c("Independent", "Dependent"),
    Medie = c(mean(T_indep), mean(T_dep)),
    Varianta = c(var(T_indep), var(T_dep))
  )
  

  par(mfrow = c(1, 2))
  
  hist(T_indep, freq = FALSE, breaks = 20,
       col = "pink",
       main = "Scenariu Independent",
       xlab = "T - Timp de răspuns",
       border = "black")
  #lines(density(T_indep), lwd = 3)
  
  hist(T_dep, freq = FALSE, breaks = 20,
       col = "magenta",
       main = "Scenariu Dependent",
       xlab = "T - Timp de răspuns",
       border = "black")
  #lines(density(T_dep), lwd = 3)
  
  par(mfrow = c(1, 1))
}

