
InegDetails = new.env()

#a markov


regenerateMarkovChebValues <- function(nsim = 1e5) {
  T_simulations <- as.numeric(rRequestTime(nsim))
  
  ET_empiric <- mean(T_simulations)
  
  ET_theretic <- integrate(
    function(x) x * dRequestTime(x),
    lower = 0, upper = Inf
  )$value
  
  P_empiric <- function(t) { mean(T_simulations >= t) }
  P_theoretic  <- function(t) { (1 - pRequestTime(t)) }
  
  Markov_empiric <- function(t) { min(ET_empiric / t, 1) }
  Markov_theretic  <- function(t) { min(ET_theretic  / t, 1) }
  
  t_values <- seq(50, 200, by = 10)
  
  #============================================#
  #a chebyshev
  mu_emp <- mean(T_simulations)
  var_emp <- var(T_simulations)
  
  # teoretic: E[T^2]
  E_T2_theoretic <- integrate(
    function(x) x^2 * dRequestTime(x),
    lower = 0, upper = Inf
  )$value
  
  var_th <- E_T2_theoretic - ET_theretic^2
  sd_th  <- sqrt(var_th)
  
  # P(|T - mu| >= r)
  P_emp_dev <- function(r) mean(abs(T_simulations - mu_emp) >= r)
  
  # teoretic: P(T <= mu - r) + P(T >= mu + r)
  P_th_dev <- function(r) {
    left  <- if (ET_theretic - r <= 0) 0 else pRequestTime(ET_theretic - r)
    right <- 1 - pRequestTime(ET_theretic + r)
    left + right
  }
  
  Cheb_emp_bound <- function(r) pmin(var_emp / (r^2), 1)
  Cheb_th_bound  <- function(r) pmin(var_th  / (r^2), 1)
  
  r_values <- seq(0.5, 3, by = 0.5) * sd_th
  
  InegDetails$markov_study <- data.frame(
    t = t_values,
    P_empirical   = sapply(t_values, P_empiric),
    P_theoretical = sapply(t_values, P_theoretic),
    Markov_empiric = sapply(t_values, Markov_empiric),
    Markov_theretic  = sapply(t_values, Markov_theretic)
  )
  
  InegDetails$cheb_study <- data.frame(
    r = r_values,
    P_empirical   = sapply(r_values, P_emp_dev),
    P_theoretical = sapply(r_values, P_th_dev),
    Cheb_emp_bound = sapply(r_values, Cheb_emp_bound),
    Cheb_th_bound  = sapply(r_values, Cheb_th_bound)
  )
  
  # d )
  
  InegDetails$phi_square_E_T <- (mean(T_simulations))^2        
  InegDetails$E_phi_square_T <- mean(T_simulations^2)          
  
  InegDetails$scale_factor <- max(mean(T_simulations), 1)
  
  InegDetails$phi_exp_E_T <- exp(mean(T_simulations) / InegDetails$scale_factor)   
  InegDetails$E_phi_exp_T <- mean(exp(T_simulations / InegDetails$scale_factor)) 
}



##### b)
regenerateChernoffValues <- function(nsimN = 1e5) {
  
  N_sims <- rRetry(nsimN)
  F_sims <- sapply(N_sims, function(n) {
    if (n < NDetails$NMAX)
      n - 1
    else 
      n - sample(c(0, 1), 1, prob = c(probSucc, probEsec))
  })
  
  NMAX <- NDetails$NMAX
  p <- probSucc
  q <- 1 - p
  
  MGF_F_emp <- function(t) mean(exp(F_sims * t))
  
  Chernoff_bound_empMGF <- function(t, a) MGF_F_emp(t) / exp(t * a)
  
  P_emp_F_ge <- function(a) mean(F_sims >= a)
  
  P_th_F_ge <- function(a) {
    #  F >= a
    if (a == NMAX)
      probEsec ^ NMAX
    else
      sum(dRetry((a+1):NMAX))
  }
  
  a_vals <- 0:(NMAX)
  t_grid <- c(0.1, 0.3, 0.5, 0.8, 0.9)
  
  InegDetails$chernoff_table <- do.call(rbind, lapply(t_grid, function(t) {
    data.frame(
      t = t,
      a = a_vals,
      P_theoretical = sapply(a_vals, P_th_F_ge),
      P_empirical   = sapply(a_vals, P_emp_F_ge),
      Chernoff_bound = sapply(a_vals, function(a) Chernoff_bound_empMGF(t, a))
    )
  }))
}


#c
#Atunci cand distributia exacta a unei variabile aleatoare este 
#necunoscuta, inegalitatile probabilistice precum Markov, Chebyshev si 
#Chernoff sunt utile deoarece permit obtinerea unor limite superioare 
#pentru probabilitati, fara a cunoaste forma distributiei. Markov si 
#Chebyshev folosesc doar informatii simple, precum media si varianta, 
#in timp ce inegalitatea de tip Chernoff ofera limite mai stricte 
#pentru probabilitatea aparitiei unor valori mari. Chiar daca aceste 
#limite pot fi conservatoare, ele furnizeaza garantii worst-case, 
#valabile indiferent de distributia exacta. Astfel, aceste inegalitati 
#sunt utile pentru evaluarea riscului si pentru luarea unor decizii 
#prudente atunci cand informatiile despre distributie sunt incomplete.



#e
#In contextul riscului, valorile mari ale timpului de raspuns au un 
#impact mai mare decat valorile mici in cazul unei functii convexe. 
#O crestere mica a timpului de raspuns pentru valorile mari produce 
#o crestere mai mare a costului, decat aceeasi crestere pentru 
#valorile mai mici. Astfel, valorile extreme sunt penalizate inegal.