nsim <- 1e5
t0 <- 20
n0 <- 3

N_sim <- rRetry(nsim)
T_sim <- rRequestTime(nsim)

A <- (N_sim < NDetails$NMAX | (sample(c(F, T), prob = c(probEsec, probSucc))))
B <- (T_sim <= t0)
C <- (N_sim <= n0)
D <- (N_sim >= 2)

Prob_A_emp <- mean(A)
Prob_B_emp <- mean(B)
Prob_C_emp <- mean(C)
Prob_AintB_emp <- mean(A & B)
Prob_AunionD_emp <- mean(A | D)

#b
Prob_A_intersect_B_empirical <- mean(A & B)

Prob_A_union_B_empirical <- mean(A | B)
Prob_A_union_B_formula  <- mean(A) + mean(B) - Prob_A_intersect_B_empirical

Prob_A_intersect_D_empirical <- mean(A & D)

Prob_A_union_D_empirical <- mean(A | D)
Prob_A_union_D_formula  <- mean(A) + mean(D) - Prob_A_intersect_D_empirical