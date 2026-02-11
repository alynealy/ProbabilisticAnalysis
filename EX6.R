###==================================================================
### Exercitiul 6 - Probabilitati conditionate si conditionari 
###==================================================================



# Parametrii specifici
NDetails$NMAX <- 5
SDetails$distribution <- "exp" # sau "norm"
t0 <- 150  # SLA 
n0 <- 5    # Prag pentru „putine incercari"

# Numar mare de simulari
n_sim <- 100000

# == Generare date ==
# Trebuie sa generam perechi (N, T, Outcome)

# Generam N (numarul de incercari)
# rRetry returneaza cate incercari s-au facut (1..NMAX)
n_vec <- rRetry(n_sim)

# Generam T (Timpul total) conditionat de N
# Functia rRequestTimeWithNTries e vectorizata pe ”n”
# Returneaza timpul total (suma + backoff-uri)
t_vec <- rRequestTimeWithNTries(n_vec, 1) # generam cate 1 timp pt fiecare n din vector

# Determinam rezultatul (Succes vs Esec)
# Daca n < NMAX -> Sigur e succes (ca geometrica se opreste la succes)
# Daca n == NMAX -> Poate fi succes (la ultima) SAU esec (dupa ultima)
# Trebuie sa dam cu banul pentru cei care au ajuns la limita

is_success <- rep(TRUE, n_sim) # Presupunem initial ca toti au reusit

# Facem o masca pt cei care au ajuns la capat
mask_limit <- (n_vec == NDetails$NMAX)

# Pentru cei de la limita, dam cu banul (Bernoulli) cu probSucc
# Daca iese 1 e succes, daca e 0 e esec
outcome_limit <- rbinom(sum(mask_limit), 1, probSucc)

# Updatam vectorul de succes doar pentru cei de la limita
# Daca outcome_limit e 0 (esec), punem FALSE
is_success[mask_limit] <- (outcome_limit == 1)

# Acum avem n_vec, t_vec, is_success

###==================================================================
### ESTIMARE PROBABILITATI CONDITIONATE
###==================================================================

# Definim evenimentele logic 
Event_A <- is_success           # Succes
Event_B <- (t_vec <= t0)        # Timp bun (SLA)
Event_C <- (n_vec <= n0)        # Putine retry-uri

# Helper function pentru probabilitate conditionata P(X | Y)
# Formula: P(X | Y) = Nr_cazuri(X si Y) / Nr_cazuri(Y)

calc_cond_prob <- function(cond_vector, target_vector) {
  # doar cazurile unde conditia e TRUE
  subset_target <- target_vector[cond_vector]
  
  # proportia
  return(mean(subset_target))
}

# --- P(A | N <= n0) ---
p_A_given_C <- calc_cond_prob(Event_C, Event_A)
cat("\nProbabilitatea de Succes (A) dat fiind putine incercari (N <= ",n0,"):\n")
cat("Valoare:", p_A_given_C, "\n")
# Ar trebui sa fie 1. Daca am facut 2 incercari si m-am oprit, 
# inseamna clar ca a 2-a a reusit. Nu ma pot opri la 2 cu Esec, Esecul e doar la 5

# --- P(B | A) ---
p_B_given_A <- calc_cond_prob(Event_A, Event_B)
cat("\nProbabilitatea sa respectam SLA (B) dat fiind ca am avut Succes (A):\n")
cat("Valoare:", p_B_given_A, "\n")


###==================================================================
### MEDII CONDITIONATE 
###==================================================================

# E[T | I = 1] -> Timpul mediu pentru cei cu SUCCES
mean_T_success <- mean(t_vec[is_success])

# E[T | I = 0] -> Timpul mediu pentru cei cu ESEC
mean_T_fail <- mean(t_vec[!is_success])

cat("\nMedii Conditionate:\n")
cat("Timpul mediu pt SUCCES (I=1):", round(mean_T_success, 2), "ms\n")
cat("Timpul mediu pt ESEC   (I=0):", round(mean_T_fail, 2), "ms\n")

###==================================================================
### INTERPRETARE
###==================================================================

cat("\n=== INTERPRETARE ===\n")
diff_ratio <- mean_T_fail / mean_T_success
cat("Utilizatorii care intampina o eroare finala asteapta, in medie, de", 
    round(diff_ratio, 1), "ori mai mult decat cei fericiti.\n")
cat("Nu doar ca nu primesc serviciul, dar pierd si cel mai mult timp.\n")
cat("Motivul: Esecul implica obligatoriu parcurgerea tuturor celor", NDetails$NMAX, 
    "incercari + toate backoff-urile.\n")


boxplot(t_vec ~ is_success, 
        names = c("Esec (I=0)", "Succes (I=1)"),
        col = c("red", "green"),
        main = "Timpul total: Esec vs Succes",
        ylab = "Timp (ms)")