###===============================================
###
### Si -> timpul de raspuns la incercarea i
### este repartizata exponential sau normala, valorea default pentru medie este 30, iar in cazul normala
### varianta o luam 25
###
### Functii si detalii pentru Si:
### - SDetails$distribution = "exp" / "norm" (in functie de ce dorim)
### - SDetails$mean poate fi schimbata (de tinut minte ca si SDetails$rate trebuie recompilat)
### - dResponseTime(n) -> functia de masa a lui Si
### - pResponseTime(n) -> functia de repartitie a lui Si
### - rResponseTime(n) -> functia generator a lui Si
### - qResponseTime(n) -> functia cuantila a lui Si
###
###   Toate acestea folosesc distributia setata, iar d, p si q pot primi vectori ca parametri si returna 
### vectori (in spate pracitc e un sapply)
###
###
### Bi -> backoff intre incercari
### avem backoff(i) functie exponentiala pentru Bi
### avem si cazul in care primim B(NMAX) ca va returna 0 deoarece nu putem avea backoff la ultima 
### incercare
###
### N -> numarul total de incercari, seamana cu o geometrica(prob succes) si asa si este in afara de un caz
### limita atunci cand cerem P(N = NMAX), in cazul acesta avem probabilitatea sa avem: NMAX-1 esecuri si succes + 
### NMAX esecuri.
###
### Functii si dealii pentru N:
### - NDetails$NMAX = 5 -> Poate fi modificat la orice moment al aplicatiei
### - probSucc, probEsec = 0.4, 0.6 -> daca una e schimbata asa trebuie si cealalta
### - dRetry(n) -> functia de masa a lui N, orice n > NMAX are dRetry 0
### - pRetry(n) -> functia de repartitie a lui N
### - rRetry(n) -> functia generator a lui N
### - qretry(n) -> functia generator a lui N
###
###
###
### T -> Timpul total pana la success sau abandon
### functia de masa, funct de rep, generator
###
### Valorile lui T sunt strans legate de distributia lui Si (norm sau exp)
###
### Functii:
### o Avem prima oara functii pentru T | N (T conditionat de T)
###   - dRequestTimeWithNTries(n, time) 
###   - pRequestTimeWithNTries(n, time)
###   - rRequestTimeWithNTries(n, tries)
###
### o Functiile pentru T
###   - dRequestTime(time)
###   - pRequestTime(time)
###   - rRequestTime(time)
###===============================================

SDetails <- new.env()
SDetails$distribution = "exp"

probEsec <- 0.4
probSucc <- 1 - probEsec

# A { I = 1} -> succes -> P(N < NMAX) + P(succes la ultima incercare) -> p_esec^(NMAX - 1) * p_succ

SDetails$distribution = "exp" # sau "norm"
SDetails$mean = 30
SDetails$variation = 25                           # norm
SDetails$std_variation = sqrt(SDetails$variation) # norm
SDetails$rate = 1 / SDetails$mean

NDetails = new.env()
NDetails$distribution = "geom"
NDetails$NMAX = 5

backoff <- function(i) {
  stopifnot(all(i > 0))
  sapply(i, function(x) {
    if (x >= NDetails$NMAX)
      0
    else 
      exp(x)
  })
}

# Functia de Masa
dResponseTime <- function(n) {
  stopifnot(all(n >= 0))
  
  if (SDetails$distribution =="exp") {
    dexp(n, SDetails$rate)
  }
  else if (SDetails$distribution == "norm") {
    dnorm(n, SDetails$mean, SDetails$std_variation)
  }
}

# Functia de repartitie
pResponseTime <- function(n) {
  stopifnot(all(n > 0))
  
  if (SDetails$distribution =="exp") {
    pexp(n, SDetails$rate)
  }
  else if (SDetails$distribution == "norm") {
    pnorm(n, SDetails$mean, SDetails$std_variation)
  }
}

# Functia Generatoare
rResponseTime <- function(n) {
  stopifnot(all(n > 0))
  
  if (SDetails$distribution =="exp") {
    rexp(n, SDetails$rate)
  }
  else if (SDetails$distribution == "norm") {
    rnorm(n, SDetails$mean, SDetails$std_variation)
  }
}

# Functia Cuantila
qResponseTime <- function(n) {
  stopifnot(all(n >= 0))
  
  if (SDetails$distribution =="exp") {
    qexp(n, SDetails$rate)
  }
  else if (SDetails$distribution == "norm") {
    qnorm(n, SDetails$mean, SDetails$std_variation)
  }
}

#Functia de masa
dRetry <- function(n) {
  if (length(n) > 1) 
  {
    sapply(n, FUN=dRetry)
  }
  else {
    stopifnot(n >= 1)
    if (n > NDetails$NMAX) 
    {
      return(0)  
    }
    else
    {
      if (n < NDetails$NMAX) 
      {
        dgeom(n - 1, probSucc)
      }
      else 
      {
        probEsec ^ (NDetails$NMAX - 1)
      }
    }
  }
}

#Functia de repartitie
pRetry <- function(n) {
  stopifnot(n >= 1)
  
  if (length(n) > 1) 
  {
    sapply(n, FUN=pRetry)
  }
  else 
  {
    sum(dRetry(1:n))  
  }
}


rRetry <- function(n) {
  stopifnot(n >= 1)
  .d_prob_vec = dRetry(1:NDetails$NMAX)  
  sample(1:NDetails$NMAX, n, replace=T, prob=.d_prob_vec)
}

qRetry <- function(p) {
  stopifnot(all(p >= 0 & p <= 1))
  x <- 1:NDetails$NMAX
  
  cdf <- cumsum(dRetry(x))
  
  sapply(p, function(pi) {
    if (pi == 1) 
        NDetails$NMAX
    else
      x[which(cdf >= pi)[1]]
  })
}


### T -> RequestTime

# T -> Suma (Si) + backoff(i)
# T -> suma de la 1 la n de exp(l) ->> Gamma(n, l)

# T | N = n ~ SUM 1..n Si + SUM 1..n-1 backoff(i)

#
dRequestTimeWithNTries <- function(n, time) {
  stopifnot(all(n <= NDetails$NMAX))
  stopifnot(all(n > 0))
  stopifnot(time > 0)
  
  ## Momentan presupunem ca mereu avem exp
  sapply(n, function (no) {
    s <- 0
    if (no != 1)
      s <- sum(backoff(1:(no - 1)))
    
    sapply(time, function(t) {
      if (t <= s)
        0
      else
      {
        if (SDetails$distribution == "norm") 
          dnorm(t, mean = no * SDetails$mean, sd = sqrt(no) * SDetails$std_variation)
        else
          dgamma(t, no, SDetails$rate)
      }
    })
  })
}

pRequestTimeWithNTries <- function(n, time) {
  stopifnot(all(n <= NDetails$NMAX))
  stopifnot(all(n > 0))
  stopifnot(time > 0)
  
  ## Momentan presupunem ca mereu avem exp
  sapply(n, function (no) {
    s <- 0
    if (no != 1)
      s <- sum(backoff(1:(no - 1)))
    
    sapply(time, function(t) {
      if (s >= t)
        0
      else 
      {
        if (SDetails$distribution == "norm")
          pnorm(t - s, mean = no * SDetails$mean, sd = sqrt(no) * SDetails$std_variation)
        else
          pgamma(t - s, no, SDetails$rate)
      }
        
    })
  })
}
.b_vec = c()
for (i in 1:NDetails$NMAX) {
  if (i != 1) {
    .b_vec = c(.b_vec, sum(backoff(1:(i - 1))))
  }
  else {
    .b_vec = c(.b_vec, 0)
  }
}
.lastNMAX = NDetails$NMAX

rRequestTimeWithNTries <- function(n = 1, tries) {
  stopifnot(all(n <= NDetails$NMAX))
  stopifnot(all(n > 0))
  stopifnot(tries > 0)
  
  if (.lastNMAX != NDetails$NMAX) {
    .b_vec = c()
    for (i in 1:NDetails$NMAX) {
      if (i != 1) {
        .b_vec = c(.b_vec, sum(backoff(1:(i - 1))))
      }
      else {
        .b_vec = c(.b_vec, 0)
      }
    }
    .lastNMAX = NDetails$NMAX
  }
  
  if (SDetails$distribution == "norm")
    vapply(n, function (no) {
      rnorm(tries, mean = no * SDetails$mean, sd = sqrt(no) * SDetails$std_variation) + .b_vec[no]
    }, numeric(tries))
  else 
    vapply(n, function (no) {
      rgamma(tries, no, SDetails$rate) + .b_vec[no]
    }, numeric(tries))
}

dRequestTime <- function(time) {
  stopifnot(time > 0)
  n <- NDetails$NMAX
  sapply(time, function(t) {
    sum(dRequestTimeWithNTries(1:n, t) * dRetry(1:n))    
  })
}

pRequestTime <- function(time) {
  stopifnot(time > 0)
  n <- NDetails$NMAX
  sapply(time, function(t) {
    sum(pRequestTimeWithNTries(1:n, t) * dRetry(1:n))    
  })
}

rRequestTime <- function(tries) {
  stopifnot(tries > 0)
  
  n <- rRetry(tries)
  vapply(n, function(no) {
    rRequestTimeWithNTries(no, 1)
  }, numeric(1))
}