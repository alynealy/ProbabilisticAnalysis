source("DailyActiveUsers.R")
source("ProcesareCereri.R")
source("EX10.R")

EconDetails <- new.env()

EconDetails$venit_per_succes <- 0.5    # Câștig per request reușit
EconDetails$cost_achizitie <- 50       # Pierdere per utilizator pierdut (LTV + Acquisition)
EconDetails$penalitate_SLA <- 2        # Cost per request care depășește t0 (SLA)
EconDetails$t0_SLA <- 100              # Pragul de timp pentru SLA
EconDetails$t <- c()

simulate_daily_profit <- function(day = 1, year = 2019, probUserLeave = 0.2) {
  
  kd <- rActiveUsers(1, day, year = year)
  
  requests_per_user <- rpois(kd, lambda = ChurnDetails$MedieCereriUtilizatori) 
  total_requests <- sum(requests_per_user)
  
  n_tries <- rRetry(total_requests)
  t_times <- rRequestTimeWithNTries(n_tries, 1)
  
  succese <- n_tries < NDetails$NMAX # | (runif(total_requests) < probSucc)

  venituri = sum(succese) * EconDetails$venit_per_succes
  
  penalitati = sum(t_times > EconDetails$t0_SLA) * EconDetails$penalitate_SLA
  
  prob_plecare <- probUserLeave
  utilizatori_pierduti <- rbinom(1, kd, prob_plecare)
  cost_churn = utilizatori_pierduti * EconDetails$cost_achizitie
  
  profit_zi = venituri - penalitati - cost_churn
  return(profit_zi)
}

regenerateProfitValues <- function(tries = 100, day = 1, year = 2019, probUserLeave = 0.2) {
  EconDetails$t <- replicate(tries, {
    simulate_daily_profit(day, year, probUserLeave)
  })
}
regenerateProfitValues(day = 1, year = 2019)

generatePlotAn <- function(year = 2019, probUserLeave = 0.2) {
  
  dt <- sapply(1:365, function(d) {
    simulate_daily_profit(d, year, probUserLeave)
  })
  
  plot(1:365, dt, type = "l", col = "green", main = paste("Profit în ", year),
       xlab="Zi", ylab="Profit")
}

#=========================================
#Profitul zilnic este o variabila aleatoare care depinde de numarul de cereri reusite, penalitatile SLA si de numarul de utilizatori pierduti. Este calculat ca diferenta intre veniturile obtinute din cereri reusite si costurile generate de penalitati si churn.
#Profitul zilnic este estimat prin simulare cu ajutorul functiei simulare_daily_profit, generand mai multe valori ale profitului. Media si varianta sunt calculate pe baza acestor valori. 
#Profitul creste atunci cand numarul de cereri este mai mare si scade din cauza penalitatilor SLA si a pierderii utilizatorilor. Pentru reducerea esecurilor si a timpilor mari de raspuns trebuie redus costul de churn.
