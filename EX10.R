source("ProcesareCereri.R")

### EXERCITIUL 10


# a)

probCerereSucces <- 0.9
probCerereEsec <- 1 - probCerereSucces

ChurnDetails <- new.env()

ChurnDetails$MedieCereriUtilizatori <- 30
ChurnDetails$probChurn = 0.05

ChurnDetails$distribution = "geom" # / "emp"

ChurnDetails$m = 5
ChurnDetails$k = 2
ChurnDetails$emp_try = 1e5

.check_fer <- function(tries) {
  t <- sample(c(0, 1), tries, replace=T, prob = c(probCerereSucces, probCerereEsec))
  
  m <- ChurnDetails$m
  k <- ChurnDetails$k
  
  for (i in 1:(tries - m + 1))
  {
    if ( sum(t[i:(i+m-1)]) >= k) 
    {
      return(i+(m-1))
    }
  }
  return(0)
}

ChurnDetails$generateHist <- function() {
  t <- replicate(ChurnDetails$emp_try, {
    .check_fer(300)
  })
  
  hist(t, breaks = max(t), plot=F)
}

ChurnDetails$hist =  hist(1,1,plot=F)

regenerateChurn <- function () {
  probCerereSucces <- pRetry(NDetails$NMAX - 1) + probEsec ^ (NDetails$NMAX - 1) * probSucc
  ChurnDetails$hist = ChurnDetails$generateHist()
}

dChurn <- function (n) {
  stopifnot(all(n > 0))
  
  m <- ChurnDetails$m
  
  sapply(n, function(no) {
    if (ChurnDetails$distribution == "emp") 
    {
      if (no < m)
        return(0)
      
      if (is.na(ChurnDetails$hist$density[no - m + 1]))
        return(0)
      else
        ChurnDetails$hist$density[no - m + 1]  
    }
    else {
      dgeom(no, ChurnDetails$probChurn)
    }
  })
}



pChurn <- function(n) {
  stopifnot(all(n > 0))
  
  sapply(n, function(p) {
    if (ChurnDetails$distribution == "emp") {
      sum(dChurn(1:p))
    }
    else {
      pgeom(p, ChurnDetails$probChurn)
    }
  })
}

generateChurnDensityPlot <- function () {
  plot(1:60, dChurn(1:60), type="l", 
       xlab="Număr de cereri",
       ylab="Funcția de masă",
       col="blue")
}

generateChurnDistributionPlot <- function () {
  plot(1:60, pChurn(1:60), type="l",
       xlab="Număr de cereri",
       ylab="Funcția de repartiție",
       col="blue")
}

# b)
UserLeaveEmp <- function (tries) {
  users <- rpois(tries, ChurnDetails$MedieCereriUtilizatori)
  users <- sapply(users, function (usa) {max(1, usa)})

  sapply(users, function(usa) {
    pChurn(usa)  
  })
}

#c
#===============================================
#In primul scenariu, pierderea utilizatorilor este modelata folosind o distributie
#geometrica, dgeom si pgeom. La fiecare cerere, probabilitatea ca utilizatorul sa fie pierdut este constanta si este reprezentata de probChurn Pierderea utilizatorului in cadrul unei cereri este independenta de ce s a intamplat in cadrul cererilor anterioare. 
#Numarul de cereri pe care le poate face un utilizator este generat aleator cu functia rpois, folosind valoarea MedieCereriUtilizatori = 20. Astfel, in model, fiecare utilizator are un numar de cereri care urmeaza o distributie Poisson cu medie 20.
#Functia UserLeaveEmp foloseste numarul de cereri per utilizator si calculeaza probabilitatea ca utilizatorul sa fi fost pierdut pana la ultima cerere. 

#In al doilea scenariu, pierderea utilizatorilor depinde de esecuri. Daca intr-o fereastra de m=5 cereri apar cel putin k=2 cereri, atunci utilizatorul este pierdut. Deci, nu este suficient un singur esec pentru ca utilizatorul sa plece. 
#Functia .check_fer genereaza o succesiune de cereri de tip succes si esec si verifica fiecare grup de 5 cereri. La gasirea unei ferestre in care sunt cel putin 2 esecuri, functia intoarce momentul in care utilizatorul pleaca. 
#Functia generateHist construieste o distributie empirica a momentului de pierdere al utilizatorului. Functiile dChurn si pChurn folosesc distributia pentru a calcula probabilitatea ca utilizatorul sa se fi pierdut pana la un anumit numar de cereri. 

#Diferenta dintre cele 2 scenarii este ca in primul scenariu pierderea depinde de esecuri izolate, iar in al doilea scenariu depinde de mai multe esecuri din aceeasi fereastra de m cereri.
#Cel de-al doilea scenariu este mai realist intrucat un esec izolat nu este suficient pentru o pierdere, probabilitatea de churn fiind mai mica. 


