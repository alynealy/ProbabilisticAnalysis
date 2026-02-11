

Details12 = new.env()
Details12$t_vals = c()
Details12$t_sim = 100
Details12$tCondEsec <- c()
Details12$tCondSucc <- c()

regenerateDetails12 <- function() {
  rep_N <- rRetry(Details12$t_sim)
  Details12$t_vals <- rRequestTimeWithNTries(rep_N, 1)
  
  
  Details12$tCondEsec <- rRequestTimeWithNTries(NDetails$NMAX, Details12$t_sim)
  Details12$tCondSucc <- rRequestTime(Details12$t_sim)
}


### a) Histograme pentru T si profit
# histrograma dupa densitate
generate12HistT <- function() {
  hist(Details12$t_vals, prob = T, breaks = 30, main = "Histograma lui T suprapusă de T teoretic",
       xlab="T (ms)", ylab="Probabilitate")
  lines(1:max(Details12$t_vals), dRequestTime(1:max(Details12$t_vals)), col = "blue")
}

generate12TCond <- function () {
  boxplot(
    Details12$tCondSucc,
    Details12$tCondEsec,
    names = c("Succes", "Eșec"),
    main = "T condiționat de succes vs eșec",
    ylab = "T",
    col = c(rgb(0.2, 0.8, 0.2, 0.5), rgb(0.9, 0.2, 0.2, 0.5)),
    border = c("darkgreen", "darkred"),
    lwd = 2
  )
}




### c) Interpretati mediana, IQR, outlieri