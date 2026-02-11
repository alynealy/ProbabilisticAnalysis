## SDetails$distributie  exp / norm


# b) 

empSTheoreticalGraph <- function (tries, distr = NULL) {
  
  if (!is.null(distr))
    SDetails$distribution = distr
  
  t <- rResponseTime(tries)
  tsq <- seq(0, 100, by = 0.01)
  
  
  hist(t, prob=TRUE, breaks = 90, xlim=c(0, 90))
  lines(tsq, dResponseTime(tsq), col="blue", lwd=3)
  
}

# c)

empSStudy <- function(nsim = 1000) {
  sim <- rResponseTime(nsim)
  
  h <- hist(sim, breaks = nsim, plot = FALSE)
  mode_estimate <- h$mids[which.max(h$density)]
  
  data.frame(
    Mean_Empirical       = mean(sim),
    Variance_Empirical   = var(sim),
    Median_Empirical = median(sim),
    Modal_Empirical = mode_estimate
  )
}

getDTempSStudy <- function (nsim = 1000) {
  datatable(
    empSStudy(nsim),
    rownames = FALSE,
    options = list(
      dom = "t"
    )
  )
}

getDTthSStudy <- function () {
  dt <- data.frame( 
    Mean_Theoretical     = 
      if (SDetails$distribution == "norm") 
        SDetails$mean
      else 
        1 / SDetails$rate
    ,
    Variance_Theoretical =
      if (SDetails$distribution == "norm") 
        SDetails$variation
      else 
        1 / (SDetails$rate ^ 2)
    ,
    Median_Theoretical = qResponseTime(1/2),
    Modal_Theoretical = 
      if (SDetails$distribution == "norm") 
        SDetails$mean
      else 
        0
  )
  
  datatable(
    dt,
    rownames = FALSE,
    options = list(
      dom = "t"
    )
  )
}
