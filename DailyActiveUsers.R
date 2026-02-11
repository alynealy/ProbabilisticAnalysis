###===============================================
###   Daily Active Users (Kd)
###
###   Library for the Kd distribution, gives all necessary functions, as well as 
### distribution changes and graphic function.
###
###
### KDetails$distribution = "pois"/"bin" (Depending on distribution)
### All lambdas and percentage values are defaultly set
### 
### Functions:
### - simulateUserYear(year = 2019) -> simuleaza un an de daily active users pentru anul dat si distributia setata
### - createYearlyPlot() -> Creaza un cu toti anii si legenda pentru distributia selectata
### - createMonthsPlot(year = 2019) -> Creaza o histograma cu numarul de useri activi pe luni intr-un an pe 
### distributia setata
### - dau_empiricalYearStudy(nsim = 5000, year = 2019) -> studiaza empirica, varianta, si media unui an pe distributia
### setata
###===============================================

.days <- 365
.days_vec <- 1:365

KDetails <- new.env();

KDetails$distribution = "pois"

KDetails$years = c(2019, 2020, 2021, 2022, 2023)
KDetails$covid_years = c(2020)

KDetails$max_active_users = 2000 # the n in the binomial distribution

KDetails$yearly_lambda = c(650, 900, 820, 760, 860)

KDetails$yearly_proc_bin = KDetails$yearly_lambda / KDetails$max_active_users
KDetails$test_emp = 1e5

.dau_seasonal_variation <- function() {
  season_base <- 1 + 0.12 * sin(2 * pi * (.days_vec - 90) / 365)
  
  bump <- function(center, width, amp) {
    1 + amp * exp(-((.days_vec - center) / width)^2)
  }
  
  season <- season_base *
    bump(75,  12, 0.15) *   # martie
    bump(245, 15, 0.10) *   # back to school
    bump(330,  8, 0.35) *   # black friday
    bump(355, 10, 0.20)     # craciun
  
  KDetails$season = season
}
.dau_seasonal_variation()

.dau_weekly_variation <- function() {
  week <- 1 + 0.02 * cos(2 * pi * .days_vec / 7)
  KDetails$week = week
}
.dau_weekly_variation()

.dau_covid_variation <- function() {
  covid <- 1 + 0.30 * (1 + tanh((.days_vec - 70) / 12)) / 2
  #  -> ~1 in ian-feb
  #  -> ~1.15 in martie
  #  -> ~1.30 din aprilie incolo
  KDetails$covid = covid
}
.dau_covid_variation()

KDetails$lambdaDistribution = cbind()
for (i in 1:length(KDetails$years)) {
  year <- KDetails$years[i]
  lambda <- KDetails$yearly_lambda[i]
  isCovid <- year %in% KDetails$covid_years
  
  KDetails$lambdaDistribution = cbind(
    KDetails$lambdaDistribution,
    lambda * KDetails$season * KDetails$week * ifelse(isCovid, KDetails$covid, 1)
  )
}

KDetails$procDistribution = cbind()
for (i in 1:length(KDetails$years)) {
  year <- KDetails$years[i]
  proc <- KDetails$yearly_proc_bin[i]
  isCovid <- year %in% KDetails$covid_years
  
  KDetails$procDistribution = cbind(
    KDetails$procDistribution,
    proc * KDetails$season * KDetails$week * ifelse(isCovid, KDetails$covid, 1)
  )
}

simulateUserYear <- function(year=2019) {
  stopifnot(year %in% KDetails$years)
  
  yearIndex = which(KDetails$years == year)
  
  if (KDetails$distribution == "pois") {
    rpois(.days, KDetails$lambdaDistribution[,yearIndex])
  }
  else {
    rbinom(.days, KDetails$max_active_users, KDetails$procDistribution[,yearIndex])
  }
}

# Functia de masa
dActiveUsers <- function (n, day = 1, year = 2019) {
  stopifnot(day %in% .days_vec)
  stopifnot(year %in% KDetails$years)
  yearIndex = which(KDetails$years == year)
  
  if (KDetails$distribution == "pois") {
    dpois(n, KDetails$lambdaDistribution[day, yearIndex])
  }
  else {
    dbinom(n, KDetails$max_active_users, KDetails$procDistribution[day, yearIndex])
  }
}

# Functia de repartitie
pActiveUsers <- function (n, day = 1, year = 2019) {
  stopifnot(day %in% .days_vec)
  stopifnot(year %in% KDetails$years)
  yearIndex = which(KDetails$years == year)
  
  if (KDetails$distribution == "pois") {
    ppois(n, KDetails$lambdaDistribution[day, yearIndex])
  }
  else {
    pbinom(n, KDetails$max_active_users, KDetails$procDistribution[day, yearIndex])
  }
}

# functie generatoare
rActiveUsers <- function (n, day = 1, year = 2019) {
  stopifnot(day %in% .days_vec)
  stopifnot(year %in% KDetails$years)
  yearIndex = which(KDetails$years == year)
  
  if (KDetails$distribution == "pois") {
    rpois(n, KDetails$lambdaDistribution[day, yearIndex])
  }
  else {
    rbinom(n, KDetails$max_active_users, KDetails$procDistribution[day, yearIndex])
  }
}

# functia Cuantila
qActiveUsers <- function (n, day = 1, year = 2019) {
  stopifnot(day %in% .days_vec)
  stopifnot(year %in% KDetails$years)
  yearIndex = which(KDetails$years == year)
  
  if (KDetails$distribution == "pois") {
    qpois(n, KDetails$lambdaDistribution[day, yearIndex])
  }
  else {
    qbinom(n, KDetails$max_active_users, KDetails$procDistribution[day, yearIndex])
  }
}

library(ggplot2)

createYearsPlot <- function(distr = NULL) {
  if (!is.null(distr))
    KDetails$distribution = distr
  

  df <- do.call(
    rbind,
    lapply(KDetails$years, function(y) {
      data.frame(
        day = .days_vec,
        users = simulateUserYear(y),
        year = factor(y)
      )
    })
  )
  
  ggplot(df, aes(x = day, y = users, color = year)) +
    geom_line(linewidth = 1.1) +
    scale_y_continuous(limits = c(0, 1500)) +
    labs(
      title = "Trafic zilnic – comparație ani",
      x = "Ziua din an",
      y = "Număr clienți",
      color = "An"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "right"
    )
}

dau_empiricalYearStudy <- function(nsim = 5000, year = 2019, distr = NULL) {
  
  if (!is.null(distr))
    KDetails$distribution <- distr
  
  stopifnot(year %in% KDetails$years)
  
  yearIndex <- which(KDetails$years == year)
  
  sim <- replicate(nsim, simulateUserYear(year))
  
  daily_means <- rowMeans(sim)
  daily_vars  <- apply(sim, 1, var)
  
  data.frame(
    Mean_Empirical       = mean(daily_means),
    Variance_Empirical   = mean(daily_vars),
    Mean_Theoretical     =
      if (KDetails$distribution == "pois")
        mean(KDetails$lambdaDistribution[, yearIndex])
    else
      mean(KDetails$max_active_users *
             KDetails$procDistribution[, yearIndex]),
    Variance_Theoretical =
      if (KDetails$distribution == "pois")
        mean(KDetails$lambdaDistribution[, yearIndex])
    else
      mean(KDetails$max_active_users *
             KDetails$procDistribution[, yearIndex] *
             (1 - KDetails$procDistribution[, yearIndex]))
  )
}

createMonthsPlot <- function(year = 2019, distr = NULL) {
  if (!is.null(distr))
    KDetails$distribution = distr
  
  stopifnot(year %in% KDetails$years)
  
  nyear <- simulateUserYear(year)
  # Nu luam in calcul an bisect
  month_lengths <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  month_names <- month.abb
  
  month_id <- rep(1:12, times = month_lengths)
  
  daily_by_month <- split(nyear, month_id)
  
  max_days <- max(month_lengths)
  M <- matrix(NA, nrow = max_days, ncol = 12)
  
  for (m in 1:12) {
    M[1:length(daily_by_month[[m]]), m] <- daily_by_month[[m]]
  }

  
  df <- data.frame(
    day   = rep(1:nrow(M), times = ncol(M)),
    month = factor(rep(month_names, each = nrow(M)), levels = month_names),
    users = as.vector(M)
  )
  
  ggplot(df, aes(x = month, y = users)) +
    geom_col(position = "dodge", na.rm = TRUE,
             fill = scales::hue_pal()(length(KDetails$years))[which(KDetails$years == year)]) +
    labs(
      title = paste("Useri Activi pe luna -", year),
      x = "Luni",
      y = "User Activi"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.minor = element_blank()
    )
}

getYearEMP <- function(year = 2019, nsim = 5000, distr = NULL) {
  datatable(
    dau_empiricalYearStudy(nsim, year, distr),
    rownames = FALSE,
    options = list(dom = "t")
  )
}

