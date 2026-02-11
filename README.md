# Probabilistic Analysis of Online Services

An advanced data analytics project that simulates and evaluates the performance of an online platform over a 5-year period (2019-2023). This project integrates **stochastic modeling**, **Monte Carlo simulations**, and **financial forecasting** into an interactive **R Shiny** dashboard.

---

## Project Objective
The goal is to model how random variables (user traffic, response times, failure rates) influence technical performance (SLAs) and economic outcomes (profit, churn rate). It captures complex phenomena like **weekly seasonality**, **Black Friday surges**, and the **COVID-19 impact** on digital traffic.

## 🛠️ Tech Stack
* **Language:** R
* **Frameworks:**
  * **Shiny:** For the interactive analytical engine.
  * **bslib:** For modern, responsive UI components.
* **Visualization:** `ggplot2` (Grammar of Graphics), `viridis` (color scales).
* **Data Handling:** `DT` (Interactive Tables), `reactive programming`.

## Key Data Analysis Features

### 1. Stochastic Traffic Modeling
* **Distributions:** Implementation of **Poisson** and **Binomial** models for user arrivals.
* **Temporal Factors:** Custom functions for peak events (Christmas/Black Friday) and sigmoid-based modeling for pandemic-induced digital adoption.

### 2. Request Performance Analysis
* **Variable Modeling:** Response times modeled using **Exponential** and **Normal** distributions.
* **Joint Distributions:** Heatmap analysis for $(N, T)$ (Total tries vs. Time) and $(N, F)$ (Total tries vs. Failures).
* **Latency Scenarios:** Comparative study between **Independent** vs. **Dependent** response times.

### 3. Statistical Validation
* **Probability Bounds:** Numerical verification of **Markov**, **Chebyshev**, and **Chernoff** inequalities to provide "worst-case" performance guarantees.
* **Monte Carlo Simulations:** 100,000+ iterations to estimate empirical mean, variance, and modal values.

### 4. Economic Impact & Churn
* **Financial Metrics:** Profit calculation based on successful requests, acquisition costs, and SLA penalties.
* **Churn Engine:** Modeling user loss as a function of system latency and failure rates.

## 🚀 How to Run
To run the interactive dashboard locally, follow these steps:

1. **Install R and RStudio** (if not already installed).
2. **Install required libraries** by running the following command in the R console:
   ```R
   install.packages(c("shiny", "ggplot2", "bslib", "DT", "viridis", "stats", "dplyr"))
   ```
3. Open `app.R` in RStudio and click **"Run App"**, or use:
   ```R
   shiny::runApp()
   ```
## 👥 Developed by
* **Leuştean Ştefan**
* **Miu Georgian-Fabian**
* **Preda Maria-Alexandra**
* **Pupezescu Alina-Elena**

