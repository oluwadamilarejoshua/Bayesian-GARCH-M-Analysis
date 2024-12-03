source("Data Preparation.R")

# Load necessary library

library(tseries)


# Assuming columns are named ExchangeRate, GDP, and Debt

ExchangeRate <- data_subset$ExchangeRate
GDP <- data_subset$GDP
Debt <- data_subset$Debt


# Function to check stationarity for a single variable

check_stationarity <- function(series, series_name) {
  cat("\n--- Stationarity Check for:", series_name, "---\n")
  
  # ADF Test
  
  adf_result <- adf.test(series, alternative = "stationary")
  cat("ADF Test:\n")
  cat("  p-value:", adf_result$p.value, "\n")
  if (adf_result$p.value < 0.05) {
    cat("  The series is stationary (reject null of unit root).\n")
  } else {
    cat("  The series is not stationary (fail to reject null of unit root).\n")
  }
  
  # KPSS Test
  
  kpss_result <- kpss.test(series, null = "Level")
  cat("\nKPSS Test:\n")
  cat("  p-value:", kpss_result$p.value, "\n")
  if (kpss_result$p.value > 0.05) {
    cat("  The series is stationary (fail to reject null of stationarity).\n")
  } else {
    cat("  The series is not stationary (reject null of stationarity).\n")
  }
}


# Apply stationarity tests to ExchangeRate, GDP, and Debt

check_stationarity(ExchangeRate, "Exchange Rate")
check_stationarity(GDP, "GDP")
check_stationarity(Debt, "Debt")


# Applying first-order diffeencing

ExchangeRate_diff <- diff(ExchangeRate)
GDP_diff <- diff(GDP)
Debt_diff <- diff(Debt)

check_stationarity(ExchangeRate_diff, "Differenced Exchange Rate")
check_stationarity(GDP_diff, "Differenced GDP")
check_stationarity(Debt_diff, "Differenced Debt")



# Assign variables

y <- ExchangeRate_diff
X <- as.matrix(Debt_diff, GDP_diff)

