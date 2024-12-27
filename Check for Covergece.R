# source('Implementing MCMC sampling.R')

library(coda)

# Convert samples matrix to mcmc object (for analysis with coda)
mcmc_samples <- as.mcmc(samples)

# Define the number of parameters (mu, gamma, beta_debt, beta_GDP, omega, alpha, beta)
num_params <- 7

# Plot trace plots for each parameter


# Define parameter names
param_names <- c("Intercept", "Variance", "Beta Debt", "Beta GDP", 
                 "Omega", "Alpha", "Beta")

# Diagnostics and Trace Plots
for (i in 1:num_params) {
  # Handle missing values
  valid_samples <- mcmc_samples[, i][!is.na(mcmc_samples[, i])]
  
  if (length(valid_samples) > 10) {
    cat("\nParameter", param_names[i], "Diagnostics:\n")
    cat("Geweke Diagnostic:\n")
    print(geweke.diag(as.mcmc(valid_samples)))
    cat("Effective Sample Size:\n")
    print(effectiveSize(as.mcmc(valid_samples)))
    
    # Trace plot
    plot(valid_samples, 
         main = paste("Trace Plot: Parameter", param_names[i]), 
         xlab = "Iteration", ylab = param_names[i], 
         col = "black", type = "l")
  } else {
    cat("Insufficient valid samples for Parameter", param_names[i], "\n")
  }
}

# Histograms of Posterior Distributions
par(mfrow = c(3, 3))  # Adjust based on the number of parameters
for (i in 1:num_params) {
  hist(samples[, i], 
       main = paste("Posterior Distribution:", param_names[i]),
       xlab = param_names[i], probability = TRUE, col = "black")
}

