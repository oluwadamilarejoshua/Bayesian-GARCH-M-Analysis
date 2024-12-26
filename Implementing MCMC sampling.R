# source('Prior, Likelihood, and Posterior functions.R')


# Define the Metropolis algorithm

metropolis <- function(init, y, X, n_iter, proposal_sd) {
  params <- init
  samples <- matrix(NA, nrow = n_iter, ncol = length(init))
  accept <- 0
  
  for (i in 1:n_iter) {
    proposal <- rnorm(length(init), mean = params, sd = proposal_sd)
    log_accept_ratio <- log_posterior(proposal, y, X) - log_posterior(params, y, X)
    
    # Debugging and safeguard
    
    if (is.na(log_accept_ratio) || is.nan(log_accept_ratio)) {
      cat("Invalid log_accept_ratio at iteration", i, "\n")
      cat("Proposed Parameters:", proposal, "\n")
      cat("Log Posterior (proposal):", log_posterior(proposal, y, X), "\n")
      cat("Log Posterior (current):", log_posterior(params, y, X), "\n")
      stop("log_accept_ratio is invalid. Check your log_posterior function.")
    }
    
    if (log(runif(1)) < log_accept_ratio) {
      params <- proposal
      accept <- accept + 1
    }
    
    samples[i, ] <- params
  }
  
  cat("Acceptance rate:", accept / n_iter, "\n")
  return(samples)
}


# Initial values for parameters

# init <- c(prior_mu, 0.1, rep(0.1, ncol(X)), 0.1, 0.1, 0.1)

ols_coefs <- coef(ols_model)
ols_sigma <- summary(ols_model)$sigma^2

init <- c(ols_coefs[1],   # Intercept
          ols_sigma,      # Residual variance
          ols_coefs[-1],  # Slopes for predictors
          rep(ols_sigma, 3))



# Run MCMC

n_iter <- 200000
proposal_sd <- 0.5
samples <- metropolis(init, y, X, n_iter, proposal_sd)


# Summarizing Results -----------------------------------------------------

# Posterior means and credible intervals
posterior_means <- colMeans(samples)
credible_intervals <- apply(samples, 2, quantile, probs = c(0.025, 0.975))

# Print results
cat("Posterior Means:\n", posterior_means, "\n")
cat("95% Credible Intervals:\n", credible_intervals, "\n")

# Plot posterior distributions
par(mfrow = c(3, 2))
for (i in 1:ncol(samples)) {
  hist(samples[, i], main = paste("Parameter", i), xlab = "Value",
       probability = TRUE)
}



