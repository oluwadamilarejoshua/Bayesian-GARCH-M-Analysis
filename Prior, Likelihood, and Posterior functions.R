source('Unit Root Evaluation and Handling.R')

# Preliminary model (OLS estimation) for informative prior ----------------

# Preliminary OLS model
ols_model <- lm(y ~ X)
summary(ols_model)

# Extract coefficients and residual variance
ols_coefficients <- coef(ols_model)
ols_variance <- var(residuals(ols_model))

# Priors for beta and mu
prior_mu <- ols_coefficients[1]  # Intercept
prior_beta <- ols_coefficients[-1]  # Coefficients for predictors
prior_variance <- ols_variance  # Prior variance for residuals

# Prior, Likelihood, and Posterior functions ------------------------------

# Defining Prior
log_prior <- function(params) {
  mu <- params[1]
  gamma <- params[2]
  beta_X <- params[3:(2 + ncol(X))]
  omega <- params[3 + ncol(X)]
  alpha <- params[4 + ncol(X)]
  beta <- params[5 + ncol(X)]
  
  # Priors based on empirical evidence
<<<<<<< HEAD
  prior_mu_val <- dnorm(mu, prior_mu, sqrt(prior_variance), log = TRUE)
  prior_gamma_val <- dnorm(gamma, 0, 10, log = TRUE)
  prior_beta_X_val <- sum(dnorm(beta_X, prior_beta, sqrt(prior_variance), 
                                log = TRUE))
  prior_omega_val <- ifelse(omega > 0, dgamma(omega, shape = 2, rate = 1, log = TRUE), -Inf)
  prior_alpha_val <- ifelse(alpha > 0 & alpha < 1, dbeta(alpha, 2, 2, log = TRUE), -Inf)
  prior_beta_val <- ifelse(beta > 0 & beta < 1, dbeta(beta, 2, 2, log = TRUE), -Inf)
  
  # Sum of log-priors
  total_prior <- prior_mu_val + prior_gamma_val + prior_beta_X_val + 
    prior_omega_val + prior_alpha_val + prior_beta_val
  return(total_prior)
=======
  prior_mu <- dnorm(mu, prior_mu, sqrt(prior_variance), log = TRUE)
  prior_gamma <- dnorm(gamma, 0, 10, log = TRUE)
  prior_beta_X <- sum(dnorm(beta_X, prior_beta, sqrt(prior_variance), log = TRUE))
  prior_omega <- ifelse(omega > 0 & omega < 1, 0, -Inf)
  prior_alpha <- ifelse(alpha > 0 & alpha < 1, 0, -Inf)
  prior_beta <- ifelse(beta > 0 & beta < 1, 0, -Inf)
  
  return(prior_mu + prior_gamma + prior_beta_X + prior_omega + prior_alpha + prior_beta)
>>>>>>> 7350dcc0f73039616ae18a92de6047921a61c0ed
}


# Defining log-likelihood function
log_likelihood <- function(params, y, X) {
  mu <- params[1]
  gamma <- params[2]
  beta_X <- params[3:(2 + ncol(X))]
  omega <- params[3 + ncol(X)]
  alpha <- params[4 + ncol(X)]
  beta <- params[5 + ncol(X)]
  
  n <- length(y)
  h <- numeric(n)
  h[1] <- omega / (1 - alpha - beta)
  
  log_lik <- 0
  for (t in 2:n) {
    h[t] <- omega + alpha * (y[t-1] - mu - gamma * h[t-1] - sum(X[t-1, ] * beta_X))^2 + beta * h[t-1]
    if (h[t] <= 0) {
      return(-Inf)  # Invalid variance
    }
    log_lik <- log_lik - 0.5 * (log(2 * pi) + log(h[t]) + 
                                  (y[t] - mu - gamma * h[t] - sum(X[t, ] * beta_X))^2 / h[t])
  }
  return(log_lik)
}


# Combining Prior and Likelihood for posterior
log_posterior <- function(params, y, X) {
  # Check parameter validity
  omega <- params[3 + ncol(X)]
  alpha <- params[4 + ncol(X)]
  beta <- params[5 + ncol(X)]
  
<<<<<<< HEAD
=======
  # Compute the log-prior
  log_prior <- 0
>>>>>>> 7350dcc0f73039616ae18a92de6047921a61c0ed
  if (omega <= 0 || alpha <= 0 || beta <= 0 || (alpha + beta) >= 1) {
    return(-Inf)  # Invalid GARCH parameters
  }
  
<<<<<<< HEAD
  # Compute log-prior
  log_prior_val <- log_prior(params)
  
  # Compute log-likelihood
  log_likelihood_val <- log_likelihood(params, y, X)
  
  # Total log-posterior
  log_posterior_val <- log_prior_val + log_likelihood_val
  
  # Debugging output
  cat("Log Prior:", log_prior_val, "\n")
  cat("Log Likelihood:", log_likelihood_val, "\n")
  cat("Log Posterior:", log_posterior_val, "\n")
  
  return(log_posterior_val)
}
=======
  # Compute the log-likelihood
  n <- length(y)
  h <- numeric(n)
  h[1] <- var(y)  # Initialize variance
  
  log_likelihood <- 0
  for (t in 2:n) {
    h[t] <- omega + alpha * (y[t - 1]^2) + beta * h[t - 1]
    if (h[t] <= 0) {
      return(-Inf)  # Invalid variance
    }
    mu_t <- beta_0 + sum(beta_X * X[t, ])
    log_likelihood <- log_likelihood + 
      dnorm(y[t], mean = mu_t, sd = sqrt(h[t]), log = TRUE)
  }
  
  # Total log-posterior
  log_posterior_value <- log_prior + log_likelihood
  
  # Debugging output
  cat("Log Prior:", log_prior, "\n")
  cat("Log Likelihood:", log_likelihood, "\n")
  cat("Log Posterior:", log_posterior_value, "\n")
  
  return(log_posterior_value)
}



>>>>>>> 7350dcc0f73039616ae18a92de6047921a61c0ed
