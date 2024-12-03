source('Implementing MCMC sampling.R')


library(coda)

# Convert samples matrix to mcmc object (for analysis with coda)
mcmc_samples <- as.mcmc(samples)

# Plot trace plots for each parameter
par(mfrow=c(2, 2))  # Arrange the plots in a 2x2 grid
plot(mcmc_samples[, 1], main="Trace Plot: Parameter 1")
plot(mcmc_samples[, 2], main="Trace Plot: Parameter 2")
plot(mcmc_samples[, 3], main="Trace Plot: Parameter 3")
plot(mcmc_samples[, 4], main="Trace Plot: Parameter 4")




