# Version for Nested Sampling containing from_prior() function by Brendon

# Prior distribution ####################

# Function for the prior distribution
# Input: parameter vector, avg_change
# Output: log of the probability density
# Assumption: The 2 parameters are independent

# Derive monthly change in subscribers
# change_subs = diff(data$y)

# Average change
# avg_change = mean(change_subs)

log_prior = function(params)
{
  logp = 0
  
  # Change later to Gamma Prior?
  # Flat prior centered on average change
  
  # A Normal(avg_change, sd=10*avg_change) prior for the first parameter
  logp = logp + dnorm(params["lambda"], avg_change, 10*avg_change, log=TRUE)
  
  # A Normal(avg_change, sd=10*avg_change) prior for the second parameter
  logp = logp + dnorm(params["mu"], avg_change, 10*avg_change, log=TRUE)
  
  return(logp)
}


# Generate from the prior
from_prior = function()
{
  # Generate from prior
  params = rep(NA, 2)
  params[1] = rnorm(1, avg_change, 10*avg_change)
  params[2] = rnorm(1, avg_change, 10*avg_change)
  
  # Name the parameters
  names(params) = c("lambda", "mu")
  
  return(params)
}


# Log likelihood function #################################

# Input: parameter vector
# Define: observed daily changes (daily_changes)
# Output: log likelihood value
log_likelihood = function(params)
{
  # load library for skellam distribution
	library("VGAM")
  
  # Skellam likelihood
  logl = sum(dskellam(change_subs, params["lambda"], params["mu"], log=TRUE))
  #cat("Log likelihood =", logl, "\n")
  
  # Brendon's original code	
  # Calculate poisson rate as a function of time
  #lambda = exp(params["log_lambda0"] + params["slope"]*data$t)
  
  # Poisson likelihood
  #logl = sum(dpois(data$y, lambda, log=TRUE))
  
  return(logl)
}

# Proposal distribution ###############################
# Input: parameter vector
# To Define: Widths

proposal = function(params)
{
  # Copy the parameters
  params2 = params
  
  # Which parameter to change?
  i = sample(1:length(params), 1)
  
  # Step size - Brendon's favourite magic
  step_size = widths[i]*10^(1.5 - 3*abs(rt(1, df=2)))
  
  params2[i] = params2[i] + step_size*rnorm(1)
  
  # Saurabh: keep param >= 0 ####
  params2[i] =  abs(params2[i])
    
  return(params2)
}

