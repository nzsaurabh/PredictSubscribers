#load the data
load("MCMCdata.rda")

# Load functions that are model-specific
source("model_Skellam.r")

# Calculate average and monthly changes for skellam distribution

# Derive monthly change in subscribers
change_subs = diff(data$y)

# Average change
avg_change = mean(change_subs)

# Explore the data 

# Plot data
plot(data$t, data$y, type="b", xlab="Time (months)", ylab="Number of Patrons")

# Plot the changes
plot(data$t[2:length(data$t)], change_subs, type="b", xlab="Time (months)", ylab="Monthly Change in Patrons")

# Grid search for widths ###########################

# Test the widths
for (i in 700:800)
{
  step_size = i*10^(1.5 - 3*abs(rt(1, df=2)))
  cat(step_size, "\n")
  step_size = 0
}

# Grid Search for params ####################
paramsdf = cbind.data.frame(lambda = 2000:10000, mu = 10000:2000)

loghdf = cbind.data.frame(logh = rep(NA, nrow(paramsdf)), paramsdf)

for (i in 1:nrow(loghdf)){
  # create params vector
  params = c(as.numeric(paramsdf[ i, 1]), as.numeric(paramsdf[ i, 2]))
  names(params) = c("lambda", "mu")
  # Compute logh
  loghdf[i, 1] = log_prior(params) + log_likelihood(params)
  colnames(loghdf) <- c( "logh", "lambda", "mu")
}

# print results
cat(" Max. logh is ", max(loghdf[ , 1]), "for lambda = ", as.numeric(loghdf[loghdf[ , 1] == max(loghdf[ , 1]), 2]),
    "and mu = ", as.numeric(loghdf[loghdf[ , 1] == max(loghdf[ , 1]), 3]))

plot(loghdf[, "lambda"], loghdf[, "logh"])

plot(loghdf[, "mu"], loghdf[, "logh"])


# Setup for Metropolis ##########################


# How many steps to take
steps = 20000

# Thinning
thin = 10

# Starting position in parameter space
params = c( 1000 , 500 )

# Give the parameters names
names(params) = c("lambda", "mu")

# Measure how good it is
logh = log_prior(params) + log_likelihood(params)

# Prior widths for each parameter (these help set scale for proposal)
widths = c(1000, 1000)

# Set up 2D array for storage of results
keep = array(NA, dim=c(steps/thin, length(params)))

# Set up 1D array
logl_keep = array(NA, dim=steps/thin)

# Count number of accepted proposals
accepted = 0

# Do Metropolis #######################

for(i in 1:steps)

{
  
  # Propose to move somewhere else
  params2 = proposal(params)
  
  # Measure how good it is
  
  logh2 = log_prior(params2)
  
  if(logh2 != -Inf)
  {
    logh2 = logh2 + log_likelihood(params2)
  }
  
  # Acceptance probability
  # logh = -Inf if initial values are bad
  # added if condition so that log_alpha is not NaN
  # if logh = -inf we should accept the new parameters anyway
  
  if(logh != -Inf) {
  log_alpha = logh2 - logh}else{
    log_alpha = 0
  }
  
  if(log_alpha > 0) { log_alpha = 0 }
  
  # Accept the proposal with probability alpha
  
  if(runif(1) < exp(log_alpha))
  
  {
    params = params2
    logh = logh2
    accepted = accepted + 1
  }
  
  # Store results
  if(i %% thin == 0)
  {
    keep[i/thin, ] = params
    logl_keep[i/thin] = log_likelihood(params)
    cat("Done", i, "iterations.\n")
  }
  
}

# Metropolis results ################################

# Print Metropolis acceptance rate 
cat("Acceptance rate =", accepted/steps, "\n")

# Plot joint posterior distribution of the two parameters,
# excluding a burn-in period.

L = dim(keep)[1]

plot(keep[1000:L, 1], keep[1000:L ,2], xlab="lambda", ylab="mu", cex=0.1)


