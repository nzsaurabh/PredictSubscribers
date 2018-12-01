# PredictSubscribers
Researching on a Bayesian method to predict online subscribers. The project is at an initial stage - experimenting with MCMC and Nested Sampling
The model assumes two poisson processes - one for the number of new customers joining, the other for customer attritions.
As expected, the the model shows that their joint distribution is highly correlated so we need to put a range to the values the two process can take.
Another challenge is that the skellam distribution (probability distribution for difference between two poisson processes) works fine for differences in the range (-10, +10). Hence, in our case it leads to numerical problems.
Look at the repository for my Masters Dissertation, the predictions have been made using a much simpler model assuming log growth in subscribers in one model and time trends in two other models.
