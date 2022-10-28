mcmc.burn.1 <- as.integer(c(1000, 1000)[run.type])
mcmc.length.1 <- as.integer(c(2000, 5000)[run.type])
mcmc.thin.1 <- as.integer(1)
mcmc.chains.1 <- as.integer(3)
mcmc.burn.2 <- as.integer(c(500, 1000)[run.type])
mcmc.length.2 <- as.integer(c(1000, 10000)[run.type])
mcmc.thin.2 <- as.integer(c(1, 25)[run.type]) # with thin of 1, very little chance to start converging
mcmc.chains.2 <- as.integer(3)
