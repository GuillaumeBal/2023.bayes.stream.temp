################################################################################
#                               Air temperature                                #
################################################################################

# Dynamic structures such as AR1 residuals are very slow in JAGS (and BUGS)
# Need to break autocorrelation by blocks to help

for(a in 1:n.ac.periods){
  
  # special definition of first at.ts with AR1 block (tau base)
  for(t in ac.period.cutoff[1 + 2 * (a - 1)]){
    at.data.s[t] ~ dnorm(mu.at[t] + 0, tau.at[set.par[t]])
    mu.at[t] <- alpha.at[ind.6m[t]] + beta.at[ind.6m[t]] * sin(2 * pi.value * (t.y.idx[t] + t0.at + step(0 - north.hem) * n.t.step.year / 2) / n.t.step.year)
    res.at[t] <- at.data.s[t] - mu.at[t]
  }
  
  # autocorrelated residuals following first residual value within AR1 block (tau autocorrelation)
  for(t in (ac.period.cutoff[1 + 2 * (a - 1)] + 1) : ac.period.cutoff[2 * a]){
    at.data.s[t] ~ dnorm(mu.at[t] + rho.at[set.par[t]] * res.at[t - 1], tau.at.ac[set.par[t]])
    mu.at[t] <- alpha.at[ind.6m[t]] + beta.at[ind.6m[t]] * sin(2 * pi.value * (t.y.idx[t] + t0.at + step(0 - north.hem) * n.t.step.year / 2) / n.t.step.year)
    res.at[t] <- at.data.s[t] - mu.at[t]
  }
  
}

### PRIORS AT ---------------------------------------------------------------------------

for(sm in 1:n.6m.windows){
  alpha.at[sm] ~ dnorm(p.mean.alpha.at, .1)
  beta.at[sm] ~ dlnorm(log(p.mean.beta.at) - .5 , 1)
  min.at[sm] <- alpha.at[sm] - beta.at[sm]
  max.at[sm] <- alpha.at[sm] + beta.at[sm]
}

t0.at ~ dnorm(- n.time.steps.year / 4, 
              1 / pow((n.time.steps.year / (4)) * .1, 2))

# in case forecasting part deemed to have different values
for(p in 1:n.set.parameters){
	rho.at[p] ~ dbeta(5, 2)
	#tau.at[p] ~ dgamma(0.001, 0.001)
	#sigma.at[p] <- sqrt(1 / tau.at[p])
	tau.at[p] <- 1 / pow(sigma.at[p], 2)
    sigma.at[p] ~ dgamma(2, 1)
	tau.at.ac[p] <- tau.at[p] / (1 - pow(rho.at[p], 2))
}
