################################################################################
#                                  Log flow                                    #
################################################################################

for(a in 1:n.ac.periods){
  
  for(t in ac.period.cutoff[1 + 2 * (a - 1)]){
    lfl.data.s[t] ~ dnorm(mu.lfl[t] + 0, tau.lfl[set.par[t]])
    mu.lfl[t] <- alpha.lfl[ind.6m[t]] - beta.lfl[ind.6m[t]] * sin(2 * pi.value * (t.y.idx[t] + t0.lfl + step(0 - north.hem) * n.t.step.year / 2) / n.t.step.year)
    res.lfl[t] <- lfl.data.s[t] - mu.lfl[t]
  }
  
  for(t in (ac.period.cutoff[1 + 2 * (a - 1)] + 1) : ac.period.cutoff[2 * a]){
    lfl.data.s[t] ~ dnorm(mu.lfl[t] + rho.lfl[set.par[t]] * res.lfl[t - 1], tau.lfl.ac[set.par[t]]) 
    mu.lfl[t] <- alpha.lfl[ind.6m[t]] - beta.lfl[ind.6m[t]] * sin(2 * pi.value * (t.y.idx[t] + t0.lfl + step(0 - north.hem) * n.t.step.year / 2) / n.t.step.year) # - because of inverse frequency with temp data
    res.lfl[t] <- lfl.data.s[t] - mu.lfl[t]
  }
  
}

### PRIORS LFL ---------------------------------------------------------------------------

for(sm in 1:n.6m.windows){
  alpha.lfl[sm] ~ dnorm(p.mean.alpha.lfl, .1)
  beta.lfl[sm] ~ dlnorm(log(p.mean.beta.lfl) - .5 , 1)
  min.lfl[sm] <- alpha.lfl[sm] - beta.lfl[sm]
  max.lfl[sm] <- alpha.lfl[sm] + beta.lfl[sm]
}

t0.lfl ~ dnorm(- n.time.steps.year / 4, 
              1 / pow((n.time.steps.year / (4)) * .1, 2))

# in case forecasting part deemed to have different values
for(p in 1:n.set.parameters){
	rho.lfl[p] ~ dbeta(5, 2)
	tau.lfl[p] <- 1 / pow(sigma.lfl[p], 2)
    sigma.lfl[p] ~ dgamma(2, 1)
	#tau.lfl[p] ~ dgamma(0.001, 0.001)
	#sigma.lfl[p] <- sqrt(1 / tau.lfl[p])
	tau.lfl.ac[p] <- tau.lfl[p] / (1 - pow(rho.lfl[p], 2))
}
