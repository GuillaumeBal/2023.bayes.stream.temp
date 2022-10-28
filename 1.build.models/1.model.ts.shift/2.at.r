# air temperature time series =========================

for(t in 1:n.time.steps.1){
  at.data[t] ~ dnorm(mu.at[t], tau.at) 
  mu.at[t] <- alpha.at + beta.at *  sin(2 * pi.value * (t.y.idx[t] + t0.at + step(0 - north.hem) * n.time.steps.year / 2) / n.time.steps.year)
}

alpha.at ~ dnorm(p.mean.alpha.at, .1)
beta.at ~ dlnorm(log(p.mean.beta.at) - .5, 1)
t0.at ~ dnorm(- n.time.steps.year / 4, 
              1 / pow((n.time.steps.year / (4)) * .1, 2))

tau.at <- 1 / pow(sigma.at, 2)
sigma.at ~ dgamma(2, 1)

#tau.at ~ dgamma(0.001, 0.001)
#sigma.at <- sqrt(1 / tau.at)
