# this model is quite crude as the aim is to get estimates of t0 parameters
# to use properly shift time series for the full water temperature model

# water temperature time series ==============================

for(t in 1:n.time.steps.1){
  wt.data[t] ~ dnorm(mu.wt[t], tau.wt) 
  mu.wt[t] <- alpha.wt + beta.wt * sin(2 * pi.value * (t.y.idx[t] + t0.wt + step(0 - north.hem) * n.time.steps.year / 2) / n.time.steps.year)
}

alpha.wt ~ dnorm(p.mean.alpha.wt, .1)
beta.wt ~ dlnorm(log(p.mean.beta.wt) - .5, 1)
t0.wt ~ dnorm(- n.time.steps.year / 4, 
              1 / pow((n.time.steps.year / (4)) * .1, 2))

tau.wt <- 1 / pow(sigma.wt, 2)
sigma.wt ~ dgamma(2, 1)

#tau.wt ~ dgamma(0.001, 0.001)
#sigma.wt <- sqrt(1 / tau.wt)
