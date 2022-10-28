# flow sine series =================================
#'-beta.lfl' because inverse frequency compared to temp data

for(t in 1:n.time.steps.1){
  lfl.data[t] ~ dnorm(mu.lfl[t], tau.lfl) 
  mu.lfl[t] <- alpha.lfl - beta.lfl * sin(2 * pi.value * (t.y.idx[t] + t0.lfl + step(0 - north.hem) * n.time.steps.year / 2) / n.time.steps.year)
}

alpha.lfl ~ dnorm(p.mean.alpha.lfl, .1)
beta.lfl ~ dlnorm(log(p.mean.beta.lfl) - .5,  1)
t0.lfl ~ dnorm(- n.time.steps.year / 4, 
               1 / pow((n.time.steps.year / (4)) * .1, 2))

tau.lfl <- 1 / pow(sigma.lfl, 2)
sigma.lfl ~ dgamma(2, 1)

#tau.lfl ~ dgamma(0.001, 0.001)
#sigma.lfl <- sqrt(1 / tau.lfl)
