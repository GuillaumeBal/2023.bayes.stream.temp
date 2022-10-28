################################################################################
#                               Water temperature                              #
################################################################################

for(a in 1:n.ac.periods){
  
  # pick first value autocorrellation 
  for(t in ac.period.cutoff[1 + 2 * (a - 1)]){
    wt.data.s[t] ~ dnorm(mu.wt[t], tau.wt) # tau.wt[set.par[t]]
    mu.wt[t] <- alpha.wt[ind.6m[t]] + beta.wt[ind.6m[t]] * sin(2 * pi.value * (t.y.idx[t] + t0.wt+ step(0 - north.hem) * n.t.step.year / 2) / n.t.step.year) + 
      delta * (at.data.s[t] - mu.at[t]) + 
      gamma * sin(2 * pi.value * (t.y.idx[t] + t0.wt+ step(0 - north.hem) * n.t.step.year / 2) / n.t.step.year) * (lfl.data.s[t] - mu.lfl[t])
    mu.wt.ac[t] <- mu.wt[t] + 0
    res.wt[t] <-  wt.data.s[t] - mu.wt[t]
    res.wt.ac[t] <- wt.data.s[t] - mu.wt.ac[t]
    wt.data.s.rep[t] ~ dnorm(mu.wt[t], tau.wt)
  }
  
  for(t in (ac.period.cutoff[1 + 2 * (a - 1)] + 1) : ac.period.cutoff[2 * a]){
    wt.data.s[t] ~ dnorm(mu.wt.ac[t], tau.wt.ac) # rho.wt[set.par[t]] tau.wt.ac[set.par[t]]
    mu.wt[t] <- alpha.wt[ind.6m[t]] + beta.wt[ind.6m[t]] * sin(2 * pi.value * (t.y.idx[t] + t0.wt+ step(0 - north.hem) * n.t.step.year / 2) / n.t.step.year) +
      delta * (at.data.s[t] - mu.at[t]) +
      gamma * sin((t.y.idx[t] + t0.wt + step(0 - north.hem) * n.t.step.year / 2 +  n.t.step.year / 2) / n.t.step.year) * (lfl.data.s[t] - mu.lfl[t])		
    mu.wt.ac[t] <- mu.wt[t] + rho.wt * res.wt[t - 1]
    res.wt[t] <-  wt.data.s[t] - mu.wt[t]
    res.wt.ac[t] <- wt.data.s[t] - mu.wt.ac[t]
    wt.data.s.rep[t] ~ dnorm(mu.wt[t], tau.wt)
  }
  
}

### PRIORS WT ---------------------------------------------------------------------------

for(sm in 1:n.6m.windows){
  alpha.wt[sm] <- (max.wt[sm] + min.wt[sm]) / 2
  beta.wt[sm] <- (max.wt[sm] - min.wt[sm]) / 2
  
  min.wt[sm] ~ dnorm(mu.min.wt[sm], tau.min.wt)
  max.wt[sm] ~ dnorm(mu.max.wt[sm], tau.max.wt)
  
  mu.min.wt[sm] <- theta.0 + 
    theta.1 * (min.at[sm] - mean(min.at[])) + 
    theta.2 * (max.lfl[sm] - mean(max.lfl[]))
  
  mu.max.wt[sm] <- theta.0.p + 
    theta.1.p * (max.at[sm] - mean(max.at[])) + # centered to help faster convergence
    theta.2.p * (min.lfl[sm] - mean(min.lfl[]))
}

theta.0 <- p.mean.alpha.wt - delta.theta.0
delta.theta.0 ~ dlnorm(log(5) - .5, 1)
theta.1 ~ dnorm(0, 0.1)
theta.2 ~ dnorm(0, 0.1)
#tau.min.wt ~ dgamma(0.001, 0.001) 
#sigma.min.wt <- sqrt(1 / tau.min.wt)
sigma.min.wt ~ dgamma(2, 1)
tau.min.wt <- 1 / pow(sigma.min.wt, 2)

theta.0.p <- p.mean.alpha.wt + delta.theta.0.p
delta.theta.0.p ~ dlnorm(log(5) - .5, 1)
theta.1.p ~ dnorm(0, 0.1)
theta.2.p ~ dnorm(0, 0.1)
#tau.max.wt ~ dgamma(0.001, 0.001) 
#sigma.max.wt <- sqrt(1 / tau.max.wt)
sigma.max.wt ~ dgamma(2, 1)
tau.max.wt <- 1 / pow(sigma.max.wt, 2)

delta ~ dnorm(0, 1)
gamma ~ dnorm(0, 1)

rho.wt <- 0
#tau.wt ~ dgamma(0.001, 0.001)
#sigma.wt <- sqrt(1 / tau.wt)
sigma.wt ~ dgamma(2, 1)
tau.wt <- 1 / pow(sigma.wt, 2)
tau.wt.ac <- tau.wt / (1 - pow(rho.wt, 2))

t0.wt ~ dnorm(- n.time.steps.year / 4, 
              1 / pow((n.time.steps.year / (4)) * .1, 2))
