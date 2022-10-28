
################################################################################
#                              further diagnostics                             #
################################################################################

# mean per 6 months of observed and replicated temperatures ======================

time.1 <- n.t.step.year - round(n.t.step.year / 2) # round value away from 0
time.2 <- n.t.step.year - time.1

#for(sm in 1:round((n.6m.windows / 2))){
  
#  check.mean.wt[sm * 2 - 1] <- mean(wt.data.s.rep[(1 + n.t.step.year * (sm - 1)):(time.1 + n.t.step.year * (sm - 1))]) -
#    mean(wt.data.s[(1 + n.t.step.year * (sm - 1)):(time.1 + n.t.step.year * (sm - 1))])
  
#  check.mean.wt[sm * 2] <- mean(wt.data.s.rep[(time.2 + n.t.step.year * (sm - 1)):(n.t.step.year + n.t.step.year * (sm - 1))]) -
#    mean(wt.data.s[(time.2 + n.t.step.year * (sm - 1)):(n.t.step.year + n.t.step.year * (sm - 1))])
  
#}

# water temperature residuals autocorrelation ====================================

ac.part.1 <- res.wt.ac[1:(start.for - 2)] * res.wt.ac[2:(start.for - 1)] # cov part
ac.part.2 <- pow(res.wt.ac[1:(start.for - 2)], 2)
ac.part.3 <- pow(res.wt.ac[2:(start.for - 1)], 2)

ac.wt <- sum(ac.part.1[]) / (pow(sum(ac.part.3[]), 0.5) * pow(sum(ac.part.3[]), 0.5))

# chi2 test ======================================================================

chi2.obs <- pow(res.wt[1:(n.time.steps - last.to.skip)], 2) / pow(sigma.wt, 2)
chi2.rep <- pow(wt.data.s.rep[1:(n.time.steps - last.to.skip)] - mu.wt[1:(n.time.steps - last.to.skip)], 2) / pow(sigma.wt, 2)

sum.chi2.obs <- sum(chi2.obs[])
sum.chi2.rep <- sum(chi2.rep[])
test.chi2 <- step(sum.chi2.rep - sum.chi2.obs)

# mean fir error centered on 0 ? ==================================================

mean.errors.fit <- mean(res.wt[1:(start.for - 1)])
bias.fit <- step(mean.errors.fit)
