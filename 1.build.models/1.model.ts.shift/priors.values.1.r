p.mean.alpha.wt <- wt.data %>% mean(na.rm = TRUE)
p.mean.beta.wt <- wt.data %>% quantile(probs = c(0.05, 0.95), na.rm = TRUE) %>% diff %>% `/`(2) %>% `names<-`(NULL)

priors.spec.1 <- list(
  'p.mean.alpha.wt' = p.mean.alpha.wt,
  'p.mean.beta.wt' = p.mean.beta.wt
)
