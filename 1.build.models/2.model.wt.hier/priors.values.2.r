p.mean.alpha.wt <- wt.data %>% mean(na.rm = TRUE)
p.mean.beta.wt <- wt.data %>% quantile(probs = c(0.05, 0.95), na.rm = TRUE) %>% diff %>% `/`(2) %>% `names<-`(NULL)

p.mean.alpha.at <- at.data %>% mean(na.rm = TRUE)
p.mean.beta.at <- at.data %>% quantile(probs = c(0.05, 0.95), na.rm = TRUE) %>% diff %>% `/`(2) %>% `names<-`(NULL)

if(lfl.inc == 1){
  p.mean.alpha.lfl <- lfl.data %>% mean(na.rm = TRUE)
  p.mean.beta.lfl <- lfl.data %>% quantile(probs = c(0.05, 0.95), na.rm = TRUE) %>% diff %>% `/`(2) %>% `names<-`(NULL)
}

priors.spec.2 <- c(
  
  'p.mean.alpha.wt',
  'p.mean.beta.wt',
  
  'p.mean.alpha.at',
  'p.mean.beta.at',
  
  if(lfl.inc == 1){
    c('p.mean.alpha.lfl',
      'p.mean.beta.lfl'
    )
  }
  
)
