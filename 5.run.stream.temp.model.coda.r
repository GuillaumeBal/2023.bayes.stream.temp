# numbers of values not to include in cross validation as set to NA by TS shift
last.to.skip <- t0.value

# need to duplicated some ts for the cross validation part
if(any(run.config == 4)){
  wt.exist <- (!is.na(wt.data.s.perf)) * 1
  wt.data.s.perf[is.na(wt.data.s.perf)] <- -100
}

# get specific priors to enter as data
'1.build.models/2.model.wt.hier/priors.values.2.r' %>% source

# model data
jags.data.2 <- c(
  'wt.data.s',
  'at.data.s',
  'n.t.step.year', 
  'ind.6m', 'n.6m.windows', 
  'ac.period.cutoff', 'n.ac.periods',
  'pi.value', 
  'n.set.parameters', 'set.par',
  'n.time.steps', 'n.time.steps.year',
  'last.to.skip',
  't.y.idx',
  'north.hem',
  if(lfl.inc == 1){'lfl.data.s'},
  if(any(run.config == 4)){
    c('wt.exist', 'wt.data.s.perf',
      'start.cross')
  },
  if(fit.checks == 1){
    'start.for'
  },
  if(some.hier == TRUE){
    c('n.6m.windows.hist' , 
      if(any(run.config == 3))'n.6m.windows.for'
    )
  }, 
  priors.spec.2
)

# model parameters to record
jags.params.2 <- c('alpha.at', 'beta.at', 't0.at', 'sigma.at', 'rho.at',
                   'alpha.wt', 'beta.wt', 't0.wt', 'sigma.wt',
                   'theta.0', 'theta.1', 'theta.0.p', 'theta.1.p', 
                   'sigma.max.wt', 'sigma.min.wt',
                   if(mod.res == 0){'rho.wt'},
                   if(lfl.inc == 1){c('alpha.lfl', 'beta.lfl', 't0.lfl', 'sigma.lfl', 'rho.lfl','theta.2','theta.2.p')},
                   if(mod.res == 1){
                     if(lfl.inc == 1){
                       c('delta', 'gamma')
                     }
                     else{'delta'}
                   }else{'rho.wt'},
                   if(fit.checks == 1){
                     c('sum.chi2.obs', 'sum.chi2.rep', 'test.chi2',
                       'mean.errors.fit', 'bias.fit', #'mean.errors.pred', 'bias.pred',
                       #'check.mean.wt',
                       'ac.wt')},
                   if(any(run.config == 4)){
                     c('rmse', 'mean.errors.pred', 'bias.pred')
                   },
                   if(save.wt == 1){'wt.data.s'},
                   if(at.hier == 1){
                     c('mu.alpha.at', 'sd.alpha.at', 'mu.beta.at', 'sd.beta.at')
                   },
                   if(exists('lfl.hier')){
                     if(lfl.hier == 1){
                       c('mu.alpha.lfl', 'sd.alpha.lfl', 'mu.beta.lfl', 'sd.beta.lfl') 
                     }
                   }
)

# build model and specify location
dir.m2 <- '1.build.models/2.model.wt.hier/' # used for model patching
"1.build.models/2.model.wt.hier/0.temp.model.patching.r" %>% source # source patching file
save.folder.2 <- 'run.outputs/2.model.wt.hier/'
dir.create(save.folder.2, recursive = TRUE)
model.loc.2 <- 'model.wt.hier.txt'

### inits gen
jags.inits.2 <- function(){
  c(
    list(
      # at priors
      "alpha.at" = at.data %>% quantile(probs = runif(n.6m.windows, 0.25, 0.75), na.rm = TRUE) %>%
        `names<-`(NULL),
      "beta.at" = at.data %>% quantile(probs = c(.1, .9), na.rm = TRUE) %>% 
        diff %>% '/'(2) %>% rnorm(n = n.6m.windows, mean = ., sd = . / 5) %>% `names<-`(NULL),
      "sigma.at" = rep(runif(1, 1 / (2 ^ 2), 1 / (0.5 ^ 2)), n.set.parameters),
      'rho.at' = rep(runif(1, 0.5, .99), n.set.parameters),
      't0.at' =   rnorm(1, mean = - n.t.step.year / 4, sd = (n.t.step.year / 4) / 10),
      # wt priors
      #'theta.0' = runif(1, 14, 20),
      'delta.theta.0' = runif(1, 2, 8),
      'delta.theta.0.p' = runif(1, 2, 8),
      'theta.1' = runif(1, 0, 1),
      'theta.1.p' = runif(1, 0, 1),
      'sigma.max.wt' = rep(runif(1, 1 / (1.1 ^ 2), 1 / (0.1 ^ 2)), 1),
      'sigma.min.wt' = rep(runif(1, 1 / (1.1 ^ 2), 1 / (0.1 ^ 2)), 1),
      't0.wt' = rnorm(1, mean = - n.t.step.year / 4, sd = (n.t.step.year / 4) / 10),
      'sigma.wt' = runif(1, 1, 3) %>% `^`(- 2) 
    ),
    # lfl extra priors if needed
    if(lfl.inc == 1){
      list(
        "alpha.lfl" = lfl.data %>% quantile(probs = runif(n.6m.windows, 0.25, 0.75), na.rm = TRUE) %>%
          `names<-`(NULL),
        "beta.lfl" = lfl.data %>% quantile(probs = c(.1, .9), na.rm = TRUE) %>% 
          diff %>% '/'(2) %>% rnorm(n = n.6m.windows, mean = ., sd = . / 5) %>% `names<-`(NULL),
        "sigma.lfl" = rep(runif(1, 1 / (2 ^ 2), 1 / (0.5 ^ 2)), n.set.parameters),
        'rho.lfl' = rep(runif(1, 0.5, .99), n.set.parameters),
        't0.lfl' = rnorm(1, mean = - n.t.step.year / 4, sd = (n.t.step.year / 4) / 10),
        'theta.2'= runif(1, 0, 1),
        'theta.2.p'= runif(1, -1, 0))
    },
    # part link to residuals modelling
    if(mod.res == 1){ 
      if(lfl.inc == 1){ 
        list('delta' = runif(1, -1, 1), 
             'gamma'= runif(1, -1, 1)
        )
      }else{ 
        list('delta' = runif(1, -1, 1))
      }
    }else{
      list('rho.wt' = runif(1, 0.5, .99))
    }
  )
}

# jags, mcmc details
'mcmc.chains.config.r' %>% source

# name files produced by jags that will be moved
jags.folder <- 'runjagsfiles'
unlink(jags.folder, recursive = TRUE)
unlink(paste0(save.folder.2, 
              jags.folder), recursive = TRUE)


# if DIC to be monitored
if(dic.details == 1){
  dic.param <- c('deviance', 'pd') 
}else{
  dic.param <- 'deviance'
}

# reformat data to fully names list for runjags
jags.data.2.list <- list()
for(i in 1:length(jags.data.2)){
  jags.data.2.list[[jags.data.2[i]]] <- get(jags.data.2[i])
}

# run hierarchical wt model 
jags.outputs.2 <- run.jags(model = model.loc.2, data = jags.data.2.list,
                           noread.monitor = jags.params.2, 
                           keep.jags.files = TRUE,
                           monitor = c(jags.params.2, dic.param),
                           module = list('glm'),
                           burnin = mcmc.burn.2, sample = mcmc.length.2, 
                           n.chains = mcmc.chains.2, thin = mcmc.thin.2,
                           inits = jags.inits.2
)

write.table(Sys.time(), 'time.end.txt')

# move jags stuff ====================================================================================

# create new jags outputs folder
dir.create(paste0(save.folder.2, 
                  jags.folder))

# move files to new folder
file.copy(from = paste0(jags.folder, '/',
                        jags.folder %>% dir), 
          to = paste0(save.folder.2, 
                      jags.folder, '/', 
                      jags.folder %>% dir),
          overwrite = TRUE)

unlink(jags.folder, recursive = TRUE)

# save DIC ===============================================================================================

if(dic.details == 1){
  deviance <- paste0(save.folder.2, "runjagsfiles/deviancetable0.txt") %>% read.table # no colnames
  pd <- paste0(save.folder.2, "runjagsfiles/pdtable0.txt") %>% read.table # no colnames
  
  dic.table <- 
    tapply(deviance$V2, INDEX = deviance$V1 %>% substring(1, 9) , FUN = sum) %>% 
    cbind(tapply(pd$V2, INDEX = pd$V1 %>% substring(1, 9) , FUN = sum)) %>%
    'colnames<-'(c('deviance', 'pd')) %>% as.data.frame %>% 
    mutate(dic = deviance + pd) %>% 
    `rownames<-`(unique(deviance$V1 %>% substring(1, 9)))
  
  write.table(dic.table, file = paste(save.folder.2, "/1.dic.table.txt", sep = ''))
  
}
