# get specific priors
'1.build.models/1.model.ts.shift/priors.values.1.r' %>% source

# jags, data
jags.data.1 <- c(list('wt.data' = wt.data,
                      'n.time.steps.1' = n.time.steps.1,
                      'n.time.steps.year' = n.time.steps.year,
                      'pi.value' = pi.value,
                      't.y.idx' = t.y.idx,
                      'north.hem' = north.hem),
                 priors.spec.1)

# jags, parameters to monitor
jags.params.1 <- c('alpha.wt', 'beta.wt', 't0.wt', 'sigma.wt')

# build model and specify location
dir.m1 <- '1.build.models/1.model.ts.shift/' # used for model patching
"1.build.models/1.model.ts.shift/0.ts.shift.patching.r" %>% source # source patching file
model.loc.1 <- 'model.ts.shift.txt'

# jags, inits gen
jags.inits.1 <- function(){
  list(
    # water temp
    "alpha.wt" = quantile(wt.data, probs = runif(1, 0.25, 0.75), na.rm = TRUE),
    "beta.wt" = (quantile(wt.data, probs = 0.975, na.rm = TRUE) - quantile(wt.data, probs = 0.025, na.rm = TRUE)) / 2 + rnorm(1, 1, 1),
    "sigma.wt" = runif(1, 1, 2)
  )
}

# jags, mcmc details
'mcmc.chains.config.r' %>% source

# jags, run model
jags.outputs.1 <- run.jags(model = model.loc.1, data = jags.data.1, monitor = jags.params.1,
                           burnin = mcmc.burn.1, sample = mcmc.length.1, 
                           n.chains = mcmc.chains.1, thin = mcmc.thin.1,
                           inits = jags.inits.1,
                           module = list('glm'))
# print outputs
print(jags.outputs.1)

# jags summary
output.sum.1 <- jags.outputs.1$summaries

# visual check of Rhat values 
hist(output.sum.1[ , 'psrf'], breaks = length(jags.params.1),
     main = 'Rhat values, issue if values above 1.10')
abline(v = 1.10, col = 'red')

# save summary outputs in main folder
# write.table(output.sum.1, 'ts.shift.outputs.txt')

# plot data times series with fitted curves (median mcmc values)
plot(wt.data, type = 'l', main = 'Used data')
lines(output.sum.1["alpha.wt" , 'Mean'] + 
        output.sum.1["beta.wt" , 'Mean'] * 
        sin(2 * pi * (1:length(wt.data) + 
                        output.sum.1["t0.wt" , 'Mean']) / n.time.steps.year), 
      col = 'red')

# more checks and save ----------------------------------------------------------------------------------

# folder to save outputs
save.folder.1 <- 'run.outputs/1.ts.shift/'
dir.create(save.folder.1, recursive = TRUE)

# mcmc chains summary table
write.table(output.sum.1, file = paste0(save.folder.1, "0.param.summary.txt"))

# pdf with posterior check figures
pdf(paste0(save.folder.1, '0.param.mcmc.diag.fread.pdf'))

par(mfrow = c(2, 2),
    oma = c(1, 1, 2, 1))

# create data frame of mcmc
jags.outputs.1$mcmc %>% 
  as.matrix(chains = TRUE) %>%
  `[`( ,  -1) %>%
  as.data.frame -> mcmc.data.frame.1

# use sapply to make plots while extracting colnames
mcmc.data.frame.1 %>% 
  colnames() %>% 
  sapply(FUN = function(x){
    mcmc.data.frame.1[ , x] %>%
      matrix(., ncol = 3) %>% 
      as.data.frame() %>% 
      lapply(function(x){
        matrix(x, ncol = 1,
               dimnames = list(NULL, colnames(x)[1])) %>% mcmc}) %>% 
      as.mcmc.list() %T>% 
      gelman.plot(auto.layout = FALSE, main = 'Gelman') %T>%
      densplot(main = 'Density') %T>%
      traceplot(main = 'Trace') %T>% 
      list %>% unlist %>% acf(plot = TRUE, main = 'acf')
    title(x, outer = TRUE)
    return(NULL)
  })

dev.off()
