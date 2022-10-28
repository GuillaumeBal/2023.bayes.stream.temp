#load the different bits

at.model.option <-
  ifelse(at.hier == 0,
         '1a.at.r',
         ifelse(any(run.config == 3),
                '1c.at.hier.for.r',
                '1b.at.hier.r'))

lfl.model.option <- '2a.lfl.r'

if(exists('lfl.hier')){
  lfl.model.option <- ifelse(lfl.hier == 0,
                             '2a.lfl.r',
                             ifelse(any(run.config == 3),
                                    '2c.lfl.hier.for.r',
                                    '2b.lfl.hier.r'))
}

m2.at <- readLines(paste0(dir.m2, at.model.option), 
                   file.info(paste0(dir.m2, at.model.option))$size)

m2.lfl <- readLines(paste0(dir.m2, lfl.model.option), 
                    file.info(paste0(dir.m2, lfl.model.option))$size)

m2.wt.base <- readLines(paste0(dir.m2, '3.wt.base.r'), 
                        file.info(paste0(dir.m2, '3.wt.base.r'))$size)

m2.fit.checks <- readLines(paste0(dir.m2, '4.fit.checks.r'), 
                           file.info(paste0(dir.m2, '4.fit.checks.r'))$size)

m2.cross.val <- readLines(paste0(dir.m2, '5.cross.validation.r'), 
                          file.info(paste0(dir.m2, '5.cross.validation.r'))$size)


# modify wt as suitable ------------------------------------------------------------------

if(mod.res == 0){
  delta.pos <- m2.wt.base %>% grepl(., pattern = 'delta ') %>% which
  m2.wt.base[delta.pos[1:2]] <- '0 +'
  m2.wt.base[delta.pos[3]] <- ''
  gamma.pos <- m2.wt.base %>% grepl(., pattern = 'gamma ') %>% which
  m2.wt.base[gamma.pos[1:2]] <- '0'
  m2.wt.base[gamma.pos[3]] <- ''
  rho.pos <- m2.wt.base %>% grepl(., pattern = 'rho.wt <-') %>% which
  m2.wt.base[rho.pos] <- 'rho.wt ~ dbeta(5, 2)'
}

# lines to suppress if not flow data
if(lfl.inc == 0){
  theta.2.pos <- m2.wt.base %>% grepl(., pattern = 'theta.2') %>% which
  m2.wt.base[theta.2.pos[1:2]] <- '0'
  m2.wt.base[theta.2.pos[3:4]] <- ''
  if(mod.res == 1){ 
    # this if might not be necessary, just in case would cause issue if gamma alrealdy removed with loop above
    gamma.pos <- m2.wt.base %>% grepl(., pattern = 'gamma ') %>% which
    m2.wt.base[gamma.pos[1:2]] <- '0'
    m2.wt.base[gamma.pos[3]] <- ''
  }
}

if(sigma.constraint == 1){
  tau.max.wt.pos <- m2.wt.base %>% grepl(., pattern = 'sigma.max.wt ~') %>% which
  m2.wt.base[tau.max.wt.pos] <- 'sigma.max.wt <- sigma.min.wt'
}

m2.wt <- m2.wt.base

# combine all bits together ------------------------------------------------------------------

model.ts <- c(
  'model{',
  m2.at,
  if(lfl.inc == 1) m2.lfl,
  m2.wt,
  if(any(run.config == 4)) m2.cross.val,
  if(fit.checks == 1) m2.fit.checks,
  '}'
)

#create empty file
file.create('model.wt.hier.txt', overwrite = TRUE)
for(l in 1:length(model.ts)){
  write(model.ts[l], 'model.wt.hier.txt', append = TRUE)
}
