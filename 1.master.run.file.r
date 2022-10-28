# stop and return error message id run.config not supported
if(length(run.config) > 2){
  stop('run.config cannot be of length > 2, check entries in "0.define.your.run.r"')
}
if(length(run.config) == 2 & ! all(run.config %in% c(2, 3))){
  stop('Only run.config of length 2 supported is c(2, 3) , check entries in "0.define.your.run.r"')
} 

#===============================================================================
# a few minor options

start.time <- Sys.time()

# save starting time
write.table(start.time, 'time.start.txt')

# to avoid scientific notation on large numbers, increase scipen
options("scipen" = 3)

#=============================================================================== 
# load required packages (install them if needed)

# lubridate for easier work with dates
if(library('lubridate', logical.return = TRUE)){
  require(lubridate)
}else{
  install.packages('lubridate')
  require(lubridate)
}

# magrittr, because sometimes pipes are cool for coding
if(library('magrittr', logical.return = TRUE)){
  require(magrittr)
}else{
  install.packages('magrittr')
  require(magrittr)
}

# magrittr, because sometimes pipes are cool for coding
if(library('dplyr', logical.return = TRUE)){
  require(dplyr)
}else{
  install.packages('dplyr')
  require(dplyr)
}

# reshape, used to formot some plot data
if(library('reshape', logical.return = TRUE)){
  require(reshape)
}else{
  install.packages("reshape")
  require(reshape)
}

# MCMCvis, used to summarise some MCMC
if(library('MCMCvis', logical.return = TRUE)){
  require(MCMCvis)
}else{
  install.packages("MCMCvis")
  require(MCMCvis)
}

# runjags to run jags from R
if(library('runjags', logical.return = TRUE)){
  require(runjags)
}else{
  install.packages("runjags")
  require(runjags)
}

# coda to analyse coda
if(library('coda', logical.return = TRUE)){
  require(coda)
}else{
  install.packages("coda")
  require(coda)
}

# ggpipe to combine ggplot2 qnd pipes from magrittr
if(library('ggpipe', logical.return = TRUE)){
  require(ggpipe)
}else{
  # need devtools to install from GITHUB
  if(library('devtools', logical.return = TRUE)){
    require(devtools)
  }else{
    install.packages("devtools")
    require(devtools)
  }
  install_github("zeehio/ggpipe")
  require(ggpipe)
}

# data.table to load big coda files faster
if(library('data.table', logical.return = TRUE)){
  require(data.table)
}else{
  install.packages("data.table")
  require(data.table)
}


# ==============================================================================
# charge and extract elements

raw.data <- read.table(file = '0.data' %>%  dir(full.names = TRUE) %>% .[grepl(., pattern = 'csv')], 
                       sep = ',', h = T,
                       stringsAsFactors = FALSE)

raw.data$date %<>% as.Date(., format = date.format) 

# if 4 column, then flow must be included
lfl.inc <- ncol(raw.data) %>% '=='(4) %>% '*'(1)

# define first year hindcast
year.start.hindcast <- 1
# define first year history
start.hist.date <- raw.data$water.temp %>% is.na %>% `!` %>% which %>% min
year.start.hist <- raw.data$date[start.hist.date] %>% year %>% `-`(min(year(raw.data$date))) %>% `+`(1)
# define end year forecast even if none
year.end.forecast <- max(year(raw.data$date)) %>%
  `-`(min(year(raw.data$date))) %>% `+`(1)
# define end year hist
end.hist.date <- raw.data$date[is.na(raw.data$water.temp) %>% `!` %>% which %>% max] 
year.end.hist <- end.hist.date %>% year %>%
  `-`(min(year(raw.data$date))) %>% `+`(1)
# define start year forecast even if none
year.start.forecast <- year.end.hist %>% `+`(1)
# subset data according to model run type chosen
year.index <- raw.data$date %>% year %>%
  `-`(min(year(raw.data$date))) %>% `+`(1)


# subset data based on run configuration chosen
if(run.config %>% length == 1 & any(run.config %in% c(1, 4))){
  raw.data %<>% `[`(year.index %in% (year.start.hist:year.end.hist), )
}
if(run.config %>% length == 1 & any(run.config == 2)){
  raw.data %<>% `[`(year.index %in% (year.start.hindcast:year.end.hist), )
}
if(run.config %>% length == 1 & any(run.config == 3)){
  raw.data %<>% `[`(year.index %in% (year.start.hist:year.end.forecast), )
}

#================================================================================
# format data and run models

if(run.or.load == 'run'){ # run model part
  
  # data formatting for time sries shifting model
  source('2.data.formatting.base.r')
  
  # find shift parameters of time series
  source('3.run.ts.shift.model.r')
  
  # remove elements to improve memory space
  rm(list = ls()[!ls() %in% c('wd', 'date.format', 'time.step.option', 'n.time.steps.year',
                              'year.start.hist', 'year.end.hist',
                              'year.start.forecast', 'year.end.forecast',
                              'year.start.hindcast',
                              't.y.idx',
                              'mcmc.length.dic', 'mcmc.thin.dic',
                              'pi.value', 't0.value',
                              'mod.res', 'run.config', 'cross.frac', 'n.set.parameters',
                              'run.type', 'dic.details', 'fit.checks',
                              'lfl.inc', 'start.cross', 'save.wt', 'sigma.constraint', 'allow.hier',
                              'north.hem')])
  
  # format data for wt modeling
  source('4.data.formatting.stream.temp.model.r')
  
  # save details run
  run.details <- save.image('run.config.RData')
  
  # run wt model
  source('5.run.stream.temp.model.coda.r')
  
}else{
  source('2.data.formatting.base.r')
  source('4.data.formatting.stream.temp.model.r')
  save.folder.2 <- 'run.outputs/2.model.wt.hier/'
}

# ==============================================================================
# diagnostics and outputs production section

# go through coda to produce summaries and diagnostics
source('6.coda.analysis.r')

# produce final wt time series including historical values and estimates
# because of time series shifting first few values of the first year are not estimated
if(save.wt == 1) source('7.full.wt.ts.coda.r')

# some visual checks
pdf(paste0(save.folder.2, 'visual.checks.pdf'))
source('8.some.visual.checks.r', print = TRUE)
dev.off()
