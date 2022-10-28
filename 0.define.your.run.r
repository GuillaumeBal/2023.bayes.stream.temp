rm(list = ls())

# define main wd
wd.main <- 'C:/Users/gbal/Desktop/2022.bayes.stream.temperature/'
setwd(wd.main)

# if run is on, it will run models and overwrite stored results
# otherwise it will load previously obtained CODA files
run.or.load <- 'load'#'run'

# pick 1 for quickly checking everything runs, 2 for proper run
# go to mcmc.config.r if you wish to customize the length of mcmc chains
run.type <- 1

# is you river in the north hemisphere ?
# 0 for no, 1 for yes
north.hem <- 1

# do you want to model residuals ? 
# 0 for no (model M1), 1 for yes (model M2) 
mod.res <- 1

# pick model run configuation
# base(1), hindcast (2), forecast(3) or cross.validation (4)
# you can use 2 and 3 at the same time but the running could be very long
run.config <- 1 # c(2, 3)

# fraction of data year to be kept for cross.validation 
cross.frac <- 2 / 3 # rounded down if needed as all as a year as to be training or validation data

# forecast (air, or air and flow) data have different autocorrelation and variability ? Enter 2
n.set.parameters <- 1

# time step option
# 1 for 5 days, leap year, 29th February suppressed
# 2 for 1 day
time.step.option <- 2

# do you want to save DIC details ?
# 0 for no, 1 for yes
dic.details <- 1

# do you want the fit checks ? (Chi2, bias, average deviation)
# 0 for no, 1 for yes
fit.checks <- 1

# contraint sigma.min sigma.max ?
# 0 for no, 1 for yes
sigma.constraint <- 0

# Save WT temperature estimates ?
# please note that it can be memory intensive to save full wt series
# you may have to run the model twice, once for forecasting and once for hindcasting
# 0 for no, 1 for yes
save.wt <- 0 

# allow model to potentially pick hierarchical configuration ?
# not advised unless you AT and Q time series have huge gaps, still exploratory 
# 0 for no, 1 for yes
allow.hier <- 0

# specify your date format (if you don't use same as example data file)
date.format <- '%d/%m/%Y'

# sourcinf the file below runs everything ================================

source('1.master.run.file.r')
