# load time series used before
data.ts <- read.table('0.data/formatted.time.series.txt', h = T)
attach(data.ts)

#if(any(run.config == 4)){
#  wt.data[start.cross:length(wt.data)] <- NA
#}

# read information about how to t0 data
t0.data <- read.table(paste0('run.outputs/1.ts.shift', '/0.param.summary.txt'), h = T)

# extract values
t0.wt <- round(t0.data[which(substring(rownames(t0.data), 1, 2) == 't0'), 2], 2)

# centering values sin
centering.sin.values <- c(18.25, 91.25)#c(6.25, 18.75)

# t0 applied
t0.value <- round(c(-t0.wt - centering.sin.values[time.step.option]), 0)  # but wt only as have to pick only one

# t0 time series for model
wt.data.s <- c(wt.data[(t0.value + 1) : length(wt.data)], rep(NA, t0.value))
wt.data.s.perf <- c(wt.data.perf[(t0.value + 1) : length(wt.data.perf)], rep(NA, t0.value))
at.data.s <- c(at.data[(t0.value + 1) : length(at.data)], rep(NA, t0.value))
if(lfl.inc == 1){
  if(t0.value > 1){
    lfl.data.s <- c(lfl.data[(t0.value + 1) : length(lfl.data)], rep(NA, t0.value))
  }else{
    lfl.data.s <- c(rep(NA, abs(t0.value)), lfl.data[1:(length(lfl.data) - abs(t0.value))])
  }
}

# ind.6m, give each data point its correspond 6 month period
if(time.step.option == 1){
  ind.6m <- rep(seq(1, (year.end.forecast - ifelse(any(run.config == 2), year.start.hindcast, year.start.hist) + 1) * 2, 1), 
                rep(c(36, 37), year.end.forecast - ifelse(any(run.config == 2), year.start.hindcast, year.start.hist) + 1))
}else{
  change.6m <- t.y.idx %>% `<=`(183) %>% `*`(1)
  ind.6m <- rep(NA, t.y.idx %>% length)
  ind.6m[1] <- 1
  ind.6m.current <- 1
  for(t in 2:length(ind.6m)){
    if(change.6m[t] != change.6m[t - 1]) ind.6m.current %<>% `+`(1)
    ind.6m[t] <- ind.6m.current
  }
  #plot(ind.6m, type = 'l') 
}

# build ac interval, ac on full time series would slow things a lot ==========================

# start of ac interval
ac.period.cutoff.1.1 <- seq(from = 1, 
                            to = data.ts$date %>% year %>% `-`(min(data.ts$date %>% year)) %>% `+`(1) %>% 
                              `<=`(year.end.hist) %>% sum, 
                            by = c(90, 270)[time.step.option])
# start ac interval for forecast (break to avoid influence sim data, might be overkill, try might not be necessary anymore)
try(ac.period.cutoff.2.1 <- seq(from = data.ts$date %>% year %>% `-`(min(data.ts$date %>% year)) %>% `+`(1) %>% 
                                  `>`(year.end.hist) %>% sum %>% `+`(1), 
                                to = data.ts$date %>% length, 
                                by = c(90, 270)[time.step.option]),
    silent = TRUE) 

# end ac periods historical data
ac.period.cutoff.1.2 <- ac.period.cutoff.1.1 + c(90, 270)[time.step.option] - 1 # - 1 for stopping before start next
ac.period.cutoff.1.2[length(ac.period.cutoff.1.2)] <- data.ts$date %>% year %>% `-`(min(data.ts$date %>% year)) %>% `+`(1) %>% 
  `<=`(year.end.hist) %>% which %>% max # make sure stop max, not further
# end ac interval forecasting period
if(exists('ac.period.cutoff.2.1')){
  ac.period.cutoff.2.2 <- ac.period.cutoff.2.1 + c(90, 270)[time.step.option] - 1
  ac.period.cutoff.2.2[length(ac.period.cutoff.2.2)] <- data.ts$date %>% length
  #get all bits together
  ac.period.cutoff <- sort(unique(c(ac.period.cutoff.1.1, ac.period.cutoff.1.2,  
                                    ac.period.cutoff.2.1, ac.period.cutoff.2.2)))
}else{
  # get all bits together
  ac.period.cutoff <- sort(unique(c(ac.period.cutoff.1.1, ac.period.cutoff.1.2)))
}

# 1 or two sets param ====================================================================

# because forecasting assumed to differ
# ifelse(year.end.hist == year.end.forecast , n.set.parameters <- 1, n.set.parameters <- 2)

# whether to sets param
ifelse(n.set.parameters == 1, 
       set.par <- rep(1, ac.period.cutoff.1.2 %>% max),
       set.par <- c(rep(1, ac.period.cutoff.1.2 %>% max),
                    rep(2, at.data %>% length %>% `-`(ac.period.cutoff.1.2 %>% max)))
)

# more indices ================================================================================================

# n.ac.periods
n.ac.periods <- length(ac.period.cutoff) / 2
n.6m.windows <- max(ind.6m)
n.t.step.year <- c(73, 365)[time.step.option]

# pi value for sinus component
pi.value <- pi

# n.time.steps
n.time.steps <- length(wt.data.s)#n.time.steps.year * (year.end.forecast - year.start.hist + 1)

# start forecast
if(any(run.config %in% 3)){
  start.for <- wt.data.s %>% is.na %>% `!` %>% which %>%
    max %>% `/`(n.time.steps.year) %>% ceiling %>% `*`(n.time.steps.year) %>% `+`(1) 
}else{
  start.for <- length(wt.data.s) + 1
}

# make param air and or log flow hierarchical ? ==========================================================

# max consecutive na in 6 m air
na.cons.max.6m.at <- at.data.s %>% 
  by(., INDICES = ind.6m,
     function(x){
       rle.outputs <- rle(is.na(x))
       max.cons.na <- ifelse(any(rle.outputs$values == TRUE),
                             max(rle.outputs$lengths[rle.outputs$values == TRUE]),
                             0)
       return(max.cons.na)
     }) %>% c()

# if in 1 6 month period more than 75% na, then hier
ifelse(max(na.cons.max.6m.at) > ((n.time.steps.year) / 2 * .75),
       at.hier <- 1,
       at.hier <- 0)

# max consecutive na in 6 m log flow if flow included
if(lfl.inc == 1){
  
  na.cons.max.6m.lfl <- lfl.data.s %>% 
    by(., INDICES = ind.6m,
       function(x){
         rle.outputs <- rle(is.na(x))
         max.cons.na <- ifelse(any(rle.outputs$values == TRUE),
                               max(rle.outputs$lengths[rle.outputs$values == TRUE]),
                               0)
         return(max.cons.na)
       }) %>% c()
  
  # if in 1 6 month period more than 75% na, then hier
  ifelse(max(na.cons.max.6m.lfl) > ((n.time.steps.year) / 2 * .75),
         lfl.hier <- 1,
         lfl.hier <- 0)
  
}

# find if at leat one is hierarchcal
some.hier <- any(c(at.hier, 
                   if(exists('lfl.hier')) lfl.hier) == 1)

# overwrite based on choice if need
if(allow.hier == 0){
  some.hier <- FALSE
  at.hier <- 0
  if(exists('lfl.hier')) lfl.hier <- 0
}

if(some.hier == TRUE){
  n.6m.windows.hist <- (year.end.hist- ifelse(any(run.config == 2), year.start.hindcast, year.start.hist) + 1) * 2
  n.6m.windows.for <- (year.end.hist- year.start.forecast + 1) * 2
}
