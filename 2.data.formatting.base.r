# if some flow values above 1000, then liters and change to m3
if(lfl.inc == 1){
  if(any(raw.data$flow > 1000)) raw.data$flow <- raw.data$flowraw.data$flow / 1000
}

# check for negative water temperature and flow data
#raw.data$water.temp[10] <- -1
if(any(raw.data$water.temp < 0 | raw.data$flow < 0, na.rm = TRUE)){
  warning("dummy error, negative water temperature or flow data, corrected to .1", call. = FALSE)
  if(any(raw.data$water.temp < 0, na.rm = TRUE)) raw.data$water.temp[raw.data$water.temp < 0] <- .1
  if(any(raw.data$flow < 0, na.rm = TRUE)) raw.data$flow[raw.data$flow < 0] <- .1
}else{
  print('No negative water temperature or flow data')
}

# derive some elements for data formatting
year.data <- year(raw.data$date)
month.data <- month(raw.data$date)
day.data <- yday(raw.data$date)
day.index <- seq(1, dim(raw.data)[1])

# formatting for 15 days time period

if(time.step.option == 1){
  # time option 2, by 5 days
  if(any(substring(as.character(raw.data$date), 6, 10) == '02-29')){
    bissex.suppress <- which(substring(as.character(raw.data$date), 6, 10) == '02-29')
    raw.data.minus.0229 <- raw.data[-bissex.suppress, ]
  }else{
    raw.data.minus.0229 <- raw.data
  }
  cut.off.series <- seq(1, dim(raw.data.minus.0229)[1], 5)
  
  # water temperature time series
  wt.data <- sapply(1:length(cut.off.series), function(i){
    ifelse(sum(!is.na(raw.data.minus.0229$water.temp[cut.off.series[i]:(cut.off.series[i] + 4)])) >= 2,
           mean(raw.data.minus.0229$water.temp[cut.off.series[i]:(cut.off.series[i] + 4)], na.rm = TRUE),
           NA)
  }) 
  
  # air temperature time series
  at.data <- sapply(1:length(cut.off.series), function(i){
    ifelse(sum(!is.na(raw.data.minus.0229$air.temp[cut.off.series[i]:(cut.off.series[i] + 4)])) >= 2,
           mean(raw.data.minus.0229$air.temp[cut.off.series[i]:(cut.off.series[i] + 4)], na.rm = TRUE),
           NA)
  }) 
  
  if(lfl.inc == 1){
    # time flow time series
    lfl.data <- log(sapply(1:length(cut.off.series), function(i){
      ifelse(sum(!is.na(raw.data.minus.0229$flow[cut.off.series[i]:(cut.off.series[i] + 4)])) >= 2,
             mean(raw.data.minus.0229$flow[cut.off.series[i]:(cut.off.series[i] + 4)], na.rm = TRUE),
             NA)
    }))
  }
  
}else{ 
  
  # do not suppress 02-29 anymore , but keep variable raw.data.minus.0229 for code consistancy
  
  #if(any(substring(as.character(raw.data$date), 6, 10) == '02-29')){
  #  bissex.suppress <- which(substring(as.character(raw.data$date), 6, 10) == '02-29')
  #  raw.data.minus.0229 <- raw.data[-bissex.suppress, ]
  #}else{
  #  raw.data.minus.0229 <- raw.data
  #}
  
  raw.data.minus.0229 <- raw.data
  
  cut.off.series <- seq(1, dim(raw.data.minus.0229)[1], 1)
  
  # water temperature time series
  wt.data <- raw.data.minus.0229$water.temp
  
  # air temperature time series
  at.data <- raw.data.minus.0229$air.temp
  
  # time flow time series
  if(lfl.inc == 1) lfl.data <- log(raw.data.minus.0229$flow)
  
} 

# some more indices
time.steps.1 <- seq(1, length(wt.data), 1)
n.time.steps.1 <- length(time.steps.1)
n.time.steps.year <- c(73, 365)[time.step.option]
pi.value <- pi

# t.y.idx, necessary for 366 days leap years
if(time.step.option == 1){
  t.y.idx <- rep(1:73, length.out = length(at.data))
}else(
  t.y.idx <- yday(raw.data.minus.0229$date)
)

# start cross
n.years <- raw.data.minus.0229$date %>% year %>% max -
  raw.data.minus.0229$date %>% year %>% min + 1

if(any(run.config == 4)){
  if(is.integer(n.years * cross.frac)){
    start.cross <- n.years * cross.frac * n.time.steps.year + 
      1
  } else{
    start.cross <- floor(n.years * cross.frac) * n.time.steps.year + 
      round(n.time.steps.year / 2) +
      1
  }
}


# plot data used
#par(mfrow = c(3, 1),
par(mfrow = c(c(2, 3)[1 + lfl.inc], 1),
    mar = c(2, 4, 2, 2),
    oma = c(1, .5, 0.5, .5))
plot(wt.data, type = 'l', main = 'Data used')
if(any(run.config == 4)) lines(x = start.cross:length(wt.data),
                          y = wt.data[start.cross:length(wt.data)], col = 'lightgrey')
plot(at.data, type = 'l')
if(lfl.inc == 1) plot(lfl.data, type = 'l')

#======================================================================================

wt.data.pred <- wt.data
if(any(run.config == 4)) wt.data[start.cross:length(wt.data)] <- NA

# save time series
if(lfl.inc == 1){
  write.table(cbind(date = as.character(raw.data.minus.0229$date[cut.off.series]),
                    wt.data = wt.data,
                    wt.data.perf = wt.data.pred, 
                    lfl.data = lfl.data,
                    at.data = at.data), 
              file = paste0('0.data/', 'formatted.time.series.txt'))
}else{
  write.table(cbind(date = as.character(raw.data.minus.0229$date[cut.off.series]),
                    at.data = at.data,
                    wt.data = wt.data, 
                    wt.data.perf = wt.data.pred), 
              file = paste0('0.data/', 'formatted.time.series.txt'))
}