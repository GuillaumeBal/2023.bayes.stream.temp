# summary of wt estimates ======================================================================

# table to save mcmc summary for main parameters
mcmc.summary.wt <- by(data = coda.fread[- main.par, ],
                      INDICES = coda.fread$param[- main.par],
                      FUN = function(x){
                        return(
                          x[ , 1:3] %>% 
                            lapply(function(x){
                              matrix(x, ncol = 1,
                                     dimnames = list(NULL, '')) %>% mcmc}) %>% 
                            as.mcmc.list() %>% 
                            MCMCsummary(n.eff = TRUE,
                                        func = function(x) quantile(x, probs = c(.25, .75)),
                                        func_name = c('25%', '75%'))
                        )
                      })

# reorder and save wt summary
mcmc.summary.wt %<>% as.list() %>% do.call(what = rbind) %>% `rownames<-`(coda.fread$param[-main.par] %>% unique %>% sort) %>%
  .[, c("mean", "sd", "2.5%", '25%', "50%", '75%', "97.5%", "Rhat", "n.eff")] %T>% 
  write.table(paste0(save.folder.2, '0.wt.mcmc.summary.txt'))

# get all the bits back in place =================================================================

wt.complete <- 
mcmc.summary.wt %>% 
  as.data.frame %>% # change to data.frame
  rbind(matrix(rep(NA, ncol(mcmc.summary.wt) * t0.value), 
               ncol = ncol(mcmc.summary.wt),
               dimnames = list(NULL, colnames(.))), .) %>%
  cbind(date = c(data.ts$date %>%  as.character %>% as.Date(),
                 data.ts$date %>% tail(1) %>% as.character %>% as.Date() %>% `+`(1:t0.value)))

# reorder output
wt.complete %<>%
  .[ , c("date", "mean", "sd", "2.5%", '25%', "50%", '75%', "97.5%", "Rhat", "n.eff")]

# if first water temp were available, add then instead of NA du to times series shift
wt.complete$mean[1:t0.value] <- data.ts$wt.data[1:t0.value]


# Save completed time series into main folder!!! ==============================================

write.table(wt.complete, 
            'wt.complete.txt', sep = ',', row.names = FALSE)
