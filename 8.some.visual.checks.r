# hist of Rhat values ==================================================

par(par(mfcol = c(1, 1),
        oma = c(.1, .1, 1.5, .1),
        mar = c(2, 2, 2 , 1)))
mcmc.summary.param[ , 'Rhat'] %>% hist(main = 'Rhat histogram', ylab = 'Value')

# plot posteriors theta parameters =====================================

ifelse(lfl.inc == 1,
       par(mfcol = c(5, 2),
           oma = c(.1, .1, 1.5, .1),
           mar = c(2, 2, 2 , 1)),
       par(mfcol = c(4, 2),
           oma = c(.1, .1, 1.5, .1),
           mar = c(2, 2, 2 , 1))
)

param.link <- c('theta.0', 'theta.1', 'theta.2',
                'theta.0.p', 'theta.1.p', 'theta.2.p', 
                'sigma.max.wt', 'sigma.min.wt',
                'delta', 'gamma')

# find lines with main param (i.e not Wt estiamtes)
param.link.pos <- coda.fread$param %>% as.character %>% 
  #strsplit(split = '[', fixed = TRUE) %>% 
  #do.call(what = rbind) %>% 
  #`[`(, 1) %>% 
  `%in%`(param.link) %>% which

# compute mcmc chains summary plus plot
by(data = coda.fread[param.link.pos, ],
   INDICES = coda.fread$param[param.link.pos],
   FUN = function(x){
     x[ , 1:3] %>% 
       unlist %>% 
       hist(breaks = 50, main = x[ , 4] %>% unique, xlab = '')
     return(NULL)
   }) %>% invisible()

title('Parameters relationships between wt / at / lfl', outer = TRUE)

# alpha estimates =============================================================================

alpha.series.mean <-
  mcmc.summary.param %>% `[`(row.names(.) %>% substring(first = 1, last = 5) %>% `==`('alpha'), ) %>% 
  as.data.frame %>% 
  cbind('var' = rownames(.) %>% substring(first = 1, last = 8)) 

alpha.series.mean %>% 
  as.data.frame %>% 
  cbind('year' = rep(0:(n.6m.windows - 1) / 2, ifelse(lfl.inc == 0, 2, 3)) + 
          data.ts$date %>% year %>% min) %>% 
  ggplot(aes(x = year, y = mean, group = var)) %>% 
  geom_line %>% 
  facet_wrap( ~ var, nrow = 2, scales = 'free') %>% 
  geom_smooth()

# if check are included ===========================================================================================

if(fit.checks == 1){
  
  par(mfrow = c(1, 1))
  
  # mcmc.summary.param %>% #head
  #   `[`(row.names(.) %>% substring(1, 10) %>% `==`('check.mean'), ) %>% #colnames
  #   `colnames<-`(c("mean", "sd", "q2.5", "q25", "q50", "q75", "q97.5", "Rhat", "n.eff")) %>% 
  #   as.data.frame %>%
  #   cbind(Index = 1:dim(.)[1] %>% factor(ordered = TRUE)) %>%
  #   ggplot(.) %>%  
  #   geom_boxplot(aes(x = Index,
  #                    ymin = q2.5,
  #                    lower = q25, 
  #                    middle = q50,
  #                    upper = q75,
  #                    ymax = q97.5),
  #                stat = "identity") %>% 
  #   geom_hline(yintercept = 0, col = 'red') %>% 
  #   scale_y_continuous(name = "Deviation °C") %>% 
  #   ggtitle('Difference between 6 month averages of observed and fitted temperatures') %>% 
  #   theme_light()
  
  
  # histogram mean error fit
  mcmc.summary.param %>% #head
    as.data.frame() %>% 
    `[`(row.names(.) %>% `==`('mean.errors.fit'), ) %>% #colnames
    `colnames<-`(c("mean", "sd", "q2.5", "q25", "q50", "q75", "q97.5", "Rhat", "n.eff")) %>% 
    cbind(Index = 1:dim(.)[1] %>% factor(ordered = TRUE)) %>% #head
    ggplot(.) %>%  
    geom_boxplot(aes(x = Index,
                     ymin = q2.5,
                     lower = q25, 
                     middle = q50,
                     upper = q75,
                     ymax = q97.5),
                 stat = "identity") %>% 
    geom_hline(yintercept = 0, col = 'red') %>% 
    scale_y_continuous(name = "Error °C") %>% 
    scale_x_discrete('') %>% 
    ggtitle('Mean error fitted values') %>% 
    theme_light()
  
  # chi2 check ======================================== 
  
  par(mar = c(2, 2, 2, 2))
  
  param.chi2 <- c('sum.chi2.obs', 'sum.chi2.rep')
  
  coda.fread %>% `[`(.$param == param.chi2[1], 1:3) %>% unlist %>% 
    cbind(coda.fread %>% `[`(.$param == param.chi2[2], 1:3) %>% unlist) %>% 
    as.data.frame() %>% 
    `colnames<-`(param.chi2) %>%
    `rownames<-`(NULL) %>%
    ggplot(., aes(x = sum.chi2.obs, y = sum.chi2.rep)) %>% 
    geom_point(shape = '.') %>% 
    stat_function(fun = function(x) x, geom = "line", aes(colour = "y = x")) %>% 
    scale_colour_manual("Function", values = c("blue"), breaks = c("y = x")) %>% 
    theme(legend.position = 'above') %>% 
    theme_light() %>% 
    labs(x = expression(paste('Sum ', Chi[obs]^2, sep = ' ')),
         y = expression(paste('Sum ', Chi[rep]^2, sep = ' ')))
  
}

# if check cross validation ===========================================================================================

if(any(run.config == 4)){
  
  param.cross <- c('rmse', 'mean.errors.pred', 'bias.pred')
  
  coda.fread %>% `[`(.$param == param.cross[1], 1:3) %>% unlist %>% 
    cbind(coda.fread %>% `[`(.$param == param.cross[2], 1:3) %>% unlist) %>% 
    cbind(coda.fread %>% `[`(.$param == param.cross[3], 1:3) %>% unlist) %>% 
    as.data.frame() %>% 
    `rownames<-`(NULL) %>% 
    `colnames<-`(c('RMSE', 'Prediction Mean Error', 'Prediction bias')) %>% #head(20)
    melt %>% 
    ggplot(., aes(value, group = variable)) %>% 
    geom_histogram() %>% 
    facet_wrap(~ variable, ncol = 1, scales = 'free') %>% 
    ggtitle('Prediction checks') %>% 
    theme_light()
  
}   
