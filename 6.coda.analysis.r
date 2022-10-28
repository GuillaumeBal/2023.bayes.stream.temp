# load coda in one file
coda.fread <- fread('run.outputs/2.model.wt.hier/runjagsfiles/CODAchain1.txt', select = 2) %>% 
  cbind(fread('run.outputs/2.model.wt.hier/runjagsfiles/CODAchain2.txt', select = 2)) %>% 
  cbind(fread('run.outputs/2.model.wt.hier/runjagsfiles/CODAchain3.txt', select = 2)) %>% 
  `colnames<-`(c('chain1', 'chain2', 'chain3'))

# rename columns
coda.idx <- fread('run.outputs/2.model.wt.hier/runjagsfiles/CODAindex.txt') %>% 
  `colnames<-`(c('param', 'start', 'end'))

# add a colum with param name for each line
coda.fread %<>% mutate(param = rep(coda.idx$param, each = coda.idx$end[1]) %>% 
                         factor(x = ., levels = coda.idx$param)) %>% as.data.frame()

# main param ===================================================================

# find lines with main param (i.e not Wt estimates)
main.par <- coda.fread$param %>% 
  substring(1, 5) %>% `!=`('wt.da') %>% which

# open pdf check figues
pdf(paste0(save.folder.2, '0.param.mcmc.diag.fread.pdf'))

par(mfrow = c(2, 2))

# compute mcmc chaines summary plus plot check figuresinto pdf
mcmc.summary.param <- by(data = coda.fread[main.par, ],
                         INDICES = coda.fread$param[main.par],
                         FUN = function(x){
                           int <- x[ , 1:3] %>% 
                             lapply(function(x){
                               matrix(x, ncol = 1,
                                      dimnames = list(NULL, '')) %>% mcmc}) %>% 
                             as.mcmc.list() 
                           int %T>% 
                             gelman.plot(auto.layout = FALSE, main = 'Gelman') %T>%
                             densplot(main = 'Density') %T>%
                             traceplot(main = 'Trace') %T>% 
                             list %>% unlist %>% acf(plot = TRUE, main = 'acf')
                           title(x[ , 4] %>% as.character() %>% unique, outer = TRUE, line = -1)
                           return(int %>%  MCMCsummary(n.eff = TRUE,
                                                       func = function(x) quantile(x, probs = c(.25, .75)),
                                                       func_name = c('25%', '75%')))
                         })

# close pdf
dev.off()

# reorder table and save
mcmc.summary.param %<>% as.list() %>% do.call(what = rbind) %>% `rownames<-`(coda.fread$param[main.par] %>% unique %>% sort) %>% 
  .[, c("mean", "sd", "2.5%", '25%', "50%", '75%', "97.5%", "Rhat", "n.eff")] %T>% 
  write.table(paste0(save.folder.2, '0.param.mcmc.summary.txt'))
