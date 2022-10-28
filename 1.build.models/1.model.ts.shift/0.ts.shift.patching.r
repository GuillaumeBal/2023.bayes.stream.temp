m1.pt.1 <- readLines(paste0(dir.m1,'1.wt.r'), 
                     file.info(paste0(dir.m1,'1.wt.r'))$size)

m1.pt.2 <- readLines(paste0(dir.m1,'2.at.r'), 
                     file.info(paste0(dir.m1,'2.at.r'))$size)

m1.pt.3 <- readLines(paste0(dir.m1,'3.lfl.r'), 
                     file.info(paste0(dir.m1,'3.lfl.r'))$size)

model.ts <- c(
  'model{',
  m1.pt.1,
  #m1.pt.2,
  #if(lfl.inc == 1) eval(parse(text = 'm1.pt.2')),
  '}'
)

#create empty file
file.create('model.ts.shift.txt', overwrite = TRUE)
for(l in 1:length(model.ts)){
  write(model.ts[l], 'model.ts.shift.txt', append = TRUE)
}
