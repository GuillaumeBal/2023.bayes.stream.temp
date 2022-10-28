
################################################################################
#                                cross validation                              #
################################################################################

# compute errors
for(p in start.cross:(n.time.steps - last.to.skip)){
 error.pred[(p - start.cross) + 1] <- 0 + (wt.data.s.perf[p] - wt.data.s[p]) * wt.exist[p]
 error.pred.squared[(p - start.cross) + 1] <- pow(error.pred[(p - start.cross) + 1], 2)
}

# compute RMSE
sum.error.preds.squared <- sum(error.pred.squared[])
mse <- sum.error.preds.squared / sum(wt.exist[ start.cross:(n.time.steps - last.to.skip)])#((n.time.steps - last.to.skip) - start.cross + 1)
rmse <- pow(mse, .5)

# compute pred biases
mean.errors.pred <- mean(error.pred[])
bias.pred <- step(mean.errors.pred)
