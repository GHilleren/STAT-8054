#Load library needed.
library(parallel)
ncores <- detectCores()

#Unix Forking Method
##Define the function.
doit <- function(nsim, estimator) {
  result <- double(nsim)
  for (i in 1:nsim) {
    x <- rnorm(n, theta, abs(theta))
    result[i] <- estimator(x)
  }
  return(result)
}

##Run the function.
mout <- mclapply(rep(nsim / ncores, ncores), doit,
                 estimator = mle, mc.cores = ncores)
lapply(mout, head)

##Plot it.
theta.hat <- unlist(mout)
hist(theta.hat, probability = TRUE, breaks = 30)
curve(dnorm(x, mean = theta, sd = theta / sqrt(3 * n)), add = TRUE)



#Cluster method.
##Set up.
clusterExport(cl, c("doit", "mle", "mlogl", "n", "nsim", "theta"))

##Run it.
pout <- parLapply(cl, rep(nsim / ncores, ncores), doit, estimator = mle)

##Stop the cluster.
stopCluster(cl)