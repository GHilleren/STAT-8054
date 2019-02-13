#Load library needed.
library(parallel)

#Define set values and set the seed.
ncores <- detectCores()
n <- 10
nsim <- 1e4
RNGkind("L'Ecuyer-CMRG")
set.seed(8054)

#Unix Forking Method
##Define the functions as given to us in class.

GregsNorm <- function(nsim, mu, sigma) {
  result <- double(nsim)
  for (i in 1:nsim) {
    norm.vec[i] <- mu + rnorm(1, 0, sigma)
  }
  return(norm.vec)
}

##Run the function of interest to perform parallelization.
mout <- mclapply(rep(nsim / ncores, ncores), GregsNorm, mu = 5, sigma = .75, mc.cores = ncores)

#Display the output.
lapply(mout, head)








#Cluster method.
##Set it up.
cl <- makePSOCKcluster(ncores)
clusterExport(cl, c("GregsNorm", "n", "nsim"))

##Run the parallelization.
pout <- parLapply(cl, rep(nsim / ncores, ncores), GregsNorm, mu = 5, sigma = .75)
##Display the output.
pout

##Stop the cluster.
stopCluster(cl)