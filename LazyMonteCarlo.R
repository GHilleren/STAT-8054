#Load library needed.
library(parallel)

#Define set values and set the seed.
ncores <- detectCores()
nsim <- 1e4
RNGkind("L'Ecuyer-CMRG")
set.seed(8054)

#Unix Forking Method
##Define a really lazy rnorm function to generate Gaussian numbers.

GregsNorm <- function(nsim, mu, sigma) {
  norm.vec <- double(nsim)
  for (i in 1:nsim) {
    norm.vec[i] <- mu + rnorm(1, 0, sigma)
  }
  return(norm.vec)
}

##Run the function of interest to perform parallelization.
mout <- mclapply(rep(nsim / ncores, ncores), GregsNorm, mu = 5, sigma = .75, mc.cores = ncores)

#Display the head of each entry in the list to show that it works.
lapply(mout, head)








#Cluster method.
##Set it up.
cl <- makePSOCKcluster(ncores)
clusterExport(cl, c("GregsNorm", "n", "nsim"))

##Run the parallelization with the cluster.
pout <- parLapply(cl, rep(nsim / ncores, ncores), GregsNorm, mu = 5, sigma = .75)
##Display the full output just to brag about how much it did.
pout

##Stop the cluster.
stopCluster(cl)