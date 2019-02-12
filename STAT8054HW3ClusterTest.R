#Load library needed.
library(parallel)

#Define set values and set the seed.
ncores <- detectCores()
n <- 10
nsim <- 1e4
theta <- 1
RNGkind("L'Ecuyer-CMRG")
set.seed(8054)

#Unix Forking Method
##Define the functions as given to us in class.
mlogl <- function(theta, x) sum(- dnorm(x, theta, abs(theta), log = TRUE))

mle <- function(x) {
  theta.start <- sign(mean(x)) * sd(x)
  if (all(x == 0) || theta.start == 0)
    return(0)
  nout <- nlm(mlogl, theta.start, iterlim = 1000, x = x)
  if (nout$code > 3)
    return(NaN)
  return(nout$estimate)
}

doit <- function(nsim, estimator) {
  result <- double(nsim)
  for (i in 1:nsim) {
    x <- rnorm(n, theta, abs(theta))
    result[i] <- estimator(x)
  }
  return(result)
}

##Run the function of interest to perform parallelization.
mout <- mclapply(rep(nsim / ncores, ncores), doit,
                 estimator = mle, mc.cores = ncores)

#Display the output.
lapply(mout, head)








#Cluster method.
##Set it up.
cl <- makePSOCKcluster(ncores)
clusterExport(cl, c("doit", "mle", "mlogl", "n", "nsim", "theta"))

##Run the parallelization.
pout <- parLapply(cl, rep(nsim / ncores, ncores), doit, estimator = mle)
##Display the output.
pout

##Stop the cluster.
stopCluster(cl)