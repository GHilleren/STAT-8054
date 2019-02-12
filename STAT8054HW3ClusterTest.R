#Load library needed.
library(parallel)

#Define values and the seed.
ncores <- detectCores()
n <- 10
nsim <- 1e4
theta <- 1
RNGkind("L'Ecuyer-CMRG")
set.seed(42)

#Unix Forking Method
##Define the functions.
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