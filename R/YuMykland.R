yu.mykland.diag <- function(x)
{
  x <- as.mcmc(as.matrix(x))
  n <- niter(x)
  xbar <- apply(x, 2, mean)
  x <- sweep(x, 2, xbar)
  B <- apply(x, 2, cumsum)
  d1 <- (B[-c(n-1,n),] < B[-c(1,n),]) & (B[-c(1,n),] > B[-c(1,2),]) 
  d2 <- (B[-c(n-1,n),] > B[-c(1,n),]) & (B[-c(1,n),] < B[-c(1,2),])
  return(sum(d1 | d2))
}

arsim <- function(a=0.5, n=100)
{
  y <- numeric(n)
  y[1] <- rnorm(1)
  e <- rnorm(n, sd=sqrt(1-a^2))
  for(i in 2:n) {
    y[i] <- a*y[i-1] + e[i-1]
  }
  return(y)
}
