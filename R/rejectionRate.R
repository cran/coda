
rejectionRate.mcmc <- function (x) {
  n <- nrow(x)
  apply(x[1:(n-1),] == x[2:n,],2,mean)
}

rejectionRate.mcmc.list <- function (x) {
  apply(sapply(x,rejectionRate.mcmc),1,mean)
}

rejectionRate <- function(x) {
  UseMethod("rejectionRate")
}


