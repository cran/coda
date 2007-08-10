\name{nchain}
\alias{niter}
\alias{nvar}
\alias{nchain}
\title{Dimensions of MCMC objects}

\usage{
niter(x)
nvar(x)
nchain(x)
}

\arguments{
  \item{x}{An \code{mcmc} or \code{mcmc.list} object}
}

\value{
  A numeric vector of length 1:
}

\description{
  These functions give the dimensions of an MCMC object

  \describe{
    \item{niter(x)}{returns the number of iterations.}
    \item{nvar(x)}{returns the number of variables.}
    \item{nchain(x)}{returns the number of parallel chains.}
  }
}

\seealso{
  \code{\link{mcmc}},
  \code{\link{mcmc.list}},
}

\keyword{ts}
