\name{rejectionRate}
\alias{rejectionRate}
\alias{rejectionRate.mcmc}
\alias{rejectionRate.mcmc.list}
\title{Rejection Rate for Metropolis--Hastings chains}

\usage{rejectionRate(x) }
\arguments{
  \item{x}{An \code{mcmc} or \code{mcmc.list} object.}
}
\description{
\code{rejectionRate} calculates the fraction of time that a
Metropolis--Hastings type chain rejected a proposed move.  The rejection
rate is calculates separately for each variable in the \code{mcmc.obj}
argument, irregardless of whether the variables were drawn separately or
in a block.  In the latter case, the values returned should be the
same. 
}
\details{
For the purposes of this function, a "rejection" has occurred if the
value of the time series is the same at two successive time points.
This test is done naively using \code{==} and may produce problems due
to rounding error.
}
\value{
A vector containing the rejection rates, one for each variable.
}

\author{Russell Almond}

\keyword{ts}
