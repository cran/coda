\name{mcpar}
\alias{mcpar}
\title{Mcpar attribute of MCMC objects}

\usage{
mcpar(x)
}

\arguments{
\item{x}{An \code{mcmcm} or \code{mcmc.list} object}
}

\description{
     The `mcpar' attribute of an MCMC object gives the start iteration
     the end iteration and the thinning interval of the chain.

     It resembles the `tsp' attribute of time series (\code{ts}) objects.
}

\seealso{
   \code{\link{ts}},
   \code{\link{mcmc}},
   \code{\link{mcmc.list}},
}

\keyword{ts}
