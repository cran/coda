\name{mcmc}
\alias{mcmc}
\alias{as.mcmc}
\alias{as.mcmc.default}
\alias{is.mcmc}
\alias{print.mcmc}
\title{Markov Chain Monte Carlo Objects}

\usage{
mcmc(data= NA, start = 1, end = numeric(0), thin = 1)
as.mcmc(x)
is.mcmc(x)
}

\arguments{
  \item{data}{a vector or  matrix of MCMC output}
  \item{start}{the iteration number of the first observation}
  \item{end}{the iteration number of the last observation}
  \item{thin}{the thinning interval between consecutive observations}
  \item{x}{An object that may be coerced to an mcmc object}
}

\description{

   The function `mcmc' is used to create a Markov Chain Monte Carlo object.
   The data are taken to be a vector, or a matrix with one column per
   variable.

   An mcmc object may be summarized by the \code{summary} function
   and visualized with the \code{plot} function.

   MCMC objects resemble time series (\code{ts}) objects and have
   methods for the generic functions \code{time}, \code{start},
   \code{end}, \code{frequency} and \code{window}.
}

\author{Martyn Plummer}

\note{
   The format of the mcmc class has changed between coda version 0.3
   and 0.4.  Older mcmc objects will now cause \code{is.mcmc} to
   fail with an appropriate warning message.  Obsolete mcmc objects can
   be upgraded with the \code{mcmcUpgrade} function.
}


\seealso{
   \code{\link{mcmc.list}},
   \code{\link{mcmcUpgrade}},
   \code{\link{thin}},
   \code{\link{window.mcmc}},
   \code{\link{summary.mcmc}},
   \code{\link{plot.mcmc}}.
}

\keyword{ts}
