\name{autocorr.diag}
\alias{autocorr.diag}
\alias{autocorr.diag.mcmc}
\alias{autocorr.diag.mcmc.list}
\title{Autocorrelation function for Markov chains}

\usage{autocorr.diag(mcmc.obj, \dots)}
\arguments{
  \item{mcmc.obj}{an object of class \code{mcmc} or \code{mcmc.list}}
  \item{\dots}{optional arguments to be passed to \code{autocorr}}
}
\description{
\code{autocorr.diag} calculates the autocorrelation function for the
Markov chain \code{mcmc.obj} at the lags given by \code{lags}.
The lag values are taken to be relative to the thinning interval
if \code{relative=TRUE}.  Unlike \code{autocorr}, if \code{mcmc.obj}
has many parmeters it only computes the autocorrelations with itself and
not the cross correlations.  In cases where \code{autocorr} would
return a matrix, this function returns the diagonal of the matrix.
Hence it is more useful for chains with many parameters, but may not
be as helpful at spotting parameters. 

If \code{mcmc.obj} is of class \code{mcmc.list} then the returned
vector is the average autocorrelation across all chains.
}

\value{
A vector containing the autocorrelations.
}

\author{Russell Almond}

\seealso{
  \code{\link{autocorr}}, \code{\link{acf}}, \code{\link{autocorr.plot}}.
}
\keyword{ts}
