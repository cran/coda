\name{autocorr}
\alias{autocorr}
\title{Autocorrelation function for Markov chains}

\usage{autocorr(x, lags = c(0, 1, 5, 10, 50), relative=TRUE)}

\arguments{
\item{x}{an mcmc object}
\item{lags}{a vector of lags at which to calculate the autocorrelation}
\item{relative}{a logical flag. TRUE if lags are relative to the thinning
interval of the chain, or FALSE if they are absolute difference in iteration
numbers}
}

\description{
\code{autocorr} calculates the autocorrelation function for the
Markov chain \code{mcmc.obj} at the lags given by \code{lags}.
The lag values are taken to be relative to the thinning interval
if \code{relative=TRUE}.

High autocorrelations within chains indicate slow mixing and, usually,
slow convergence. It may be useful to thin out a chain with high
autocorrelations before calculating summary statistics: a thinned
chain may contain most of the information, but take up less space in
memory. Re-running the MCMC sampler with a different parameterization
may help to reduce autocorrelation.
}

\value{
A vector or array containing the autocorrelations.
}

\author{Martyn Plummer}

\seealso{
\code{\link{acf}}, \code{\link{autocorr.plot}}.}
}
\keyword{ts}
