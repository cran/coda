\name{effectiveSize}
\alias{effectiveSize}
\title{Effective sample size for estimating the mean}
\description{
Sample size adjusted for autocorrelation.
}
\usage{
effectiveSize(x) 
}
\arguments{
\item{x}{An mcmc or mcmc.list object.}
}
\details{
For a time series \code{x} of length \code{N}, the standard error of
the mean is \code{var(x)/n} where \code{n} is the effective sample
size.  \code{n = N} only when there is no autocorrelation.

Estimation of the effective sample size requires estimating the
spectral density at frequency zero.  This is done by the function
\code{spectrum0.ar}
}
\value{
A vector giving the effective sample size for each column of \code{x}.
}
\seealso{
   \code{\link{spectrum0.ar}}.
}
\keyword{ts}
