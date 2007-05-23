\name{thin}
\alias{thin}
\title{Thinning interval}

\usage{
thin(x, \dots)
}

\arguments{
   \item{x}{a regular time series}
   \item{\dots}{a list of arguments}
 }

\description{
   \code{thin} returns the interval between successive
   values of a time series. \code{thin(x)} is equivalent
   to \code{1/frequency(x)}.

   This is a generic function. Methods have been implemented
   for \code{mcmc} objects.
}

\author{Martyn Plummer}

\seealso{
   \code{\link{time}}.
}
\keyword{ts}
