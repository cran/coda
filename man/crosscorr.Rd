\name{crosscorr}
\alias{crosscorr}
\title{Cross correlations for MCMC output}

\usage{crosscorr(x)}

\arguments{
   \item{x}{an \code{mcmc} or \code{mcmc.list} object.}
}
\description{
   \code{crosscorr} calculates cross-correlations between
   variables in Markov Chain Monte Carlo output. If \code{x}
   is an mcmc.list then all chains in \code{x} are combined
   before calculating the correlation.
}

\value{
A matrix or 3-d array containing the correlations.
}

\seealso{
   \code{\link{crosscorr.plot}}, \code{\link{autocorr}}.
}
\keyword{multivariate}
\keyword{array}
