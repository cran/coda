\name{as.ts.mcmc}
\alias{as.ts.mcmc}
\title{Coerce mcmc object to time series}

\usage{as.ts.mcmc(x)}

\arguments{
   \item{x}{an mcmc object}
}

\description{
\code{as.ts.mcmc} will coerce an mcmc object to a time series.}

\note{
This is pretending to be an \code{as.ts} method for mcmc objects. But
\code{as.ts} is not generic.}

\author{Martyn Plummer}

\seealso{
\code{\link{as.ts}}
}
\keyword{ts}
\keyword{utilities}
