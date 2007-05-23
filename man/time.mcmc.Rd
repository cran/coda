\name{time.mcmc}
\alias{time.mcmc}
\alias{start.mcmc}
\alias{end.mcmc}
\alias{frequency.mcmc}
\alias{thin.mcmc}
\alias{time.mcmc.list}
\alias{start.mcmc.list}
\alias{end.mcmc.list}
\alias{thin.mcmc.list}
\title{Time attributes for mcmc objects}

\usage{
\method{time}{mcmc}(x, \dots)
\method{start}{mcmc}(x, \dots)
\method{end}{mcmc}(x, \dots)
\method{thin}{mcmc}(x, \dots)
}

\arguments{
  \item{x}{an \code{mcmc} or \code{mcmc.list} object}
  \item{\dots}{extra arguments for future methods}
}

\description{
   These are methods for mcmc objects for the generic time
   series functions.
}

\seealso{
   \code{\link{time}},
   \code{\link{start}},
   \code{\link{frequency}},
   \code{\link{thin}}.
}
\keyword{ts}
