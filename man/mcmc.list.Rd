\name{mcmc.list}
\alias{mcmc.list}
\alias{as.mcmc.list}
\alias{as.mcmc.list.default}
\alias{is.mcmc.list}
\alias{plot.mcmc.list}
\title{Replicated Markov Chain Monte Carlo Objects}

\usage{
mcmc.list(...)
as.mcmc.list(x, ...)
is.mcmc.list(x)
}

\arguments{
  \item{...}{a list of mcmc objects}
  \item{x}{an object that may be coerced to mcmc.list}
}


\description{

The function `mcmc.list' is used to represent parallel runs of the same
chain, with different starting values and random seeds.  The list must
be balanced: each chain in the list must have the same iterations and
the same variables.

Diagnostic functions which act on \code{mcmc} objects may also be applied
to \code{mcmc.list} objects. In general, the chains will be combined,
if this makes sense, otherwise the diagnostic function will be applied
separately to each chain in the list.

Since all the chains in the list have the same iterations, a single time
dimension can be ascribed to the list. Hence there are time series methods
\code{time}, \code{window}, \code{start}, \code{end}, \code{frequency}
and \code{thin} for \code{mcmc.list} objects.

An \code{mcmc.list} can be indexed as if it were a single mcmc object
using the \code{[} operator (see examples below). The \code{[[} operator
selects a single \code{mcmc} object from the list.
}

\author{Martyn Plummer}

\seealso{
   \code{\link{mcmc}}.
}

\examples{
data(line)
x1 <- line[[1]]                    #Select first chain
x2 <- line[,1, drop=FALSE]         #Select first var from all chains
varnames(x2) == varnames(line)[1]  #TRUE
}

\keyword{ts}
