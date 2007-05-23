\name{mcmc.convert}
\alias{as.matrix.mcmc}
\alias{as.matrix.mcmc.list}
\alias{as.array.mcmc.list}
\alias{as.mcmc.mcmc.list}
\title{Conversions of MCMC objects}

\usage{
\method{as.matrix}{mcmc}(x, iters = FALSE, ...)
\method{as.matrix}{mcmc.list}(x, iters = FALSE, chains = FALSE, ...)
\method{as.array}{mcmc.list}(x, drop, ...)
}

\arguments{
  \item{x}{An \code{mcmc} or \code{mcmc.list} object}
  \item{iters}{logical flag: add column for iteration number?}
  \item{chains}{logical flag: add column for chain number? (if mcmc.list)}
  \item{drop}{logical flag: if \code{TRUE} the result is coerced to the
    lowest possible dimension}
  \item{...}{optional arguments to the various methods}
}

\description{
  These are methods for the generic functions \code{as.matrix()},
  \code{as.array()} and \code{as.mcmc()}.
  
  \code{as.matrix()} strips the MCMC attributes from an \code{mcmc}
  object and returns a matrix.  If \code{iters = TRUE} then a
  column is added with the iteration number.  For \code{mcmc.list}
  objects, the rows of multiple chains are concatenated and, if
  \code{chains = TRUE} a column is added with the chain number.
  
  \code{mcmc.list} objects can be coerced to 3-dimensional arrays
  with the \code{as.array()} function.
  
  An \code{mcmc.list} object with a single chain can be coerced
  to an \code{mcmc} object with \code{as.mcmc()}.  If the argument
  has multiple chains, this causes an error.
}

\seealso{
   \code{\link{as.matrix}},
   \code{\link{as.array}},
   \code{\link{as.mcmc}},
}

\keyword{array}
