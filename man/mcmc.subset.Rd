\name{mcmc.subset}
\alias{[.mcmc}
\alias{[.mcmc.list}
\title{Extract or replace parts of MCMC objects}

\usage{
x[i,j]
}

\arguments{
   \item{x}{An \code{mcmc} object}
   \item{i}{Row to extract}
   \item{j}{Column to extract}
}

\description{

   These are methods for subsetting \code{mcmc} objects.  You can select
   iterations using the first dimension and variables using the second
   dimension.  Selecting iterations will return a vector or matrix, not an
   \code{mcmc} object. If you want to do row-subsetting of an \code{mcmc}
   object and preserve its dimensions, use the \code{window} function.

   Subsetting applied to an \code{mcmc.list} object will simultaneously 
   affect all the parallel chains in the object.
}

\seealso{
   \code{\link{[}},
   \code{\link{window.mcmc}}
}

\keyword{ts}
