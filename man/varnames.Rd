\name{varnames}
\alias{varnames}
\alias{varnames<-}
\alias{chanames}
\alias{chanames<-}
\title{Named dimensions of MCMC objects}

\usage{
varnames(x, allow.null=TRUE)
chanames(x, allow.null=TRUE)
varnames(x) <- value
chanames(x) <- value
}

\arguments{
\item{x}{an \code{mcmc} or \code{mcmc.list} object}
\item{allow.null}{Logical argument that determines whether the function may return NULL}
\item{value}{A character vector, or NULL}
}

\value{
   A character vector , or NULL.
}

\description{
   \code{varnames()} returns the variable names and \code{chanames}
   returns the chain names, or NULL if these are not set.  

   If \code{allow.null = FALSE} then \code{NULL} values will be
   replaced with canonical names.
}

\seealso{
   \code{\link{mcmc}},
   \code{\link{mcmc.list}}.
}
\keyword{manip}
