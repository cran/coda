\name{read.bugs.interactive}
\alias{read.bugs.interactive}
\title{Read BUGS output files interactively}

\usage{
read.bugs.interactive()
}

\description{
   \code{read.bugs.interactive} reads Markov Chain Monte Carlo output
   in the format produced by the classic BUGS program. No arguments are
   required. Instead, the user is prompted for the required information.
}

\value{
   An object of class \code{mcmc.list} containing a representation of 
   the data in one or more BUGS output files.
}

\references{
   Spiegelhalter DJ, Thomas A, Best NG and Gilks WR (1995).
   \emph{BUGS: Bayesian inference Using Gibbs Sampling, Version 0.50.}
   MRC Biostatistics Unit, Cambridge.
}

\note{
   This function is normally called by the \code{codamenu} function,
   but can also be used on a stand-alone basis.
}

\author{Nicky Best, Martyn Plummer}

\seealso{
   \code{\link{mcmc}}, 
   \code{\link{mcmc.list}}, 
   \code{\link{read.bugs}}, 
   \code{\link{codamenu}}. 
}
\keyword{files}
