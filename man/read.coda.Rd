\name{read.coda}
\alias{read.coda}
\alias{read.bugs}
\alias{read.jags}
\title{Read BUGS output files}

\usage{
read.coda(file, start, end, thin, quiet=FALSE)
read.bugs(file = "bugs.out", start, end, thin, quiet=FALSE)
read.jags(file = "jags.out", start, end, thin, quiet=FALSE)
}

\arguments{
   \item{file}{the name of the file from which the data are to be
   read.  The extension ".out" may be omitted. The data must be
   in the format produced by the BUGS program, and there  must be
   a corresponding ".ind" file} 
   \item{start}{First iteration of chain}
   \item{end}{Last iteration of chain}
   \item{thin}{Thinning interval for chain}
   \item{quiet}{Logical flag. If true, a progress summary will be printed}
}

\description{
   \code{read.coda} reads Markov Chain Monte Carlo output in
   the CODA format produced by WinBUGS and JAGS. By default, all
   of the data in the file is read, but the arguments \code{start},
   \code{end} and \code{thin} may be used to read a subset of the
   data.  If the arguments given to \code{start}, \code{end} or
   \code{thin} are incompatible with the data, they are ignored.
}

\value{
   An object of class \code{mcmc} containing a representation of 
   the data in the file.
}

\references{
   Spiegelhalter DJ, Thomas A, Best NG and Gilks WR (1995).
   \emph{BUGS: Bayesian inference Using Gibbs Sampling, Version 0.50.}
   MRC Biostatistics Unit, Cambridge.
}

\author{Karen Vines, Martyn Plummer}

\seealso{
   \code{\link{mcmc}}, 
   \code{\link{read.bugs.interactive}}.
}
\keyword{file}