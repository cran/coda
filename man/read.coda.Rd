\name{read.coda}
\alias{read.coda}
\alias{read.jags}
\title{Read output files in CODA format}

\usage{
read.coda(output.file, index.file, start, end, thin, quiet=FALSE)
read.jags(file = "jags.out", start, end, thin, quiet=FALSE)
}

\arguments{
  \item{output.file}{The name of the file containing the monitored
    output}
  \item{index.file}{The name of the file containing the index, showing
    which rows of the output file correspond to which variables}
  \item{file}{For JAGS output, the name of the output file.  The
    extension ".out" may be omitted. There must be a corresponding
    ".ind" file with the same file stem.}
  \item{start}{First iteration of chain}
  \item{end}{Last iteration of chain}
  \item{thin}{Thinning interval for chain}
  \item{quiet}{Logical flag. If true, a progress summary will be printed}
}

\description{
   \code{read.coda} reads Markov Chain Monte Carlo output in
   the CODA format produced by OpenBUGS and JAGS. By default, all
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
   \code{\link{read.coda.interactive}},
   \code{\link{read.openbugs}}.
}
\keyword{file}
