\name{read.openbugs}
\alias{read.openbugs}
\title{Read CODA output files produced by OpenBUGS}

\usage{
read.openbugs(stem="", start, end, thin, quiet=FALSE)
}

\arguments{
  \item{stem}{Character string giving the stem for the output files.
    OpenBUGS produces files with names "<stem>CODAindex.txt",
    "<stem>CODAchain1.txt", "<stem>CODAchain2.txt", ...}
  \item{start}{First iteration of chain}
  \item{end}{Last iteration of chain}
  \item{thin}{Thinning interval for chain}
  \item{quiet}{Logical flag. If true, a progress summary will be printed}
}

\description{
   \code{read.openbugs} reads Markov Chain Monte Carlo output in
   the CODA format produced by OpenBUGS.

   This is a convenience wrapper around the function \code{read.coda}
   which allows you to read all the data output by OpenBUGS by
   specifying only the file stem.
}

\value{
  An object of class \code{mcmc.list} containing output from all
  chains.
}

\references{
  Spiegelhalter DJ, Thomas A, Best NG and Lunn D (2004).
  \emph{WinBUGS User Manual, Version 2.0, June 2004},
  MRC Biostatistics Unit, Cambridge.
}

\author{Martyn Plummer}

\seealso{
   \code{\link{read.coda}}.
}
\keyword{file}
