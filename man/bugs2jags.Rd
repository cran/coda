\name{bugs2jags}
\alias{bugs2jags}
\title{Convert WinBUGS data file to JAGS data file}
\usage{
bugs2jags(infile, outfile)
}
\arguments{
  \item{infile}{name of the input file}
  \item{outfile}{name of the output file}
}
\description{
  \code{bugs2jags} converts a WinBUGS data in the format called "S-Plus"
  (i.e. the format created by the \code{dput} function) and writes it in
  \code{dump} format used by JAGS.
  
  NB WinBUGS stores its arrays in row order.  This is different
  from R and JAGS which both store arrays in column order. This
  difference is taken into account by \code{bugs2jags} which will
  automatically reorder the data in arrays, without changing the
  dimension.
}
\references{
  Spiegelhalter DJ, Thomas A, Best NG and Lunn D (2003).
  \emph{WinBUGS version 1.4 user manual}
  MRC Biostatistics Unit, Cambridge, UK.
}
\author{Martyn Plummer}
\seealso{
   \code{\link{dput}}, 
   \code{\link{dump}}.
}
\keyword{file}

