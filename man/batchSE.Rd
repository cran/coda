\name{batchSE}
\alias{batchSE}
\title{Batch Standard Error}
\description{
Effective standard deviation of population to produce the correct
standard errors.
}
\usage{
batchSE(x, batchSize=100)
}
\arguments{
  \item{x}{An \code{mcmc} or \code{mcmc.list} object.}
  \item{batchSize}{Number of observations to include in each batch.}
}
\details{
  Because of the autocorrelation, the usual method of taking
  \code{var(x)/n} overstates the precision of the estimate.  This method
  works around the problem by looking at the means of batches of the
  parameter.  If the batch size is large enough, the batch means should
  be approximately uncorrelated and the normal formula for computing the
  standard error should work.

  The batch standard error procedure is usually thought to be not as
  accurate as the time series methods used in \code{summary} and
  \code{effectiveSize}.  It is included here for completeness.
}
\value{
A vector giving the standard error for each column of \code{x}.
}
\references{
Roberts, GO (1996) Markov chain concepts related to sampling algorithms,
in Gilks, WR, Richardson, S and Spiegelhalter, DJ, \emph{Markov Chain
  Monte Carlo in Practice}, Chapman and Hall, 45-58.
}
\seealso{
   \code{\link{spectrum0.ar}}, \code{\link{effectiveSize}},
   \code{\link{summary.mcmc}} 
}
\author{Russell Almond}
\keyword{ts}
