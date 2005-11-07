\name{HPDinterval}
\alias{HPDinterval}
\alias{HPDinterval.mcmc}
\alias{HPDinterval.mcmc.list}
\title{Highest Posterior Density intervals}
\description{
  Create Highest Posterior Density (HPD) intervals for the parameters in
  an MCMC sample.
}
\usage{
HPDinterval(obj, prob = 0.95, ...)
\method{HPDinterval}{mcmc}(obj, prob = 0.95, ...)
\method{HPDinterval}{mcmc.list}(obj, prob = 0.95, ...)
}
\arguments{
  \item{obj}{The object containing the MCMC sample - usually of class
    \code{"mcmc"} or \code{"mcmc.list"}}.
  \item{prob}{A numeric scalar in the interval (0,1) giving the target
    probability content of the intervals.  The nominal probability
    content of the intervals is the multiple of \code{1/nrow(obj)}
    nearest to \code{prob}.}
  \item{\dots}{Optional additional arguments for methods.  None are used
    at present.}
}
\details{
  For each parameter the interval is constructed from the empirical cdf
  of the sample as the shortest interval for which the difference in
  the ecdf values of the endpoints is the nominal probability.  Assuming
  that the distribution is not severely multimodal, this is the HPD interval.
}
\value{
  For an \code{"mcmc"} object, a matrix with columns \code{"lower"} and
  \code{"upper"} and rows corresponding to the parameters.  The
  attribute \code{"Probability"} is the nominal probability content of
  the intervals.  A list of such matrices is returned for an
  \code{"mcmc.list"} object.
}
\author{Douglas Bates}
\examples{
data(line)
HPDinterval(line)
}
\keyword{univar}
\keyword{htest}
