\name{spectrum0.ar}
\alias{spectrum0.ar}
\title{Estimate spectral density at zero}
\description{
The spectral density at frequency zero is estimated by fitting an
autoregressive model.  \code{spectrum0(x)/length(x)} estimates the
variance of \code{mean(x)}.
}
\usage{
spectrum0.ar(x) 
}
\arguments{
\item{x}{A time series.}
}
\details{
The \code{ar()} function to fit an autoregressive model to the time
series x. For multivariate time series, separate models are fitted for
each column. The value of the spectral density at zero is then given
by a well-known formula.
}
\value{
A list with the following values
\item{spec}{The predicted value of the spectral density at frequency zero.}
\item{order}{The order of the fitted model}
}
\note{
The definition of the spectral density used here differs from that used by
\code{spec.pgram}. We consider the frequency range to be between 0 and 0.5,
not between 0 and \code{frequency(x)/2}.
}
\seealso{
   \code{\link{spectrum}}, \code{\link{spectrum0}}, \code{\link{glm}}.
}
\keyword{ts}
