\name{spectrum0}
\alias{spectrum0}
\title{Estimate spectral density at zero}
\description{
The spectral density at frequency zero is estimated by fitting a glm to
the low-frequency end of the periodogram.  \code{spectrum0(x)/length(x)}
estimates the variance of \code{mean(x)}.

}
\usage{
spectrum0(x, max.freq = 0.5, order = 1, max.length = 200) 
}
\arguments{
\item{x}{A time series.}
\item{max.freq}{The glm is fitted on the frequency range (0, max.freq]}
\item{order}{Order of the polynomial to fit to the periodogram.}

\item{max.length}{The data \code{x} is aggregated if necessary by
taking batch means so that the length of the series is less than
\code{max.length}.  If this is set to \code{NULL} no aggregation occurs.}
}
\details{
The raw periodogram is calculated for the series \code{x} and a generalized
linear model with family \code{Gamma} and log link is fitted to
the periodogram.

The linear predictor is a polynomial in terms of the frequency.  The
degree of the polynomial is determined by the parameter \code{order}.
}
\value{
A list with the following values
\item{spec}{The predicted value of the spectral density at frequency zero.}
}
\references{
Heidelberger, P and Welch, P.D. A spectral method for confidence interval
generation and run length control in simulations. Communications of the
ACM, Vol 24, pp233-245, 1981.
}
\section{Theory}{
Heidelberger and Welch (1991) observed that the usual non-parametric
estimator of the spectral density, obtained by smoothing the periodogram,
is not appropriate for frequency zero.  They proposed an alternative
parametric method which consisted of fitting a linear model to the
log periodogram of the batched time series. Some technical problems 
with model fitting in their original proposal can be overcome by using
a generalized linear model.

Batching of the data, originally proposed in order to save space, has the
side effect of flattening the spectral density and making a polynomial
fit more reasonable.  Fitting a polynomial of degree zero is equivalent
to using the `batched means' method.
}
\note{
The definition of the spectral density used here differs from that used by
\code{spec.pgram}. We consider the frequency range to be between 0 and 0.5,
not between 0 and \code{frequency(x)/2}.

The model fitting may fail on chains with very high autocorrelation.
}
\seealso{
   \code{\link{spectrum}}, \code{\link{spectrum0.ar}}, \code{\link{glm}}.
}
\keyword{ts}
