\name{summary.mcmc}
\alias{summary.mcmc}
\alias{summary.mcmc.list}
%\alias{print.summary.mcmc}
%\alias{print.summary.mcmc.list}
\title{Summary statistics for  Markov Chain Monte Carlo chains}

\usage{
\method{summary}{mcmc}(object, quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), ...)
}

\arguments{
  \item{object}{an object of class \code{mcmc} or \code{mcmc.list}}
  \item{quantiles}{a vector of quantiles to evaluate for each variable}
  \item{...}{a list of further arguments}
}

\description{
   \code{summary.mcmc} produces two sets of summary statistics for
   each variable:
   
   Mean, standard deviation, naive standard error of the mean
   (ignoring autocorrelation of the chain) and time-series standard
   error based on an estimate of the spectral density at 0.

   Quantiles of the sample distribution using the \code{quantiles}
   argument.
}

\author{Martyn Plummer}

\seealso{
   \code{\link{mcmc}},
   \code{\link{mcmc.list}}.
}
\keyword{univar}
