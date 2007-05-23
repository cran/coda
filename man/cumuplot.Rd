\name{cumuplot}
\alias{cumuplot}
\title{Cumulative quantile plot}

\usage{
  cumuplot(x, probs=c(0.025,0.5,0.975), ylab="",
           lty=c(2,1), lwd=c(1,2), type="l", ask=dev.interactive(),
           auto.layout=TRUE, col=1, \dots)
}

\arguments{
  \item{x}{an \code{mcmc} object}
  \item{probs}{vector of desired quantiles}
  \item{ylab, lty, lwd, type, col}{graphical parameters}
  \item{auto.layout}{If \code{TRUE}, then set up own layout for
   plots, otherwise use existing one.}
  \item{ask}{If \code{TRUE} then prompt user before displaying
   each page of plots.}
  \item{\dots}{further graphical parameters}
}

\description{
  Plots the evolution of the sample quantiles as a function of the
  number of iterations.
}
  
\author{
  Arni Magnusson <arnima@u.washington.edu>
}
\keyword{hplot}
