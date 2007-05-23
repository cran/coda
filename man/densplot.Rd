\name{densplot}
\alias{densplot}
\title{Probability density function estimate from MCMC output}

\usage{densplot(x, show.obs=TRUE, bwf, main, ylim, \dots)}

\arguments{
  \item{x}{An \code{mcmc} or \code{mcmc.list} object}
  \item{show.obs}{Show observations along the x-axis}
  \item{bwf}{Function for calculating the bandwidth.  If omitted,
    the bandwidth is calculate by 1.06 times the minimum of the standard
    deviation and the interquartile range divided by 1.34 times the sample
    size to the negative one fifth power}
  \item{main}{Title. See \code{par()}}
  \item{ylim}{Limits on y axis.  See \code{par()}}
  \item{\dots}{Further graphical parameters}
}

\description{
  Displays a plot of the density estimate for each variable in \code{x},
  calculated by the \code{density} function.
}

\note{
  You can call this function directly, but it is more usually
  called by the \code{plot.mcmc} function.

  If a variable is bounded below at 0, or bounded in the interval [0,1],
  then the data are reflected at the boundary before being passed to the
  density() function. This allows correct estimation of a non-zero
  density at the boundary.
}

\seealso{
  \code{\link{density}}, \code{\link{plot.mcmc}}.
}
\keyword{hplot}
