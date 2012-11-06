\name{densplot}
\alias{densplot}
\title{Probability density function estimate from MCMC output}

\usage{densplot(x, show.obs = TRUE, bwf, 
                ylim, xlab, ylab = "", type="l", main, \ldots)}

\arguments{
  \item{x}{An \code{mcmc} or \code{mcmc.list} object}
  \item{show.obs}{Show observations along the x-axis}
  \item{bwf}{Function for calculating the bandwidth.  If omitted,
    the bandwidth is calculate by 1.06 times the minimum of the standard
    deviation and the interquartile range divided by 1.34 times the sample
    size to the negative one fifth power}
  \item{ylim}{Limits on y axis.  See \code{plot.window}}
  \item{xlab}{X-axis label. By default this will show the sample size
    and the bandwidth used for smoothing. See \code{plot}}
  \item{ylab}{Y-axis label. By default, this is blank. See \code{plot}}
  \item{type}{Plot type. See \code{plot}}
  \item{main}{An overall title for the plot. See \code{title}}
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
