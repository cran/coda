\name{densplot}
\alias{densplot}
\title{Probability density function estimate from MCMC output}

\usage{densplot(x, show.obs=TRUE, bwf, ...)}

\arguments{
\item{x}{An \code{mcmc} or \code{mcmc.list} object}
\item{show.obs}{Show observations along the x-axis}
\item{bwf}{Function for calculating the bandwidth.  If omitted,
the bandwidth is calculate by 1.06 times the minimum of the standard
deviation and the interquartile range divided by 1.34 times the sample
size to the negative one fifth power}
\item{...}{Graphical parameters}
}

\description{
Displays a plot of the density estimate for each variable in \code{x},
calculated by the \code{density} function.
}

\note{
You can call this function directly, but it is more usually
called by the \code{plot.mcmc} function.
}

\seealso{
   \code{\link{density}}, \code{\link{plot.mcmc}}.
}
\keyword{hplot}
