\name{crosscorr.plot}
\alias{crosscorr.plot}
\title{Plot image of correlation matrix}

\usage{crosscorr.plot (x, col = topo.colors(10), ...)}

\arguments{
\item{x}{an \code{mcmc} or \code{mcmc.list} object}
\item{col}{color palette to use}
\item{...}{graphical parameters}
}

\description{
\code{crosscorr.plot} provides an image of the correlation matrix for
\code{x}. If \code{x} is an \code{mcmc.list} object, then all chains
are combined.

The range [-1,1] is divided into a number of equal-length categories
given by the length of \code{col} and assigned the corresponding color.
By default, topographic colours are used as this makes it easier to
distinguish positive and negative correlations.
}

\seealso{
\code{\link{crosscorr}}, \code{\link{image}}, \code{\link{topo.colors}}.
}
\keyword{hplot}
