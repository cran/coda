\name{plot.mcmc}
\alias{plot.mcmc}
\title{Summary plots of mcmc objects}

\usage{
summary(data, trace = TRUE, density = TRUE, smooth = TRUE, bwf,
	auto.layout = TRUE, ask = TRUE, ...)
}

\arguments{
\item{data}{an objects of class \code{mcmc} or \code{mcmc.list}}
\item{trace}{Plot trace of each variable}
\item{density}{Plot density estimate of each variable}
\item{smooth}{Draw a smooth line through trace plots}
\item{bwf}{Bandwidth function for density plots}
\item{auto.layout}{Automatically generate output format}
\item{ask}{Prompt user before each page of plots}
}

\description{
   \code{plot.mcmc} summarizes an mcmc or mcmc.list object
   with a trace of the sampled output and a density estimate
   for each variable in the chain.
}

\author{Martyn Plummer}

\seealso{
   \code{\link{densplot}}, \code{\link{traceplot}}.
}
\keyword{hplot}
