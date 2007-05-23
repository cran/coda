\name{window.mcmc}
\alias{window.mcmc}
\alias{window.mcmc.list}
\title{Time windows for mcmc objects}

\usage{
\method{window}{mcmc}(x, start, end, thin, \dots)
}

\arguments{
   \item{x}{an mcmc object}
   \item{start}{the first iteration of interest}
   \item{end}{the last iteration of interest}
   \item{thin}{the required interval between successive samples}
   \item{\dots}{futher arguments for future methods}
 }

\description{
   \code{window.mcmc} is a method for \code{mcmc} objects which is
   normally called by the generic function \code{window}

   In addition to the generic parameters, \code{start} and \code{end}
   the additional parameter \code{thin} may be used to thin out the
   Markov chain. Setting thin=k selects every kth iteration starting
   with the first. Note that the value of \code{thin} is \emph{absolute}
   not relative. The value supplied given to the parameter \code{thin}
   must be a multiple of \code{thin(x)}.

   Values of \code{start}, \code{end} and \code{thin} which are inconsistent
   with \code{x} are ignored, but a warning message is issued.
}

\seealso{
   \code{\link{window}}, \code{\link{thin}}.
}
\keyword{ts}
