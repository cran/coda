\name{gelman.plot}
\alias{gelman.plot}
%\alias{gelman.preplot}
\title{Gelman-Rubin-Brooks plot}

\usage{
gelman.plot(x, bin.width = 10, max.bins = 50,
confidence = 0.95, transform = FALSE, auto.layout = TRUE,
ask, col, lty, xlab, ylab, type, \dots)
} 

\arguments{
  \item{x}{an mcmc object}
  \item{bin.width}{Number of observations per segment, excluding the
    first segment which always has at least 50 iterations.}
  \item{max.bins}{Maximum number of bins, excluding the last one.}
  \item{confidence}{Coverage probability of confidence interval.}
  \item{transform}{Automatic variable transformation (see \code{gelman.diag})}
  \item{auto.layout}{If \code{TRUE} then, set up own layout for
    plots, otherwise use existing one.}
  \item{ask}{Prompt user before displaying each page of plots.  Default is 
    \code{dev.interactive()} in R and \code{interactive()} in S-PLUS.}
  \item{col}{graphical parameter (see \code{par})}
  \item{lty}{graphical parameter (see \code{par})}
  \item{xlab}{graphical parameter (see \code{par})}
  \item{ylab}{graphical parameter (see \code{par})}
  \item{type}{graphical parameter (see \code{par})}
  \item{\dots}{further graphical parameters.}
}

\description{
This plot shows the evolution of Gelman and Rubin's shrink factor as
the number of iterations increases. 
}

\details{
The Markov chain is divided into bins according to the arguments
\code{bin.width} and \code{max.bins}. Then the Gelman-Rubin shrink factor
is repeatedly calculated. The first shrink factor is calculated with
observations 1:50, the second with observations \eqn{1:(50+n)} where n is
the bin width, the third contains samples \eqn{1:(50+2n)} and so on.
}

\references{
Brooks, S P. and Gelman, A. (1998) General Methods for Monitoring
Convergence of Iterative Simulations. Journal of Computational and
Graphical Statistics. 7. p434-455.
}

\section{Theory}{
A potential problem with \code{gelman.diag} is that it may mis-diagnose
convergence if the shrink factor happens to be close to 1 by chance.
By calculating the shrink factor at several points in time,
\code{gelman.plot} shows if the shrink factor has really converged, or
whether it is still fluctuating.
}

\seealso{
   \code{\link{gelman.diag}}.
}
\keyword{hplot}
