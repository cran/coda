\name{geweke.plot}
\alias{geweke.plot}
\title{Geweke-Brooks plot}

\usage{
geweke.plot(x, frac1 = 0.1, frac2 = 0.5, nbins = 20,
            pvalue = 0.05, auto.layout = TRUE, ask, \dots)
} 

\arguments{
  \item{x}{an mcmc object}
  \item{frac1}{fraction to use from beginning of chain.}
  \item{frac2}{fraction to use from end of chain.}
  \item{nbins}{Number of segments.}
  \item{pvalue}{p-value used to plot confidence limits for the null hypothesis.}
  \item{auto.layout}{If \code{TRUE} then, set up own layout for
    plots, otherwise use existing one.}
  \item{ask}{If \code{TRUE} then prompt user before displaying
    each page of plots.  Default is \code{dev.interactive()} in
    R and \code{interactive()} in S-PLUS.}
  \item{\dots}{Graphical parameters.}
}

\description{
If \code{geweke.diag} indicates that the first and last part of a sample
from a Markov chain are not drawn from the same distribution, it may
be useful to discard the first few iterations to see if the rest of the
chain has "converged". This plot shows what happens to Geweke's Z-score
when successively larger numbers of iterations are discarded from the
beginning of the chain. To preserve the asymptotic conditions required
for Geweke's diagnostic, the plot never discards more than half the chain.

The first half of the Markov chain is divided into \code{nbins - 1}
segments, then Geweke's Z-score is repeatedly calculated. The first
Z-score is calculated with all iterations in the chain, the second
after discarding the first segment, the third after discarding the first
two segments, and so on. The last Z-score is calculated using only the
samples in the second half of the chain.
}

\note{
The graphical implementation of Geweke's diagnostic was suggested
by Steve Brooks.
}

\seealso{
   \code{\link{geweke.diag}}.
}
\keyword{hplot}
