\name{geweke.diag}
\alias{geweke.diag}
%\alias{print.geweke.diag}
\title{Geweke's convergence diagnostic}

\usage{geweke.diag(x, frac1=0.1, frac2=0.5)}

\arguments{
  \item{x}{an mcmc object}
  \item{frac1}{fraction to use from beginning of chain}
  \item{frac2}{fraction to use from end of chain}
}

\value{
Z-scores for a test of equality of means
between the first and last parts of the chain. A separate
statistic is calculated for each variable in each chain.
}

\description{
Geweke (1992) proposed a convergence diagnostic for Markov chains
based on a test for equality of the means of the first and last part
of a Markov chain (by default the first 10\% and the last 50\%).  If the
samples are drawn from the stationary distribution of the chain, the two
means are equal and Geweke's statistic has an asymptotically standard
normal distribution.

The test statistic is a standard Z-score: the difference between the
two sample means divided by its estimated standard error.  The standard
error is estimated from the spectral density at zero and so takes into
account any autocorrelation.

The Z-score is calculated under the assumption that the two parts of
the chain are asymptotically independent, which requires that the sum
of \code{frac1} and \code{frac2} be strictly less than 1.
}

\seealso{
   \code{\link{geweke.plot}}.
}

\references{
Geweke, J. Evaluating the accuracy of sampling-based approaches
to calculating posterior moments. In \emph{Bayesian Statistics 4}
(ed JM Bernado, JO Berger, AP Dawid and AFM Smith). Clarendon Press,
Oxford, UK.}

\keyword{htest}
