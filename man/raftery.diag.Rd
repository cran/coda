\name{raftery.diag}
\alias{raftery.diag}
%\alias{print.raftery.diag}
\title{Raftery and Lewis's diagnostic}

\usage{
raftery.diag(data, q=0.025, r=0.005, s=0.95, converge.eps=0.001)
}

\arguments{
   \item{data}{an \code{mcmc} object}
   \item{q}{the quantile to be estimated.}
   \item{r}{the desired margin of error of the estimate.}
   \item{s}{the probability of obtaining an estimate in the interval (q-r,q+r).}
   \item{converge.eps}{Precision required for estimate of time to convergence.}
}

\description{
   \code{raftery.diag} is a run length control diagnostic based on a
   criterion of accuracy of estimation of the quantile \code{q}.  It is
   intended for use on a short pilot run of a Markov chain.  The number
   of iterations required to estimate the quantile \eqn{q} to within an
   accuracy of +/- \eqn{r} with probability \eqn{p} is calculated. Separate
   calculations are performed for each variable within each chain.

   If the number of iterations in \code{data} is too small,
   an error message is printed indicating the minimum length of
   pilot run.  The minimum length is the required sample size for a
   chain with no correlation between consecutive samples. Positive
   autocorrelation will increase the required sample size above this
   minimum value. An estimate \code{I} (the `dependence factor') of the
   extent to which autocorrelation inflates the required sample size
   is also provided. Values of \code{I} larger than 5 indicate strong
   autocorrelation which may be due to a poor choice of starting value,
   high posterior correlations or `stickiness' of the MCMC algorithm.

   The number of `burn in' iterations to be discarded at the beginning
   of the chain is also calculated.
}

\value{
   A list with class \code{raftery.diag}.  A print method is available
   for objects of this class. the contents of the list are
      \item{tspar}{The time series parameters of \code{data}}
      \item{params}{A vector containing the parameters \code{r}, \code{s}
      and \code{q}}
      \item{Niters}{The number of iterations in \code{data}}
      \item{resmatrix}{A 3-d array containing the results: \eqn{M} the
      length of "burn in", \eqn{N} the required sample size, \eqn{Nmin}
      the minimum sample size based on zero autocorrelation and 
      \eqn{I = (M+N)/Nmin} the "dependence factor"}
}

\section{Theory}{
   The estimated sample size for variable U is based on the process \eqn{Z_t
   = d(U_t <= u)} where \eqn{d} is the indicator function and u is the
   qth quantile of U. The process \eqn{Z_t} is derived from the Markov
   chain \code{data} by marginalization and truncation, but is not itself
   a Markov chain.  However, \eqn{Z_t} may behave as a Markov chain if
   it is sufficiently thinned out.  \code{raftery.diag} calculates the
   smallest value of thinning interval \eqn{k} which makes the thinned
   chain \eqn{Z^k_t} behave as a Markov chain. The required sample size is
   calculated from this thinned sequence.  Since some data is `thrown away'
   the sample size estimates are conservative.

   The criterion for the number of `burn in' iterations \eqn{m} to be
   discarded, is that the conditional distribution of \eqn{Z^k_m}
   given \eqn{Z_0} should be within \code{converge.eps} of the equilibrium
   distribution of the chain \eqn{Z^k_t}.
}

\note{
   \code{raftery.diag} is based on the FORTRAN program `gibbsit' 
   written by Steven Lewis, and available from the Statlib archive.
}

\references{
   Raftery, A.E. and Lewis, S.M. (1992).  One long run with diagnostics:
   Implementation strategies for Markov chain Monte Carlo.
   \emph{Statistical Science}, \bold{7}, 493-497.

   Raftery, A.E. and Lewis, S.M. (1995).  The number of iterations,
   convergence diagnostics and generic Metropolis algorithms.  \emph{In}
   Practical Markov Chain Monte Carlo (W.R. Gilks, D.J. Spiegelhalter
   and S. Richardson, eds.). London, U.K.: Chapman and Hall.
}

\keyword{htest}
