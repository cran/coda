\name{gelman.diag}
\alias{gelman.diag}
%\alias{gelman.mv.diag}
%\alias{gelman.transform}
%\alias{print.gelman.diag}
\title{Gelman and Rubin's convergence diagnostic}

\usage{gelman.diag(x, confidence = 0.95, transform=FALSE, autoburnin=TRUE)}

\arguments{
\item{x}{An \code{mcmc.list} object with more than one chain,
and with starting values that are overdispersed with respect to the
posterior distribution.}
\item{confidence}{the coverage probability of the confidence interval for the
potential scale reduction factor}
\item{transform}{a logical flag indicating whether variables in
\code{x} should be transformed to improve the normality of the
distribution. If set to TRUE, a log transform or logit transform, as
appropriate, will be applied.}
\item{autoburnin}{a logical flag indicating whether only the second half
  of the series should be used in the computation.  If set to TRUE
  (default) and \code{start(x)} is less than \code{end(x)/2} then start
  of series will be adjusted so that only second half of series is used.}
}

\description{
The `potential scale reduction factor' is calculated for each variable in
\code{x}, together with upper and lower confidence limits. Approximate
convergence is diagnosed when the upper limit is close to 1. For
multivariate chains, a multivariate value is calculated that bounds
above the potential scale reduction factor for any linear combination
of the (possibly transformed) variables.

The confidence limits are based on the assumption that the stationary
distribution of the variable under examination is normal. Hence the
`transform' parameter may be used to improve the normal approximation.
}

\section{Theory}{
Gelman and Rubin (1992) propose a general approach to monitoring
convergence of MCMC output in which two or more parallel chains are run
with starting values that are overdispersed relative to the posterior
distribution. Convergence is diagnosed when the chains have `forgotten'
their initial values, and the output from all chains is indistinguishable.
The \code{gelman.diag} diagnostic is applied to a single variable from
the chain. It is based a comparison of within-chain and between-chain
variances, and is similar to a classical analysis of variance.

There are two ways to estimate the variance of the stationary distribution:
the mean of the empirical variance within each chain, \eqn{W}, and
the empirical variance from all chains combined, which can be expressed as
\deqn{ \widehat{\sigma}^2 = 
   \frac{(n-1) B }{n} + \frac{W}{n} }{ sigma.hat^2 =  (n-1)B/n + W/n }
where \eqn{B} is the empirical between-chain variance.

If the chains have converged, then both estimates are unbiased. Otherwise
the first method will \emph{underestimate} the variance, since the
individual chains have not had time to range all over the stationary
distribution, and the second method will \emph{overestimate} the variance,
since the starting points were chosen to be overdispersed. 

The convergence diagnostic is based on the assumption that the
target distribution is normal. A Bayesian credible interval can
be constructed using a t-distribution with mean
\deqn{\widehat{\mu}=\mbox{Sample mean of all chains
combined}}{mu.hat = Sample mean of all chains combined}
and variance
\deqn{\widehat{V}=\widehat{\sigma}^2 + \frac{B}{mn}}{V.hat=sigma.hat2 + B/(mn)}
where \eqn{m} is the number of chains, and degrees of freedom
estimated by the method of moments
\deqn{d = \frac{2*\widehat{V}}{\mbox{Var}(\widehat{V})}}{d = 2*V.hat/Var(V.hat)}
Use of the t-distribution accounts for the fact that the mean
and variance of the posterior distribution are estimated.

The convergence diagnostic itself is
\deqn{R=\sqrt{\frac{(d+3) \widehat{V}}{(d+1)W}}}{R=sqrt((d+3) V.hat /((d+1)W)}
Values substantially above 1 indicate lack of convergence.  If the
chains have not converged, Bayesian credible intervals based on the
t-distribution are too wide, and have the potential to shrink by this
factor if the MCMC run is continued.
}

\note{
The multivariate a version of Gelman and Rubin's diagnostic was
proposed by Brooks and Gelman (1997).
}

\references{
Gelman, A and Rubin, DB (1992) Inference from iterative simulation
using multiple sequences, \emph{Statistical Science}, \bold{7}, 457-511.

Brooks, SP. and Gelman, A. (1997) General methods for monitoring
convergence of iterative simulations. \emph{Journal of Computational and
Graphical Statistics}, \bold{7}, 434-455.
}

\seealso{
   \code{\link{gelman.plot}}.
}
\keyword{htest}
