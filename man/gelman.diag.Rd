\name{gelman.diag}
\title{Gelman and Rubin's diagnostic}

\usage{gelman.diag(data, confidence = 0.95, transform=FALSE)}

\arguments{
\item{data}{An \code{mcmc.list} object with more than one chain,
and with starting values that are overdispersed with respect to the
posterior distribution.}
\item{confidence}{the coverage probability of confidence interval for the
shrink factor}
\item{transform}{a logical vector indicating whether variables in
\code{data} should be transformed to improve the normality of the
distribution. If set to TRUE, a log transform or logit transform, as
appropriate, will be applied.  If \code{transform} is of length 1
it is replicated to be equal to the number of variables in \code{data}.}
}

\description{
The "shrink factor" is calculated for each variable in \code{data},
together with upper and lower confidence limits. Approximate
convergence is diagnosed when the upper limit is close to 1.

The confidence limits are based on the assumption that the stationary
distribution of the variable under examination is normal. Hence
the `transform' parameter may be used to the skewness of the
distribution.
}

\section{theory}{
Gelman and Rubin (1992) propose a general approach to monitoring
convergence of MCMC output in which two or more parallel chains are run,
with starting values that are overdispersed relative to the posterior
distribution. Convergence is diagnosed when the chains have "forgotten"
their initial values, and the output from all chains is indistinguishable.
The \code{gelman.diag} diagnostic is applied to a single variable from
the chain. It is based a comparison of within-chain and between-chain
variances, and is similar to a classical analysis of variance.

There are two ways to estimate the variance of the stationary distribution.
\item{the mean of the empirical variance within each chain, \eqn{W}, and}
\item{the empirical variance from all chains combined, which can be
expressed as
\deqn{ sigma.hat^2 =  (\frac{(n-1) B }{n}) + \frac{W}{n} }
where \eqn{B} is the empirical between-chain variance.}
If the chains have converged, then both estimates are unbiased. Otherwise
the first method will \emph{underestimate} the variance, since the
individual chains have not had time to range all over the stationary
distribution, and the second method will \emph{overestimate} the variance,
since the starting points were chosen to be overdispersed. 

The convergence diagnostic is based on the assumption that the
target distribution is normal. A Bayesian credible interval can
be constructed using a t-distribution with mean
\deqn{mu.hat = Sample mean of all chains combined}
and variance
\deqn{V.hat = sigma.hat^2 + \frac{B}{mn}}
where \eqn{m} is the number of chains, and degrees of freedom
estimated by the method of moments
\deqn{d = \frac{2*V.hat}{\Var(V.hat)}}
Use of the t-distribution accounts for the fact that the mean
and variance of the posterior distribution are estimated.

The convergence diagnostic itself is
\deqn{ R = \sqrt{\frac{(d+3) V.hat}{(d+1)W}}}
Values substantially above 1 indicate lack
of convergence.  If the chains have not converged, Bayesian credible
intervals based on the t-distribution are too wide, and have the potential
to shrink by this factor if the MCMC run is continued.
}

\references{
\item{Gelman, A and Rubin, DB. Inference from iterative simulation
using multiple sequences, \emph{Statistical Science}, &, 457-511 (1992)}
\item{Brooks, S and Gelman, A. Some new ideas on inference from
iterative simulation using multiple sequences. \emph {Technical Report},
DPMMS, Cambridge.}

\seealso{
   \code{\link{gelman.plot}}
}
