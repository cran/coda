\name{heidel.diag}
\title{Heidelberger and Welch's convergence diagnostic}

\usage{heidel.diag(mcmc.obj, eps=0.1)}

\arguments{
\item{mcmc.obj} {An \code{mcmc} object}
\item{eps}{Target value for ratio of halfwidth to sample mean}
}

\description{
\code{heidel.diag} implements two diagnostic tests. The first is a test
for stationarity of the Markov chain. The second is a test to see
if the mean values of the parameters are estimated with sufficient
accuracy. Both tests are applied separately to all the variables in
\code{mcmc.obj}.

The first test uses the Cramer-von-Mises statistic to test the null
hypothesis that the sampled values come from a stationary distribution.
The test is successively applied, firstly  to the whole chain, then after
discarding the first 10\%, 20\%, ... of the chain until either the null
hypothesis is accepted, or 50\% of the chain has been discarded. The latter
outcome constitutes "failure" of the stationarity test and indicates
that a longer MCMC run is needed. If the stationarity test is passed,
the number of iterations to keep and the number to discard are reported.

The second test calculates a 95\% confidence interval for the
mean, using the portion of the chain which passed the stationarity test.
Half the width of this interval is compared with the estimate of the mean.
If the ratio between the half-width and the mean is lower than \code{eps},
the halfwidth test is passed. Otherwise the length of the sample is
deemed not long enough to estimate the mean with sufficient accuracy.
}

\section{theory}{
The \code{heidel} test is based on the work of Heidelberger and Welch (1983)
who devised a method for detecting an initial transient in simulated
sequences of discrete events using Brownian bridge theory.
}

\references{
Heidelberger P and Welch P. Simulation run length control in the 
presence of an initial transient. \emph{Operations Research}, \bold{31},
1109-44 (1983)
}
