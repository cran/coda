\name{heidel.diag}
\alias{heidel.diag}
%\alias{print.heidel.diag}
\title{Heidelberger and Welch's convergence diagnostic}

\usage{heidel.diag(x, eps=0.1, pvalue=0.05)}

\arguments{
\item{x}{An \code{mcmc} object}
\item{eps}{Target value for ratio of halfwidth to sample mean}
\item{pvalue}{significance level to use}
}

\description{
\code{heidel.diag} is a run length control diagnostic based on a criterion
of relative accuracy for the estimate of the mean.  The default setting
corresponds to a relative accuracy of two significant digits.

\code{heidel.diag} also implements a convergence diagnostic, and removes
up to half the chain in order to ensure that the means are estimated
from a chain that has converged.
}

\details{
The convergence test uses the Cramer-von-Mises statistic to test the null
hypothesis that the sampled values come from a stationary distribution.
The test is successively applied, firstly  to the whole chain, then after
discarding the first 10\%, 20\%, \ldots of the chain until either the null
hypothesis is accepted, or 50\% of the chain has been discarded. The latter
outcome constitutes `failure' of the stationarity test and indicates
that a longer MCMC run is needed. If the stationarity test is passed,
the number of iterations to keep and the number to discard are reported.

The half-width test calculates a 95\% confidence interval for the
mean, using the portion of the chain which passed the stationarity test.
Half the width of this interval is compared with the estimate of the mean.
If the ratio between the half-width and the mean is lower than \code{eps},
the halfwidth test is passed. Otherwise the length of the sample is
deemed not long enough to estimate the mean with sufficient accuracy.
}

\section{Theory}{
The \code{heidel.diag} diagnostic is based on the work of Heidelberger
and Welch (1983), who combined their earlier work on simulation run
length control (Heidelberger and Welch, 1981) with the work of Schruben
(1982) on detecting initial transients using Brownian bridge theory.
}

\note{
If the half-width test fails then the run should be extended.  In order
to avoid problems caused by sequential testing, the test should not
be repeated too frequently.  Heidelberger and Welch (1981) suggest
increasing the run length by a factor I > 1.5, each time, so that 
estimate has the same, reasonably large, proportion of new data.
}

\references{
Heidelberger P and Welch PD. A spectral method for confidence interval
generation and run length control in simulations. Comm. ACM. \bold{24},
233-245 (1981)

Heidelberger P and Welch PD. Simulation run length control in the 
presence of an initial transient. \emph{Opns Res.}, \bold{31},
1109-44 (1983)

Schruben LW. Detecting initialization bias in simulation experiments.
\emph{Opns. Res.}, \bold{30}, 569-590 (1982).
}

\keyword{htest}
