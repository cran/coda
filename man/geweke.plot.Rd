\name{geweke.plot}
\title{Geweke-Brooks plot}

\usage{
geweke.plot((mcmc.obj, frac1 = 0.1, frac2 = 0.5, bin.width = 10,
max.bins = 50, auto.layout = TRUE, ask = TRUE, ...))
} 

\arguments{
\item{frac1}{fraction to use from beginning of chain.}
\item{frac2}{fraction to use from end of chain.}
\item{bin.width}{Number of iterations per segment, not counting the
last segment which always has at least 50 iterations.}
\item{max.bins}{Maximum number of segments, excluding the last one.}
\item{auto.layout}{If \code{TRUE} then, set up own layout for
plots, otherwise use existing one.}
\item{ask}{Prompt user before displaying each page of plots}
\item{...}{Graphical parameters}
}

\description{
If \code{geweke} indicates that the first and last part of a sample
from a Markov chain are not drawn from the same distribution, it
may be useful to discard the first few iterations to see if the
rest of the chain has "converged". This plot shows what happens to
Geweke's Z-score when successively larger numbers of iterations
are discarded from the beginning of the chain.

The Markov chain is divided into segments according to the arguments
\code{bin.width} and \code{max.bins}. Then Geweke's Z-score is repeatedly
calculated. The first Z-score is calculated with all iterations in the
chain, the second after discarding the first segment, the third after
discarding the first two segments, and so on. The last Z-score is
calculated using only the samples in the last segment, which always
contains at least 50 observations.

}

\note{
The graphical implementation of Geweke's diagnostic was suggested
by Steve Brooks.
}

\section{theory}{
The obvious danger in discarding iterations is that the diagnostic test
loses power. In particular, using the default parameters, the last Z-score
is based on a comparison of one sample of 5 observations and another
sample of 25 observations, which cannot be regarded as a large sample.
}

\seealso{
\code{\link{geweke.diag}}
}
