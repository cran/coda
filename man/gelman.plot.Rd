\name{gelman.plot}
\title{Gelman-Rubin-Brooks plot}

\usage{
gelman.plot(mcmc.list.obj, bin.width = 10, max.bins = 50,
confidence = 0.95, transform = FALSE, auto.layout = TRUE, ask = TRUE,
...)
} 

\arguments{
\item{bin.width}{Number of observations per segment, excluding the
first segment which always has at least 50 iterations.}
\item{max.bins}{Maximum number of bins, excluding the last one.}
\item{confidence}{Coverage probability of confidence interval.}
\item{transform}{Automatic variable transformation (see \code{gelman.diag})
\item{auto.layout}{If \code{TRUE} then, set up own layout for
plots, otherwise use existing one.}
\item{ask}{Prompt user before displaying each page of plots.}
\item{...}{Graphical parameters.}
}

\description{
This plot shows what happens to Gelman and Rubin's shrink factor when
successively larger numbers of iterations are discarded from the end of
the chain (NB This is diferent from \code{geweke.plot}).

The Markov chain is divided into bins according to the arguments
\code{bin.width} and \code{max.bins}. Then the Gelman-Rubin shrink factor
is repeatedly calculated. The first shrink factor is calculated with
observations 1:50, the second with observations \eqn{1:(50+n)} where n is
the bin width, the third contains samples \eqn{1:(50+2n)} and so on.
}

\note{
The graphical implementation of Gelman and Rubin's diagnostic was suggested
by Steve Brooks.
}

\seealso{
\code{\link{gelman.diag}},
\code{\link{geweke.plot}}.
}
