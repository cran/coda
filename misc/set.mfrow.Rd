\name{set.mfrow}
\alias{set.mfrow}
\title{Set layout for graphical plots}

\usage{
   set.mfrow (Nchains = 1, Nparms = 1, nplots = 1, sepplot = FALSE) 
}

\arguments{
   \item{Nchains}{Number of chains to plot}
   \item{Nparms}{Number of variables in each chain}
   \item{nplots}{Number of plots to produce}
   \item{sepplot}{Logical value controlling whether separate plots are created for each chain}
}

\description{
   This utility function is used by plotting functions in the coda package
   to determine the default layout, unless overriden by the \code{user.layout}
   option.
}

\note{
   This is not meant to be called directly by the user.
}

\keyword{dplot}
