\name{upgrade.mcmc}
\title{Upgrade mcmc objects in obsolete format}

\usage{
   upgrade.mcmc(data)
}

\arguments{
   \item{data}{an obsolete \code{mcmc} object}.
}

\description{
   In previous releases of CODA, an mcmc  object could
   be a single or multiple chains.  A new class \code{mcmc.list}
   has now been introduced to deal with multiple chains and
   \code{mcmc} objects can only have data from a single chain.

   Objects stored in the old format are now obsolete
   and must be upgraded.
}

\author{Martyn Plummer}

\seealso{
   \code{\link{mcmc}}
}

\keyword{ts}
