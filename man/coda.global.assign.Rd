\name{coda.global.assign}
\alias{coda.global.assign}
\title{Cross-platform assignment to global environment}

\usage{
coda(x, value, alias = FALSE) 
}

\arguments{
\item{x}{A variable name (given as a quoted string)}
\item{value}{Value to be assigned to \code{x}}
\item{alias}{Logical value that determines whether a copy is made or an alias is assigned}
}

\description{
An attempt to overcome syntactic differences between R and S when assigning
to the global environment, but not a very good one.
}

\keyword{data}
