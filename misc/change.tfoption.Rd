\name{change.tfoption}
\alias{change.tfoption}
\title{Interactively modify logical codamenu options.}

\usage{
change.tfoption(string, option)
}

\arguments{
\item{string}{Prompt string to print}
\item{option}{String giving the name of an option to change}
}

\description{
Interactively changes logical codamenu options using the 
\code{read.yesno} function.
}

\note{
This is a utility function for the codamenu system, and is not
meant to be called directly by the user.
}

\seealso{
   \code{\link{coda.options}}, 
   \code{\link{read.yesno}}.
}

\keyword{utilities}
