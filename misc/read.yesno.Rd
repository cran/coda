\name{read.yesno}
\alias{read.yesno}
\title{Read logical values from the command line}

\usage{
   read.yesno(string, default=TRUE)
}

\arguments{
   \item{string}{A string to prompt the user with}
   \item{default}{Logical value giving the default answer}
}

\value{
   A single logical value.
}

\description{
   This is a utility function for interactively reading a logical
   value from the command line.

   The prompt string is printed, along a hint about the default answer.
   Answers "yes", "YES", "no", "NO" or any partial match are accepted.
   If the user enters a blank line, the default value is returned.
}

\author{Martyn Plummer}

\keyword{utilities}
