\name{read.and.check}
\alias{read.and.check}
\title{Read data interactively and check that it satisfies conditions}

\usage{
read.and.check(message = "", what = numeric(), lower, upper, answer.in,
default)
}

\arguments{
   \item{message}{message displayed before prompting for user input.}
   \item{what}{the type of \code{what} gives the type of data to be read.}
   \item{lower}{lower limit of input, for numeric input only.}
   \item{upper}{upper limit of input, for numeric input only.}
   \item{answer.in}{the input must correspond to one of the elements of the
   vector \code{answer.in}, if supplied.}
   \item{default}{value assumed if user enters a blank line.}
}

\description{
   Input is read interactively and checked against conditions specified
   by the arguments \code{what}, \code{lower}, \code{upper} and
   \code{answer.in}. If the input does not satisfy all the conditions,
   an appropriate error message is produced and the user is prompted
   to provide input. This process is repeated until a valid input value
   is entered.
}

\value{
   The value of the valid input.  When the \code{default} argument is
   specified, a blank line is accepted as valid input and in this case
   \code{read.and.check} returns the value of \code{default}.
}

\note{
   Since the function does not return a value until it receives valid
   input, it extensively checks the conditions for consistency before
   prompting the user for input.  Inconsistent conditions will cause
   an error.
}

\author{Martyn Plummer}

\keyword{utilities}
