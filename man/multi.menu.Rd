\name{multi.menu}
\alias{multi.menu}
\title{Choose multiple options from a menu}

\usage{
multi.menu(choices, title, header, allow.zero = TRUE)
}

\arguments{
  \item{choices}{Character vector of labels for choices}
  \item{title}{Title printed before menu}
  \item{header}{Character vector of length 2 giving column titles}
  \item{allow.zero}{Permit 0 as an acceptable response}
}

\description{
\code{multi.menu} presents the user with a menu of choices
labelled from 1 to the number of choices.  The user may choose
one or more options by entering a comma separated list. A range
of values may also be specified using the ":" operator. Mixed
expressions such as "1,3:5, 6" are permitted.

If \code{allow.zero} is set to TRUE, one can select `0' to exit
without choosing an item.
}

\value{
Numeric vector giving the numbers of the options selected, or
0 if no selection is made.
}

\author{Martyn Plummer}

\seealso{
   \code{\link{menu}}.
}
\keyword{utilities}
