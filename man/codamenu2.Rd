\name{codamenu2}
\alias{codamenu.main}
\alias{codamenu.anal}
\alias{codamenu.diags}
\alias{codamenu.diags.autocorr}
\alias{codamenu.diags.crosscorr}
\alias{codamenu.diags.heidel}
\alias{codamenu.diags.raftery}
\alias{codamenu.diags.gelman}
\alias{codamenu.diags.geweke}
\alias{codamenu.options}
\alias{codamenu.options.data}
\alias{codamenu.options.diag}
\alias{codamenu.options.gelman}
\alias{codamenu.options.geweke.bin}
\alias{codamenu.options.geweke.win}
\alias{codamenu.options.heidel}
\alias{codamenu.options.plot}
\alias{codamenu.options.plot.kernel}
\alias{codamenu.options.plot.ps}
\alias{codamenu.options.raftery}
\alias{codamenu.options.stats}
\alias{tidy.up}
\alias{codamenu.ps}
\alias{codamenu.output.header}

\title{Submenus for the coda package}

\description{
These functions are not meant to be called directly by the user, but
should be accessed via the \code{codamenu} function.

All submenus are called from the \code{codamenu.main} function.
Each submenu returns a string giving the name of the next submenu to be
called, or "quit" if the user decides to quit.  This system avoids the
recursive calling of submenus which would accumulate `dead' environments.
}

\author{Martyn Plummer}
\keyword{utilities}
