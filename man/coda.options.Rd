\name{coda.options}
\alias{coda.options}
\alias{display.coda.options}
\alias{.Coda.Options}
\alias{.Coda.Options.Default}
\title{Options settings for the codamenu driver}
\usage{
coda.options(\dots)
display.coda.options(data = FALSE, stats = FALSE, plots = FALSE, diags = FALSE)
.Coda.Options
.Coda.Options.Default
}
\arguments{
  \item{data}{logical flag: show data options?}
  \item{stats}{logical flag: show summary statistic options?}
  \item{plots}{logical flag: show plotting options?}
  \item{diags}{logical flag: show plotting options?}
  \item{\dots}{list of options}
}
\description{
  \code{coda.options} is a utility function that queries and sets
  options for the \code{codamenu()} function.  These settings affect
  the behaviour of the functions in the coda library only when they
  are called via the \code{codamenu()} interface.
  
  The \code{coda.options()} function behaves just like the
  \code{options()} function in the base library, with the additional
  feature that \code{coda.options(default=TRUE)} will reset all options
  to the default values.
  
  Options can be pretty-printed using the \code{display.coda.options()}
  function, which groups the options into sections.
  
  Available options are
  \describe{
    \item{bandwidth}{Bandwidth function used when smoothing samples to
      produce density estimates.  Defaults to Silverman's ``Rule of thumb''.}
    \item{combine.corr}{Logical option that determines whether to
      combine multiple chains when calculating cross-correlations.}
    \item{combine.plots}{Logical option that determines whether to
      combine multiple chains when plotting.}
    \item{combine.plots}{Logical option that determines whether to
      combine multiple chains when calculating summary statistics.}
    \item{data.saved}{For internal use only.}
    \item{densplot}{Logical option that determines whether to plot
      a density plot when plot methods are called for mcmc objects.}
    \item{digits}{Number of significant digits to use when printing.}
    \item{frac1}{For Geweke diagnostic, fraction to use from
      start of chain.  Defaults to 0.1}
    \item{frac2}{For Geweke diagnostic, fraction to use from
      end of chain.  Default to 0.5.}
    \item{gr.bin}{For Geweke-Brooks plot, number of iterations to use
      per bin.}
    \item{gr.max}{For Geweke-Brooks plot, maximum number of bins to
      use.  This option overrides \code{gr.bin}.}
    \item{halfwidth}{For Heidelberger and Welch diagnostic, the target
      value for the ratio of half width to sample mean.}
    \item{lowess}{Logical option that controls whether to plot a 
      smooth line through a trace plot when plotting MCMC output.}
    \item{q}{For Raftery and Lewis diagnostic, the target quantile to
      be estimated}
    \item{r}{For Raftery and Lewis diagnostic, the required precision.}
    \item{s}{For Raftery and Lewis diagnostic, the probability of
      obtaining an estimate in the interval (q-r, q+r).}
    \item{quantiles}{Vector of quantiles to print when calculating
      summary statistics for MCMC output.}
    \item{trace}{Logical option that determines whether to plot a trace
      of the sampled output when plotting MCMC output.}
    \item{user.layout}{Logical option that determines whether current
      value of par("mfrow") should be used for plots (TRUE) or whether
      the optimal layout should be calculated (FALSE).}
  }
}
\seealso{
  \code{\link{options}}
}
\keyword{utilities}
