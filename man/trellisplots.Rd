\name{trellisplots}
\title{Trellis plots for mcmc objects}
\alias{densityplot.mcmc}
\alias{levelplot.mcmc}
\alias{qqmath.mcmc}
\alias{xyplot.mcmc}
\alias{densityplot.mcmc.list}
\alias{levelplot.mcmc.list}
\alias{qqmath.mcmc.list}
\alias{xyplot.mcmc.list}
\alias{acfplot}
\alias{acfplot.mcmc}
\alias{acfplot.mcmc.list}
\usage{
\method{densityplot}{mcmc}(x, data,
             outer, aspect = "xy",
             default.scales = list(relation = "free"),
             start = 1, thin = 1,
             main = attr(x, "title"),
             xlab = "",
             plot.points = "rug",
             \dots,
             subset)
\method{densityplot}{mcmc.list}(x, data,
             outer = FALSE, groups = !outer,
             aspect = "xy",
             default.scales = list(relation = "free"),
             start = 1, thin = 1,
             main = attr(x, "title"),
             xlab = "",
             plot.points = "rug",
             \dots,
             subset)
\method{levelplot}{mcmc}(x, data, main = attr(x, "title"),
             start = 1, thin = 1,
             \dots,
             xlab = "", ylab = "",
             cuts = 10, at,
             col.regions = topo.colors(100),
             subset)
\method{qqmath}{mcmc}(x, data,
             outer, aspect = "xy",
             default.scales = list(y = list(relation = "free")),
             prepanel = prepanel.qqmathline,
             start = 1, thin = 1,
             main = attr(x, "title"),
             ylab = "",
             \dots,
             subset)
\method{qqmath}{mcmc.list}(x, data,
             outer = FALSE, groups = !outer,
             aspect = "xy",
             default.scales = list(y = list(relation = "free")),
             prepanel = prepanel.qqmathline,
             start = 1, thin = 1,
             main = attr(x, "title"),
             ylab = "",
             \dots,
             subset)
\method{xyplot}{mcmc}(x, data,
             outer, layout = c(1, ncol(x)),
             default.scales = list(y = list(relation = "free")),
             type = 'l',
             start = 1, thin = 1,
             ylab = "", 
             xlab = "Iteration number",
             main = attr(x, "title"),
             \dots,
             subset)
\method{xyplot}{mcmc.list}(x, data, outer = FALSE, groups = !outer,
             aspect = "xy", layout = c(1, ncol(x[[1]])),
             default.scales = list(y = list(relation = "free")),
             type = 'l',
             start = 1, thin = 1,
             main = attr(x, "title"),
             ylab = "",
             \dots,
             subset)
acfplot(x, data, \dots)
\method{acfplot}{mcmc}(x, data, outer,
             prepanel, panel, 
             type = 'h',
             aspect = "xy",
             start = 1, thin = 1,
             lag.max = NULL,
             ylab = "Autocorrelation",
             xlab = "Lag",
             main = attr(x, "title"),
             \dots,
             subset)
\method{acfplot}{mcmc.list}(x, data, outer = FALSE, groups = !outer,
             prepanel, panel,
             type = if (groups) 'b' else 'h',
             aspect = "xy",
             start = 1, thin = 1,
             lag.max = NULL,
             ylab = "Autocorrelation",
             xlab = "Lag",
             main = attr(x, "title"),
             \dots,
             subset)
}
\description{
  These methods use the Trellis framework as implemented in the
  \code{lattice} package to produce space-conserving diagnostic plots
  from \code{"mcmc"} and \code{"mcmc.list"} objects.  The \code{xyplot}
  methods produce trace plots.  The \code{densityplot} methods and
  \code{qqmath} methods produce empirical density and probability
  plots.  The \code{levelplot} method depicts the correlation of the
  series.  The \code{acfplot} methods plot the auto-correlation in the
  series.  

  Not yet available in S-PLUS.
}
\arguments{
  \item{x}{ an \code{"mcmc"} or \code{"mcmc.list"} object. }
  \item{data}{ ignored, present for consistency with generic. }
  \item{outer}{ for the \code{"mcmc.list"} methods, a logical flag to
    control whether multiple runs of a series are displayed in the same
    panel (they are if \code{FALSE}, not if \code{TRUE}).  If specified
    in the \code{"mcmc"} methods, this argument is ignored with a
    warning.  }
  \item{groups}{ for the \code{"mcmc.list"} methods, a logical flag to
    control whether the underlying \code{lattice} call will be supplied
    a \code{groups} arguments indicating which run a data point
    originated from.  The panel function is responsible for handling
    such an argument, and will usually differentiate runs within a panel
    by using different graphical parameters.  When \code{outer=FALSE},
    the default of \code{groups} is \code{TRUE} if the corresponding
    default panel function is able to make use of such information.
    When \code{outer=FALSE}, \code{groups=TRUE} will be ignored with a
    warning.  }
  \item{aspect}{ controls the physical aspect ratio of the panel.  See
    \code{\link[lattice:xyplot]{xyplot}} for details.  The default for
    these methods is chosen carefully - check what the default plot
    looks like before changing this parameter.}
  \item{default.scales}{ this parameter provides a reasonable default
    value of the \code{scales} parameter for the method. It is unlikely
    that a user will wish to change this parameter.  Pass a value for
    \code{scales} (see \code{\link[lattice:xyplot]{xyplot}}) instead,
    which will override values specified here.  }
  \item{type}{ a character vector that determines if lines, points,
    etc. are drawn on the panel.  The default values for the methods are
    carefully chosen.  See
    \code{\link[lattice:panel.xyplot]{panel.xyplot}} for possible
    values. }
  \item{thin}{ an optional thinning interval that is applied before the
    plot is drawn.}
  \item{start}{ an optional value for the starting point within the
    series.  Values before the starting point are considered part of the
    "burn-in" of the series and dropped.}
  \item{plot.points}{ character argument giving the style in which
    points are added to the plot.  See
    \code{\link[lattice:panel.densityplot]{panel.densityplot}} for
    details. }
  \item{layout}{a method-specific default for the \code{layout} argument
    to the lattice functions.}
  \item{xlab,ylab,main}{Used to provide default axis annotations and
    plot labels.}
  \item{cuts, at}{ defines number and location of values where colors
    change }
  \item{col.regions}{ color palette used }
  \item{lag.max}{ maximum lag for which autocorrelation is computed.  By
    default, the value chosen by \code{\link{acf}} is used }
  \item{prepanel,panel}{ suitable prepanel and panel functions for
    \code{acfplot}.  The prepanel function omits the lag-0
    auto-correlation (which is always 1) from the range calculations.  }
  \item{\dots}{other arguments, passed to the lattice function.
    Documentation of the corresponding generics in the \code{lattice}
    package should be consulted for possible arguments. }
  \item{subset}{indices of the subset of the series to plot.  The
    default is constructed from the \code{start} and \code{thin}
    arguments.}
}
\value{

  An object of class \code{"trellis"}. The relevant
  \code{\link[lattice:update.trellis]{update}} method can be used to
  update components of the object and the
  \code{\link[lattice:print.trellis]{print}} method (usually called by
  default) will plot it on an appropriate plotting device.

}
\seealso{
  \code{\link[lattice:Lattice]{Lattice}} for a brief introduction to
  lattice displays and links to further documentation.
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\examples{
data(line)

\dontrun{
xyplot(line)
xyplot(line[[1]], start = 10)
densityplot(line, start = 10)
qqmath(line, start = 10)
levelplot(line[[2]])
acfplot(line, outer = TRUE)
}
}
\keyword{hplot}
