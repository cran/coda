"autocorr" <-
function (x, lags = c(0, 1, 5, 10, 50), relative = TRUE)
{
  ## RGA moved MCMC list processing first, else thinning gets
  ## applied twice.  Thanks to Denise Chang for finding this.
  if (is.mcmc.list(x))
    return(lapply(x, autocorr, lags = lags, relative = relative))
  lag.max <- max(lags)
  if (relative)
    lags <- lags * thin(x)
  else if (any(lags%%thin(x) != 0))
    stop("Lags do not conform to thinning interval")
  lags <- lags[lags < niter(x) * thin(x)]
  x <- as.mcmc(x)
  y <- array(dim = c(length(lags), nvar(x), nvar(x)))
  dimnames(y) <- list(paste("Lag", lags), varnames(x), varnames(x))
  acf.out <- acf(as.ts.mcmc(x), lag.max = lag.max, plot = FALSE)$acf
  y[, , ] <- if (is.array(acf.out))
    acf.out[lags%/%thin(x) + 1, , ]
  else acf.out[lags%/%thin(x) + 1]
  return(y)
}

"autocorr.plot" <-
function (x, lag.max, auto.layout = TRUE, ask, ...)
{
    if (missing(ask)) {
      ask <- if (is.R()) {
        dev.interactive()
      }
      else {
        interactive()
      }
    }
    oldpar <- NULL
    on.exit(par(oldpar))
    if (auto.layout)
        oldpar <- par(mfrow = set.mfrow(Nchains = nchain(x),
                      Nparms = nvar(x)))
    if (!is.mcmc.list(x))
        x <- mcmc.list(as.mcmc(x))
    for (i in 1:nchain(x)) {
        xacf <- if (missing(lag.max))
            acf(as.ts.mcmc(x[[i]]), plot = FALSE)
        else acf(as.ts.mcmc(x[[i]]), lag.max = lag.max, plot = FALSE)
        for (j in 1:nvar(x)) {
            plot(xacf$lag[, j, j], xacf$acf[, j, j], type = "h",
                 ylab = "Autocorrelation", xlab = "Lag", ylim = c(-1, 1), ...)
            title(paste(varnames(x)[j],
                        ifelse(is.null(chanames(x)), "", ":"),
                        chanames(x)[i], sep = ""))
            if (i==1 && j==1)
                oldpar <- c(oldpar, par(ask = ask))
        }
    }
    invisible(x)
}

"crosscorr" <-
function (x)
{
    cor(as.matrix(x))
}

"crosscorr.plot" <-
function (x, col = topo.colors(10), ...)
{
    Nvar <- nvar(x)
    pcorr <- crosscorr(x)
    dens <- ((pcorr + 1) * length(col))%/%2 + (pcorr < 1) + (pcorr <
        -1)
    cutoffs <- format(seq(from = 1, to = -1, length = length(col) +
        1), digits = 2)
    leg <- paste("(", cutoffs[-1], ",", cutoffs[-length(cutoffs)],
        "]", sep = "")
    oldpar <- NULL
    on.exit(par(oldpar))
    oldpar <- c(par(pty = "s", adj = 0.5), oldpar)
    plot(0, 0, type = "n", xlim = c(0, Nvar), ylim = c(0, Nvar),
        xlab = "", ylab = "", xaxt = "n", yaxt = "n", ...)
    if (!is.R()){ # In S-PLUS, specify that the y-axis labels should be right-justified
      par(adj = 1)
    }
    axis(1, at = 1:Nvar - 0.5, labels = abbreviate(varnames(x,
        allow.null = FALSE), minlength = 7))
    axis(2, at = 1:Nvar - 0.5, labels = abbreviate(varnames(x,
        allow.null = FALSE), minlength = 7)[Nvar:1])
    for (cl in 1:Nvar) {
        for (rw in 1:(Nvar - cl + 1)) polygon(y = c(cl - 1, cl -
            1, cl, cl, cl - 1), x = c(rw - 1, rw, rw, rw - 1,
            rw - 1), col = col[dens[nrow(dens) - cl + 1, rw]])
    }
    yval <- seq(from = Nvar/2, to = Nvar, length = length(col) +
        1)
    ydelta <- Nvar/(2 * (length(col) + 1))
    for (i in 1:length(col)) {
        polygon(y = c(yval[i], yval[i + 1], yval[i + 1], yval[i],
            yval[i]), col = col[i], x = c(Nvar - ydelta, Nvar -
            ydelta, Nvar, Nvar, Nvar - ydelta))
    }
    text(Nvar - ydelta, Nvar, "1", adj = c(1, 1))
    text(Nvar - ydelta, 0.5 * Nvar, "-1", adj = c(1, 0))
    text(Nvar - ydelta, 0.75 * Nvar, "0", adj = c(1, 0.5))
    invisible()
}

"pretty.discrete" <- function(y, right)
{
    ## Used to created break points for hist() for discrete data.
    ## Works around some limitations of pretty() for discrete data.
    ## The acid test is that the histogram produced by densplot for
    ## discrete data should be visually uniform if the underlying
    ## discrete distribution is uniform.

    ybreaks <- pretty(y, nclass.Sturges(y))
    yunique <- unique(y)
    if (length(yunique) == 1) {
        return(ybreaks)
    }

    if (length(ybreaks) > length(yunique)) {
        ## Pretty puts in too many breaks
        ybreaks <- sort(yunique)
    }

    nb <- length(ybreaks)

    if (right) {
        if (max(y) < ybreaks[nb]) {
            ## Last bin is too wide
            ybreaks[nb] <- max(y)
        }
        if (min(y) > ybreaks[1]) {
            ## First bin is too wide
            ybreaks[1] <- min(y) - 1
        }
        else if (min(y) == ybreaks[1]) {
            ## The hist() function adds some fuzz to its break
            ## points causing the first two categories to be
            ## merged.  Work around this by adding extra
            ## breakpoints on the left.
            ybreaks <- c(ybreaks[1] - 1, ybreaks)
        }
    }
    else {
        if (min(y) > ybreaks[1]) {
            ## First bin is too wide
            ybreaks[1] <- min(y)
        }
        if (max(y) < ybreaks[nb]) {
            ## Last bin is too wide
            ybreaks[nb] <- max(y) + 1
        }
        else if (max(y) == ybreaks[nb]) {
            ## The hist() function adds some fuzz to its break
            ## points causing the first two categories to be
            ## merged.  Work around this by adding extra
            ## breakpoints on the left.
            ybreaks <- c(ybreaks[nb] + 1, ybreaks)
        }
    }

    ybreaks
}

"densplot" <-
function (x, show.obs = TRUE, bwf, ylim, xlab, ylab = "", type = "l", main,
          right=TRUE, ...)
{
    xx <- as.matrix(x)
    for (i in 1:nvar(x)) {
        y <- xx[, i, drop = TRUE]
        if (missing(bwf))
            bwf <- function(x) {
                x <- x[!is.na(as.vector(x))]
                return(1.06 * min(sd(x), IQR(x)/1.34) * length(x)^-0.2)
            }
        bw <- bwf(y)
        width <- 4 * bw

        ## Override the default main titles generated by histogram and
        ## plot.density
        main.par <- if (missing(main)) {
            ## Suppress default title given by plot.density
            if (is.null(varnames(x))) ""
            else paste("Density of", varnames(x)[i])
        }
        else main

        if (max(abs(y - floor(y))) == 0 || bw == 0 || length(unique(y)) == 1)
        {
            ## Draw histogram for discrete data or constant data or if
            ## bandwidth is zero.

            ybreaks <- pretty.discrete(y, right)

            ## Set default values for graphical parameters
            if (missing(xlab)) {
                xlab <- ""
            }
            if (missing(ylim)) {
                ylim.par <- NULL
            }
            yhist <- hist(y, breaks=ybreaks, right=right, plot=FALSE)
            plot(yhist, xlab=xlab, ylab=ylab, ylim=ylim.par, main=main.par,
                 xaxt="n", freq=FALSE, ...)
            axis(side=1, at=ybreaks)
        }
        else {
            ## Draw density plot

            ## Reflect data at boundary, if necessary
            scale <- "open"
            if (max(y) <= 1 && 1 - max(y) < 2 * bw) {
                if (min(y) >= 0 && min(y) < 2 * bw) {
                    scale <- "proportion"
                    y <- c(y, -y, 2 - y)
                }
            }
            else if (min(y) >= 0 && min(y) < 2 * bw) {
                scale <- "positive"
                y <- c(y, -y)
            }
            else scale <- "open"
            dens <- density(y, width = width)
            if (scale == "proportion") {
                dens$y <- 3 * dens$y[dens$x >= 0 & dens$x <= 1]
                dens$x <- dens$x[dens$x >= 0 & dens$x <= 1]
            }
            else if (scale == "positive") {
                dens$y <- 2 * dens$y[dens$x >= 0]
                dens$x <- dens$x[dens$x >= 0]
            }

            ## Set default graphics parameters
            ylim.par <- if (missing(ylim)) NULL else ylim
            xlab.par <- if (missing(xlab)) {
                if (is.R()) {
                    paste("N =", niter(x), "  Bandwidth =", formatC(dens$bw))
                }
                else {
                    ##In S-PLUS the bandwidth is not returned by the
                    ##"density" function
                    paste("N =", niter(x), "  Bandwidth =", formatC(bw))
                }
            }
            else xlab

            plot(dens, xlab=xlab.par, ylab = ylab, type = type,
                 ylim = ylim.par, main = main.par, ...)

            if (show.obs) {
                lines(y[1:niter(x)], rep(max(dens$y)/100, niter(x)),
                      type = "h")
            }
        }
    }
    return(invisible(x))
}

if (FALSE) {# was is.R

"IQR"<-
function(x, na.rm = FALSE)
diff(quantile(as.numeric(x), c(0.25, 0.75), na.rm = na.rm))

}

"read.jags" <- function (file = "jags.out", start, end, thin, quiet=FALSE)
{
  nc <- nchar(file)
  if (nc > 3 && substring(file, nc - 3, nc) == ".out")
    root <- substring(file, 1, nc - 4)
  else root <- file
  index.file = paste(root, ".ind", sep="")

  read.coda(file, index.file, start, end, thin, quiet)
}

"read.openbugs" <-
function (stem = "", start, end, thin, quiet = FALSE)
{
  index.file <- paste(stem, "CODAindex.txt", sep = "")
  if (!file.exists(index.file))
    stop("No index file found")
  index.date <- file.info(index.file)$ctime

  nchain <- 0
  while (TRUE) {
    output.file <- paste(stem, "CODAchain", nchain + 1, ".txt",
                         sep = "")
    if (file.exists(output.file)) {
      nchain <- nchain + 1
      output.date <- file.info(output.file)$ctime
      dt <- difftime(index.date, output.date, units="mins")
      if(abs(as.numeric(dt)) > 1 ) {
        warning(paste("Files \"",index.file,"\" and \"",output.file,
                      "\" were created at different times\n",sep=""))
      }
    }
    else break

  }
  if (nchain == 0)
    stop("No output files found")

  ans <- vector("list", nchain)
  for (i in 1:nchain) {
    output.file <- paste(stem, "CODAchain", i, ".txt", sep = "")
    ans[[i]] <- read.coda(output.file, index.file, start,
                          end, thin, quiet)
  }
  return(mcmc.list(ans))
}

"read.coda" <- function (output.file, index.file, start, end, thin,quiet=FALSE) {
  index <- read.table(index.file,
                      row.names = 1, col.names = c("", "begin", "end"))
  vnames <- row.names(index)
  if (is.R()) {
    temp <- scan(output.file, what = list(iter = 0, val = 0), quiet = TRUE)
  }
  else {
    temp <- scan(output.file, what = list(iter = 0, val = 0))
  }
  ## Do one pass through the data to see if we can construct
  ## a regular time series easily
  ##
  start.vec <- end.vec <- thin.vec <- numeric(nrow(index))
  for (i in 1:length(vnames)) {
    iter.i <- temp$iter[index[i, "begin"]:index[i, "end"]]
    thin.i <- unique(diff(iter.i))
    thin.vec[i] <- if (length(thin.i) == 1)
      thin.i
    else NA
    start.vec[i] <- iter.i[1]
    end.vec[i] <- iter.i[length(iter.i)]
  }
  if (any(is.na(start.vec)) || any(thin.vec != thin.vec[1]) ||
      any((start.vec - start.vec[1])%%thin.vec[1] != 0)) {
    ##
    ## Do it the brute force way
    ##
    iter <- sort(unique(temp$iter))
    old.thin <- unique(diff(iter))
    if (length(old.thin) == 1)
      is.regular <- TRUE
    else {
      if (all(old.thin%%min(old.thin) == 0))
        old.thin <- min(old.thin)
      else old.thin <- 1
      is.regular <- FALSE
    }
  }
  else {
    iter <- seq(from = min(start.vec), to = max(end.vec),
                by = thin.vec[1])
    old.thin <- thin.vec[1]
    is.regular <- TRUE
  }
  if (missing(start))
    start <- min(start.vec)
  else if (start < min(start.vec)) {
    warning("start not changed")
    start <- min(start.vec)
  }
  else if (start > max(end.vec))
    stop("Start after end of data")
  else iter <- iter[iter >= start]
  if (missing(end))
    end <- max(end.vec)
  else if (end > max(end.vec)) {
    warning("end not changed")
    end <- max(end.vec)
  }
  else if (end < min(start.vec))
    stop("End before start of data")
  else iter <- iter[iter <= end]
  if (missing(thin))
    thin <- old.thin
  else if (thin%%old.thin != 0) {
    thin <- old.thin
    warning("thin not changed")
  }
  else {
    new.iter <- iter[(iter - start)%%thin == 0]
    new.thin <- unique(diff(new.iter))
    if (length(new.thin) != 1 || new.thin != thin)
      warning("thin not changed")
    else {
      iter <- new.iter
      end <- max(iter)
      is.regular <- TRUE
    }
  }
  out <- matrix(NA, nrow = length(iter), ncol = nrow(index))
  dimnames(out) <- list(iter, vnames)
  for (v in vnames) {
    if(!quiet)
      cat("Abstracting", v, "... ")
    inset <- index[v, "begin"]:index[v, "end"]
    iter.v <- temp$iter[inset]
    if (!is.regular) {
      use.v <- duplicated(c(iter, iter.v))[-(1:length(iter))]
      use <- duplicated(c(iter.v, iter))[-(1:length(iter.v))]
    }
    else {
      use.v <- (iter.v - start)%%thin == 0 & iter.v >=
        start & iter.v <= end
      use <- (iter.v[use.v] - start)%/%thin + 1
    }
    if (length(use) > 0 && any(use.v))
      out[use, v] <- temp$val[inset[use.v]]
    if(!quiet)
      cat(length(use), "valid values\n")
  }
  if (is.regular)
    out <- mcmc(out, start = start, end = end, thin = thin)
  else warning("not returning an mcmc object")
  return(out)
}

"traceplot" <-
function (x, smooth = FALSE, col = 1:6, type = "l", xlab = "Iterations",
          ylab = "", ...)
{
    x <- mcmc.list(x)
    args <- list(...)
    for (j in 1:nvar(x)) {
        xp <- as.vector(time(x))
        yp <- if (nvar(x) > 1)
            x[, j, drop = TRUE]
        else x
        yp <- do.call("cbind", yp)
        matplot(xp, yp, xlab = xlab, ylab = ylab, type = type, col = col, ...)
        if (!is.null(varnames(x)) && is.null(list(...)$main))
            title(paste("Trace of", varnames(x)[j]))
        if (smooth) {
            scol <- rep(col, length = nchain(x))
            for (k in 1:nchain(x)) lines(lowess(xp, yp[, k]),
                                         col = scol[k])
        }
    }
}

"plot.mcmc" <- function (x, trace = TRUE, density = TRUE, smooth = FALSE, bwf,
                         auto.layout = TRUE, ask = dev.interactive(), ...)
{
  oldpar <- NULL
  on.exit(par(oldpar))
  if (auto.layout) {
    mfrow <- set.mfrow(Nchains = nchain(x), Nparms = nvar(x),
                       nplots = trace + density)
    oldpar <- par(mfrow = mfrow)
  }
  for (i in 1:nvar(x)) {
    y <- mcmc(as.matrix(x)[, i, drop=FALSE], start(x), end(x), thin(x))
    if (trace)
      ## RGA fixed to propagate ... argument.
      traceplot(y, smooth = smooth, ...)
    if (density) {
      if (missing(bwf))
        ## RGA fixed to propagate ... argument.
        densplot(y, ...)
      else
        densplot(y, bwf = bwf, ...)
    }
    if (i==1)
       oldpar <- c(oldpar, par(ask=ask))
  }
}


### RGA This is a wrapper for spectrum0 which returns NA if
### spectrum0 crashes.  This has happened to me several times when
### there was bug in my MCMC algorithm.
"safespec0" <-
  function (x) {
  result <- try(spectrum0.ar(x)$spec)
  ## R
  if (class(result) == "try-error") result <- NA
  ## S-Plus
  if (class(result) == "try") result <- NA
  result
}

"summary.mcmc" <-
  function (object, quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), ...)
{
  x <- as.mcmc(object)
  statnames <- c("Mean", "SD", "Naive SE", "Time-series SE")
  varstats <- matrix(nrow = nvar(x), ncol = length(statnames),
                     dimnames = list(varnames(x), statnames))
  ## RGA replaced with safespec0
  #sp0 <- function(x) spectrum0(x)$spec
  if (is.matrix(x)) {
    xmean <- apply(x, 2, mean)
    xvar <- apply(x, 2, var)
    xtsvar <- apply(x, 2, safespec0)
    varquant <- t(apply(x, 2, quantile, quantiles))
  }
  else {
    xmean <- mean(x, na.rm = TRUE)
    xvar <- var(x, na.rm = TRUE)
    xtsvar <- safespec0(x)
    varquant <- quantile(x, quantiles)
  }
  varstats[, 1] <- xmean
  varstats[, 2] <- sqrt(xvar)
  varstats[, 3] <- sqrt(xvar/niter(x))
  varstats[, 4] <- sqrt(xtsvar/niter(x))
  varstats <- drop(varstats)
  varquant <- drop(varquant)
  out <- list(statistics = varstats, quantiles = varquant,
              start = start(x), end = end(x), thin = thin(x), nchain = 1)
  if (is.R()) {
    class(out) <- "summary.mcmc"
  }
  else {
    oldClass(out) <- "summary.mcmc"
  }
  return(out)
}

"print.summary.mcmc" <-
  function (x, digits = max(3, .Options$digits - 3), ...)
{
  cat("\n", "Iterations = ", x$start, ":", x$end, "\n", sep = "")
  cat("Thinning interval =", x$thin, "\n")
  cat("Number of chains =", x$nchain, "\n")
  cat("Sample size per chain =", (x$end - x$start)/x$thin +
      1, "\n")
  cat("\n1. Empirical mean and standard deviation for each variable,")
  cat("\n   plus standard error of the mean:\n\n")
  print(x$statistics, digits = digits, ...)
  cat("\n2. Quantiles for each variable:\n\n")
  print(x$quantiles, digits = digits, ...)
  cat("\n")
  invisible(x)
}








