"[.mcmc" <-
function (x, i, j, drop = missing(i)) 
{
    y <- NextMethod("[")
    if (length(y) == 0 || is.null(y)) 
        return(y)
    if (missing(i)) 
        y <- mcmc(y, start = start(x), thin = thin(x))
    else {
        xtimes <- time(x)
        ytimes <- time(x)[i]
        delta <- unique(ytimes[-1] - ytimes[-length(ytimes)])
        if (length(delta) != 1 || delta <= 0) {
            if (!drop) 
                warning("Not returning an mcmc object")
            attr(y, "mcpar") <- NULL
            class(y) <- NULL
        }
        else {
            #Coerce to vector to avoid annoying warnings from
            #"[.ts"
            start <- as.vector(ytimes)[1]
            end <- as.vector(ytimes)[length(ytimes)]
            y <- mcmc(y, start = start, end = end, thin = delta)
        }
    }
    return(y)
}
"as.mcmc" <-
function (x) 
UseMethod("as.mcmc")
"as.mcmc.default" <-
function (x) 
if (is.mcmc(x)) x else mcmc(x)
"as.ts.mcmc" <-
function (x) 
{
    x <- as.mcmc(x)
    if (nchain(x) > 1) 
        stop("Can't coerce to time series")
    else y <- ts(x, start = start(x), end = end(x), deltat = thin(x))
    attr(y, "mcpar") <- NULL
    return(y)
}
"start.mcmc" <-
function (x) 
{
    attr(as.mcmc(x), "mcpar")[1]
}
"end.mcmc" <-
function (x) 
{
    attr(as.mcmc(x), "mcpar")[2]
}
"frequency.mcmc" <-
function (x) 
{
    1/attr(as.mcmc(x), "mcpar")[3]
}
"thin.mcmc" <-
function (x) 
{
    attr(as.mcmc(x), "mcpar")[3]
}
"is.mcmc" <-
function (x) 
{
    if (inherits(x, "mcmc")) 
        if (length(dim(x)) == 3) 
            stop("Obsolete mcmc object\nUpdate with a command like\nx <- upgrade.mcmc(x)")
        else TRUE
    else FALSE
}
"mcmc" <-
function (data = NA, start = 1, end = numeric(0), thin = 1) 
{
    niter <- NROW(data)
    nvar <- NCOL(data)
    thin <- round(thin)
    if (length(start) > 1) 
        stop("Invalid start")
    if (length(end) > 1) 
        stop("Invalid end")
    if (length(thin) != 1) 
        stop("Invalid thin")
    if (missing(end)) 
        end <- start + (niter - 1) * thin
    else if (missing(start)) 
        start <- end - (niter - 1) * thin
    nobs <- floor((end - start)/thin + 1.01)
    if (niter < nobs) 
        stop("Start, end and thin incompatible with data")
    else {
        end <- start + thin * (nobs - 1)
        if (nobs < niter) 
            data <- data[1:nobs, , , drop = FALSE]
    }
    attr(data, "mcpar") <- c(start, end, thin)
    attr(data, "class") <- "mcmc"
    data
}
"print.mcmc" <-
function (x, ...) 
{
    cat("Markov Chain Monte Carlo (MCMC) output:\nStart =", start(x), 
        "\nEnd =", end(x), "\nThinning interval =", thin(x), 
        "\n")
    attr(x, "mcpar") <- NULL
    attr(x, "class") <- NULL
    NextMethod("print", ...)
    invisible(x)
}
"plot.mcmc" <-
function (x, trace = TRUE, density = TRUE, smooth = TRUE, bwf, 
    auto.layout = TRUE, ask = TRUE, ...) 
{
    oldpar <- NULL
    on.exit(par(oldpar))
    if (auto.layout) {
        mfrow <- set.mfrow(Nchains = nchain(x), Nparms = nvar(x), 
            nplots = trace + density, sepplot = FALSE, one.page = FALSE)
        oldpar <- par(mfrow = mfrow)
    }
    oldpar <- c(oldpar, par(ask = ask))
    for (i in 1:nvar(x)) {
        y <- as.matrix(x)[, i, drop = FALSE]
        if (trace) 
            traceplot(y, smooth = smooth)
        if (density) 
            if (missing(bwf)) 
                densplot(y)
            else densplot(y, bwf = bwf)
    }
}
"summary.mcmc" <-
function (x, quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), ...) 
{
    x <- as.mcmc(x)
    statnames <- c("Mean", "SD", "Naive SE", "Time-series SE")
    varstats <- matrix(nrow = nvar(x), ncol = length(statnames), 
        dimnames = list(varnames(x), statnames))
    if (is.matrix(x)) {
        xmean <- apply(x, 2, mean)
        xvar <- apply(x, 2, var)
        xnse <- apply(x, 2, geweke.nse)
        varquant <- t(apply(x, 2, quantile, quantiles))
    }
    else {
        xmean <- mean(x, na.rm = TRUE)
        xvar <- var(x, na.rm = TRUE)
        xnse <- geweke.nse(x)
        varquant <- quantile(x, quantiles)
    }
    varstats[, 1] <- xmean
    varstats[, 2] <- sqrt(xvar)
    varstats[, 3] <- sqrt(xvar/niter(x))
    varstats[, 4] <- xnse
    varstats <- drop(varstats)
    varquant <- drop(varquant)
    out <- list(statistics = varstats, quantiles = varquant, 
        start = start(x), end = end(x), thin = thin(x), nchain = 1)
    class(out) <- "summary.mcmc"
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
"as.matrix.mcmc" <-
function (x, iters = FALSE) 
{
    #as.matrix.mcmc
    y <- matrix(nrow = niter(x), ncol = nvar(x) + iters)
    var.cols <- iters + 1:nvar(x)
    if (iters) 
        y[, 1] <- as.vector(time(x))
    y[, var.cols] <- x
    rownames <- character(ncol(y))
    if (iters) 
        rownames[1] <- "ITER"
    rownames[var.cols] <- varnames(x, allow.null = FALSE)
    dimnames(y) <- list(NULL, rownames)
    mcmc(y, start = start(x), end = end(x), thin = thin(x))
}
"time.mcmc" <-
function (x) 
{
    x <- as.mcmc(x)
    ts(seq(from = start(x), to = end(x), by = thin(x)), start = start(x), 
        end = end(x), deltat = thin(x))
}
"window.mcmc" <-
function (x, start, end, thin, ts.eps = .Options$ts.eps) 
{
    xmcpar <- mcpar(x)
    xstart <- xmcpar[1]
    xend <- xmcpar[2]
    xthin <- xmcpar[3]
    if (missing(thin)) 
        thin <- xthin
    else if (thin%%xthin != 0) {
        thin <- xthin
        warning("Thin value not changed")
    }
    xtime <- as.vector(time(x))
    if (missing(start)) 
        start <- xstart
    else if (length(start) != 1) 
        stop("bad value for start")
    else if (start < xstart) {
        start <- xstart
        warning("start value not changed")
    }
    if (missing(end)) 
        end <- xend
    else if (length(end) != 1) 
        stop("bad value for end")
    else if (end > xend) {
        end <- xend
        warning("end value not changed")
    }
    if (start > end) 
        stop("start cannot be after end")
    if (all(abs(xtime - start) > abs(start) * ts.eps)) {
        start <- xtime[(xtime > start) & ((start + xthin) > xtime)]
    }
    if (all(abs(end - xtime) > abs(end) * ts.eps)) {
        end <- xtime[(xtime < end) & ((end - xthin) < xtime)]
    }
    use <- 1:NROW(x)
    use <- use[use >= trunc((start - xstart)/xthin + 1.5) & use <= 
        trunc((end - xstart)/xthin + 1.5) & (use - trunc((start - 
        xstart)/xthin + 1.5))%%(thin%/%xthin) == 0]
    if (is.matrix(x)) 
        return(x[use, , drop = FALSE])
    else return(x[use])
}
"mcpar" <-
function (x) 
{
    attr(x, "mcpar")
}
"upgrade.mcmc" <-
function (x) 
{
    if (inherits(x, "mcmc")) {
        if (length(dim(x)) == 3) {
            nchain <- dim(x)[3]
            xtspar <- attr(x, "tspar")
            xstart <- xtspar[1]
            xend <- xtspar[2]
            xthin <- xtspar[3]
            out <- vector("list", nchain)
            for (i in 1:nchain) {
                y <- unclass(x)[, , 1, drop = TRUE]
                attr(y, "title") <- NULL
                attr(y, "tspar") <- NULL
                out[[i]] <- mcmc(y, start = xstart, end = xend, 
                  thin = xthin)
            }
            if (nchain == 1) 
                return(out[[1]])
            else return(mcmc.list(out))
        }
        else return(x)
    }
    else stop("Can't upgrade")
}
