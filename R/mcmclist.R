"[.mcmc.list" <-
function (x, i, j, drop = TRUE) 
{
    if (nargs() < 3 + !missing(drop)) {
        y <- NextMethod("[")
        if (is.list(y)) 
            y <- mcmc.list(y)
    }
    else {
        y <- vector("list", length(x))
        names(y) <- names(x)
        for (k in 1:length(y)) {
            y[[k]] <- if (missing(i) && missing(j)) 
                x[[k]]
            else if (missing(i)) 
                as.matrix(x[[k]])[, j, drop = drop]
            else if (missing(j)) 
                as.matrix(x[[k]])[i, , drop = drop]
            else as.matrix(x[[k]])[i, j, drop = drop]
        }
        if (all(sapply(y, is.mcmc, simplify = TRUE))) 
            y <- mcmc.list(y)
    }
    return(y)
}
"mcmc.list" <-
function (...) 
{
    x <- list(...)
    if (length(x) == 1 && is.list(x[[1]])) 
        x <- x[[1]]
    if (!all(unlist(lapply(x, is.mcmc)))) 
        stop("Arguments must be mcmc objects")
    nargs <- length(x)
    if (nargs >= 2) {
        xmcpar <- lapply(x, mcpar)
        if (!all(unlist(lapply(xmcpar, "==", xmcpar[[1]])))) 
            stop("Different start, end or thin values in each chain")
        xnvar <- lapply(x, nvar)
        if (!all(unlist(lapply(xnvar, "==", xnvar[[1]])))) 
            stop("Different number of variables in each chain")
        xvarnames <- lapply(x, varnames, allow.null = FALSE)
        if (!all(unlist(lapply(xvarnames, "==", xvarnames[[1]])))) 
            stop("Different variable names in each chain")
    }
    class(x) <- "mcmc.list"
    return(x)
}
"start.mcmc.list" <-
function (x) 
{
    start(x[[1]])
}
"end.mcmc.list" <-
function (x) 
{
    end(x[[1]])
}
"thin.mcmc.list" <-
function (x) 
{
    thin(x[[1]])
}
"is.mcmc.list" <-
function (x) 
inherits(x, "mcmc.list")
"plot.mcmc.list" <-
function (x, trace = TRUE, density = TRUE, smooth = TRUE, bwf, 
    auto.layout = TRUE, ...) 
{
    oldpar <- NULL
    on.exit(par(oldpar))
    if (auto.layout) {
        mfrow <- set.mfrow(Nchains = nchain(x), Nparms = nvar(x), 
            nplots = trace + density, sepplot = FALSE, one.page = FALSE)
        oldpar <- par(mfrow = mfrow)
    }
    oldpar <- c(oldpar, par(ask = TRUE))
    for (i in 1:nvar(x)) {
        if (trace) 
            traceplot(x[, i, drop = FALSE], smooth = smooth)
        if (density) 
            if (missing(bwf)) 
                densplot(x[, i, drop = FALSE])
            else densplot(x[, i, drop = FALSE], bwf = bwf)
    }
}
"summary.mcmc.list" <-
function (x, quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), ...) 
{
    x <- mcmc.list(x)
    statnames <- c("Mean", "SD", "Naive SE", "Time-series SE")
    varstats <- matrix(nrow = nvar(x), ncol = length(statnames), 
        dimnames = list(varnames(x), statnames))
    xnse <- matrix(nrow = nchain(x), ncol = nvar(x))
    if (is.matrix(x[[1]])) {
        for (i in 1:nchain(x)) xnse[i, ] <- apply(x[[i]], 2, 
            geweke.nse)
        xlong <- do.call("rbind", x)
    }
    else {
        for (i in 1:nchain(x)) xnse[i, ] <- geweke.nse(x[[i]])
        xlong <- as.matrix(x)
    }
    xnse <- sqrt(apply(xnse^2, 2, mean))
    xmean <- apply(xlong, 2, mean)
    xvar <- apply(xlong, 2, var)
    varquant <- t(apply(xlong, 2, quantile, quantiles))
    varstats[, 1] <- xmean
    varstats[, 2] <- sqrt(xvar)
    varstats[, 3] <- sqrt(xvar/niter(x))
    varstats[, 4] <- xnse
    varquant <- drop(varquant)
    varstats <- drop(varstats)
    out <- list(statistics = varstats, quantiles = varquant, 
        start = start(x), end = end(x), thin = thin(x), nchain = nchain(x))
    class(out) <- "summary.mcmc"
    return(out)
}
"as.matrix.mcmc.list" <-
function (x, chains = FALSE, iters = FALSE) 
{
    #as.matrix.mcmc.list
    x <- mcmc.list(x)
    y <- matrix(nrow = niter(x) * nchain(x), ncol = nvar(x) + 
        chains + iters)
    var.cols <- chains + iters + 1:nvar(x)
    for (i in 1:nchain(x)) {
        use.rows <- niter(x) * (i - 1) + 1:niter(x)
        if (chains) 
            y[use.rows, 1] <- i
        if (iters) 
            y[use.rows, chains + 1] <- as.vector(time(x))
        y[use.rows, var.cols] <- x[[i]]
    }
    rownames <- character(ncol(y))
    if (chains) 
        rownames[1] <- "CHAIN"
    if (iters) 
        rownames[1 + chains] <- "ITER"
    rownames[var.cols] <- varnames(x, allow.null = FALSE)
    dimnames(y) <- list(NULL, rownames)
    return(y)
}
"as.mcmc.mcmc.list" <-
function (x) 
{
    if (nchain(x) == 1) 
        return(x[[1]])
    else stop("Can't coerce mcmc.list to mcmc object: more than 1 chain")
}
"time.mcmc.list" <-
function (x) 
time(x[[1]])
"window.mcmc.list" <-
function (x, ...) 
{
    structure(lapply(x, window.mcmc, ...), class = "mcmc.list")
}
"as.mcmc.list" <-
function (x, ...) 
UseMethod("as.mcmc.list")
"as.mcmc.list.default" <-
function (x, ...) 
if (is.mcmc.list(x)) x else mcmc.list(x)
