"geweke.diag" <-
function (x, frac1 = 0.1, frac2 = 0.5) 
{
    if (is.mcmc.list(x)) 
        return(lapply(x, geweke.diag, frac1, frac2))
    x <- as.mcmc(x)
    xstart <- c(start(x), end(x) - frac2 * (end(x) - start(x)))
    xend <- c(start(x) + frac1 * (end(x) - start(x)), end(x))
    y.vom <- y.mean <- vector("list", 2)
    for (i in 1:2) {
        y <- window(x, start = xstart[i], end = xend[i])
        spans <- min(sqrt(niter(y))/0.3 + 1, niter(y) - 1)
        spec0 <- spec.pgram(y, spans = spans, demean = TRUE, 
            detrend = FALSE, plot = FALSE)$spec[1, ]
        y.vom[[i]] <- 10^(spec0/10)/niter(y)
        y.mean[[i]] <- apply(as.matrix(y), 2, mean)
    }
    z <- (y.mean[[1]] - y.mean[[2]])/sqrt(y.vom[[1]] + y.vom[[2]])
    out <- list(z = z, frac = c(frac1, frac2))
    class(out) <- "geweke.diag"
    return(out)
}
"geweke.plot" <-
function (x, frac1 = 0.1, frac2 = 0.5, bin.width = 10, max.bins = 50, 
    coverage = 0.95, auto.layout = TRUE, ask = TRUE, ...) 
{
    x <- as.mcmc.list(x)
    nbin <- min(floor((niter(x) - 50)/bin.width), max.bins)
    binw <- floor((niter(x) - 50)/nbin)
    oldpar <- NULL
    on.exit(par(oldpar))
    if (auto.layout) 
        oldpar <- par(mfrow = set.mfrow(Nchains = nchain(x), 
            Nparms = nvar(x)))
    oldpar <- c(oldpar, par(ask = ask))
    ystart <- seq(from = start(x), to = end(x) - 49 * thin(x), 
        by = binw * thin(x))
    gcd <- array(dim = c(length(ystart), nvar(x), nchain(x)), 
        dimnames = c(ystart, varnames(x), chanames(x)))
    for (n in 1:length(ystart)) {
        geweke.out <- geweke.diag(window(x, start = ystart[n]), 
            frac1 = frac1, frac2 = frac2)
        for (k in 1:nchain(x)) gcd[n, , k] <- geweke.out[[k]]$z
    }
    for (k in 1:nchain(x)) for (j in 1:nvar(x)) {
        climit <- qnorm((1 + coverage)/2)
        ylimit <- max(c(climit, abs(gcd[, j, k])))
        plot(ystart, gcd[, j, k], type = "p", xlab = "First iteration in segment", 
            ylab = "Z-score", pch = 4, ylim = c(-ylimit, ylimit), 
            ...)
        abline(h = c(ylimit, -ylimit), lty = 2)
        if (nchain(x) > 1) {
            title(main = paste(varnames(x, allow.null = FALSE)[j], 
                " (", chanames(x, allow.null = FALSE)[k], ")", 
                sep = ""))
        }
        else {
            title(main = paste(varnames(x, allow.null = FALSE)[j], 
                sep = ""))
        }
    }
    invisible(list(start.iter = ystart, z = gcd))
}
"geweke.power" <-
function (x) 
{
    # 
    # geweke.power 
    # 
    # spans parm for smoothing periodogram 
    a <- length(x)
    nspans <- sqrt(a)/0.3 + 1
    if (nspans >= a) {
        # spans is longer than time series 
        nspans <- a - 1
    }
    pgram1 <- spec.pgram(x, spans = nspans, demean = TRUE, detrend = FALSE, 
        plot = FALSE)
    #changed 03/20/94        
    # power <- 2 * pi * (10^(pgram1$spec[1]/10))     
    power <- (10^(pgram1$spec[1]/10))
    # spectral density converted from decibels       
    nse <- sqrt(power/a)
    power
}
"print.geweke.diag" <-
function (x, digits = min(4, .Options$digits), ...) 
{
    cat("\nFraction in 1st window =", x$frac[1])
    cat("\nFraction in 2nd window =", x$frac[2], "\n\n")
    print.default(x$z, digits = digits, ...)
    cat("\n")
    invisible(x)
}
"geweke.nse" <-
function (x) 
{
    # 
    # geweke.nse 
    # 
    # Author: Kate Cowles 
    # 
    # spans parm for smoothing periodogram 
    a <- length(x)
    #  
    nspans <- sqrt(a)/0.3 + 1
    if (nspans >= a) {
        # spans is longer than time series 
        nspans <- a - 1
    }
    pgram1 <- spec.pgram(x, spans = nspans, demean = TRUE, detrend = FALSE, 
        plot = FALSE)
    # changed 03/20/94 
    # power <- 2 * pi * (10^(pgram1$spec[1]/10))     
    power <- (10^(pgram1$spec[1]/10))
    # spectral density converted from decibels       
    nse <- sqrt(power/a)
    nse
}
