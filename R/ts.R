"acf" <-
function (x, lag.max = NULL, plot = FALSE, type = "correlation") 
{
    if (plot) 
        warning(" acf plot not yet supported.")
    if (0 == charmatch(type, c("covariance", "correlation", "partial"), 
        nomatch = 0)) 
        stop("type not allowed in acf")
    if (!is.array(x)) 
        x <- matrix(x, length(x), 1)
    series <- deparse(substitute(x))
    x.freq <- frequency(as.ts(x))
    attr(x, "tsp") <- NULL
    attr(x, "class") <- NULL
    x <- as.matrix(x)
    sampleT <- nrow(x)
    #  or ? lag.max <- floor(10 * log10(sampleT))
    if (is.null(lag.max)) 
        lag.max <- floor(10 * (log10(sampleT) - log10(ncol(x))))
    lag.max <- min(lag.max, sampleT - 1)
    acf <- lag <- array(NA, c(lag.max + 1, ncol(x), ncol(x)))
    xb <- sweep(x, 2, apply(x, 2, mean))
    for (i in 0:lag.max) {
        Om <- (t(xb[(i + 1):sampleT, , drop = FALSE]) %*% xb[1:(sampleT - 
            i), , drop = FALSE])/sampleT
        if (type == "correlation") {
            # nrow above for univariate case
            if (i == 0) 
                Om0 <- diag(1/sqrt(diag(Om)), nrow = nrow(Om))
            Om <- Om0 %*% Om %*% Om0
        }
        acf[i + 1, , ] <- Om
        for (j in 1:ncol(x)) for (k in 1:ncol(x)) lag[i + 1, 
            j, k] <- ifelse(j > k, -i, i)
    }
    if (type == "partial") {
        warning("acf type partial not yet supported. 0 value being returned")
        acf <- array(0, dim(acf))
    }
    list(acf = acf, type = type, n.used = sampleT, lag = lag, 
        series = series)
}
"spec.pgram" <-
function (x, spans = 1, taper = 0.1, demean = FALSE, detrend = TRUE, 
    pad = FALSE, plot = FALSE) 
{
    x <- as.matrix(x)
    N <- nrow(x)
    if (detrend) {
        t <- 1:N
        for (i in 1:ncol(x)) x[, i] <- residuals(lm(x[, i] ~ 
            t))
    }
    else if (demean) {
        x <- sweep(x, 2, apply(x, 2, mean))
    }
    if (taper > 0.5 || taper < 0) 
        stop("taper must be between 0 and 0.5")
    else if (taper > 0) {
        w <- rep(1, N)
        n <- max(round(N * taper), 1)
        w[1:n] <- sin(((1:n - 0.5) * pi)/(2 * n))^2
        w[N:(N - n + 1)] <- w[1:n]
        x <- x * w
    }
    if (pad) 
        x <- rbind(x, matrix(0, nrow = (nextn(N) - N), ncol = ncol(x)))
    Nspec <- ceiling(N/2) + 1
    #
    # Should use mvfft here but there is a fatal bug!!!
    # 22.4.99 - MTP
    #
    spec <- matrix(NA, nrow = nrow(x), ncol = ncol(x))
    for (i in 1:ncol(x)) spec[, i] <- (Mod(fft(x[, i]))^2)/N
    filter.list <- vector("list", length(spans))
    for (i in 1:length(spans)) {
        m <- floor(spans[i]/2)
        filter.list[[i]] <- if (m > 0) 
            c(0.5, rep(1, 2 * m - 1), 0.5)/(2 * m)
        else 1
    }
    filter <- filter.list[[1]]
    if (length(spans) > 1) 
        for (i in 2:length(spans)) {
            filter <- convolve.open(filter.list[[i]], filter)
        }
    if (length(filter) > 1) {
        ndiff <- nrow(spec) - length(filter)
        m <- floor(length(filter)/2)
        if (ndiff < 0) 
            stop("filter too long!")
        else for (i in 1:ncol(spec)) {
            spec[, i] <- convolve(spec[, i], c(filter[(m + 1):(2 * 
                m + 1)], rep(0, ndiff), filter[1:m]))
        }
    }
    spec <- spec[1:(1 + ceiling(N/2)), , drop = FALSE]
    spec <- 10 * log10(spec)
    freq <- seq(from = 0, to = 0.5, length = Nspec)
    return(spec = spec, freq = freq)
}
