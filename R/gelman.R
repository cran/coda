"gelman.diag" <-
function (x, confidence = 0.95, transform = FALSE) 
{
    #
    # Gelman and Rubin's code
    #
    # Adapted to work on mcmc objects. Now you can analyse
    # several variables at once.
    #
    #
    # We compute the following statistics:
    #
    #  xdot:  vector of sequence means
    #  s2:  vector of sequence sample variances (dividing by n-1)
    #  W = mean(s2):  within MS
    #  B = n*var(xdot):  between MS.
    #  muhat = mean(xdot):  grand mean; unbiased under strong stationarity
    #  varW = var(s2)/m:  estimated sampling var of W
    #  varB = B^2 * 2/(m-1):  estimated sampling var of B
    #  covWB = (n/m)*(var(s2,xdot^2) - 2*muhat*var(s^2,xdot)):
    #          estimated sampling cov(W,B)
    #  sig2hat = ((n-1)/n))*W + (1/n)*B:  estimate of sig2; unbiased under
    #            strong stationarity
    #  quantiles:  emipirical quantiles from last half of simulated
    #              sequences
    #
    x <- as.mcmc.list(x)
    if (nchain(x) == 1) 
        stop("You need at least two chains")
    if (start(x) < end(x)/2) 
        x <- window(x, start = end(x)/2 + 1)
    Niter <- niter(x)
    Nchain <- nchain(x)
    confshrink <- matrix(nrow = nvar(x), ncol = 2, dimnames = list(varnames(x), 
        c("Point est.", paste(50 * (1 + confidence), "% quantile", 
            sep = ""))))
    z <- matrix(NA, nrow = niter(x), ncol = nchain(x))
    for (i in 1:nvar(x)) {
        for (j in 1:nchain(x)) z[, j] <- as.matrix(x[[j]])[, 
            i, drop = TRUE]
        if (transform) 
            if (min(z) > 0) 
                z <- if (max(z) < 1) 
                  log(z/(1 - z))
                else log(z)
        s2 <- apply(z, 2, var)
        W <- mean(s2)
        zbar <- apply(z, 2, mean)
        B <- Niter * var(zbar)
        sig2hat <- ((Niter - 1) * W + B)/Niter
        muhat <- mean(zbar)
        varW <- var(s2)/Nchain
        varB <- (2 * B^2)/(Nchain - 1)
        #
        covWB <- (Niter/Nchain) * (var(s2, zbar^2) - 2 * muhat * 
            var(s2, zbar))
        # Posterior interval post.range combines all uncertainties
        # in a t interval with center muhat, scale sqrt(postvar), 
        # and postvar.df degrees of freedom.
        #
        # postvar = sig2hat + B/(mn):  variance for the posterior
        #           interval. The B/(mn) term is there because of the
        #           sampling variance of muhat.
        # varpostvar:  estimated sampling variance of postvar
        #
        # 
        # Posterior interval post.range combines all uncertainties
        # in a t interval with center muhat, scale sqrt(postvar), 
        # and postvar.df degrees of freedom.
        #
        # postvar = sig2hat + B/(mn):  variance for the posterior
        #           interval. The B/(mn) term is there because of the
        #           sampling variance of muhat.
        # varpostvar:  estimated sampling variance of postvar
        #
        postvar <- sig2hat + B/(Niter * Nchain)
        varpostvar <- ((Niter - 1)^2 * varW + (1 + 1/Nchain)^2 * 
            varB + 2 * (Niter - 1) * (1 + 1/Nchain) * covWB)/Niter^2
        post.df <- (2 * postvar^2)/varpostvar
        #
        df.adj <- (post.df + 3)/(post.df + 1)
        # Estimated potential scale reduction (that would be achieved
        # by continuing simulations forever) has two components: an
        # estimate and an approx upper bound.
        #
        # confshrink = sqrt(postvar/W), 
        #     multiplied by sqrt((df+3)/(df+1)) as an adjustment for the
        #     width of the t-interval with df degrees of freedom.
        #
        # postvar/W = (n-1)/n + (1+1/m)(1/n)(B/W); we approximate
        # the sampling dist.  of (B/W) by an F distribution, with
        # degrees of freedom estimated from the approximate
        # chi-squared sampling dists for B and W.  (The F
        # approximation assumes that the sampling dists of B and W
        # are independent; if they are positively correlated, the
        # approximation is conservative.)
        #
        varlo.df <- (2 * W^2)/varW
        R2.fixed <- (Niter - 1)/Niter
        R2.random <- (1 + 1/Nchain) * (1/Niter) * (B/W)
        R2.estimate <- R2.fixed + R2.random
        R2.upper <- R2.fixed + qf((1 + confidence)/2, Nchain - 
            1, varlo.df) * R2.random
        confshrink[i, 1] <- sqrt(df.adj * R2.estimate)
        confshrink[i, 2] <- sqrt(df.adj * R2.upper)
    }
    out <- list(confshrink = confshrink)
    class(out) <- "gelman.diag"
    out
}
"print.gelman.diag" <-
function (x, digits = 3, ...) 
{
    cat("Shrink factors:\n\n")
    print.default(x$confshrink, digits = digits, ...)
    cat("\n")
}
"gelman.plot" <-
function (x, bin.width = 10, max.bins = 50, confidence = 0.95, 
    transform = FALSE, auto.layout = TRUE, ask = TRUE, col = 1:2, 
    lty = 1:2, xlab = "last iteration in chain", ylab = "shrink factor", 
    type = "l", ...) 
{
    x <- as.mcmc.list(x)
    oldpar <- NULL
    on.exit(par(oldpar))
    if (auto.layout) 
        oldpar <- par(mfrow = set.mfrow(Nchains = nchain(x), 
            Nparms = nvar(x)))
    oldpar <- c(oldpar, par(ask = ask))
    y <- gelman.preplot(x, bin.width = bin.width, max.bins = max.bins, 
        confidence = confidence)
    all.na <- apply(is.na(y$shrink[, , 1, drop = FALSE]), 2, 
        all)
    if (!any(all.na)) 
        for (j in 1:nvar(x)) {
            matplot(y$last.iter, y$shrink[, j, ], col = col, 
                lty = lty, xlab = xlab, ylab = ylab, type = type, 
                ...)
            abline(h = 1)
            ymax <- max(c(1, y$shrink[, j, ]), na.rm = TRUE)
            leg <- dimnames(y$shrink)[[3]]
            if (is.R()) {
                xmax <- max(y$last.iter)
                legend(xmax, ymax, legend = leg, lty = lty, bty = "n", 
                  col = col, xjust = 1, yjust = 1)
            }
            else {
                xmid <- (max(y$last.iter) + min(y$last.iter))/2
                legend(xmid, ymax, legend = leg, lty = lty, bty = "n", 
                  col = col)
            }
            title(main = varnames(x)[j])
        }
    return(invisible(y))
}
"gelman.preplot" <-
function (x, confidence = 0.95, transform = FALSE, bin.width = 10, 
    max.bins = 50) 
{
    x <- as.mcmc.list(x)
    if (niter(x) <= 50) 
        stop("Less than 50 iterations in chain")
    nbin <- min(floor((niter(x) - 50)/thin(x)), max.bins)
    binw <- floor((niter(x) - 50)/nbin)
    last.iter <- c(seq(from = start(x) + 50 * thin(x), by = binw * 
        thin(x), length = nbin), end(x))
    shrink <- array(dim = c(nbin + 1, nvar(x), 2))
    dimnames(shrink) <- list(last.iter, varnames(x), c("median", 
        paste(50 * (confidence + 1), "%", sep = "")))
    for (i in 1:(nbin + 1)) {
        shrink[i, , ] <- gelman.diag(window(x, end = last.iter[i]), 
            confidence = confidence, transform = transform)$confshrink
    }
    all.na <- apply(is.na(shrink[, , 1, drop = FALSE]), 2, all)
    if (any(all.na)) {
        cat("\n******* Error: *******\nCannot compute Gelman & Rubin's diagnostic for any chain \nsegments for variables", 
            varnames(x)[all.na], "\nThis indicates convergence failure ==> Run chains for more iterations\n")
    }
    return(shrink = shrink, last.iter = last.iter)
}
