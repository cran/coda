"heidel.diag" <-
function (x, eps = 0.1) 
{
    if (is.mcmc.list(x)) 
        return(lapply(x, heidel.diag, eps))
    x <- as.mcmc(x)
    HW.mat0 <- matrix(0, ncol = 7, nrow = nvar(x))
    dimnames(HW.mat0) <- list(varnames(x), c("stest", "keep", 
        "discard", "C-vonM", "htest", "mean", "halfwidth"))
    HW.mat <- HW.mat0
    for (j in 1:nvar(x)) {
        Y <- as.matrix(x)[, j, drop = TRUE]
        n1 <- length(Y)
        n <- length(Y)
        S0 <- geweke.power(Y[(n/2 + 1):n])
        passed <- FALSE
        while (n >= n1/2 && !passed) {
            T1 <- cumsum(Y)
            ybar <- mean(Y)
            B <- T1 - ybar * (1:n)
            Bsq <- (B * B)/(n * S0)
            I <- (2 * sum(Bsq[seq(2, n - 2, by = 2)]) + 4 * sum(Bsq[seq(1, 
                n - 1, by = 2)]) + Bsq[n])/(3 * n)
            passed <- !is.na(I) & I < 0.46
            if (!passed) {
                Y <- Y[(n1/10 + 1):n]
                n <- length(Y)
            }
        }
        S0ci <- geweke.power(Y)
        halfwidth <- 1.96 * sqrt(S0ci/n)
        passed2 <- (!is.na(halfwidth) & abs(halfwidth/ybar) <= 
            eps)
        if (is.na(I) | is.na(halfwidth) | !passed) {
            n <- NA
            nd <- NA
            passed2 <- NA
            ybar <- NA
            halfwidth <- NA
        }
        else {
            nd <- length(as.matrix(x)[, j, drop = TRUE]) - n
        }
        HW.mat[j, ] <- c(passed, n * thin(x), nd * thin(x), I, 
            passed2, ybar, halfwidth)
    }
    class(HW.mat) <- "heidel.diag"
    return(HW.mat)
}
"print.heidel.diag" <-
function (x, digits = 3, ...) 
{
    HW.title <- matrix(c("Stationarity", "test", "# of iters.", 
        "to keep", "# of iters.", "to discard", "C-vonM", "stat.", 
        "Halfwidth", "test", "Mean", "", "Halfwidth", ""), nrow = 2)
    y <- matrix("", nrow = nrow(x), ncol = 7)
    for (j in 1:ncol(y)) {
        y[, j] <- format(x[, j], digits = digits)
    }
    y[, c(1, 5)] <- ifelse(x[, c(1, 5)], "passed", "failed")
    y <- rbind(HW.title, y)
    vnames <- if (is.null(rownames(x))) 
        paste("[,", 1:nrow(x), "]", sep = "")
    else rownames(x)
    dimnames(y) <- list(c("", "", vnames), rep("", 7))
    print.default(y[, 1:4], quote = FALSE, ...)
    print.default(y[, 5:7], quote = FALSE, ...)
    invisible(x)
}
