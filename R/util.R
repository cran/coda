"change.tfoption" <-
function (string, option) 
{
    current.value <- coda.options(option)
    if (!is.logical(current.value)) 
        stop("Invalid option: must take logical values")
    wrd <- ifelse(current.value, " (Y/n)?\n:", " (y/N)?\n:")
    cat("\n", string, wrd, sep = "")
    ans <- readline()
    changeit <- (current.value && (ans == "N" | ans == "n")) || 
        (!current.value && (ans == "Y" | ans == "y"))
    if (changeit) {
        arg <- list(!current.value)
        names(arg) <- option
        coda.options(arg)
    }
    return()
}
"coda.options" <-
function (...) 
{
    #Set and display coda options
    #Works like options() and par(), ie
    #MTP start
    #Displays current values if no arguments are given
    #Displays current values of selected arguments if arguments are names
    #Resets values if argument is a named list
    #Like par it returns a value (rather than a list) if asked to
    #display one argument 
    #In addition
    #Resets to defaults if the "default=TRUE" argument is given.
    #TODO put in value checking. Currently only checks mode
    #MTP finish
    single <- FALSE
    if (!exists(".Coda.Options", frame = 1)) 
        .Coda.Options <<- .Coda.Options.Default
    if (nargs() == 0) {
        return(.Coda.Options)
    }
    else {
        args <- list(...)
        if (length(args) == 1) {
            if (is.list(args[[1]])) 
                args <- args[[1]]
            else if (is.null(names(args))) 
                single <- TRUE
        }
    }
    if (is.null(names(args))) {
        #Display options
        args <- unlist(args)
        value <- vector("list", length(args))
        names(value) <- args
        for (v in args) if (any(v == names(.Coda.Options))) 
            value[v] <- .Coda.Options[v]
        if (single) 
            return(value[[1]])
        else return(value)
    }
    else {
        #Set options
        oldvalue <- vector("list", length(args))
        names(oldvalue) <- names(args)
        if (any(names(args) == "default") && args$default == 
            TRUE) 
            .Coda.Options <<- .Coda.Options.Default
        for (v in names(args)) if (any(v == names(.Coda.Options))) {
            oldvalue[v] <- .Coda.Options[v]
            if (is.null(args[[v]])) 
                .Coda.Options[v] <<- list(NULL)
            else if (mode(.Coda.Options[[v]]) == mode(args[[v]])) 
                .Coda.Options[v] <<- args[v]
        }
        invisible(oldvalue)
    }
}
"multi.menu" <-
function (choices, title, header, allow.zero = TRUE) 
{
    # Select more than one value from a menu 
    # 
    if (!missing(title)) 
        cat(title, "\n\n")
    mat <- matrix(c(1:length(choices), choices), ncol = 2)
    if (!missing(header)) {
        if (length(header) == 2) 
            mat <- rbind(header, mat)
        else stop("header is wrong length")
    }
    cat(paste(format(mat[, 1]), format(mat[, 2])), sep = "\n")
    repeat {
        cat("\nEnter relevant number(s), separated by commas", 
            "Ranges such as 3:7 may be specified)", sep = "\n")
        if (allow.zero) 
            cat("(Enter 0 for none)\n")
        if (is.R()) 
            ans <- scan(what = character(), sep = ",", strip.white = TRUE, 
                nlines = 1, quiet = TRUE)
        else ans <- scan(what = character(), sep = ",", strip.white = TRUE)
        if (length(ans) > 0) {
            out <- numeric(0)
            for (i in 1:length(ans)) {
                nc <- nchar(ans[i])
                wrd <- substring(ans[i], 1:nc, 1:nc)
                colons <- wrd == ":"
                err <- any(is.na(as.numeric(wrd[!colons]))) | 
                  sum(colons) > 1 | colons[1] | colons[nc]
                if (err) {
                  cat("Error: you have specified a non-numeric value!\n")
                  break
                }
                else {
                  out <- c(out, eval(parse(text = ans[i])))
                  if (min(out) < ifelse(allow.zero, 0, 1) | max(out) > 
                    length(choices) | (any(out == 0) & length(out) > 
                    1)) {
                    err <- TRUE
                    cat("Error: you have specified variable number(s) out of range!\n")
                    break
                  }
                }
            }
            if (!err) 
                break
        }
    }
    return(out)
}
"set.mfrow" <-
function (Nchains = 1, Nparms = 1, nplots = 1, sepplot = FALSE, 
    one.page = FALSE) 
{
    #  
    # Set up dimensions of graphics window: 
    # If only density plots OR trace plots are requested, dimensions are: 
    #	1 x 1	if Nparms = 1 
    #	1 X 2 	if Nparms = 2 
    #	2 X 2 	if Nparms = 3 or 4 
    #	3 X 2 	if Nparms = 5 or 6 or 10 - 12 
    #	3 X 3 	if Nparms = 7 - 9 or >= 13 
    # If both density plots AND trace plots are requested, dimensions are: 
    #	1 x 2	if Nparms = 1 
    #	2 X 2 	if Nparms = 2 
    #	3 X 2 	if Nparms = 3, 5, 6, 10, 11, or 12 
    #	4 x 2	if Nparms otherwise 
    # If separate plots are requested for each chain, dimensions are: 
    #	1 x 2	if Nparms = 1 & Nchains = 2 
    #	2 X 2 	if Nparms = 2 & Nchains = 2 OR Nparms = 1 & Nchains = 3 or 4 
    #	3 x 2	if Nparms = 3 or >= 5 & Nchains = 2  
    #		   OR Nchains = 5 or 6 or 10 - 12 (and any Nparms) 
    #	2 x 3	if Nparms = 2 or 4 & Nchains = 3 
    #	4 x 2   if Nparms = 4 & Nchains = 2  
    #		   OR Nchains = 4 & Nparms > 1 
    #	3 x 3	if Nparms = 3 or >= 5  & Nchains = 3  
    #		   OR Nchains = 7 - 9 or >= 13 (and any Nparms) 
    # If more plots are required than will fit on one page, the 
    # browser function is used to cycle round multiple pages of plots 
    #  
    if (one.page) 
        Nparms <- 1
    if (sepplot && Nchains > 1) {
        if (nplots == 1) {
            mrows <- ifelse((Nparms == 1 && Nchains == 2), 1, 
                ifelse((Nparms == 2 && Nchains == 2) || (Nparms == 
                  1 && any(Nchains == c(3, 4))) || (any(Nparms == 
                  c(2, 4)) && Nchains == 3), 2, ifelse((Nparms == 
                  4 && Nchains == 2) || (Nchains == 4 && Nparms > 
                  1), 4, 3)))
            mcols <- ifelse((Nchains == 3 && any(Nparms == c(2, 
                4))) || ((Nparms == 3 || Nparms >= 5) && Nchains == 
                3) || any(Nchains == c(7:9)) || Nchains >= 13, 
                3, 2)
        }
        else {
            mrows <- ifelse(Nchains <= 4, Nchains, ifelse(any(Nchains == 
                c(5, 6, 10:12)), 3, 4))
            mcols <- 2
        }
    }
    else {
        if (nplots == 1) {
            mrows <- ifelse(Nparms <= 2, 1, ifelse(Nparms <= 
                4, 2, 3))
            mcols <- ifelse(Nparms <= 9, Nparms%/%mrows + ifelse(Nparms%%mrows == 
                0, 0, 1), ifelse(any(Nparms == c(10:12)), 2, 
                3))
        }
        else {
            mrows <- ifelse(Nparms <= 4, Nparms, ifelse(any(Nparms == 
                c(5, 6, 10:12)), 3, 4))
            mcols <- 2
        }
    }
    return(mfrow = c(mrows, mcols))
}
"test.bandwidth" <-
function () 
{
    # 
    # test.bandwidth -- Attempts to evaluate the bandwidth function specified 
    #                   by the user. If a single numeric value results, then 
    #                   the function is passed, else an error is returned or 
    #                   the function is dumped  
    # 
    # Author: Nicky Best 
    # 
    out <- FALSE
    y <- 1:10
    err <- coda.options("bandwidth")(y)
    if (is.numeric(err) & length(err) == 1) {
        out <- TRUE
    }
    else {
        cat("Error: this function is not appropriate for calculating kernel bandwidths!\n\n")
        out <- FALSE
    }
    out
}
"read.and.check" <-
function (message = "", what = numeric(), lower, upper, answer.in, 
    default) 
{
    #Read data from the command line and check that it satisfies 
    #certain conditions.  The function will loop until it gets 
    #and answer satisfying the conditions. This entails extensive 
    #checking of the conditions to  make sure they are consistent 
    #so we don't end up in an infinite loop. 
    have.lower <- !missing(lower)
    have.upper <- !missing(upper)
    have.ans.in <- !missing(answer.in)
    have.default <- !missing(default)
    if (have.lower | have.upper) {
        if (!is.numeric(what)) 
            stop("Can't have upper or lower limits with non numeric input")
        if (have.lower && !is.numeric(lower)) 
            stop("lower limit not numeric")
        if (have.upper && !is.numeric(upper)) 
            stop("upper limit not numeric")
        if ((have.upper & have.lower) && upper < lower) 
            stop("lower limit greater than upper limit")
    }
    if (have.ans.in) {
        if (mode(answer.in) != mode(what)) 
            stop("inconsistent values of what and answer.in")
        if (have.lower) 
            answer.in <- answer.in[answer.in >= lower]
        if (have.upper) 
            answer.in <- answer.in[answer.in <= upper]
        if (length(answer.in) == 0) 
            stop("No possible response matches conditions")
    }
    if (have.default) {
        if (mode(default) != mode(what)) 
            stop("inconsistent values of what and default")
        if (have.lower && default < lower) 
            stop("default value below lower limit")
        if (have.upper && default > upper) 
            stop("default value above upper limit")
        if (have.ans.in && !any(answer.in == default)) 
            stop("default value does not satisfy conditions")
    }
    err <- TRUE
    while (err) {
        if (nchar(message) > 0) {
            cat("\n", message, "\n", sep = "")
            if (have.default) 
                cat("(Default = ", default, ")\n", sep = "")
        }
        repeat {
            cat("1:")
            ans <- readline()
            if (length(ans) == 1 && nchar(ans) > 0) 
                break
            else if (have.default) {
                ans <- default
                break
            }
        }
        if (is.numeric(what)) {
            err1 <- TRUE
            ans <- as.numeric(ans)
            message <- "You must enter a number"
            if (is.na(ans)) 
                NULL
            else if ((have.lower & have.upper) && (ans < lower | 
                ans > upper)) 
                message <- paste(message, "between", lower, "and", 
                  upper)
            else if (have.lower && ans < lower) 
                message <- paste(message, ">=", lower)
            else if (have.upper && ans > upper) 
                message <- paste(message, "<=", upper)
            else err1 <- FALSE
        }
        else err1 <- FALSE
        if (have.ans.in) {
            if (!is.na(ans) && !any(ans == answer.in)) {
                message <- paste("You must enter one of the following:", 
                  paste(answer.in, collapse = ","))
                err2 <- TRUE
            }
            else err2 <- FALSE
        }
        else err2 <- FALSE
        err <- err1 | err2
    }
    return(ans)
}
"make.coda.package" <-
function () 
{
    if (search()[2] != "package:coda") 
        stop("CODA package must be loaded in position 2")
    mcmc.obs <- c("[.mcmc", "as.mcmc", "as.mcmc.default", "as.ts.mcmc", 
        "start.mcmc", "end.mcmc", "frequency.mcmc", "thin.mcmc", 
        "is.mcmc", "mcmc", "print.mcmc", "plot.mcmc", "summary.mcmc", 
        "print.summary.mcmc", "as.matrix.mcmc", "time.mcmc", 
        "window.mcmc", "mcpar", "upgrade.mcmc")
    thin.obs <- c("thin")
    mcmclist.obs <- c("[.mcmc.list", "mcmc.list", "start.mcmc.list", 
        "end.mcmc.list", "thin.mcmc.list", "is.mcmc.list", "plot.mcmc.list", 
        "summary.mcmc.list", "as.matrix.mcmc.list", "as.mcmc.mcmc.list", 
        "time.mcmc.list", "window.mcmc.list", "as.mcmc.list", 
        "as.mcmc.list.default")
    mcextractor.obs <- c("chanames", "chanames<-", "varnames", 
        "varnames<-", "nchain", "nvar", "niter")
    gelman.obs <- c("gelman.diag", "print.gelman.diag", "gelman.plot", 
        "gelman.preplot")
    geweke.obs <- c("geweke.diag", "geweke.plot", "geweke.power", 
        "print.geweke.diag", "geweke.nse")
    heidel.obs <- c("heidel.diag", "print.heidel.diag")
    raftery.obs <- c("raftery.diag", "print.raftery.diag")
    ts.obs <- c("acf", "spec.pgram")
    util.obs <- c("change.tfoption", "coda.options", "multi.menu", 
        "set.mfrow", "test.bandwidth", "read.and.check", "make.coda.package", 
        ".Coda.Options.Default", "convolve.open")
    output.obs <- c("autocorr", "autocorr.plot", "crosscorr", 
        "crosscorr.plot", "densplot", "read.bugs", "traceplot")
    codamenu.obs <- c("coda.credits", "codamenu", "codamenu.anal", 
        "codamenu.diags", "codamenu.diags.autocorr", "codamenu.diags.crosscorr", 
        "codamenu.diags.heidel", "codamenu.diags.raftery", "codamenu.main", 
        "codamenu.diags.gelman", "codamenu.diags.geweke", "codamenu.options", 
        "codamenu.options.data", "codamenu.options.diag", "codamenu.options.gelman", 
        "codamenu.options.geweke.bin", "codamenu.options.geweke.win", 
        "codamenu.options.heidel", "codamenu.options.plot", "codamenu.options.plot.kernel", 
        "codamenu.options.plot.ps", "codamenu.options.raftery", 
        "codamenu.options.stats", "print.coda.options", "read.bugs.interactive", 
        "coda.objects", "tidy.up", "codamenu.ps", "codamenu.output.header", 
        "codamenu.devices")
    ob.list <- c("mcmc.obs", "thin.obs", "mcmclist.obs", "mcextractor.obs", 
        "gelman.obs", "geweke.obs", "heidel.obs", "ts.obs", "util.obs", 
        "output.obs", "raftery.obs", "codamenu.obs")
    all.obs <- character(0)
    for (i in 1:length(ob.list)) {
        all.obs <- c(all.obs, get(ob.list[i]))
    }
    if (length(unique(all.obs)) < length(all.obs)) {
        dup.obs <- paste(all.obs[duplicated(all.obs)], collapse = "\n")
        stop(paste("Duplicated objects in package list:\n", dup.obs))
    }
    lib.obs <- unique(c(objects(pos = 1, all.names = TRUE), objects(pos = 2, 
        all.names = TRUE)))
    x <- c(lib.obs, all.obs)
    x <- x[!duplicated(x)]
    x <- x[-(1:length(lib.obs))]
    if (length(x) > 0) {
        x <- paste(x, collapse = "\n")
        stop(paste("package objects not found:\n", x, sep = "\n"))
    }
    y <- c(all.obs, lib.obs)
    y <- y[!duplicated(y)]
    y <- y[-(1:length(all.obs))]
    if (length(y) > 0) {
        y <- paste(y, collapse = "\n")
        stop(paste("objects found not in package list:\n", y, 
            sep = "\n"))
    }
    for (i in 1:length(ob.list)) {
        root <- substring(ob.list[i], 1, nchar(ob.list[i]) - 
            4)
        filename <- paste(root, ".R", sep = "")
        dump(get(ob.list[i]), filename)
    }
    cat("DONE\n")
    invisible()
}
".Coda.Options.Default" <-
structure(list(trace = TRUE, densplot = TRUE, lowess = FALSE, 
    combine.plots = TRUE, onepage = FALSE, bandwidth = function (x) 
    {
        x <- x[!is.na(x)]
        1.06 * min(sd(x), IQR(x)/1.34) * length(x)^-0.2
    }, pos.parms = 0, pos.parmsmat = 0, prop.parms = 0, prop.parmsmat = 0, 
    batch.size = 25, digits = 3, quantiles = c(0.025, 0.25, 0.5, 
    0.75, 0.975), frac1 = 0.1, frac2 = 0.5, q = 0.025, r = 0.005, 
    s = 0.95, combine.stats = FALSE, combine.corr = FALSE, ps.plot = 1, 
    halfwidth = 0.1, user.layout = FALSE, mrows = 1, mcols = 1, 
    gr.bin = 10, geweke.bin = 10, gr.max = 50, geweke.max = 50, 
    data.saved = FALSE), .Names = c("trace", "densplot", "lowess", 
"combine.plots", "onepage", "bandwidth", "pos.parms", "pos.parmsmat", 
"prop.parms", "prop.parmsmat", "batch.size", "digits", "quantiles", 
"frac1", "frac2", "q", "r", "s", "combine.stats", "combine.corr", 
"ps.plot", "halfwidth", "user.layout", "mrows", "mcols", "gr.bin", 
"geweke.bin", "gr.max", "geweke.max", "data.saved"))
"convolve.open" <-
function (x, y, conj = TRUE) 
{
    nx <- length(x)
    ny <- length(y)
    n <- nx + ny - 1
    x <- c(rep(0, ny - 1), x)
    y <- c(y, rep(0, nx - 1))
    Re(fft(fft(x) * (if (conj) Conj(fft(y)) else fft(y)), inv = TRUE))/n
}
