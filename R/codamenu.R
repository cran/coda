"coda.credits" <-
function (file = "") 
{
    credits <- c("  _______________________________________________________________\n", 
        "|                                                               |\n", 
        "|                 Welcome to CODA for R!                        |\n", 
        "|  Convergence Diagnostics and Output Analysis for BUGS output  |\n", 
        "|_______________________________________________________________|\n", 
        "|                                                               |\n", 
        "|                                                               |\n", 
        "|  Authors : Martyn Plummer, Nicky Best, Kate Cowles,           |\n", 
        "|            Karen Vines                                        |\n", 
        "|                                                               |\n", 
        "|  R-CODA version 0.4 Copyright (c) 1995-9 MRC Biostatistics    |\n", 
        "|  Unit and others (see AUTHORS file)                           |\n", 
        "|  This is free software and commes with ABSOLUTELY NO WARRANTY |\n", 
        "|_______________________________________________________________|\n", 
        "\n")
    cat(credits, file = file)
}
"codamenu" <-
function () 
{
    on.exit(tidy.up())
    coda.options(.Coda.Options.Default)
    file.menu <- c("Begin a new CODA session using BUGS output files", 
        "Begin a new CODA session using data saved from a previous CODA session", 
        "Quit")
    coda.credits()
    pick <- menu(file.menu, title = "CODA startup menu")
    if (pick == 0 || pick == 3) 
        return(invisible())
    else if (pick == 1) {
        coda.dat <<- read.bugs.interactive()
        coda.options(data.saved = FALSE)
        if (is.null(coda.dat)) {
            rm(coda.dat, pos = 1)
            return(invisible())
        }
    }
    else if (pick == 2) {
        msg <- "\nEnter name of object saved from a previous CODA session:\n"
        repeat {
            cat(msg, "\n")
            outname <- readline()
            if (length(outname) == 0) 
                msg <- "You must enter something"
            else if (outname == "0") {
                return(invisible())
            }
            else if (!exists(outname)) 
                msg <- "Can't find this object"
            else if (!is.mcmc.list(eval(parse(text = outname)))) 
                msg <- "Not an mcmc list object"
            else {
                if (is.R()) 
                  coda.dat <<- .Alias(eval(parse(text = outname)))
                else coda.dat <<- eval(parse(text = outname))
                coda.options(data.saved = TRUE)
                break
            }
            msg <- paste(msg, "Please re-enter another name or type 0 to quit:\n", 
                sep = "\n")
        }
    }
    else stop("Invalid option")
    if (is.null(chanames(coda.dat))) 
        chanames(coda.dat) <<- chanames(coda.dat, allow.null = FALSE)
    if (is.null(varnames(coda.dat))) 
        varnames(coda.dat) <<- varnames(coda.dat, allow.null = FALSE)
    if (is.R()) 
        work.dat <<- .Alias(coda.dat)
    else work.dat <<- coda.dat
    codamenu.devices()
    current.menu <- "codamenu.main"
    repeat {
        next.menu <- do.call(current.menu, vector("list", 0))
        if (next.menu == "quit") {
            cat("Are you sure you want to quit? (y/N)\n")
            if (any(readline() == c("y", "Y"))) 
                break
        }
        else current.menu <- next.menu
    }
    invisible()
}
"codamenu.anal" <-
function () 
{
    # 
    # outanal -- Output analysis menu for CODA system 
    # 
    # Author: Kate Cowles 
    # 
    next.menu <- "codamenu.anal"
    choices <- c("Plots", "Statistics", "List/Change Options", 
        "Return to Main Menu")
    next.menu.list <- c("plots", "summary", "codamenu.options", 
        "codamenu.main")
    cat("\n")
    pick <- menu(choices, title = "CODA Output Analysis menu")
    if (pick == 0) 
        next.menu <- "quit"
    else if (next.menu.list[pick] == "summary") {
        if (coda.options("combine.stats")) {
            print(summary(work.dat, quantiles = coda.options("quantiles"), 
                digits = coda.options("digits")))
        }
        else for (i in 1:nchain(work.dat)) {
            cat(chanames(work.dat, allow.null = FALSE)[i], "\n")
            print(summary(work.dat[[i]], quantiles = coda.options("quantiles"), 
                digits = coda.options("digits")))
        }
    }
    else if (next.menu.list[pick] == "plots") {
        auto.layout <- !coda.options("user.layout") && !coda.options("onepage")
        if (coda.options("onepage")) {
            opar <- par(mfrow = c(1, 1))
            on.exit(par(opar))
        }
        ask <- TRUE
        repeat {
            if (coda.options("combine.plots")) 
                plot(work.dat, trace = coda.options("trace"), 
                  density = coda.options("densplot"), smooth = coda.options("lowess"), 
                  auto.layout = auto.layout, bwf = coda.options("bandwidth"), 
                  combine.chains = !coda.options("combine.plots"), 
                  ask = ask)
            else for (i in 1:nchain(work.dat)) {
                plot(work.dat[[i]], trace = coda.options("trace"), 
                  density = coda.options("densplot"), smooth = coda.options("lowess"), 
                  auto.layout = auto.layout, bwf = coda.options("bandwidth"), 
                  combine.chains = coda.options("combine.plots"), 
                  ask = ask)
            }
            codamenu.ps()
            if (names(dev.cur()) == "postscript") 
                ask <- FALSE
            else break
        }
    }
    else next.menu <- next.menu.list[pick]
    return(next.menu)
}
"codamenu.diags" <-
function () 
{
    #
    # diags -- Diagnostics menu for CODA system
    #
    # Author: Kate Cowles
    #
    next.menu <- "diags"
    while (next.menu == "diags") {
        choices <- c("Geweke", "Gelman and Rubin", "Raftery and Lewis", 
            "Heidelberger and Welch", "Autocorrelations", "Cross-Correlations", 
            "List/Change Options", "Return to Main Menu")
        next.menu.list <- c("codamenu.diags.geweke", "codamenu.diags.gelman", 
            "codamenu.diags.raftery", "codamenu.diags.heidel", 
            "codamenu.diags.autocorr", "codamenu.diags.crosscorr", 
            "codamenu.options", "codamenu.main")
        pick <- menu(choices, title = "CODA Diagnostics Menu")
        if (pick == 0) 
            return("quit")
        else next.menu <- next.menu.list[pick]
    }
    return(next.menu)
}
"codamenu.diags.autocorr" <-
function () 
{
    #
    # autocorr -- Calculate Autocorrelations for CODA system
    #
    # Author: Kate Cowles
    # Modified by: Nicky Best
    #
    next.menu <- "codamenu.diags"
    codamenu.output.header("AUTOCORRELATIONS WITHIN EACH CHAIN:")
    print(autocorr(work.dat), digits = coda.options("digits"))
    choices <- c("Plot autocorrelations", "Return to Diagnostics Menu")
    pick <- menu(choices, title = "Autocorrelation Plots Menu")
    if (pick == 0) 
        next.menu <- "quit"
    else if (pick == 1) {
        ask <- TRUE
        repeat {
            autocorr.plot(work.dat, auto.layout = !coda.options("user.layout"), 
                ask = ask)
            codamenu.ps()
            if (names(dev.cur()) == "postscript") 
                ask <- FALSE
            else break
        }
    }
    return(next.menu)
}
"codamenu.diags.crosscorr" <-
function () 
{
    next.menu <- "codamenu.diags.crosscorr"
    crosscorr.out <- if (coda.options("combine.corr")) {
        crosscorr(work.dat)
    }
    else lapply(work.dat, crosscorr)
    if (coda.options("combine.corr") & nchain(work.dat) > 1) 
        cat("Pooling over chains:", chanames(work.dat, allow.null = FALSE), 
            sep = "\n", collapse = "\n")
    print(crosscorr.out, digits = coda.options("digits"))
    cat("\n")
    choices <- c("Change options", "Plot Cross Correlations", 
        "Return to Diagnostics Menu")
    pick <- menu(choices, title = "Cross correlation plots menu")
    if (pick == 0) 
        next.menu <- "quit"
    else switch(pick, change.tfoption("Combine chains", "combine.corr"), 
        {
            repeat {
                if (coda.options("combine.corr")) 
                  crosscorr.plot(work.dat)
                else {
                  opar <- par(ask = TRUE)
                  lapply(work.dat, crosscorr.plot)
                  par(opar)
                }
                codamenu.ps()
                if (names(dev.cur()) != "postscript") 
                  break
            }
        }, next.menu <- "codamenu.diags")
    return(next.menu)
}
"codamenu.diags.heidel" <-
function () 
{
    this.menu <- "codamenu.diags.heidel"
    next.menu <- "codamenu.diags"
    title <- "HEIDELBERGER AND WELCH STATIONARITY AND INTERVAL HALFWIDTH TESTS"
    codamenu.output.header(title)
    cat("Precision of halfwidth test =", coda.options("halfwidth"), 
        "\n\n")
    heidel.out <- heidel.diag(work.dat, eps = coda.options("halfwidth"))
    print(heidel.out, digits = coda.options("digits"))
    choices <- c("Change precision", "Return to diagnostics menu")
    pick <- menu(choices)
    if (pick == 0) 
        next.menu <- "quit"
    else if (pick == 1) 
        next.menu <- codamenu.options.heidel(this.menu)
    return(next.menu)
}
"codamenu.diags.raftery" <-
function () 
{
    next.menu <- this.menu <- "codamenu.diags.raftery"
    codamenu.output.header("RAFTERY AND LEWIS CONVERGENCE DIAGNOSTIC")
    print(raftery.diag(work.dat, q = coda.options("q"), r = coda.options("r"), 
        s = coda.options("s")), digits = coda.options("digits"))
    choices <- c("Change parameters", "Return to diagnostics menu")
    pick <- menu(choices)
    next.menu <- if (pick == 0) 
        "quit"
    else if (pick == 1) {
        codamenu.options.raftery(this.menu)
    }
    else "codamenu.diags"
    return(next.menu)
}
"codamenu.main" <-
function () 
{
    # 
    # codamenu.main -- CODA main menu 
    # 
    # Author: Kate Cowles 
    # Modified by: Nicky Best 
    # and by: Martyn Plummer 
    # 
    choices <- c("Output Analysis", "Diagnostics", "List/Change Options", 
        "Quit")
    next.menu.list <- c("codamenu.anal", "codamenu.diags", "codamenu.options", 
        "quit")
    pick <- menu(choices, title = "CODA Main Menu")
    if (pick == 0) 
        next.menu <- "quit"
    else next.menu <- next.menu.list[pick]
    return(next.menu)
}
"codamenu.diags.gelman" <-
function (tol = 1e-08) 
{
    next.menu <- this.menu <- "codamenu.diags.gelman"
    if (nchain(work.dat) == 1) {
        cat("\nError: you need more than one chain.\n\n")
        return(next.menu = "codamenu.diags")
    }
    else if (niter(work.dat) <= 50) {
        cat("\nError: you need > 50 iterations in the working data\n")
        return(next.menu = "codamenu.diags")
    }
    z <- window(work.dat, start = niter(work.dat)/2)
    for (i in 2:nchain(z)) {
        for (j in 1:(i - 1)) {
            if (any(apply(as.matrix(z[[i]] - z[[j]]), 2, var)) < 
                tol) {
                cat("\nError: 2nd halves of", chanames(z, allow.null = FALSE)[c(j, 
                  i)], "are identical for at least one variable\n")
                return(next.menu = "codamenu.diags")
            }
        }
    }
    codamenu.output.header("GELMAN AND RUBIN DIAGNOSTIC")
    print(gelman.diag(work.dat, transform = TRUE), digits = coda.options("digits"))
    choices <- c("Shrink Factor Plots", "Change bin size for shrink plot", 
        "Return to Diagnostics Menu")
    action.list <- c("ShrinkPlot", "ChangeBin", "Return")
    while (next.menu == "codamenu.diags.gelman") {
        pick <- menu(choices, title = "Gelman & Rubin menu")
        if (pick == 0) 
            next.menu <- "quit"
        else switch(action.list[pick], ShrinkPlot = {
            ask <- TRUE
            repeat {
                gelman.plot(work.dat, max.bins = coda.options("gr.max"), 
                  bin.width = coda.options("gr.bin"), auto.layout = !coda.options("user.layout"), 
                  ask = ask)
                codamenu.ps()
                if (names(dev.cur()) == "postscript") 
                  ask <- FALSE
                else break
            }
        }, ChangeBin = {
            codamenu.options.gelman(NULL)
        }, Return = {
            next.menu <- "codamenu.diags"
        })
    }
    return(next.menu)
}
"codamenu.diags.geweke" <-
function () 
{
    next.menu <- "codamenu.diags.geweke"
    codamenu.output.header("GEWEKE CONVERGENCE DIAGNOSTIC (Z-score)")
    geweke.out <- geweke.diag(work.dat, frac1 = coda.options("frac1"), 
        frac2 = coda.options("frac2"))
    print(geweke.out, digits = coda.options("digits"))
    choices <- c("Change window size", "Plot Z-scores", "Change bin size for plot", 
        "Return to Diagnostics Menu")
    action.list <- c("ChangeWindow", "Plot", "ChangeBin", "Return")
    while (next.menu == "codamenu.diags.geweke") {
        pick <- menu(choices, title = "Geweke plots menu")
        if (pick == 0) 
            return("quit")
        switch(action.list[pick], ChangeWindow = {
            codamenu.options.geweke.win(NULL)
            geweke.out <- geweke.diag(work.dat, frac1 = coda.options("frac1"), 
                frac2 = coda.options("frac2"))
            print(geweke.out, digits = coda.options("digits"))
        }, Plot = {
            ask <- TRUE
            repeat {
                geweke.plot(work.dat, frac1 = coda.options("frac1"), 
                  frac2 = coda.options("frac2"), max.bins = coda.options("geweke.max"), 
                  bin.width = coda.options("geweke.bin"), auto.layout = !coda.options("user.layout"), 
                  ask = ask)
                codamenu.ps()
                if (names(dev.cur()) == "postscript") 
                  ask <- FALSE
                else break
            }
        }, ChangeBin = {
            codamenu.options.geweke.bin(NULL)
        }, Return = {
            next.menu <- "codamenu.diags"
        })
    }
    return(next.menu)
}
"codamenu.options" <-
function () 
{
    #
    # codamenu.options-- Main options menu for CODA system
    #
    # Author: Nicky Best
    #
    next.menu <- "codamenu.options"
    choices <- c("List current options", "Data Options", "Plot Options", 
        "Summary Statistics Options", "Diagnostics Options", 
        "Output Analysis", "Diagnostics", "Main Menu")
    action.list <- c("ListOptions", "codamenu.options.data", 
        "codamenu.options.plot", "codamenu.options.stats", "codamenu.options.diag", 
        "codamenu.anal", "codamenu.diags", "codamenu.main")
    pick <- menu(choices, title = "CODA main options menu")
    if (pick == 0) 
        return("quit")
    if (action.list[pick] == "ListOptions") {
        print.coda.options(data = TRUE, stats = TRUE, plots = TRUE, 
            diags = TRUE)
        next.menu <- "codamenu.options"
    }
    else next.menu <- action.list[pick]
    return(next.menu)
}
"codamenu.options.data" <-
function () 
{
    next.menu <- "codamenu.options.data"
    #
    work.vars <- varnames(work.dat)
    work.chains <- chanames(work.dat)
    work.start <- start(work.dat)
    work.end <- end(work.dat)
    work.thin <- thin(work.dat)
    #
    choices <- c("List current data options", "Select variables for analysis", 
        "Select chains for analysis", "Select iterations for analysis", 
        "Select thinning interval", "Return to main options menu")
    action.list <- c("ListDataOptions", "SelectVars", "SelectChains", 
        "SelectIters", "SelectThinInterval", "MainOptionsMenu")
    pick <- menu(choices, title = "CODA data options menu")
    if (pick == 0) 
        return("quit")
    switch(action.list[pick], ListDataOptions = {
        print.coda.options(data = TRUE)
    }, SelectVars = {
        work.vars <- multi.menu(varnames(coda.dat, allow.null = FALSE), 
            "Select variables for analysis", c("VARIABLE NUMBER", 
                "VARIABLE NAME"), allow.zero = FALSE)
    }, SelectChains = {
        work.chains <- multi.menu(chanames(coda.dat, allow.null = FALSE), 
            "Select chains for analysis:", c("CHAIN NUMBER", 
                "CHAIN NAME"), allow.zero = FALSE)
    }, SelectIters = {
        cat("\nIterations available = ", start(coda.dat), ":", 
            end(coda.dat), "\n", sep = "")
        work.start <- read.and.check("Enter iteration you wish to start at", 
            lower = start(coda.dat), upper = end(coda.dat), default = start(work.dat))
        work.end <- read.and.check("Enter iteration you wish to end at", 
            lower = work.start, upper = end(coda.dat), default = end(work.dat))
    }, SelectThinInterval = {
        cat("\nThinning interval of full data = ", thin(coda.dat), 
            "\n", sep = "")
        work.thin <- read.and.check("Enter thinning interval:", 
            lower = thin(coda.dat), default = thin(work.dat))
    }, MainOptionsMenu = {
        next.menu <- "codamenu.options"
    })
    if (action.list[pick] != "ListDataOptions" && action.list[pick] != 
        "MainOptionsMenu") {
        cat("Recreating working data...\n")
        wd <- window(coda.dat[, work.vars, drop = FALSE], start = work.start, 
            end = work.end, thin = work.thin)
        work.dat <<- wd[work.chains, drop = FALSE]
    }
    return(next.menu)
}
"codamenu.options.diag" <-
function () 
{
    next.menu <- this.menu <- "codamenu.options.diag"
    choices <- c("Display current diagnostic options", "Window sizes for Geweke's diagnostic", 
        "Bin size for plotting Geweke's diagnostic", "Bin size for plotting Gelman & Rubin's diagnostic", 
        "Parameters for Raftery & Lewis' diagnostic", "Halfwidth precision for Heidelberger & Welch's diagnostic", 
        "Combine chains to calculate correlation matrix", "Return to main options menu")
    pick <- menu(choices, title = "CODA diagnostics options menu")
    if (pick == 0) 
        return("quit")
    switch(pick, print.coda.options(diags = TRUE), next.menu <- codamenu.options.geweke.win(this.menu), 
        next.menu <- codamenu.options.geweke.bin(this.menu), 
        next.menu <- codamenu.options.gelman(this.menu), next.menu <- codamenu.options.raftery(this.menu), 
        next.menu <- codamenu.options.heidel(this.menu), {
            change.tfoption("Do you want to combine all chains to calculate correlation matrix", 
                "combine.corr")
        }, next.menu <- "codamenu.options")
    return(next.menu)
}
"codamenu.options.gelman" <-
function (last.menu) 
{
    choices <- c("Default: bin width = 10; maximum number of bins = 50", 
        "User-specified bin width", "User-specified total number of bins")
    pick <- menu(choices, title = "Options for defining bin size to plot Gelman-Rubin-Brooks diagnostic")
    if (pick == 0) 
        return("quit")
    switch(pick, {
        coda.options(gr.max = 50)
        coda.options(gr.bin = 10)
    }, {
        coda.options(gr.max = Inf)
        default <- if (coda.options("gr.bin") == 0) 
            10
        else coda.options("gr.bin")
        msg <- "Enter required bin width:"
        coda.options(gr.bin = read.and.check(msg, lower = 1, 
            upper = niter(work.dat) - 50, default = default))
    }, {
        coda.options(gr.bin = 0)
        default <- if (is.infinite(coda.options("gr.max"))) 
            50
        else coda.options("gr.max")
        msg <- "Enter total number of bins required:"
        coda.options(gr.max = read.and.check(msg, lower = 1, 
            upper = niter(work.dat) - 50, default = default))
    })
    return(last.menu)
}
"codamenu.options.geweke.bin" <-
function (last.menu) 
{
    choices <- c("Default: bin width = 10; maximum number of bins = 50", 
        "User-specified bin width", "User-specified total number of bins")
    pick <- menu(choices, title = "Options for defining bin size to plot Geweke's diagnostic")
    if (pick == 0) 
        return("quit")
    switch(pick, {
        coda.options(geweke.max = 50)
        coda.options(geweke.bin = 10)
    }, {
        coda.options(geweke.max = Inf)
        default <- if (coda.options("geweke.bin") == 0) 
            10
        else coda.options("geweke.bin")
        default <- min(default, niter(work.dat) - 50)
        msg <- "Enter required bin width:"
        coda.options(geweke.bin = read.and.check(msg, lower = 1, 
            upper = niter(work.dat) - 50, default = default))
    }, {
        coda.options(geweke.bin = 0)
        default <- if (is.infinite(coda.options("geweke.max"))) 
            min(50, niter(work.dat) - 50)
        else coda.options("geweke.max")
        default <- min(default, niter(work.dat) - 50)
        msg <- "Enter total number of bins required:"
        coda.options(geweke.max = read.and.check(msg, lower = 1, 
            upper = niter(work.dat) - 50, default = default))
    })
    return(last.menu)
}
"codamenu.options.geweke.win" <-
function (last.menu) 
{
    msg1 <- "Enter fraction of chain to include in 1st window of \nGeweke's diagnostic:"
    msg2 <- "Enter fraction of chain to include in 2nd window of \nGeweke's diagnostic:"
    ans1 <- ans2 <- 1
    while (ans1 + ans2 >= 1) {
        ans1 <- read.and.check(msg1, lower = 0, upper = 1, default = coda.options("frac1"))
        ans2 <- read.and.check(msg2, lower = 0, upper = 1, default = coda.options("frac2"))
        # Check that sum of fractions doesn't exceed 1.0
        msg1 <- if (ans1 + ans2 == 1) 
            "Error: Sum of fractions in 1st and 2nd windows equals 1.0\nYou must leave a gap in the chain between the 2 windows\nPlease re-enter fraction of chain to include in 1st window:"
        else "Error: Sum of fractions in 1st and 2nd windows exceeds 1.0\nPlease re-enter fraction of chain to include in 1st window:"
        msg2 <- "Now re-enter fraction of chain to include in 2nd window:"
    }
    coda.options(frac1 = ans1, frac2 = ans2)
    return(last.menu)
}
"codamenu.options.heidel" <-
function (last.menu) 
{
    coda.options(halfwidth = read.and.check("Enter precision for halfwidth test", 
        default = coda.options("halfwidth")))
    return(last.menu)
}
"codamenu.options.plot" <-
function () 
{
    next.menu <- "codamenu.options.plot"
    choices <- c("Show current plotting options", "Plot trace of samples", 
        "Plot kernel density estimate", "Add smooth line through trace plot", 
        "Combine chains", "Single plot per page", "Specify page layout for plots", 
        "Select bandwidth function for kernel smoothing", "Return to main options menu")
    pick <- menu(choices, title = "CODA plotting options menu")
    if (pick == 0) 
        return("quit")
    switch(pick, print.coda.options(plots = TRUE), change.tfoption(choices[2], 
        "trace"), change.tfoption(choices[3], "densplot"), change.tfoption(choices[4], 
        "lowess"), change.tfoption(choices[5], "combine.plots"), 
        change.tfoption(choices[6], "onepage"), {
            change.tfoption("Do you want to specify your own page layout for the plots", 
                "user.layout")
            if (coda.options("user.layout")) {
                mrows <- read.and.check("Enter number of rows of plots per page (maximum=7)", 
                  lower = 1, upper = 7)
                mcols <- read.and.check("Enter number of columns of plots per page (maximum=8)", 
                  lower = 1, upper = 8)
                coda.options(mrows = mrows)
                coda.options(mcols = mcols)
                par(mfrow = c(mrows, mcols))
            }
        }, {
            next.menu <- "codamenu.options.plot.kernel"
        }, NULL)
    if (pick == length(choices)) 
        next.menu <- "codamenu.options"
    return(next.menu)
}
"codamenu.options.plot.kernel" <-
function () 
{
    if (!coda.options("densplot")) {
        cat("\nNo density plots requested - this option is irrelevant\n")
    }
    else {
        kernel.menu <- c("Smooth (0.25 * sample range)", "Coarse (Silverman 1986 eqn. 3.28 & 3.30)", 
            "User-defined function", "Return to Plotting Options Menu")
        pick1 <- menu(kernel.menu, title = "Select kernel bandwidth function")
        if (pick1 == 0) 
            return("quit")
        switch(pick1, {
            bw <- function(y) {
                (max(y) - min(y))/4
            }
            coda.options(bandwidth = bw)
        }, {
            bw <- function(x) {
                1.06 * min(sd(x), IQR(x)/1.34) * length(x)^-0.2
            }
            coda.options(bandwidth = bw)
        }, {
            if (is.R()) {
                cat("DANGER! DANGER WILL ROBINSON!\n")
                cat("If you make a mistake here you will crash\n")
                cat("the codamenu session. Sorry, but there is\n")
                cat("no way round this at the moment (R 0.64.0)\n")
                cat("Do you want to try? (y/N)\n")
                ans <- readline()
                if (ans != "y" && ans != "Y") 
                  return("codamenu.options.plot")
            }
            else restart()
            func.OK <- FALSE
            while (!func.OK) {
                cat("Enter bandwidth as a function of y, the sampled values, e.g. \n(max(y) - min(y)) / 4\n")
                ans <- scan(what = character())
                if (length(ans) > 0) {
                  bw <- "function(y){"
                  for (i in 1:length(ans)) {
                    bw <- paste(bw, ans[i], sep = "")
                  }
                  #
                  bw <- paste(bw, "}", sep = "")
                  bw <- eval(parse(text = bw))
                  # Carry out simple test to check whether the
                  # function entered makes sense
                  #
                  func.OK <- test.bandwidth()
                }
            }
            coda.options(bandwidth = bw)
        }, NULL)
    }
    return("codamenu.options.plot")
}
"codamenu.options.plot.ps" <-
function () 
{
    choices <- c("Portrait", "Landscape")
    pick <- menu(choices, "Select options for saving plots to PostScript files")
    if (pick == 0) 
        return("quit")
    else coda.options(ps.orientation = c("portrait", "landscape")[pick])
    if (.Device == "X11") 
        x11(orientation = coda.options("ps.orientation"))
    else if (.Device == "Win32") 
        windows(orientation = coda.options("ps.orientation"))
    return("codamenu.options.plot")
}
"codamenu.options.raftery" <-
function (last.menu) 
{
    coda.options(q = read.and.check("Enter quantile to be estimated:", 
        lower = 0, upper = 1, default = coda.options("q")))
    coda.options(r = read.and.check("Enter required precision:", 
        upper = coda.options("q"), default = coda.options("r")))
    coda.options(s = read.and.check("Enter required probability:", 
        lower = 0, upper = 1, default = coda.options("s")))
    return(last.menu)
}
"codamenu.options.stats" <-
function () 
{
    next.menu <- "codamenu.options.stats"
    choices <- c("Display current statistics options", "Combine chains for summary statistics", 
        "Quantiles for summary statistics", "Number of significant digits for printing", 
        "Return to main options menu")
    pick <- menu(choices, title = "CODA options for summary statistics")
    if (pick == 0) 
        return("quit")
    switch(pick, print.coda.options(stats = TRUE), {
        mssg <- "Do you want to combine all chains when calculating summary statistics"
        change.tfoption(mssg, "combine.stats")
    }, {
        mssg <- paste("Enter quantiles required, separated by commas\n(Default =", 
            paste(coda.options("quantiles"), collapse = ", "))
        repeat {
            cat("\n", mssg, "\n")
            if (is.R()) 
                ans <- as.numeric(scan(what = character(), sep = ",", 
                  quiet = TRUE, nlines = 1))
            else ans <- as.numeric(scan(what = character(), sep = ","))
            if (length(ans) == 0) 
                ans <- coda.options("quantiles")
            if (any(is.na(ans))) 
                mssg <- "You must enter numeric values"
            else if (any(ans >= 1) || any(ans <= 0)) 
                mssg <- "You must enter values between 0 and 1"
            else break
        }
        if (length(ans) > 0) 
            coda.options(quantiles = sort(ans))
    }, {
        mssg <- "Enter number of significant digits to be printed"
        ans <- read.and.check(mssg, what = integer(), lower = 0, 
            default = coda.options("digits"))
        coda.options(digits = ans)
    }, {
        next.menu <- "codamenu.options"
    })
    return(next.menu)
}
"print.coda.options" <-
function (data = FALSE, stats = FALSE, plots = FALSE, diags = FALSE) 
{
    #Display working data and coda options in pretty format 
    ## 
    # First define some formatting functions 
    # 
    doline <- function(x, title, type) {
        if (is.null(Version()$language)) 
            doline <- get("doline", frame = sys.parent())
        if (length(x) > 1) {
            doline(x[1], title)
            for (i in 2:length(x)) doline(x[i], "")
            return()
        }
        if (is.logical(x)) 
            x <- ifelse(x, "Yes", "No")
        if (!missing(type) && mode(x) != type) 
            x <- "N/A"
        len.title <- nchar(title)
        endchar <- ifelse(x == "", " ", ":")
        cat("| ", title, paste(rep(" ", 20 - nchar(title)), collapse = ""), 
            sep = "")
        widthleft <- options("width")$width - 25 - nchar(x)
        if (widthleft > 0) 
            cat("| ", x, paste(rep(" ", widthleft), collapse = ""), 
                "|\n", sep = "")
        else cat("| ", x, "\n", sep = "")
    }
    dotitle <- function(title) {
        if (is.null(Version()$language)) 
            doline <- get("doline", frame = sys.parent())
        doline("", title)
        doline("", paste(rep("-", nchar(title)), collapse = ""))
    }
    domaintitle <- function(title) {
        cat("| ", title, paste(rep(" ", options()$width - nchar(title) - 
            3), collapse = ""), "|\n", sep = "")
    }
    doblank <- function() {
        if (is.null(Version()$language)) 
            doline <- get("doline", frame = sys.parent())
        doline("", "")
    }
    dosepline <- function() {
        cat("+", paste(rep("-", options("width")$width - 2), 
            collapse = ""), "+\n", sep = "")
    }
    strbrk <- function(x) {
        WIDTH <- options("width")$width - 26
        N <- length(x)
        x[-N] <- paste(x[-N], ", ", sep = "")
        tmp <- cumsum(nchar(x))
        lr <- vector("list", 1)
        lr[[1]] <- (1:N)[tmp <= WIDTH]
        rr <- x[lr[[1]]]
        i <- 1
        while (rr[length(rr)] != x[N]) {
            tmp <- tmp + (WIDTH - sum(nchar(rr)))
            lr <- c(lr, c(1:N)[tmp > (WIDTH * i) & tmp <= (WIDTH * 
                (i + 1))])
            rr <- x[lr[[i + 1]]]
            i <- i + 1
        }
        y <- character(length(lr))
        for (i in 1:length(y)) y[[i]] <- paste(x[lr[[i]]], collapse = "")
        return(y)
    }
    # 
    # Now we can get on with it 
    # 
    cat("\nCurrent option settings:")
    cat("\n=======================\n\n")
    dosepline()
    if (data) {
        domaintitle("WORKING DATA")
        dosepline()
        doblank()
        ans <- strbrk(varnames(work.dat, allow.null = FALSE))
        doline(ans, "Variables selected")
        ans <- strbrk(chanames(work.dat, allow.null = FALSE))
        doline(ans, "Chains selected")
        doline(start(work.dat), "Iterations - start")
        doline(end(work.dat), "             end")
        doline(thin(work.dat), "Thinning interval")
        doblank()
        dosepline()
    }
    if (stats) {
        domaintitle("SUMMARY STATISTICS OPTIONS")
        dosepline()
        doblank()
        doline(coda.options("combine.stats"), "Combine chains")
        ans <- strbrk(paste(coda.options("quantiles") * 100, 
            "%", sep = ""))
        doline(ans, "Quantiles")
        doline(coda.options("digits"), "Significant digits")
        doblank()
        dosepline()
    }
    if (plots) {
        domaintitle("PLOTTING OPTIONS")
        dosepline()
        doblank()
        doline(coda.options("trace"), "Trace")
        doline(coda.options("densplot"), "Density")
        doline(coda.options("lowess"), "Smooth lines")
        doline(coda.options("combine.plots"), "Combine chains")
        doline(coda.options("onepage"), "One plot/page.")
        doline(coda.options("user.layout"), "User-defined layout")
        if (coda.options("user.layout")) 
            doline(paste(par("mfrow"), collapse = " X "), "")
        width.cut <- options("width")[[1]] - 26
        if (is.null(options()$language)) {
            func <- deparse(coda.options("bandwidth"))
            for (i in 1:length(func)) {
                func.i <- substring(func[i], 1:nchar(func[i]), 
                  1:nchar(func[i]))
                if (any(func.i == "\t")) {
                  func.i[func.i == "\t"] <- "     "
                  func[i] <- paste(func.i, collapse = "")
                }
            }
        }
        else func <- deparse(coda.options("bandwidth"), width.cutoff = width.cut)
        func.print <- vector("list", length(func))
        for (i in 1:length(func)) {
            if (nchar(func[i]) <= width.cut) 
                func.print[[i]] <- func[i]
            else {
                first <- seq(from = 1, to = nchar(func[i]), by = width.cut)
                last <- seq(from = width.cut, to = nchar(func[i]), 
                  by = width.cut)
                if (max(last) < nchar(func[i])) 
                  last <- c(last, nchar(func[i]))
                func.print[[i]] <- substring(func[i], first = first, 
                  last = last)
            }
        }
        doline(unlist(func.print), "Bandwidth")
        doblank()
        dosepline()
    }
    if (diags) {
        domaintitle("DIAGNOSTICS OPTIONS")
        dosepline()
        doblank()
        dotitle("Geweke")
        doline(coda.options("frac1"), "Window 1 fraction")
        doline(coda.options("frac2"), "Window 2 fraction")
        doline(coda.options("geweke.bin"), "Bin width", type = "numeric")
        doline(coda.options("geweke.max"), "Max number of bins", 
            type = "numeric")
        doblank()
        dotitle("Gelman & Rubin")
        ans <- coda.options("gr.bin")
        doline(ans, "Bin width", type = "numeric")
        doline(coda.options("gr.max"), "Max number of bins", 
            type = "numeric")
        doblank()
        dotitle("Raftery & Lewis")
        doline(coda.options("q"), "Quantile (q)")
        doline(coda.options("r"), "Precision (+/- r)")
        doline(coda.options("s"), "Probability (s)")
        doblank()
        dotitle("Cross-correlations")
        doline(coda.options("combine.corr"), "Combine chains")
        doblank()
        dosepline()
    }
    invisible()
}
"read.bugs.interactive" <-
function () 
{
    repeat {
        cat("Enter BUGS output filenames, separated by return key\n")
        cat("(leave blank to exit)\n")
        filenames <- scan(what = character(), sep = "\n", strip.white = TRUE, 
            quiet = TRUE)
        if (length(filenames) == 0) 
            return()
        else {
            root <- character(length(filenames))
            for (i in 1:length(filenames)) {
                nc <- nchar(filenames[i])
                if (nc > 3) {
                  file.ext <- substring(filenames[i], nc - 3, 
                    nc)
                  root[i] <- if (any(file.ext == c(".ind", ".out"))) 
                    substring(filenames[i], 0, nc - 4)
                  else filenames[i]
                }
            }
            root <- unique(root)
            all.files <- c(paste(root, ".ind", sep = ""), paste(root, 
                ".out", sep = ""))
            if (any(!file.exists(all.files))) {
                cat("The following files were not found:\n")
                cat(paste(all.files[!file.exists(all.files)], 
                  collapse = "\n"), "\n\n")
            }
            else break
        }
    }
    nfiles <- length(root)
    chains <- vector("list", nfiles)
    names(chains) <- filenames
    for (i in 1:nfiles) chains[[i]] <- read.bugs(file = root[i])
    return(mcmc.list(chains))
}
"coda.objects" <-
c("autocorr.plot", "chanames", "change.tfoption", "coda.credits", 
"coda.options", "codamenu", "codamenu.anal", "codamenu.diags", 
"codamenu.diags.autocorr", "codamenu.diags.crosscorr", "codamenu.diags.gelman", 
"codamenu.diags.geweke", "codamenu.diags.heidel", "codamenu.diags.raftery", 
"codamenu.main", "codamenu.options", "codamenu.options.data", 
"codamenu.options.diag", "codamenu.options.gelman", "codamenu.options.geweke.bin", 
"codamenu.options.geweke.win", "codamenu.options.heidel", "codamenu.options.plot", 
"codamenu.options.plot.kernel", "codamenu.options.plot.ps", "codamenu.options.raftery", 
"codamenu.options.stats", "codamenu.output.header", "crosscorr", 
"crosscorr.plot", "densplot", "end.mcmc", "frequency.mcmc", "gelman.diag", 
"geweke.diag", "geweke.nse", "geweke.plot", "geweke.power", "heidel.diag", 
"is.mcmc", "mcmc", "multi.menu", "nchain", "niter", "nvar", "plot.mcmc", 
"print.coda.options", "print.gelman.diag", "print.geweke.diag", 
"print.heidel.diag", "print.mcmc", "print.raftery.diag", "print.summary.mcmc", 
"raftery.diag", "read.and.check", "read.bugs", "read.bugs.interactive", 
"set.mfrow", "spec.pgram", "start.mcmc", "summary.mcmc", "test.bandwidth", 
"thin", "thin.mcmc", "tidy.up", "time.mcmc", "traceplot", "varnames", 
"window.mcmc")
"tidy.up" <-
function () 
{
    # 
    # tidy.up -- gives option to save input files in S format; then deletes all  
    #	     S-plus objects created during current CODA session, and 
    #	     closes all graphics windows 
    # 
    # Author: Nicky Best 
    # 
    cat("\nQuitting CODA....\n")
    if (exists("coda.dat", where = 1) && !coda.options("data.saved")) {
        cat("\nDo you want to save the BUGS output as an R object file(y/N) ?\n")
        ans <- read.and.check(what = character(), default = "n")
        if (ans == "Y" | ans == "y") {
            cat("Enter name you want to call this object file:\n")
            fname <- scan(what = character(), nmax = 1, strip.white = TRUE)
            assign(fname, coda.dat, envir = sys.frame(0))
        }
    }
    coda.objects <- c("coda.dat", "work.dat")
    if (is.R()) 
        for (i in coda.objects) {
            if (exists(i)) 
                rm(list = i, inherits = TRUE)
        }
    else {
        for (i in coda.objects) {
            if (exists(i)) 
                remove(i, where = 1)
        }
    }
    graphics.off()
}
"codamenu.ps" <-
function () 
{
    if (names(dev.cur()) == "postscript") {
        dev.off()
    }
    else {
        cat("\nSave plots as a postscript file (y/N) ?\n")
        ans <- readline()
        if (length(ans) == 0) 
            ans <- "n"
        if (ans == "Y" | ans == "y") {
            repeat {
                mssg <- "Enter name you want to call this postscript file"
                ps.name <- read.and.check(mssg, what = character(), 
                  default = "Rplots.ps")
                if (is.R() && file.exists(ps.name)) {
                  pick <- menu(title = "File exists", choices = c("overwrite", 
                    "choose another file name"))
                  if (pick == 1) 
                    break
                }
                else break
            }
            postscript(file = ps.name)
        }
    }
    return(dev.cur())
}
"codamenu.output.header" <-
function (title) 
{
    #
    # A short header: common to most codamenu output
    #
    cat("\n", title, sep = "")
    cat("\n", paste(rep("=", nchar(title)), collapse = ""), "\n\n", 
        sep = "")
    cat("Iterations used = ", start(work.dat), ":", end(work.dat), 
        "\n", sep = "")
    cat("Thinning interval =", thin(work.dat), "\n")
    cat("Sample size per chain =", niter(work.dat), "\n\n")
    invisible()
}
"codamenu.devices" <-
function () 
{
    devices <- c("X11", "motif", "openlook", "win.graph")
    have.device <- vector("logical", length(devices))
    for (i in 1:length(devices)) {
        have.device[i] <- exists(devices[i])
    }
    devices <- devices[have.device]
    if (length(devices) == 0) 
        stop("Can't find any graphics devices")
    else {
        if (length(devices) == 1) 
            do.call(devices[1], list())
        else repeat {
            pick <- menu(choices = devices, title = "choose graphics device")
            if (pick != 0) {
                do.call(devices[pick], list())
                break
            }
        }
    }
    return(dev.cur())
}
