"IQR" <-
function (x, na.rm = FALSE) 
as.vector(diff(quantile(as.numeric(x), c(0.25, 0.75), na.rm = na.rm)))
"NCOL" <-
function (x) 
if (is.array(x) || is.data.frame(x)) ncol(x) else as.integer(1)
"NROW" <-
function (x) 
if (is.array(x) || is.data.frame(x)) nrow(x) else length(x)
"Nchains" <-
2
"colnames" <-
function (x, do.NULL = TRUE, prefix = "row") 
{
    dn <- dimnames(x)
    if (!is.null(dn[[1]])) 
        dn[[1]]
    else {
        if (do.NULL) 
            NULL
        else paste(prefix, seq(length = NROW(x)), sep = "")
    }
}
"crosscorr.plot.S" <-
function (corr.list, this.chain, this.parm, scale, title.scale, 
    mrows) 
{
    #
    # parmcorr.plot -- function to plot autocorrelations within each chain for
    #		   each parameter - called by setup.plots()
    #
    # Author: Nicky Best
    #
    title.scale <- switch(func.defaults$ps.plot, title.scale, 
        title.scale * 0.85, title.scale * 0.7)
    scale <- switch(func.defaults$ps.plot, scale, scale * 0.8, 
        scale * 0.6)
    scale1 <- switch(func.defaults$ps.plot, scale * 0.6, scale * 
        0.7, scale * 0.7)
    par(xaxt = "n", yaxt = "n")
    plot(0, 0, type = "n", xlim = c(0, Nparms), ylim = c(0, Nparms), 
        xlab = "", ylab = "", cex = scale)
    par(adj = 1)
    axis(2, at = c(Nparms:1) - 0.5, labels = short.labels[data.defaults$parms], 
        cex = scale)
    par(crt = 90, srt = 90)
    axis(1, at = c(1:Nparms) - 0.5, labels = short.labels[data.defaults$parms], 
        cex = scale)
    par(xaxt = "s", yaxt = "s", crt = 0, srt = 0, adj = 0.5)
    for (cl in 1:Nparms) {
        for (rw in 1:(Nparms - cl + 1)) {
            #
            dens <- switch(abs(corr.list[[this.chain]][Nparms - 
                rw + 1, cl] * 10)%/%2 + 1, 0, 10, 25, 40, 65, 
                100)
            #			Fudge to ensure density on diagonal = 1.0, 
            #			as rounding errors may occur in switch:
            #
            #
            if (cl == Nparms - rw + 1) 
                dens <- 100
            ang <- ifelse(corr.list[[this.chain]][Nparms - rw + 
                1, cl] < 0, 315, 45)
            polygon(c(cl - 1, cl - 1, cl, cl, cl - 1), c(rw - 
                1, rw, rw, rw - 1, rw - 1), density = dens, angle = ang)
        }
    }
    leg.scale <- c(0.35, 0.675)
    if (mrows == 1) {
        legend(c(leg.scale[1] * Nparms, leg.scale[2] * Nparms), 
            c(0.99 * Nparms, 0.75 * Nparms), legend = c("|r| = [0.0, 0.2)", 
                "", "|r| = [0.2, 0.4)", "", "|r| = [0.4, 0.6)"), 
            density = c(0.1, -99, 10, -99, 25), angle = rep(45, 
                5), cex = scale1 * 0.85, bty = "n")
        legend(c(leg.scale[2] * Nparms, 1.02 * Nparms), c(0.99 * 
            Nparms, 0.75 * Nparms), legend = c("|r| = [0.6, 0.8)", 
            "", "|r| = [0.8, 1.0)", "", "|r| = 1.0"), density = c(40, 
            -99, 65, -99, 100), angle = rep(45, 5), cex = scale1 * 
            0.85, bty = "n")
        par(adj = 0)
        text(0.5 * (leg.scale[1] + leg.scale[2]) * Nparms, 1.025 * 
            Nparms, "Gradient=sign of correlation", cex = scale1 * 
            0.85)
    }
    else {
        legend(c(leg.scale[1] * Nparms, leg.scale[2] * Nparms), 
            c(0.97 * Nparms, 0.8 * Nparms), legend = c("|r| = [0.0, 0.2)", 
                "|r| = [0.2, 0.4)", "|r| = [0.4, 0.6)"), density = c(0.1, 
                10, 25), angle = rep(45, 3), cex = scale1 * 0.8, 
            bty = "n")
        legend(c(leg.scale[2] * Nparms, 1.02 * Nparms), c(0.97 * 
            Nparms, 0.8 * Nparms), legend = c("|r| = [0.6, 0.8)", 
            "|r| = [0.8, 1.0)", "|r| = 1.0"), density = c(40, 
            65, 100), angle = rep(45, 3), cex = scale1 * 0.8, 
            bty = "n")
        par(adj = 0)
        text(0.5 * (leg.scale[1] + leg.scale[2]) * Nparms, Nparms, 
            "Gradient=sign of correlation", cex = scale1 * 0.8)
    }
    par(adj = 0.5)
    if (Nchains > 1) {
        title(filenames[data.defaults$chains[this.chain]], cex = title.scale)
    }
}
"is.R" <-
function () 
{
    !is.null(version$language) && version$language == "R"
}
"rownames" <-
function (x, do.NULL = TRUE, prefix = "row") 
{
    dn <- dimnames(x)
    if (!is.null(dn[[1]])) 
        dn[[1]]
    else {
        if (do.NULL) 
            NULL
        else paste(prefix, seq(length = NROW(x)), sep = "")
    }
}
"sd" <-
function (x) 
sqrt(var(x))
