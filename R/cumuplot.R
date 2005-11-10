cumuplot <- function(x, probs=c(0.025,0.5,0.975), ylab="", lty=c(2,1),
                     lwd=c(1,2), type="l", ask=dev.interactive(),
                     auto.layout=TRUE, col=1, ...)
{
    cquantile <- function(z, probs)
    {
        ## Calculates cumulative quantile of a vector
        cquant <- matrix(0, nrow=length(z), length(probs))
        for(i in seq(along=z))  # for loop proved faster than apply here
            cquant[i,] <- quantile(z[1:i], probs=probs, names=FALSE)
        cquant <- as.data.frame(cquant)
        names(cquant) <- paste(formatC(100*probs,format="fg",wid=1,digits=7),
                               "%", sep="")  # just like quantile.default
        return(cquant)
    }

    oldpar <- NULL
    on.exit(par(oldpar))
    if (auto.layout) {
        oldpar <- par(mfrow = set.mfrow(Nchains = nchain(x), 
                      Nparms = nvar(x)))
    }
    
    if (!is.mcmc.list(x)) 
        x <- mcmc.list(as.mcmc(x))

    Iterations <- time(x)
    for (i in 1:nchain(x)) {
        for (j in 1:nvar(x)) {
            Y <- cquantile(as.matrix(x[[i]])[,j], probs=probs)
            matplot(Iterations, Y, ylab=ylab, lty=lty, lwd=lwd, type=type,
                    col=col, ...)
            title(paste(varnames(x)[j], ifelse(is.null(chanames(x)), 
                  "", ":"), chanames(x)[i], sep = ""))
            if (i == 1 & j == 1)
                oldpar <- c(oldpar, par(ask=ask))
        }
    }
}


