"read.jags" <- function (file = "jags.out", start, end, thin, quiet=FALSE) 
{
    read.coda(file, start, end, thin, quiet)
}

bugs2jags <- function(infile, outfile)
{
    ## Convert S-style data for WinBUGS into the R dump format
    ## used by JAGS.
    bugs.dat <- dget(infile)
    for (n in names(bugs.dat)) {
        if(!is.null(dim(bugs.dat[[n]]))) {
            ## Manually reverse order of dimensions of arrays
            dim(bugs.dat[[n]]) <- rev(dim(bugs.dat[[n]]))
            ## Then transpose
            bugs.dat[[n]] <- aperm(bugs.dat[[n]])
        }
        assign(n, bugs.dat[[n]])
    }
    dump(names(bugs.dat), file=outfile)
}
