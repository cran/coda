.ptime <- proc.time()
postscript('coda-Examples.ps')
.par.postscript <- par(no.readonly = TRUE)
options(contrasts = c(unordered = "contr.treatment", ordered =  "contr.poly"))
library('coda')
.Random.seed <- c(0,rep(7654,3)); rm(list = ls())
###--- >>> `mcmc.list' <<<----- Replicated Markov Chain Monte Carlo Objects

	## alias	 help(as.mcmc.list)
	## alias	 help(is.mcmc.list)

##___ Examples ___:

data(line)
x1 <- line[[1]]			#Select first chain
x2 <- line[,1, drop=FALSE]	#Select first var from all chains
varnames(x2) == varnames(line)[1]	#TRUE
x3 <- line[1:10,]		#Select first 10 iterations from all chains
all(time(x3) == time(line)[1:10])	#TRUE

## Keywords: 'ts'.


cat("Time elapsed: ", proc.time() - .ptime,"\n")
dev.off(); quit('no')
