\name{acf}
\title{autocorrelation function for time series}

\usage{acf(x, lag.max, type = "correlation", plot = FALSE)}

\arguments{
\item{x}{a time series, vector or matrix}
\item{lag.max}{maximum lag at which to calculate acf}
\item{type}{by default an autocorrelation function is calculated.
If type="covariance" the autocovariance function is calculated.
Partial correlations are not implemented}
\item{plot}{flag for S-PLUS compatibility. Ignored}
}

\value{
A list with the following elements:
\item{acf}{Autocorrelation function}
\item{lag}{Lag values at which acf is calculated}
\item{n.used}{Number of observations used in calculation}
\item{type}{See above}
\item{series}{Name of time series used as input}
}

\notes{
An attempt to re-implement the S-PLUS \code{acf} function. Still
lacking in some features of the S-PLUS version. 
}

\author{Paul Gilbert}
