\name{spec.pgram}
\title{Estimate spectral density from smoothed periodogram}

\usage{
spec.pgram(x, spans=1, taper=0.1, demean=FALSE, detrend=TRUE, pad=FALSE, plot=FALSE)
}

\arguments{
\item{x}{A vector or matrix time series}
\item{spans}{vector of widths of modified Daniell windows to be
successively applied when smoothing the periodogram}
\item{taper}{A split cosine taper is applied to the first and
last 100 * spans \% of the data before calculating the periodogram}
\item{demean}{Sweep out mean of time series}
\item{detrend}{Subtract linear trend from time series to improve
stationarity. This option overrides \code{demean}}
\item{pad}{If set to TRUE, the end of the series will be padded out
with zeros so that the chain is a convenient length for
the Fast Fourier Transform}
\item{plot}{This doesn't do anything. It's just there for S-PLUS
compatibility}
}

\value{
A list with the following elements
\item{spec}{Spectral density estimated from the smoothed
periodogram and expressed in decibels}
\item{freq}{Corresponding frequencies at which the spectral
density is calculated}
}

\note{
This is an attempt to clone the S-PLUS spec.pgram function.
It has comparatively limited functionality.
}

\author{Martyn Plummer}
