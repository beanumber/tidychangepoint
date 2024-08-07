% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_nhpp.R
\name{mcdf}
\alias{mcdf}
\title{Cumulative distribution of the exceedances of a time series}
\usage{
mcdf(x, dist = "weibull")
}
\arguments{
\item{x}{An NHPP \code{model} returned by \code{\link[=fit_nhpp]{fit_nhpp()}}}

\item{dist}{Name of the distribution. Currently only \code{weibull} is implemented.}
}
\value{
a numeric vector of length equal to the \link{exceedances} of \code{x}
}
\description{
Cumulative distribution of the exceedances of a time series
}
\examples{
# Fit an NHPP model using the mean as a threshold
nhpp <- fit_nhpp(DataCPSim, tau = 826)

# Compute the cumulative exceedances of the mean
mcdf(nhpp)

# Fit an NHPP model using another threshold
nhpp <- fit_nhpp(DataCPSim, tau = 826, threshold = 200)

# Compute the cumulative exceedances of the threshold
mcdf(nhpp)

}
\seealso{
\code{\link[=plot_intensity]{plot_intensity()}}
}
