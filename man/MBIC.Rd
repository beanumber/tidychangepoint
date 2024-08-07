% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaa_generics.R, R/pkg-stats.R
\name{MBIC}
\alias{MBIC}
\alias{MBIC.default}
\alias{MBIC.logLik}
\title{Modified Bayesian Information Criterion}
\usage{
MBIC(object, ...)

\method{MBIC}{default}(object, ...)

\method{MBIC}{logLik}(object, ...)
}
\arguments{
\item{object}{any object from which a log-likelihood value, or a
   contribution to a log-likelihood value, can be extracted.}

\item{...}{some methods for this generic function require additional
   arguments.}
}
\value{
A \code{double} vector of length 1
}
\description{
Generic function to compute the Modified Bayesian Information Criterion for a
changepoint detection model.
}
\references{
Zhang and Seigmmund (2007) for MBIC: \doi{10.1111/j.1541-0420.2006.00662.x}
}
\seealso{
\code{\link[stats:AIC]{stats::BIC()}}

Other penalty-functions: 
\code{\link{BMDL}()},
\code{\link{MDL}()}
}
\concept{penalty-functions}
