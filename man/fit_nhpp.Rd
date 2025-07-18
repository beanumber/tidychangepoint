% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_nhpp.R
\name{fit_nhpp}
\alias{fit_nhpp}
\title{Fit a non-homogeneous Poisson process model to the exceedances of a time series.}
\usage{
fit_nhpp(x, tau, ...)
}
\arguments{
\item{x}{A time series}

\item{tau}{A vector of changepoints}

\item{...}{currently ignored}
}
\value{
An \code{nhpp} object, which inherits from \link{mod_cpt}.
}
\description{
Fit a non-homogeneous Poisson process model to the exceedances of a time series.
}
\details{
Any time series can be modeled as a non-homogeneous Poisson process of the
locations of the \link{exceedances} of a threshold in the series.
This function uses the \link{BMDL} criteria to determine the best fit
parameters for each
region defined by the changepoint set \code{tau}.
}
\examples{
# Fit an NHPP model using the mean as a threshold
fit_nhpp(DataCPSim, tau = 826)

# Fit an NHPP model using other thresholds
fit_nhpp(DataCPSim, tau = 826, threshold = 20)
fit_nhpp(DataCPSim, tau = 826, threshold = 200)

# Fit an NHPP model using changepoints determined by PELT
fit_nhpp(DataCPSim, tau = changepoints(segment(DataCPSim, method = "pelt")))

}
\seealso{
Other model-fitting: 
\code{\link{fit_arima}()},
\code{\link{fit_lmshift}()},
\code{\link{fit_meanshift}()},
\code{\link{fit_meanvar}()},
\code{\link{model_args}()},
\code{\link{model_name}()},
\code{\link{new_fun_cpt}()},
\code{\link{whomademe}()}
}
\concept{model-fitting}
