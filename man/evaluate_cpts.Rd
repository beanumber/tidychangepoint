% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaa_generics.R, R/class-seg_basket.R
\name{evaluate_cpts}
\alias{evaluate_cpts}
\alias{evaluate_cpts.seg_basket}
\alias{evaluate_cpts.list}
\alias{evaluate_cpts.tbl_df}
\title{Evaluate candidate changepoints sets}
\usage{
evaluate_cpts(x, ...)

\method{evaluate_cpts}{seg_basket}(x, ...)

\method{evaluate_cpts}{list}(x, .data, model_fn, ...)

\method{evaluate_cpts}{tbl_df}(x, .data, model_fn, ...)
}
\arguments{
\item{x}{An object to evaluate}

\item{...}{arguments passed to methods}

\item{.data}{A time series}

\item{model_fn}{Name of the function to fit the model.
See, for examples, \code{\link[=fit_meanshift_norm]{fit_meanshift_norm()}}.}
}
\value{
A \link[tibble:tbl_df-class]{tibble::tbl_df}
}
\description{
Evaluate candidate changepoints sets
}
\keyword{internal}
