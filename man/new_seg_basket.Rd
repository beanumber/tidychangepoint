% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-seg_basket.R, R/generics.R
\name{new_seg_basket}
\alias{new_seg_basket}
\alias{validate_seg_basket}
\alias{seg_basket}
\alias{evaluate_cpts.seg_basket}
\alias{evaluate_cpts.list}
\alias{evaluate_cpts.tbl_df}
\alias{evaluate_cpts}
\title{Default class for candidate changepoint sets}
\usage{
new_seg_basket(
  x = numeric(),
  algorithm = NA,
  cpt_list = list(),
  seg_params = list(),
  model_name = "meanshift_norm",
  penalty = "BIC",
  ...
)

validate_seg_basket(x)

seg_basket(x, ...)

\method{evaluate_cpts}{seg_basket}(x, ...)

\method{evaluate_cpts}{list}(x, .data, model_fn, ...)

\method{evaluate_cpts}{tbl_df}(x, .data, model_fn, ...)

evaluate_cpts(x, ...)
}
\arguments{
\item{x}{a numeric vector coercible into a \code{ts} object}

\item{algorithm}{Algorithm used to find the changepoints}

\item{cpt_list}{a possibly empty \code{list()} of candidate changepoints}

\item{seg_params}{a possibly empty \code{list()} of segmenter parameters}

\item{model_name}{character indicating the model used to find the changepoints.}

\item{penalty}{character indicating the name of the penalty function used to
find the changepoints.}

\item{...}{currently ignored}

\item{.data}{A time series}

\item{model_fn}{Name of the function to fit the model.
See, for examples, \code{\link[=fit_meanshift]{fit_meanshift()}}.}
}
\description{
Default class for candidate changepoint sets
}
\examples{
seg <- seg_basket(DataCPSim, cpt_list = list(c(365), c(330, 839)))
str(seg)
as.ts(seg)
changepoints(seg)
fitness(seg)
}
