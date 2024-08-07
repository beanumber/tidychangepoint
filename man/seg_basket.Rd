% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-seg_basket.R
\name{new_seg_basket}
\alias{new_seg_basket}
\alias{seg_basket}
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

seg_basket(x, ...)
}
\arguments{
\item{x}{a numeric vector coercible into a \code{\link[stats:ts]{stats::ts()}} object}

\item{algorithm}{Algorithm used to find the changepoints}

\item{cpt_list}{a possibly empty \code{\link[=list]{list()}} of candidate changepoints}

\item{seg_params}{a possibly empty \code{\link[=list]{list()}} of segmenter parameters}

\item{model_name}{character indicating the model used to find the changepoints.}

\item{penalty}{character indicating the name of the penalty function used to
find the changepoints.}

\item{...}{currently ignored}
}
\value{
A \code{\link[=seg_basket]{seg_basket()}} object.
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
