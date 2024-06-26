% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-tidycpt.R, R/generics.R, R/utils.R
\name{as.model.tidycpt}
\alias{as.model.tidycpt}
\alias{as.segmenter.tidycpt}
\alias{as.segmenter}
\alias{as.seg_cpt}
\alias{as.model}
\alias{as.model.default}
\alias{is_model}
\alias{is_segmenter}
\title{Convert a segmenter to a model object}
\usage{
\method{as.model}{tidycpt}(object, ...)

\method{as.segmenter}{tidycpt}(object, ...)

as.segmenter(object, ...)

as.seg_cpt(object, ...)

as.model(object, ...)

\method{as.model}{default}(object, ...)

is_model(x, ...)

is_segmenter(object, ...)
}
\arguments{
\item{object}{A segmenter object, typically returned by \code{\link[=segment]{segment()}}}

\item{...}{currently ignored}

\item{x}{An object, typically returned by \verb{fit_*()}}
}
\value{
\itemize{
\item \code{\link[=as.segmenter]{as.segmenter()}} returns a segmenter object
}

\itemize{
\item \code{\link[=as.seg_cpt]{as.seg_cpt()}} returns a \link{seg_cpt} object
}

\itemize{
\item \code{\link[=as.model]{as.model()}} returns a \link{mod_cpt} model object
}

\itemize{
\item \code{\link[=is_model]{is_model()}} a \code{logical} vector of length 1
}

\itemize{
\item \code{\link[=is_segmenter]{is_segmenter()}} a \code{logical} vector of length 1
}
}
\description{
Convert segmenters into models or standardized segmenters
}
\details{
\itemize{
\item \code{\link[=as.model.tidycpt]{as.model.tidycpt()}} returns the \code{model} object of a \code{tidycpt} object
}

\itemize{
\item \code{\link[=as.segmenter.tidycpt]{as.segmenter.tidycpt()}} returns the \code{segmenter} object of a \code{tidycpt} object
}

\itemize{
\item \code{\link[=as.segmenter]{as.segmenter()}} returns the segmenter of a \code{tidycpt} object
}

\itemize{
\item \code{\link[=as.seg_cpt]{as.seg_cpt()}} converts a wild-caught segmenter of any class into a
\code{seg_cpt} object
}

\itemize{
\item \code{\link[=as.model]{as.model()}} converts a segmenter object into a \link{mod_cpt} model object
}

\itemize{
\item \code{\link[=is_model]{is_model()}} checks to see if a model object implements all of the
S3 methods necessary to be considered a model.
}

\itemize{
\item \code{\link[=is_segmenter]{is_segmenter()}} checks to see if a segmenter object implements all of the
S3 methods necessary to be considered a segmenter.
}
}
\examples{
cpt <- segment(DataCPSim, method = "ga", maxiter = 5)
as.model(cpt$segmenter)
\dontrun{
cpt <- segment(DataCPSim, method = "ga-coen", model_fn_args = list(threshold = 80), maxiter = 5)
as.model(cpt$segmenter)$model_params
}
x <- fit_nhpp(CET, tau = 330)
is_model(x)
x <- segment(CET, method = "pelt")
is_segmenter(x$segmenter)
}
