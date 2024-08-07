% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaa_generics.R, R/class-tidycpt.R, R/utils.R
\name{as.model}
\alias{as.model}
\alias{as.model.default}
\alias{as.model.tidycpt}
\alias{is_model}
\title{Convert, retrieve, or verify a model object}
\usage{
as.model(object, ...)

\method{as.model}{default}(object, ...)

\method{as.model}{tidycpt}(object, ...)

is_model(x, ...)
}
\arguments{
\item{object}{A \linkS4class{tidycpt} object, typically returned by \code{\link[=segment]{segment()}}}

\item{...}{currently ignored}

\item{x}{An object, typically returned by \verb{fit_*()}}
}
\value{
\itemize{
\item \code{\link[=as.model]{as.model()}} returns a \link{mod_cpt} model object
}

\itemize{
\item \code{\link[=is_model]{is_model()}} a \code{logical} vector of length 1
}
}
\description{
Convert, retrieve, or verify a model object
}
\details{
\linkS4class{tidycpt} objects have a \code{model} component.
The functions documented here are convenience utility functions
for working with the \code{model} components.
\code{\link[=as.model]{as.model()}} is especially useful in pipelines to avoid having to use
the \code{$} or \code{[} notation for subsetting.

When applied to a \linkS4class{tidycpt} object, \code{\link[=as.model]{as.model()}} simply returns the
\code{model} component of that object.
However, when applied to a \code{segmenter} object, \code{\link[=as.model]{as.model()}} attempts to
converts that object into a \link{mod_cpt} model object.

\code{\link[=is_model]{is_model()}} checks to see if a model object implements all of the
S3 methods necessary to be considered a model.
}
\examples{
# Segment a time series using PELT
x <- segment(CET, method = "pelt")

# Retrieve the model component
x |> 
  as.model()

# Explicitly convert the segmenter to a model
x |>
  as.segmenter() |>
  as.model()

# Is that model valid? 
x |>
  as.model() |>
  is_model()
  

# Fit a model directly, without using [segment()]
x <- fit_nhpp(CET, tau = 330)
is_model(x)
}
\seealso{
Other tidycpt-generics: 
\code{\link{as.segmenter}()},
\code{\link{changepoints}()},
\code{\link{diagnose}()},
\code{\link{fitness}()},
\code{\link{model_name}()}
}
\concept{tidycpt-generics}
