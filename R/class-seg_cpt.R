#' Base class for segmenters
#' @name seg_cpt-class
#' @export
#' @param x a numeric vector coercible into a [stats::ts()] object
#' @param pkg name of the package providing the segmenter
#' @param algorithm Algorithm used to find the changepoints
#' @param changepoints a possibly empty [list()] of candidate changepoints
#' @param fitness A named `double` vector whose name reflects the penalty applied
#' @param seg_params a possibly empty [list()] of segmenter parameters
#' @param model_name character indicating the model used to find the changepoints. 
#' @param penalty character indicating the name of the penalty function used to
#' find the changepoints.
#' @param ... currently ignored
#' @returns A [seg_cpt-class] object.
#' @examples
#' x <- seg_cpt(DataCPSim, pkg = "tidychangepoint", changepoints = c(330))
#' str(x)
#' as.ts(x)
#' changepoints(x)
#' 
#' y <- segment(CET, method = "pelt")
#' z <- as.seg_cpt(y$segmenter)
#' class(z)
#' fitness(x)
#' glance(z)
#' model_name(z)
#' model_args(z)
#' nobs(z)
#' seg_params(z)

new_seg_cpt <- function(x = numeric(), 
                        pkg = character(),
                        algorithm = NA, 
                        changepoints = integer(),
                        fitness = double(),
                        seg_params = list(), 
                        model_name = "meanshift_norm",
                        penalty = "BIC", ...) {
  stopifnot(is.numeric(x))
  structure(
    list(
      data = stats::as.ts(x),
      pkg = pkg,
      algorithm = algorithm,
      changepoints = changepoints,
      fitness = fitness,
      seg_params = seg_params,
      model_name = model_name,
      penalty = names(fitness)
    ), 
    class = "seg_cpt"
  )
}

#' @rdname new_seg_cpt
#' @export

validate_seg_cpt <- function(x) {
  if (!stats::is.ts(as.ts(x))) {
    stop("data attribute is not coercible into a ts object.")
  }
  if (!(is.integer(changepoints(x)) && is_valid_tau(changepoints(x), nobs(x)))) {
    stop("changepoint set is invalid")
  }
  if (!is.double(fitness(x)) && names(fitness(x)) && length(fitness(x) == 1)) {
    stop("fitness must be named")
  }
  x
}

#' @rdname new_seg_cpt
#' @export

seg_cpt <- function(x, ...) {
  obj <- new_seg_cpt(x, ...)
  validate_seg_cpt(obj)
}

#' @rdname seg-cpt-generics
#' @export
as.seg_cpt.seg_cpt <- function(object, ...) {
  object
}

#' Methods for seg_cpt objects
#' @name seg-cpt-generics
#' @param x An `seg_cpt` object
#' @param ... arguments passed to methods
#' @export
as.ts.seg_cpt <- function(x, ...) {
  as.ts(x$data)
}

#' @rdname changepoints
#' @export
changepoints.seg_cpt <- function(x, ...) {
  x$changepoints |>
    as.integer()
}

#' @rdname fitness
#' @export
fitness.seg_cpt <- function(object, ...) {
  object$fitness
}

#' @rdname seg-cpt-generics
#' @export
glance.seg_cpt <- function(x, ...) {
  tibble::tibble(
    pkg = x$pkg,
    version = utils::packageVersion(x$pkg),
    algorithm = x$algorithm,
    seg_params = list(x$seg_params),
    model_name = model_name(x),
    criteria = names(fitness(x)),
    fitness = fitness(x)
  )
}

#' @rdname model_name
#' @export
model_name.seg_cpt <- function(object, ...) {
  object$model_name
}

#' @rdname model_args
#' @export
model_args.seg_cpt <- function(object, ...) {
  NA
}

#' @rdname seg-cpt-generics
#' @param object A `seg_cpt` object
#' @export
nobs.seg_cpt <- function(object, ...) {
  length(as.ts(object))
}

#' @rdname seg-cpt-generics
#' @export
print.seg_cpt <- function(x, ...) {
  utils::str(x)
}

#' @rdname seg-cpt-generics
#' @export
seg_params.seg_cpt <- function(x, ...) {
  x$seg_params
}
