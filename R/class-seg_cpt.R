#' @rdname seg_cpt
#' @export
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

#' Base class for segmenters
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
#' @returns A [seg_cpt] object.
seg_cpt <- function(x, ...) {
  obj <- new_seg_cpt(x, ...)
  validate_seg_cpt(obj)
}

#' @rdname as.segmenter
#' @export
as.seg_cpt.seg_cpt <- function(object, ...) {
  object
}

#' @rdname reexports
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

#' @rdname reexports
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

#' @rdname reexports
#' @export
nobs.seg_cpt <- function(object, ...) {
  length(as.ts(object))
}

#' @rdname reexports
#' @export
print.seg_cpt <- function(x, ...) {
  utils::str(x)
}

#' @rdname seg_params
#' @export
seg_params.seg_cpt <- function(object, ...) {
  object$seg_params
}
