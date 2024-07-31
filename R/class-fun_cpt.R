#' @rdname fun_cpt
#' @export
new_fun_cpt <- function(x, ...) {
  f <- eval(parse(text = x))
  stopifnot(is.function(f))
  structure(
    f,
    model_name = gsub(pattern = "fit_", replacement = "", x),
    class = "fun_cpt"
  )
}

#' @rdname fun_cpt
#' @export
validate_fun_cpt <- function(x) {
  args <- methods::formalArgs(x)
  if (!all(c("x", "tau", "...") %in% args)) {
    stop("Model-fitting functions must have x, tau, and ... as arguments")
  }
  x
}

#' Class for model-fitting functions
#' @export
#' @param x a `character` giving the name of a model-fitting function
#' @param ... currently ignored
#' @details
#' All model-fitting functions must be registered through a call to [fun_cpt()].
#' 
#' All model-fitting functions must take at least three arguments: 
#' 
#' - `x`: a time series, 
#' - `tau`: a set of changepoint indices
#' - `...`: other arguments passed to methods
#' 
#' See [fit_meanshift_norm()], 
#' 
#' @family model-fitting
#' @returns A [fun_cpt] object.
#' @examples
#' # Register a model-fitting function
#' f <- fun_cpt("fit_meanvar")
#' 
#' # Verify that it now has class `fun_cpt`
#' str(f)
#' 
#' # Use it
#' f(CET, 42)
fun_cpt <- function(x, ...) {
  obj <- new_fun_cpt(x, ...)
  validate_fun_cpt(obj)
}

#' Recover the function that created a model
#' @param x A `character` giving the name of a model. To be passed to 
#' [model_name()].
#' @param ... currently ignored
#' @details
#' Model objects (inheriting from [mod_cpt]) know the name of the function
#' that created them. 
#' [whomademe()] returns that function. 
#' 
#' @returns A `function`
#' @family model-fitting
#' @export
#' @examples
#' # Get the function that made a model
#' f <- whomademe(fit_meanshift_norm(CET, tau = 42))
#' str(f)
#' 
whomademe <- function(x, ...) {
  paste0("fit_", model_name(x)) |>
    parse(text = _) |>
    eval()
}
