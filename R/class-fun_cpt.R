#' Class for model-fitting functions
#' @export
#' @param x a character giving the name of a model-fitting function
#' @param ... currently ignored
#' @details
#' All model-fitting functions must take at least three arguments: 
#' `x` (a time series), 
#' `tau` (a set of changepoint indices), and the dots `...`. 
#' See [fit_meanshift_norm()], 
#' 
#' @family model-fitting
#' @examples
#' f <- fun_cpt("fit_meanvar")
#' str(f)
#' f(CET, 42)

new_fun_cpt <- function(x, ...) {
  f <- eval(parse(text = x))
  stopifnot(is.function(f))
  structure(
    f,
    model_name = gsub(pattern = "fit_", replacement = "", x),
    class = "fun_cpt"
  )
}

#' @rdname new_fun_cpt
#' @export
validate_fun_cpt <- function(x) {
  args <- methods::formalArgs(x)
  if (!all(c("x", "tau", "...") %in% args)) {
    stop("Model-fitting functions must have x, tau, and ... as arguments")
  }
  x
}

#' @rdname new_fun_cpt
#' @export
fun_cpt <- function(x, ...) {
  obj <- new_fun_cpt(x, ...)
  validate_fun_cpt(obj)
}

#' @rdname new_fun_cpt
#' @param x A `character` giving the name of a model. Typically the result of
#' [model_name()].
#' @return - [whomademe()]: A function
#' @family modeling
#' @export
#' @examples
#' f <- whomademe(fit_meanshift_norm(CET, tau = 42))
#' str(f)
whomademe <- function(x, ...) {
  paste0("fit_", model_name(x)) |>
    parse(text = _) |>
    eval()
}
