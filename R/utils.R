#' Utility functions
#' @param tau a numeric vector of changepoints
#' @param n the length of the original time series
#' @export
pad_tau <- function(tau, n) {
  if (!is_valid_tau(tau, n)) {
    tau <- tau[tau >= 1 & tau <= n]
  }
  unique(c(0, tau, n))
}

#' @rdname pad_tau
#' @param padded_tau Output from [pad_tau()]
#' @export
unpad_tau <- function(padded_tau) {
  padded_tau |>
    utils::head(-1) |>
    utils::tail(-1)
}

#' @rdname pad_tau
#' @export
#' @examples
#' is_valid_tau(0, length(DataCPSim))
#' is_valid_tau(1, length(DataCPSim))
#' is_valid_tau(826, length(DataCPSim))
#' is_valid_tau(1096, length(DataCPSim))
#' is_valid_tau(1097, length(DataCPSim))
#' 
is_valid_tau <- function(tau, n) {
  # the first and last points cannot be changepoints!
  all(tau %in% 2:(n-1))
}

#' @rdname pad_tau
#' @export
#' @examples
#' validate_tau(0, length(DataCPSim))
#' validate_tau(1, length(DataCPSim))
#' validate_tau(826, length(DataCPSim))
#' validate_tau(c(826, 826), length(DataCPSim))
#' validate_tau(1096, length(DataCPSim))
#' validate_tau(1097, length(DataCPSim))
#' validate_tau(c(-4, 0, 1, 4, 5, 5, 824, 1096, 1097, 182384), length(DataCPSim))
#' 
validate_tau <- function(tau, n) {
  # the first and last points cannot be changepoints!
  tau[tau %in% 2:(n-1)] |>
    unique()
}


#' @rdname pad_tau
#' @export
#' @examples
#' binary2tau(c(0, 0, 1, 0, 1))
#' binary2tau(round(runif(10)))
#' 
binary2tau <- function(x) {
  # tau.vec <- loc.ind * (1:N) # convert binary to CPT location
  which(x == 1)
}

#' @rdname pad_tau
#' @export
#' @examples
#' tau2binary(c(7, 17), n = 24)
#' tau2binary(binary2tau(c(0, 0, 1, 1, 0, 1)), n = 6)
tau2binary <- function(tau, n) {
  out <- rep(0, times = n)
  out[tau] <- 1
  out
}

#' @rdname pad_tau
#' @param index Index of times, typically returned by [stats::time()]
#' @seealso [stats::time()]
#' @export
#' @examples
#' tau2time(c(42, 81, 330), index = as_year(time(CET)))
tau2time <- function(tau, index) {
  index[tau]
}

#' @rdname pad_tau
#' @param cpts Time series observation labels to be converted to indices
#' @export
#' @examples
#' time2tau(c(1700, 1739, 1988), index = as_year(time(CET)))
time2tau <- function(cpts, index) {
  match(cpts, index)
}


#' @rdname pad_tau
#' @param x A numeric vector
#' @export
cut_inclusive <- function(x, tau) {
  cut(x, breaks = tau, include.lowest = TRUE, right = FALSE)
}

#' Simulate time series with known changepoint sets
#' @param n Number of true changepoints in set
#' @param sd Standard deviation passed to [stats::rnorm()]
#' @param seed Value passed to [base::set.seed()]
#' @export
#' @seealso [DataCPSim]
#' @examples
#' x <- test_set()
#' plot(x)
#' changepoints(x)
test_set <- function(n = 1, sd = 1, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  num_obs <- 1000
  tau <- sample.int(n = num_obs, size = n) |>
    sort()
  means <- sample.int(n = 100, size = n + 1)
  
  region_lengths <- tau |>
    pad_tau(num_obs) |>
    diff()
  
  out <- purrr::map2(region_lengths, means, ~rnorm(.x, mean = .y, sd = sd)) |>
    c(recursive = TRUE) |>
    stats::as.ts()
  attr(out, "cpt_true") <- tau
  return(out)
}

#' @rdname pad_tau
#' @export
#' @examples
#' split_by_tau(DataCPSim, c(365, 826))

split_by_tau <- function(x, tau) {
  idx <- cut_inclusive(1:length(x), pad_tau(tau, length(x)))
  split(x, idx)
}

#' @rdname pad_tau
#' @export
#' @examples
#' regions_by_tau(1096, c(365, 826))

regions_by_tau <- function(n, tau) {
  cut_inclusive(1:n, pad_tau(tau, n)) |>
    levels()
}

#' @rdname pad_tau
#' @export
#' @examples
#' deg_free(segment(DataCPSim)$model)

deg_free <- function(x) {
  attr(logLik(x), "df")
}

#' @rdname pad_tau
#' @export
#' @examples
#' as_year("1988-01-01")

as_year <- function(x) {
  x |> 
    as.Date() |>
    format("%Y")
}

#' Vectors implementation for logLik
#' 
#' @export
#' @inheritParams vctrs::vec_ptype2
#' @seealso [stats::logLik()]
#' @examples
#' a <- logLik(lm(mpg ~ disp, data = mtcars))
#' b <- logLik(lm(mpg ~ am, data = mtcars))
#' vec_ptype2(a, b)
#' c(a, b)
#' vec_cast(a, b)
vec_ptype2.logLik.logLik <- function(x, y, ...) {
  x
}

#' @rdname vec_ptype2.logLik.logLik
#' @inheritParams vctrs::vec_cast
#' @export
vec_cast.logLik.logLik <- function(x, to, ...) {
  x
}

#' @rdname as.model
#' @param x An object, typically returned by `fit_*()`
#' @export
#' @examples
#' x <- fit_nhpp(CET, tau = 330)
#' is_model(x)
is_model <- function(x, ...) {
  req <- c(common, mods_only)
  implements_all_methods(x, req)
}

#' @rdname as.model
#' @export
#' @examples
#' x <- segment(CET, method = "pelt")
#' is_segmenter(x$segmenter)
is_segmenter <- function(object, ...) {
  req <- c(common, segs_only)
  implements_all_methods(object, req)
}

get_all_methods <- function(object) {
  if (isS4(object)) {
    classes <- object |>
      class() |>
      methods::extends()
  } else {
    classes <- object |>
      class() 
  }
  classes |>
    purrr::map(~methods(class = .x)) |>
    purrr::map(attr, "info") |>
    purrr::list_rbind() |>
    dplyr::filter(!isS4) |>
    dplyr::pull("generic") |>
    unique()
}

implements_all_methods <- function(object, required_methods, ...) {
  available <- object |>
    get_all_methods()
  
  missing <- setdiff(required_methods, available)
  
  if (length(missing) > 0) {
    message(paste("No methods for:"), missing)
    return(FALSE)
  } else {
    return(TRUE)
  }
}

common <- c("as.ts", "changepoints", "model_name", "nobs")
segs_only <- c("fitness", "model_args", "seg_params")
mods_only <- c("augment", "coef", "fitted", "glance", "logLik", "plot", "residuals", "tidy")
