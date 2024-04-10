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

#' @rdname pad_tau
#' @param n Number of changepoints
#' @param sd Standard deviation passed to [stats::rnorm()]
#' @param seed Value passed to [base::set.seed()]
#' @export
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
    as.ts()
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
#' regions_by_tau(n, c(365, 826))

regions_by_tau <- function(n, tau) {
  cut_inclusive(1:n, pad_tau(tau, n)) |>
    levels()
}


#' @rdname pad_tau
#' @export
#' @examples
#' ds <- data.frame(y = as.ts(CET), t = 1:length(CET))
#' tbl_coef(lm(y ~ 1, data = ds))
#' tbl_coef(lm(y ~ (t >= 42) + (t >= 81), data = ds))
#' tbl_coef(lm(y ~ t * (t >= 42) + t * (t >= 81), data = ds))

tbl_coef <- function(mod, ...) {
  out <- mod |>
    stats::coef() |>
    tibble::enframe(name = "variable", value = "value") |>
    dplyr::mutate(
      region = stringr::str_extract(variable, pattern = "t >= [0-9]+"),
      is_slope = grepl(pattern = "^t$|t:t", variable)
    ) |>
    dplyr::select(-variable) |>
    tidyr::pivot_wider(names_from = "is_slope", values_from = "value") |>
    dplyr::mutate(
      tau = stringr::str_extract(region, pattern = "[0-9]+$") |>
        as.integer(),
      tau = ifelse(is.na(tau), 0, tau),
      mu = cumsum(`FALSE`)
    )
  if ("TRUE" %in% names(out)) {
    out <- out |>
      dplyr::mutate(
        beta = cumsum(`TRUE`),
        mu = mu + tau * beta
      )
  }
  vars <- c("region", "mu", "beta")
  out |>
    dplyr::select(any_of(vars))
}
    
#' @rdname pad_tau
#' @export
#' @examples
#' deg_free(segment(DataCPSim)$nhpp)

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

#' @rdname pad_tau
#' @export
#' @examples
#' whoami(fit_meanshift)
#' whoami(fit_meanshift_ar1)
#' whoami(fit_lmshift)
#' whoami(fit_nhpp)

whoami <- function(x = fit_meanshift, ...) {
  attr(x, "model_name")
}

#' @rdname pad_tau
#' @export
model_fit <- function(object, ...) {
  paste0("fit_", model_name(object)) |>
    parse(text = _) |>
    eval()
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
