#' Pad and unpad changepoint sets with boundary points
#' @param tau a numeric vector of changepoint indices
#' @param n the length of the original time series
#' @details
#' If a time series contains \eqn{n} observations, we label them from 1 
#' to \eqn{n}. 
#' Neither the 1st point nor the \eqn{n}th point can be a changepoint, since the
#' regions they create on one side would be empty. 
#' However, for dividing the time series into non-empty segments, we start with 
#' 1, add \eqn{n+1}, and then divide the half-open interval \eqn{[1, n+1)} into
#' half-open subintervals that define the regions. 
#' 
#' [pad_tau()] ensures that 1 and \eqn{n+1} are included. 
#' 
#' [unpad_tau()] removes 1 and \eqn{n+1}, should they exist.
#' 
#' [is_valid_tau()] checks to see if the supplied set of changepoints is valid
#' 
#' [validate_tau()] removes duplicates and boundary values.
#' 
#' @returns 
#'   - [pad_tau()]: an `integer` vector that starts with 0 and ends in \eqn{n}.
#' @export
pad_tau <- function(tau, n) {
  if (!is_valid_tau(tau, n)) {
    tau <- tau[tau >= 1 & tau <= n]
  }
  unique(c(0, tau, n))
}

#' @rdname pad_tau
#' @param padded_tau Output from [pad_tau()]
#' @returns 
#'   - [unpad_tau()]: an `integer` vector stripped of its first and last entries.
#' @export
unpad_tau <- function(padded_tau) {
  padded_tau |>
    utils::head(-1) |>
    utils::tail(-1)
}

#' @rdname pad_tau
#' @export
#' @returns 
#'   - [is_valid_tau()]: a `logical` if all of the entries are between 2 and 
#'   \eqn{n-1}.
#' @examples
#' # Anything less than 2 is not allowed
#' is_valid_tau(0, length(DataCPSim))
#' is_valid_tau(1, length(DataCPSim))
#' 
#' # Duplicates are allowed
#' is_valid_tau(c(42, 42), length(DataCPSim))
#' is_valid_tau(826, length(DataCPSim))
#' 
#' # Anything greater than \eqn{n} (in this case 1096) is not allowed
#' is_valid_tau(1096, length(DataCPSim))
#' is_valid_tau(1097, length(DataCPSim))
#' 
is_valid_tau <- function(tau, n) {
  # the first and last points cannot be changepoints!
  all(tau %in% 2:(n-1))
}

#' @rdname pad_tau
#' @export
#' @returns 
#'   - [validate_tau()]: an `integer` vector with only the [base::unique()] 
#'   entries between 2 and \eqn{n-1}, inclusive.  
#' @examples
#' # Anything less than 2 is not allowed
#' validate_tau(0, length(DataCPSim))
#' validate_tau(1, length(DataCPSim))
#' validate_tau(826, length(DataCPSim))
#' 
#' # Duplicates are removed
#' validate_tau(c(826, 826), length(DataCPSim))
#' 
#' # Anything greater than \eqn{n} (in this case 1096) is not allowed
#' validate_tau(1096, length(DataCPSim))
#' validate_tau(1097, length(DataCPSim))
#' 
#' # Fix many problems
#' validate_tau(c(-4, 0, 1, 4, 5, 5, 824, 1096, 1097, 182384), length(DataCPSim))
#' 
validate_tau <- function(tau, n) {
  # the first and last points cannot be changepoints!
  tau[tau %in% 2:(n-1)] |>
    unique()
}


#' Convert changepoint sets to binary strings
#' @param x A binary string that encodes a changepoint set. See 
#' [GA::gabin_Population()].
#' @details
#' 
#' In order to use [GA::ga()] in a genetic algorithm, we need to encoude a 
#' changepoint set as a binary string. 
#' 
#' [binary2tau()] takes a binary string representation of a changepoint set and
#' converts it into a set of changepoint indices. 
#' 
#' @returns
#'   - [binary2tau()]: an `integer` vector
#'   
#' @export
#' @examples
#' # Recover changepoint set indices from binary strings
#' binary2tau(c(0, 0, 1, 0, 1))
#' binary2tau(round(runif(10)))
#' 
binary2tau <- function(x) {
  # tau.vec <- loc.ind * (1:N) # convert binary to CPT location
  which(x == 1)
}

#' @rdname binary2tau
#' @inheritParams pad_tau
#' @details
#' 
#' [tau2binary()] takes a set of changepoint indices the number of observations
#' in the time series and converts them into a binary string representation of
#' that changepoint set. 
#' 
#' @returns
#'   - [tau2binary()]: an `integer` vector of length `n`
#' 
#' @export
#' @examples
#' # Recover binary strings from changepoint set indices
#' tau2binary(c(7, 17), n = 24)
#' tau2binary(binary2tau(c(0, 0, 1, 1, 0, 1)), n = 6)
#' 
tau2binary <- function(tau, n) {
  out <- rep(0, times = n)
  out[tau] <- 1
  out
}

#' Convert changepoint sets to time indices
#' @inheritParams tau2binary
#' @param index Index of times, typically returned by [stats::time()]
#' @seealso [stats::time()], [as_year()]
#' @export
#' @returns
#'   - [tau2time()]: a `character` of time labels
#' @examples
#' # Recover the years from a set of changepoint indices
#' tau2time(c(42, 81, 330), index = as_year(time(CET)))
#' 
tau2time <- function(tau, index) {
  index[tau]
}

#' @rdname tau2time
#' @param cpts Time series observation labels to be converted to indices
#' @export
#' @returns
#'   - [time2tau()]: an `integer` vector of changepoint indices
#' @examples
#' # Recover the changepoint set indices from the years
#' time2tau(c(1700, 1739, 1988), index = as_year(time(CET)))
#' 
time2tau <- function(cpts, index) {
  match(cpts, index)
}

#' Use a changepoint set to break a time series into regions
#' @param x A numeric vector
#' @inheritParams pad_tau
#' @details
#' A changepoint set `tau` of length \eqn{k} breaks a time series of length 
#' \eqn{n} into \eqn{k+1} non-empty regions.
#' These non-empty regions can be defined by half-open intervals, starting with
#' 1 and ending with \eqn{n+1}. 
#' 
#' [cut_inclusive()] splits a set of indices into a [base::factor()] of 
#' half-open intervals
#' 
#' @returns 
#'   - [cut_inclusive()] a [base::factor()] of half-open intervals
#' 
#' @export
#' @examples
#' n <- length(CET)
#' 
#' # Return a factor of intervals
#' cut_inclusive(1:n, tau = pad_tau(c(42, 81, 330), n))
#' 
cut_inclusive <- function(x, tau) {
  cut(x, breaks = tau, include.lowest = TRUE, right = FALSE)
}

#' @rdname cut_inclusive
#' @details
#' [split_by_tau()] splits a time series into a named [base::list()] of numeric
#' vectors
#' @returns 
#'   - [split_by_tau()] a named [base::list()] of numeric
#' vectors
#' 
#' @export
#' @examples
#' # Return a list of observations
#' split_by_tau(DataCPSim, c(365, 826))
#' 
split_by_tau <- function(x, tau) {
  tau <- validate_tau(tau, n = length(x))
  idx <- cut_inclusive(1:length(x), pad_tau(tau, length(x)))
  split(x, idx)
}

#' Simulate time series with known changepoint sets
#' @param n Number of true changepoints in set
#' @param sd Standard deviation passed to [stats::rnorm()]
#' @param seed Value passed to [base::set.seed()]
#' @export
#' @returns A [stats::ts()] object
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

#' Retrieve the degrees of freedom from a `logLik` object
#' @param x An object that implements a method for [stats::logLik()].
#' @returns The `df` attribute of the [stats::logLik()] of the given object.
#' @export
#' @examples
#' # Retrieve the degrees of freedom model a changepoint model
#' DataCPSim |>
#'   segment() |>
#'   as.model() |>
#'   deg_free()
#'   
deg_free <- function(x) {
  attr(logLik(x), "df")
}

#' Convert a date into a year
#' @param x an object coercible into a [base::Date]. See [base::as.Date()].
#' @export
#' @returns A `character` vector representing the years of the input
#' @examples
#' # Retrieve only the year
#' as_year("1988-01-01")
#' 

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
#' @keywords internal
#' @returns A [stats::logLik()] vector.
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
#' @details
#' [is_model()] checks to see if a model object implements all of the 
#' S3 methods necessary to be considered a model. 
#' @return 
#'   - [is_model()] a `logical` vector of length 1
#' @examples
#' 
#' # Fit a model directly, without using [segment()]
#' x <- fit_nhpp(CET, tau = 330)
#' is_model(x)
is_model <- function(x, ...) {
  req <- c(common, mods_only)
  implements_all_methods(x, req)
}

#' @rdname as.segmenter
#' @export
#' @details
#' [is_segmenter()] checks to see if a segmenter object implements all of the 
#' S3 methods necessary to be considered a segmenter. 
#' @return 
#'   - [is_segmenter()] a `logical` vector of length 1
#' @examples
#' # Segment a time series using PELT
#' x <- segment(CET, method = "pelt")
#' 
#' # Return the segmenter component
#' x |>
#'   as.segmenter()
#'   
#' # Note the class of this object could be anything
#' x |>
#'   as.segmenter() |>
#'   class()
#'   
#' # Convert the segmenter into the standardized seg_cpt class
#' x |>
#'   as.segmenter() |>
#'   as.seg_cpt()
#' 
#' # Is the segmenter valid?
#' x |>
#'   as.segmenter() |>
#'   is_segmenter()
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
