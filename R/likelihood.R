#' Likelihood
#' @export
#' @examples
#' ds <- as.data.frame(rlnorm_ts_1)
#' ds["t"] <- 1:length(rlnorm_ts_1)
#' mod <- lm(x ~ 1, data = ds)
#' mu <- coef(mod)
#' sigma <- sqrt(mean(mod$residuals^2))
#' logLik(mod)
#' log_likelihood_norm(mod)
#' log_likelihood_norm_param(rlnorm_ts_1, mean = coef(mod), sd = sqrt(mean(mod$residuals^2)))
#' likelihood_dnorm(rlnorm_ts_1, mean = mu, sd = sigma)
#' 
#' mod_cp <- lm(x ~ t > 826, data = ds)
#' logLik(mod_cp)
#' logLik_cp(rlnorm_ts_1, 634)
#' logLik_cp(rlnorm_ts_1, 826)
#' cp <- best_cp(rlnorm_ts_1)
#' plot(1:length(rlnorm_ts_1), cp)
#' plot(1:length(rlnorm_ts_2), best_cp(rlnorm_ts_2))
#' plot(1:length(rlnorm_ts_3), best_cp(rlnorm_ts_3))

log_likelihood_norm <- function(mod) {
  sigma <- sqrt(mean(mod$residuals^2))
  n <- length(mod$residuals)
  a <- -(n / 2) * log(2 * pi)
  b <- -(n / 2) * log(sigma^2)
  c <- -(1 / (2 * sigma^2)) * sum(mod$residuals^2)
  return(a + b + c)
}

log_likelihood_norm_param <- function(x, mean, sd) {
  n <- length(x)
  a <- -(n / 2) * log(2 * pi)
  b <- -(n / 2) * log(sd^2)
  c <- -(1 / (2 * sd^2)) * sum((x - mean)^2)
  return(a + b + c)
}

likelihood_dnorm <- function(x, mean, sd) {
  prod(stats::dnorm(x, mean, sd))
}

#' @rdname log_likelihood_norm
#' @export

logLik_cp <- function(x, i = 1) {
  n <- length(x)
  ds <- data.frame(x, t = 1:n)
  form <- stats::as.formula(paste("x ~ (t > ", i, ")"))
  mod <- stats::lm(form, data = ds)
  stats::logLik(mod)
}

#' @rdname log_likelihood_norm
#' @export

best_cp <- function(x) {
  i <- 1:length(x)
  lapply(i, logLik_cp, x = x) |>
    unlist()
}
