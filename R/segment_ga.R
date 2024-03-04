#' Segment a time series using a genetic algorithm
#' @export
#' @examples
#' res <- segment_ga(DataCPSim)
#' summary(res)
#' str(res)
#' plot(res)

segment_ga <- function(x, ...) {
  fun <- function(y) {
    -BMDL(fit_nhpp(x, y))
  }
  GA::ga(
    type = "real-valued", 
    fitness = fun,
    lower = 1, 
    upper = length(x),
    ...
  )
}


# res <- GA::ga(
#   type = "real-valued", 
#   fitness = function(x, data = mtcars) x * data$mpg,
#   lower = -20, 
#   upper = 20
# ) |> suppressWarnings()
# # the problem is that out doesn't have a "data" slot that stored the original data
# str(res)
# 
# # So I try to make one
# # ...using S4, the slot is not defined
# res@data <- mtcars
# # ...using S3, now I'm mixing and matching S3 with S4
# res$data <- mtcars
# # Define an extension class
#' @importClassesFrom GA ga
methods::setClass("tidyga", contains = "ga", slots = c(data = "ts"))
# out <- as(res, "myga")
# out@data <- mtcars$mpg
# str(out)

