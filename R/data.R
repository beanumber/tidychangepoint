#' Mediciones
#' @docType data
#' @description
#' A short description...
#' 
"pm_25"

#' Simulated time series data
#' @docType data
#' @description
#' Randomly-generated time series data, using the [stats::rlnorm()] function.
#' * For `ts_1`, there is one changepoint located at 826. 
#' * For `ts_2`, there are two changepoints, located at 366 and 731. 
#' * For `ts_3`, there are three changepoints, located at 548, 823, and 973. 
#' @seealso [stats::ts()]
#' @examples
#' plot(rlnorm_ts_1)
#' plot(rlnorm_ts_2)
#' plot(rlnorm_ts_3)
#' 
"rlnorm_ts_1"

#' @rdname rlnorm_ts_1
"rlnorm_ts_2"

#' @rdname rlnorm_ts_1
"rlnorm_ts_3"

#' DataCPSim
#' @docType data
"DataCPSim"

#' Output from running genetic algorithm on DataCPSim
#' @docType data
"lista_AG"
