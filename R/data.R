#' Particulate matter in Bogotá, Colombia
#' @docType data
#' @description
#' Particulate matter of less than 2.5 microns of diameter in Bogotá, Colombia.
#' Daily readings from 2018-2020 are included. 
#' 
#' @examples
#' class(bogota_pm)
#' 
#' 
"bogota_pm"

#' Rainfall in Medellín, Colombia
#' @docType data
"medellin_rainfall"

#' Hadley Centre Central England Temperature
#' @docType data
#' @description
#' Mean annual temperatures in Central England
#' 
#' @details
#' The CET time series is perhaps the longest instrumental record of 
#' surface temperatures in the world, commencing in 1659 and 
#' spanning 362 years through 2020. The CET series is a benchmark
#' for European climate studies, as it is sensitive to atmospheric variability 
#' in the North Atlantic (Parker et al. 1992). This record has been previously 
#' analyzed for long-term changes (Plaut et al. 1995;
#' Harvey and Mills 2003; Hillebrand and Proietti 2017); however, to our 
#' knowledge, no detailed changepoint analysis of it has been previously 
#' conducted. The length of the CET record affords us the opportunity to 
#' explore a variety of temperature features.
#' @source <https://www.metoffice.gov.uk/hadobs/hadcet/>
#' @references 
#'   - Shi, et al. (2022, \doi{10.1175/JCLI-D-21-0489.1}), 
#'   - Parker, et al. (1992, \doi{10.1002/joc.3370120402})
"CET"

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

#' Differences between leagues in Major League Baseball
#' @docType data
"mlb_hrs"
