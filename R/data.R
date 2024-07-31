#' Particulate matter in Bogotá, Colombia
#' @docType data
#' @description
#' Particulate matter of less than 2.5 microns of diameter in Bogotá, Colombia.
#' @details
#' Daily readings from 2018-2020 are included. 
#' 
#' @examples
#' class(bogota_pm)
#' 
"bogota_pm"

#' Rainfall in Medellín, Colombia
#' @docType data
#' @details
#' Daily rainfall measurements for 13 different weather stations positioned
#' around Medellín, Colombia. 
#' Variables:
#'  - `station_id`: 
#'  - `lat`, `long`: latitude and longitude for the weather station
#'  - `date`, `year`, `month`, `day`: date variables
#'  - `rainfall`: daily rainfall (in cubic centimeters) as measured by the weather station
#' @references [OpenStreetMap](https://www.openstreetmap.org/?mlat=6.244747&mlon=-75.574828&zoom=12)
"mde_rain"

#' @rdname mde_rain
#' @docType data
#' @details
#'   - `mean_rainfall`: average rainfall across all weather stations
#' 
"mde_rain_monthly"

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
#' @details
#' - `DataCPSim`: Simulated time series of the same length as [bogota_pm].
#' @seealso [bogota_pm]
#' 
"DataCPSim"

#' @rdname DataCPSim
#' @docType data
#' @description
#' Randomly-generated time series data, using the [stats::rlnorm()] function.
#' * For `rlnorm_ts_1`, there is one changepoint located at 826. 
#' * For `rlnorm_ts_2`, there are two changepoints, located at 366 and 731. 
#' * For `rlnorm_ts_3`, there are three changepoints, located at 548, 823, and 973. 
#' @seealso [stats::ts()], [test_set()]
#' @examples
#' plot(rlnorm_ts_1)
#' plot(rlnorm_ts_2)
#' plot(rlnorm_ts_3)
#' changepoints(rlnorm_ts_1)
#' 
"rlnorm_ts_1"

#' @rdname DataCPSim
"rlnorm_ts_2"

#' @rdname DataCPSim
"rlnorm_ts_3"

#' Differences between leagues in Major League Baseball
#' @docType data
#' @description
#' The difference in home runs hit per plate appearance between the 
#' American League and the National League from 1925 to 2022. 
"mlb_hrs"
