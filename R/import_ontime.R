#' Import on-time Data
#'
#' Imports on-time Data directly from BTS/RITA/Transtats website raw data (prezipped file),
#' for SKYNET's import function.
#'
#' Files can be found here \url{https://www.transtats.bts.gov/Fields.asp?gnoyr_VQ=FGJ}.
#' More information on variables to select and type of files to use can be found \href{https://github.com/ropensci/skynet}{here}
#'
#' @param x On-time csv (from zipped file)
#' @param auto Automatically assigns object
#'
#' @return A data frame (data.table) with one row per flight: origin and
#' destination airports and city market ids, date, carrier, tail and flight
#' numbers, scheduled and actual departure and arrival times, delays (by cause),
#' taxi and air times, cancellations, diversions and distance.
#' With \code{auto = TRUE} it is assigned to the global environment as
#' \code{ontime_year_month} (e.g. \code{ontime_2011_1}) and returned invisibly,
#' otherwise it is returned.
#'
#' @examples
#' import_ontime(skynet_example("Ontime_2011_1.csv"), auto = FALSE)
#'
#' @export
#'

import_ontime <- function(x, auto = TRUE){

  ontime <- data.table::fread(x, header = TRUE)
  ontime <- ontime %>%
    select(origin = Origin, dest = Dest,
           year = Year, month = Month, flightdate = FlightDate, tail_number = Tail_Number,
           flight_number = Flight_Number_Reporting_Airline, op_carrier = Reporting_Airline,
           origin_mkt_id = OriginCityMarketID, dest_mkt_id = DestCityMarketID,
           sch_dep = CRSDepTime, dep_time = DepTime, dep_delay = DepDelay,
           taxi_out = TaxiOut, taxi_in = TaxiIn,
           sch_arr = CRSArrTime, arr_time = ArrTime, arr_delay = ArrDelay,
           cancelled = Cancelled, diverted = Diverted, airtime = AirTime,
           distance = Distance, carrier_delay = CarrierDelay,
           weather_delay = WeatherDelay, nas_delay = NASDelay,
           security_delay = SecurityDelay)

  if (auto == TRUE) {
    assign(paste("ontime_", as.character(ontime$year[1]), "_",
                 as.character(ontime$month[1]), sep = ""),
           ontime, envir = envir)
  }
  else {
    return(ontime)
  }
}


globalVariables(c("ontime_path", "Month", "FlightDate", "Tail_number",
                  "Flight_Number_Reporting_Airline", "Reporting_Airline",
                  "CRSDepTime", "DepDelay", "TaxiOut", "TaxiIn", "CRSArrTime",
                  "ArrTime", "ArrDelay", "Cancelled", "Diverted", "AirTime",
                  "CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay",
                  "import_t100", "Tail_Number", "DepTime"))

# For CRAN submission
pos <- 1
envir <- as.environment(pos)

