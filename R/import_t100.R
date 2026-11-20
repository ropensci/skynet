#' Import T-100 Data
#'
#' Imports T-100 Data directly from BTS/RITA/Transtats website raw data (prezipped file),
#' for SKYNET's import function.
#'
#' Files can be found here \url{https://www.transtats.bts.gov/Fields.asp?gnoyr_VQ=FIL}.
#' More information on variables to select and type of files to use can be found \href{https://github.com/ropensci/skynet}{here}
#'
#' @param x T-100 csv
#' @param nonsch Should equal TRUE to include non-scheduled flights
#' @param auto Automatically assigns object
#'
#' @return A data frame with one row per route, carrier and month: origin and
#' destination airports and city market ids, carrier, distance, passengers, year,
#' quarter and month (plus seats, departures and aircraft type for segment files).
#' With \code{auto = TRUE} it is assigned to the global environment as
#' \code{T100_year_mkt} or \code{T100_year_seg} (e.g. \code{T100_2011_mkt}) and
#' returned invisibly, otherwise it is returned.
#'
#' @examples
#' import_t100(skynet_example("T100_2011_mkt.csv"), auto = FALSE)
#'
#' @export
#'


import_t100 <- function(x, nonsch = FALSE, auto = TRUE){

  T100 <- fread(x, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                integer64 = "numeric")

  if("AIRCRAFT_CONFIG" %in% colnames(T100)){

    # Segment
      if(nonsch == FALSE){
      T100 <- T100 %>%
        filter(CLASS == "F", PASSENGERS > 0) %>%
        select(origin_mkt_id = ORIGIN_CITY_MARKET_ID,
               origin = ORIGIN, year = YEAR, quarter = QUARTER,
               dest_mkt_id = DEST_CITY_MARKET_ID , dest = DEST,
               op_carrier = UNIQUE_CARRIER, distance = DISTANCE,
               passengers = PASSENGERS, airtime_avg = AIR_TIME,
               month = MONTH, departures = DEPARTURES_PERFORMED,
               seats = SEATS, aircraft_type = AIRCRAFT_TYPE) %>%
        mutate(itin_fare = NA, itin_yield = NA, roundtrip = NA,
               airtime_avg = round(airtime_avg/departures)) %>%
        left_join(., skynet::aircraft_type) %>%
        select(-aircraft_type, aircraft_type = Description)

      }else{

      T100 <- T100 %>%
        filter(CLASS == "F" | CLASS == "L", PASSENGERS > 0) %>%
        select(origin_mkt_id = ORIGIN_CITY_MARKET_ID,
               origin = ORIGIN, year = YEAR, quarter = QUARTER,
               dest_mkt_id = DEST_CITY_MARKET_ID , dest = DEST,
               op_carrier = UNIQUE_CARRIER, distance = DISTANCE,
               passengers = PASSENGERS, airtime_avg = AIR_TIME,
               month = MONTH, departures = DEPARTURES_PERFORMED,
               seats = SEATS, aircraft_type = AIRCRAFT_TYPE) %>%
        mutate(itin_fare = NA, itin_yield = NA, roundtrip = NA,
               airtime_avg = round(airtime_avg/departures)) %>%
        left_join(., aircraft_type) %>%
        select(-aircraft_type, aircraft_type = Description)
    }

    T100 <- data.frame(T100)

    if (auto == TRUE){
      assign(paste("T100_", as.character(T100$year)[1],
                   "_seg", sep = ""), T100, envir = envir)
    }else{
      return(T100)
    }

  }else{

    # Market
    if(nonsch == FALSE){
      T100 <- T100 %>%
        filter(CLASS == "F", PASSENGERS > 0) %>%
        select(origin_mkt_id = ORIGIN_CITY_MARKET_ID,
               origin = ORIGIN, year = YEAR, quarter = QUARTER,
               dest_mkt_id = DEST_CITY_MARKET_ID , dest = DEST,
               op_carrier = UNIQUE_CARRIER, distance = DISTANCE,
               passengers = PASSENGERS, month = MONTH) %>%
        mutate(itin_fare = NA, itin_yield = NA, roundtrip = NA)

       }else{

         T100 <- T100 %>%
        filter(CLASS == "F" | CLASS == "L", PASSENGERS > 0) %>%
        select(origin_mkt_id = ORIGIN_CITY_MARKET_ID,
               origin = ORIGIN, year = YEAR, quarter = QUARTER,
               dest_mkt_id = DEST_CITY_MARKET_ID , dest = DEST,
               op_carrier = UNIQUE_CARRIER, distance = DISTANCE,
               passengers = PASSENGERS, month = MONTH) %>%
        mutate(itin_fare = NA, itin_yield = NA, roundtrip = NA)
    }



    T100 <- data.frame(T100)

    if (auto == TRUE){
      assign(paste("T100_", as.character(T100$year)[1],
                   "_mkt", sep = ""), T100, envir = envir)
    }else{
      return(T100)
    }


  }
}

globalVariables(c("AIR_TIME", "airtime_avg", "T100", "MONTH", "month",
                  "DEPARTURES_PERFORMED", "departures",
                  "AIRCRAFT_TYPE", "aircraft_type",
                  "SEATS", "seats",
                  "PASSENGERS", "passengers", "Description"))

# For CRAN submission
pos <- 1
envir <- as.environment(pos)
