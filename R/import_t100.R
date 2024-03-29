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
#' @examples
#' \dontrun{
#'
#' import_t100(skynet_example("T100_2011_mkt.csv"))
#'
#' }
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

    assign(paste("T100_", as.character(T100$year)[1],
                 "_seg", sep = ""), T100, envir = envir)

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
