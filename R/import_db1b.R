#' Import Data from DB1B files
#'
#' Imports data from BTS/RITA/Transtats files
#'
#' Coupon files can be found at \url{https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=289}.
#' Ticket files can be found at \url{https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=272}.
#' Both files should belong to the same year and same quarter.
#' \strong{Note}: We do recommend sparklyr to be used for larger sets of data.
#' More information on variables to select and type of files to use can be found \href{https://github.com/ropensci/skynet}{here}
#'
#' @param c Coupon csv file to be imported, in case of DB1B database
#' @param t Ticket csv file to be imported, in case of DB1B database
#' @param zip Should equal TRUE if original file comes from the BTS prezipped option.
#' @param auto Automatically assigns object
#'
#' @examples
#' \dontrun{
#'
#' import_db1b(skynet_example("Coupon_2001Q1.csv"), skynet_example("Ticket_2001Q1.csv"))
#'
#' }
#' @export

import_db1b <- function(c, t, zip = FALSE, auto = TRUE){

    if(zip == FALSE){
      do.call(ODImport, list(c, t, auto))
    }else{
      do.call(ODRaw, list(c, t, auto))
    }
  }


# netImport function
ODImport <- function(c, t, auto = TRUE){

  # Import Coupon file
  coupon <- fread(c, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                  integer64 = "numeric")
  coupon <- coupon %>%
    select(-grep("V", names(coupon))) %>%
    rename(itin_id = ITIN_ID, mkt_id = MKT_ID, seq_num = SEQ_NUM,
           origin_mkt_id = ORIGIN_CITY_MARKET_ID,
           origin = ORIGIN, year = YEAR, quarter = QUARTER,
           dest_mkt_id = DEST_CITY_MARKET_ID, dest = DEST,
           trip_break = TRIP_BREAK, op_carrier = OPERATING_CARRIER,
           distance = DISTANCE, gateway = GATEWAY)

  # Import Ticket file
  ticket <- fread(t, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                  integer64 = "numeric")
  ticket <- ticket %>%
    select(-grep("V", names(ticket))) %>%
    rename(itin_id = ITIN_ID, roundtrip = ROUNDTRIP,
           itin_yield = ITIN_YIELD, passengers = PASSENGERS,
             itin_fare = ITIN_FARE, bulk_fare = BULKFARE,
           distance_full = DISTANCE_FULL)

  #Merge data
  netMerged <- merge(coupon, ticket, by = "itin_id", all.x = TRUE)
  netMerged <- netMerged %>%
    data.frame() %>% #Convert to data.frame for data.table/dplyr compatibility
    filter(distance != 0) #To clean data imput inconsistencies


  if (auto == TRUE){
    assign(paste("OD_", as.character(coupon$year)[1], "Q",
                 as.character(coupon$quarter)[1], sep = ""),
           netMerged, envir = envir)
  }else{
    return(netMerged)
  }


}



ODRaw <- function(c,t, auto = TRUE){

  coupon <- fread(c, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                       integer64 = "numeric")

  coupon <- coupon %>%
    select(itin_id = ItinID, mkt_id = MktID, seq_num = SeqNum,
           origin_mkt_id = OriginCityMarketID,
           origin = Origin, year = Year, quarter = Quarter,
           dest_mkt_id = DestCityMarketID , dest = Dest, trip_break = Break,
           op_carrier = OpCarrier, distance = Distance, gateway = Gateway)


  ticket <- fread(t, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                  integer64 = "numeric")

  ticket <- ticket %>%
    select(itin_id = ItinID, roundtrip = RoundTrip,
           itin_yield = FarePerMile, passengers = Passengers,
           itin_fare = ItinFare, bulk_fare = BulkFare,
           distance_full = Distance)


  #Merge data
  netMerged <- merge(coupon, ticket, by = "itin_id", all.x = TRUE)
  netMerged <- netMerged %>%
    data.frame() %>% #Convert to data.frame for data.table/dplyr compatibility
    filter(distance != 0) #To clean data imput inconsistencies


  if (auto == TRUE){
    assign(paste("OD_", as.character(coupon$year)[1], "Q",
                 as.character(coupon$quarter)[1], sep = ""),
           netMerged, envir = envir)
  }else{
    return(netMerged)
  }

}

globalVariables(c("ITIN_ID", "MKT_ID", "SEQ_NUM", "YEAR", "QUARTER",
                  "TRIP_BREAK", "OPERATING_CARRIER", "DISTANCE",
                  "GATEWAY", "ROUNDTRIP", "ITIN_YIELD",
                  "ITIN_FARE", "BULKFARE", "DISTANCE_FULL", "UNIQUE_CARRIER",
                  "UNIQUE_CARRIER_NAME", "MONTH", "REPORTING_CARRIER",
                  "AIRCRAFT_CONFIG", "import_db1b", "CLASS", "auto",
                  "ItinID", "RoundTrip", "FarePerMile",
                  "Passengers", "ItinFare", "BulkFare",
                  "Distance", "MktID", "SeqNum", "OriginCityMarketID",
                  "Origin", "Year", "Quarter", "DestCityMarketID",
                  "Dest", "Break", "OpCarrier", "Gateway", "RPCarrier"))


pos <- 1
envir <- as.environment(pos)

#--------------------------------------------------------------------------------
                              # END OF SCRIPT
#--------------------------------------------------------------------------------
