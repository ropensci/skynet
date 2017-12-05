#' Import Data
#'
#' Imports data from BTS/RITA/Transtats website
#' File order doesn't matter, but it is recomended to name the files using the following
#' syntax: \emph{"Coupon YearQuarter.csv", "Ticket YearQuarter.csv", "T100 Year".}
#' Note: We do recommend sparklyr to be used for larger sets of data.
#'
#' @param x First csv file to be imported, in case of DB1B database, or in case of using
#' the T-100 database, the only file to be included.
#' @param y Second csv file to be imported.
#' @param zip Should equal TRUE if original file comes from the BTS prezipped option.
#'
#' @examples
#' \dontrun{
#' # DB1B Database Files - Ticket and Coupon order doesn't matter
#' netImport("Coupon 2016Q1.csv", "Ticket 2016Q1.csv")
#'
#' # T-100
#' netImport("T100 2016.csv")
#' }
#'
#' @export

netImport <- function(x, y, zip = FALSE){

  if(grepl("T100", deparse(substitute(x)), ignore.case = TRUE) == TRUE){

    T100 <- fread(x, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                  integer64 = "numeric")

    T100 <- T100 %>%
      filter(AIRCRAFT_CONFIG == 1 | AIRCRAFT_CONFIG == 3, PASSENGERS > 0) %>%
      select(ORIGIN, ORIGIN_CITY_MARKET_ID, DEST, DEST_CITY_MARKET_ID, UNIQUE_CARRIER,
             PASSENGERS, QUARTER, YEAR, DISTANCE) %>%
      rename(origin_mkt_id = ORIGIN_CITY_MARKET_ID, origin = ORIGIN, year = YEAR, quarter = QUARTER,
             dest_mkt_id = DEST_CITY_MARKET_ID , dest = DEST,
             op_carrier = UNIQUE_CARRIER, distance = DISTANCE, passengers = PASSENGERS)

    T100 <- data.frame(T100)

    #extracts name from file
    filename <- gsub(" ", "",
                     tools::file_path_sans_ext(
                       basename(x)))
    assign(paste(filename), T100, envir = envir)

  }else{

    if(zip == FALSE){
      do.call(DB1BImport, list(x,y))
    }else{
      do.call(DB1BRaw, list(x,y))
    }
  }
}




# netImport function
DB1BImport <- function(x, y){

  if(grepl("Ticket", deparse(substitute(x)), ignore.case = TRUE) == TRUE)
    t = x
  if(grepl("Ticket", deparse(substitute(y)), ignore.case = TRUE) == TRUE)
    t = y
  if(grepl("Coupon", deparse(substitute(x)), ignore.case = TRUE) == TRUE)
    c = x
  if(grepl("Coupon", deparse(substitute(y)), ignore.case = TRUE) == TRUE)
    c = y

  # Import Coupon file
  # Include ItinID, MktID, Origin, Dest, SeqNum,
  #OriginCityMarketID, DestCityMarketID and Trip Break (8 variables)
  coupon <- fread(c, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                  integer64 = "numeric")
  coupon <- coupon %>%
    select(-grep("V", names(coupon))) %>%
    rename(itin_id = ITIN_ID, mkt_id = MKT_ID, seq_num = SEQ_NUM,
           origin_mkt_id = ORIGIN_CITY_MARKET_ID, origin = ORIGIN, year = YEAR, quarter = QUARTER,
           dest_mkt_id = DEST_CITY_MARKET_ID , dest = DEST, trip_break = TRIP_BREAK,
           op_carrier = OPERATING_CARRIER, distance = DISTANCE, gateway = GATEWAY)

  # Import Ticket file
  # Include ItinID, RoundTrip, Passengers and ItinFare (4 variables)
  ticket <- fread(t, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                  integer64 = "numeric")
  ticket <- ticket %>%
    select(-grep("V", names(ticket))) %>%
    rename(itin_id = ITIN_ID, roundtrip = ROUNDTRIP, itin_yield = ITIN_YIELD, passengers = PASSENGERS,
             itin_fare = ITIN_FARE, bulk_fare = BULKFARE, distance_full = DISTANCE_FULL)

  #Merge data
  netMerged <- merge(coupon, ticket, by = "itin_id", all.x = TRUE)
  netMerged <- data.frame(netMerged)

  # Get name from file
  filename <- gsub(" ", "",
                   tools::file_path_sans_ext(
                     basename(x)))
  filename <- substring(filename, (nchar(filename)-5))
  assign(paste("OD",filename, sep = "_"), netMerged, envir = envir)

}



DB1BRaw <- function(x,y){


  if(grepl("Ticket", deparse(substitute(x)), ignore.case = TRUE) == TRUE)
    t = x
  if(grepl("Ticket", deparse(substitute(y)), ignore.case = TRUE) == TRUE)
    t = y
  if(grepl("Coupon", deparse(substitute(x)), ignore.case = TRUE) == TRUE)
    c = x
  if(grepl("Coupon", deparse(substitute(y)), ignore.case = TRUE) == TRUE)
    c = y


  ticket <- fread(t, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                       integer64 = "numeric")

  ticket <- ticket %>%
    select("ItinID", "RoundTrip", "FarePerMile", "Passengers", "ItinFare", "BulkFare", "Distance") %>%
    rename(itin_id = ItinID, roundtrip = RoundTrip, itin_yield = FarePerMile, passengers = Passengers,
           itin_fare = ItinFare, bulk_fare = BulkFare, distance_full = Distance)

  coupon <- fread(c, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                       integer64 = "numeric")

  coupon <- coupon %>%
    select("ItinID", "MktID", "SeqNum",
           "OriginCityMarketID", "Origin",
           "DestCityMarketID", "Dest", "Break",
           "OpCarrier", "Distance","Year", "Quarter", "Gateway") %>%
    rename(itin_id = ItinID, mkt_id = MktID, seq_num = SeqNum,
           origin_mkt_id = OriginCityMarketID, origin = Origin, year = Year, quarter = Quarter,
           dest_mkt_id = DestCityMarketID , dest = Dest, trip_break = Break,
           op_carrier = OpCarrier, distance = Distance, gateway = Gateway)

  #Merge data
  netMerged <- merge(coupon, ticket, by = "itin_id", all.x = TRUE)
  netMerged <- data.frame(netMerged)

  # Get name from file
  filename <- gsub(" ", "",
                   tools::file_path_sans_ext(
                     basename(x)))
  filename <- substring(filename, (nchar(filename)-5))
  filename <- gsub("_", "Q", filename)
  assign(paste("OD",filename, sep = "_"), netMerged, envir = envir)

}

globalVariables(c("ITIN_ID", "MKT_ID", "SEQ_NUM", "YEAR", "QUARTER", "TRIP_BREAK",
                  "OPERATING_CARRIER", "DISTANCE", "GATEWAY", "ROUNDTRIP", "ITIN_YIELD",
                  "ITIN_FARE", "BULKFARE", "DISTANCE_FULL", "UNIQUE_CARRIER",
                  "UNIQUE_CARRIER_NAME", "MONTH", "AIRCRAFT_CONFIG"))

pos = 1
envir = as.environment(pos)

#--------------------------------------------------------------------------------
                              # END OF SCRIPT
#--------------------------------------------------------------------------------
