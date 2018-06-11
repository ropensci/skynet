#' Convert Raw
#'
#' Converts DB1B files from BTS/RITA/Transtats website raw data (prezipped file), for SKYNET's import function.
#'
#' Coupon files can be found at \url{https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=289}.
#' Ticket files can be found at \url{https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=272}.
#' Both files should belong to the same year and same quarter.
#' More information on variables to select and type of files to use can be found \href{https://github.com/FilipeamTeixeira/skynet}{here}
#'
#' @param x Coupon csv file to be processed
#' @param y Ticket csv file to be processed
#' @param path Path to save file to
#'
#' @examples
#' \dontrun{
#'
#' temp <- tempdir()
#' convertRaw(skynet_example("Origin_and_Destination_Survey_DB1BCoupon_2001_1.csv"),
#' skynet_example("Origin_and_Destination_Survey_DB1BTicket_2001_1.csv"),
#' path = temp)
#'
#' }
#'
#' @export
#'

convertRaw <- function(c,t,path = NULL){

  if(missing(path)){
    stop("Please specify the path argument.")
  }

  Coupon_temp <- fread(c, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                       integer64 = "numeric")

  Coupon_temp <- Coupon_temp %>%
    select("ItinID", "MktID", "SeqNum",
           "OriginCityMarketID", "Origin",
           "DestCityMarketID", "Dest", "Break",
           "OpCarrier", "Distance","Year", "Quarter", "Gateway") %>%
    rename(ITIN_ID = ItinID, MKT_ID = MktID, SEQ_NUM = SeqNum,
           ORIGIN_CITY_MARKET_ID = OriginCityMarketID, ORIGIN = Origin,
           YEAR = Year, QUARTER = Quarter,
           DEST_CITY_MARKET_ID = DestCityMarketID,
           DEST = Dest, TRIP_BREAK = Break,
           OPERATING_CARRIER = OpCarrier,
           DISTANCE = Distance, GATEWAY = Gateway)

  Ticket_temp <- fread(t, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                       integer64 = "numeric")

  Ticket_temp <- Ticket_temp %>%
    select("ItinID", "RoundTrip", "FarePerMile", "Passengers",
           "ItinFare", "BulkFare", "Distance") %>%
    rename(ITIN_ID = ItinID, ROUNDTRIP = RoundTrip,
           ITIN_YIELD = FarePerMile, PASSENGERS = Passengers,
           ITIN_FARE = ItinFare, BULKFARE = BulkFare, DISTANCE_FULL = Distance)


  write.csv(Ticket_temp,
            file = paste(path, "Ticket"," ", Coupon_temp$YEAR[1],
                         "Q", Coupon_temp$QUARTER[1], ".csv", sep = ""),
            row.names=FALSE)
  write.csv(Coupon_temp,
            file = paste(path, "Coupon", " ", Coupon_temp$YEAR[1],
                         "Q", Coupon_temp$QUARTER[1], ".csv", sep = ""),
            row.names=FALSE)


}

globalVariables(c("ItinID", "RoundTrip", "FarePerMile",
                  "Passengers", "ItinFare", "BulkFare",
                  "Distance", "MktID", "SeqNum", "OriginCityMarketID",
                  "Origin", "Year", "Quarter", "DestCityMarketID",
                  "Dest", "Break", "OpCarrier", "Gateway"))
