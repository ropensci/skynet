#' Convert Raw
#'
#' Converts DB1B files from BTS/RITA/Transtats website raw data (prezipped file), for SKYNET's import function.
#'
#' Reduces DB1B raw filesize, to an adequate SKYNET compatible format. This function will create a csv file for you.
#' File order doesn't matter, but it is recomended to name the files using the following
#' syntax: \emph{"Origin_and_Destination_Survey_DB1BCoupon_year_quarter.csv", "Origin_and_Destination_Survey_DB1BTicket_year_quarter.csv".}
#' Note: We do recommend sparklyr to be used for larger sets of data.
#'
#' @param x First csv file to be processed
#' @param y Second csv file to be processed
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

convertRaw <- function(x,y,path = NULL){

  if(missing(path)){
    stop("Please specify the path argument.")
  }

  if(grepl("Ticket", deparse(substitute(x)), ignore.case = TRUE) == TRUE)
    t = x
  if(grepl("Ticket", deparse(substitute(y)), ignore.case = TRUE) == TRUE)
    t = y
  if(grepl("Coupon", deparse(substitute(x)), ignore.case = TRUE) == TRUE)
    c = x
  if(grepl("Coupon", deparse(substitute(y)), ignore.case = TRUE) == TRUE)
    c = y


  Ticket_temp <- fread(t, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                       integer64 = "numeric")

  Ticket_temp <- Ticket_temp %>%
    select("ItinID", "RoundTrip", "FarePerMile", "Passengers", "ItinFare", "BulkFare", "Distance",
           "Year", "Quarter") %>%
    rename(ITIN_ID = ItinID, ROUNDTRIP = RoundTrip, ITIN_YIELD = FarePerMile, PASSENGERS = Passengers,
           ITIN_FARE = ItinFare, BULKFARE = BulkFare, DISTANCE_FULL = Distance,
           YEAR = Year, QUARTER = Quarter)

  Coupon_temp <- fread(c, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                       integer64 = "numeric")

  Coupon_temp <- Coupon_temp %>%
    select("ItinID", "MktID", "SeqNum",
           "OriginCityMarketID", "Origin",
           "DestCityMarketID", "Dest", "Break",
           "OpCarrier", "Distance","Year", "Quarter", "Gateway") %>%
    rename(ITIN_ID = ItinID, MKT_ID = MktID, SEQ_NUM = SeqNum,
           ORIGIN_CITY_MARKET_ID = OriginCityMarketID, ORIGIN = Origin, YEAR = Year, QUARTER = Quarter,
           DEST_CITY_MARKET_ID = DestCityMarketID , DEST = Dest, TRIP_BREAK = Break,
           OPERATING_CARRIER = OpCarrier, DISTANCE = Distance, GATEWAY = Gateway)


  write.csv(Ticket_temp, file = paste(path, "Ticket", " ", Ticket_temp$YEAR[1], "Q", Ticket_temp$QUARTER[1], ".csv", sep = ""), row.names=FALSE)
  write.csv(Coupon_temp, file = paste(path, "Coupon", " ", Coupon_temp$YEAR[1], "Q", Coupon_temp$QUARTER[1], ".csv", sep = ""), row.names=FALSE)


}

globalVariables(c("ItinID", "RoundTrip", "FarePerMile", "Passengers", "ItinFare",
"BulkFare", "Distance", "MktID", "SeqNum", "OriginCityMarketID", "Origin", "Year",
"Quarter", "DestCityMarketID", "Dest", "Break", "OpCarrier", "Gateway"))
