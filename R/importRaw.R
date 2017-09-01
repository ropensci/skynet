#' Extract Variables
#'
#' Extracts necessary variables from BTS/RITA/Transtats website raw data (prezipped file), for SKYNET's import function.
#'
#' File order doesn't matter, but it is recomended to name the files using the following
#' syntax: \emph{"Origin_and_Destination_Survey_DB1BCoupon_year_quarter.csv", "Origin_and_Destination_Survey_DB1BTicket_year_quarter.csv".}
#' Note: We do recommend sparklyr to be used for larger sets of data.
#'
#' @param x First csv file to be processed
#' @param y Second csv file to be processed
#'
#' @examples
#'
#' # DB1B Database Files - Ticket and Coupon order doesn't matter
#' netImport("Origin_and_Destination_Survey_DB1BCoupon_2017_1.csv",
#' "Origin_and_Destination_Survey_DB1BTicket_2017_1.csv")
#'
#' @export
#'

netConvert <- function(x,y){


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
    select("ItinID", "RoundTrip", "FarePerMile", "Passengers", "ItinFare", "BulkFare", "Distance") %>%
    rename(ITIN_ID = ItinID, ROUNDTRIP = RoundTrip, ITIN_YIELD = FarePerMile, PASSENGERS = Passengers,
           ITIN_FARE = ItinFare, BULKFARE = BulkFare, DISTANCE_FULL = Distance)

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

  write.csv(Ticket_temp, file = paste("/Filtered/Ticket", " ", Coupon_temp$YEAR[1], "Q", Coupon_temp$QUARTER[1], ".csv", sep = ""), row.names=FALSE)
  write.csv(Coupon_temp, file = paste("/Filtered/Coupon", " ", Coupon_temp$YEAR[1], "Q", Coupon_temp$QUARTER[1], ".csv", sep = ""), row.names=FALSE)



}
