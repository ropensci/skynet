#' Raw Extract
#'
#'
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
