#' Skynet network analysis for BTS Data
#' Creates networks from the BTS Airline Origin and Destination Survey (DB1B)
#'
#' Given the DB1BCoupon and DB1BTicket csv's exported
#' this package allows creating sociomatrixes and subsequent igraph graphs.
#' @author Filipe Teixeira
#' @references
#' NA
#' @examples
#' NA
#' @aliases skynet
#' @include netDir.R
#' @include netUnd.R
#' @include netMetro.R
#' #@importFrom semnet getBackboneNetwork
#' @export


# netImport function
netImport <- function(x, y){

  # Import Coupon file
  # Include ItinID, MktID, Origin, Dest, SeqNum,
  #OriginCityMarketID, DestCityMarketID and Trip Break (8 variables)
  coupon <- fread(x, header = TRUE, sep = ",", stringsAsFactors = TRUE,
                  integer64 = "numeric")
  coupon <- select(coupon, -grep("V", names(coupon)))
  #coupon[,"V9"] <- NULL

  # Import Ticket file
  # Include ItinID, RoundTrip, Passengers and ItinFare (4 variables)
  ticket <- fread(y, header = TRUE, sep = ",", stringsAsFactors = TRUE,
                  integer64 = "numeric")
  ticket <- select(ticket, -grep("V", names(ticket)))
  #ticket[,"V5"] <-  NULL

  #Merge data
  netMerged <<- merge(coupon, ticket, by = "ITIN_ID", all.x = TRUE)

  #--------------------------------------------------------------------------

  #Create nodes and node frequency

  nodesTemp <- netMerged %>%
    group_by(DEST) %>%
    summarize(PASSENGERS = sum(PASSENGERS)) %>%
    rename(ORIGIN = DEST)
  #colnames(nodesTemp)[1] <- "ORIGIN"

  nodes <- netMerged %>%
    group_by(ORIGIN) %>%
    summarize(PASSENGERS = sum(PASSENGERS))

  nodes <- nodes %>%
    merge(nodesTemp, by = "ORIGIN", all = TRUE) %>%
    mutate(PASSENGERS.x = replace(PASSENGERS.x, is.na(PASSENGERS.x), 0),
           PASSENGERS.y = replace(PASSENGERS.y, is.na(PASSENGERS.y), 0),
           freq = (PASSENGERS.x + PASSENGERS.y)) %>%
    select(ORIGIN, freq) %>%
    merge(airportCode, by = "ORIGIN", all.x = TRUE)

  nodes <<- nodes

  #Create nodes for transfer stuff
  netMergedtemp <- netMerged
  netMergedtemp <- dplyr::filter(netMergedtemp, TRIP_BREAK == "")

  nodesTemp <- netMergedtemp %>%
    group_by(DEST) %>%
    summarize(PASSENGERS = sum(PASSENGERS)) %>%
    rename(ORIGIN = DEST)
  #colnames(nodesTemp)[1] <- "ORIGIN"

  nodesTr <- netMergedtemp %>%
    group_by(ORIGIN) %>%
    summarize(PASSENGERS = sum(PASSENGERS))

  nodesTr <- nodesTr %>%
    merge(nodesTemp, by = "ORIGIN", all = TRUE) %>%
    mutate(PASSENGERS.x = replace(PASSENGERS.x, is.na(PASSENGERS.x), 0),
           PASSENGERS.y = replace(PASSENGERS.y, is.na(PASSENGERS.y), 0),
           freq = (PASSENGERS.x + PASSENGERS.y)) %>%
    select(ORIGIN, freq) %>%
    merge(airportCode, by = "ORIGIN", all.x = TRUE)

  nodesTr <<- nodesTr

}



#--------------------------------------------------------------------------------
                              # END OF SCRIPT
#--------------------------------------------------------------------------------
