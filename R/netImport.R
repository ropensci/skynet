# Skynet network analysis for BTS Data

library(data.table)
library(dplyr)
library(igraph)
library(disparityfilter)
library(poweRlaw)

#netImport function
netImport <- function(x, y){

  # Import Coupon file
  # Include ItinID, MktID, Origin, Dest, SeqNum,
  #OriginCityMarketID, DestCityMarketID and Trip Break (8 variables)
  coupon <- fread(x, header = TRUE, sep = ",", stringsAsFactors = TRUE,
                  integer64 = "numeric")
  coupon[,"V9"] <- NULL

  # Import Ticket file
  # Include ItinID, RoundTrip, Passengers and ItinFare (4 variables)
  ticket <- fread(y, header = TRUE, sep = ",", stringsAsFactors = TRUE,
                  integer64 = "numeric")
  ticket[,"V5"] <-  NULL

  #Merge data
  netMerged <<- merge(coupon, ticket, by = "ITIN_ID", all.x = TRUE)

  #--------------------------------------------------------------------------

  #Create nodes and node frequency

  nodesTemp <- netMerged %>%
    group_by(DEST) %>%
    summarize(PASSENGERS = sum(PASSENGERS))
  colnames(nodesTemp)[1] <- "ORIGIN"

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
    summarize(PASSENGERS = sum(PASSENGERS))
  colnames(nodesTemp)[1] <- "ORIGIN"

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
