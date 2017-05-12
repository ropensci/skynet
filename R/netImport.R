#' Import Data
#'
#' Imports data from BTS/RITA/Transtats website
#' File order doesn't matter, but it is recomended to name the files using the following
#' syntax: \emph{"Coupon YearQuarter.csv", "Ticket YearQuarter.csv", "T100 Year".}
#'
#' @param x First csv file to be imported, in case of DB1B database, or in case of using
#' the T-100 database, the only file to be included.
#' @param y Second csv file to be imported.
#'
#' @examples
#'
#' # DB1B Database Files - Ticket and Coupon order doesn't matter
#' netImport("Coupon 2016Q1.csv", "Ticket 2016Q1.csv")
#'
#' # T-100
#' netImport("T100 2016.csv")
#'
#' @include netDir.R
#' @include netUnd.R
#' @include netMetro.R
#'
#' @export


netImport <- function(x, y){

  if(grepl("T100", deparse(substitute(x)), ignore.case = TRUE) == TRUE){

    T100 <- fread(x, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                  integer64 = "numeric")

    T100 <- T100 %>%
      filter(PASSENGERS > 0) %>%
      select(ORIGIN, ORIGIN_CITY_MARKET_ID, DEST, DEST_CITY_MARKET_ID, UNIQUE_CARRIER,
             UNIQUE_CARRIER_NAME, PASSENGERS, MONTH) %>%
      rename(OPERATING_CARRIER = UNIQUE_CARRIER)

    T100 <- data.frame(T100)

    #extracts name from file
    filename <- gsub(" ", "",
                     tools::file_path_sans_ext(
                       basename(x)))
    assign(paste(filename), T100, .GlobalEnv)

    #Create nodes and node frequency

    nodesTemp <- T100 %>%
      group_by(DEST) %>%
      summarize(PASSENGERS = sum(PASSENGERS)) %>%
      rename(ORIGIN = DEST)

    nodesT <- T100 %>%
      group_by(ORIGIN) %>%
      summarize(PASSENGERS = sum(PASSENGERS))

    nodesT <- nodesT %>%
      merge(nodesTemp, by = "ORIGIN", all = TRUE) %>%
      mutate(PASSENGERS.x = replace(PASSENGERS.x, is.na(PASSENGERS.x), 0),
             PASSENGERS.y = replace(PASSENGERS.y, is.na(PASSENGERS.y), 0),
             freq = (PASSENGERS.x + PASSENGERS.y)) %>%
      select(ORIGIN, freq) %>%
      merge(airportCode, by = "ORIGIN", all.x = TRUE)

    nodesT <<- nodesT

  }
  else

    do.call(DB1BImport, list(x,y))

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
  coupon <- select(coupon, -grep("V", names(coupon)))
  #coupon[,"V9"] <- NULL

  # Import Ticket file
  # Include ItinID, RoundTrip, Passengers and ItinFare (4 variables)
  ticket <- fread(t, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                  integer64 = "numeric")
  ticket <- select(ticket, -grep("V", names(ticket)))

  #Merge data
  netMerged <- merge(coupon, ticket, by = "ITIN_ID", all.x = TRUE)
  netMerged <- data.frame(netMerged)

  # Get name from file
  filename <- gsub(" ", "",
                   tools::file_path_sans_ext(
                     basename(x)))
  filename <- substring(filename, (nchar(filename)-5))
  assign(paste("OD",filename, sep = "_"), netMerged, .GlobalEnv)


  #--------------------------------------------------------------------------

  #Create nodes and node frequency

  nodesTemp <- netMerged %>%
    group_by(DEST) %>%
    summarize(PASSENGERS = sum(PASSENGERS)) %>%
    rename(ORIGIN = DEST)

  nodesOD <- netMerged %>%
    group_by(ORIGIN) %>%
    summarize(PASSENGERS = sum(PASSENGERS))

  nodesOD <- nodesOD %>%
    merge(nodesTemp, by = "ORIGIN", all = TRUE) %>%
    mutate(PASSENGERS.x = replace(PASSENGERS.x, is.na(PASSENGERS.x), 0),
           PASSENGERS.y = replace(PASSENGERS.y, is.na(PASSENGERS.y), 0),
           freq = (PASSENGERS.x + PASSENGERS.y)) %>%
    select(ORIGIN, freq) %>%
    merge(airportCode, by = "ORIGIN", all.x = TRUE)

  nodesOD <<- nodesOD

  #Create nodes for transfer info
  netMergedtemp <- netMerged
  netMergedtemp <- dplyr::filter(netMergedtemp, TRIP_BREAK == "")

  nodesTemp <- netMergedtemp %>%
    group_by(DEST) %>%
    summarize(PASSENGERS = sum(PASSENGERS)) %>%
    rename(ORIGIN = DEST)

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
