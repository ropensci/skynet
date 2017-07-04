#' Path and OD Network
#'
#' Generates an OD network and a Leg Count data frame(on demand)
#'
#' @param x Data frame
#' @param leg Generates Leg Count Data frame, based on Path taken.
#' For example, all passengers doing the BOS-ATL-LAX path, are summed by Air Carrier.
#'
#' @examples
#' make.Path(OD_2016Q1)
#'
#' # Generate Leg Count
#' make.Path(OD_2016Q1, leg = TRUE)
#'
#' @export
#'
#'

make.Path <- function(x, leg = FALSE){


    #alternative1
  DT <- as.data.table(x)
  DT <- DT[order(MKT_ID, SEQ_NUM)]
#  DT <- DT[, , by = MKT_ID]
  DT <- DT[, .(ORIGIN=ORIGIN[1], DEST=DEST[.N], ITIN_FARE=ITIN_FARE[1], PASSENGERS = PASSENGERS[1],
               ROUNDTRIP = ROUNDTRIP[1], ITIN_YIELD = ITIN_YIELD[1], NUM_STOPS = .N), by=MKT_ID]

  DT <- DT[, ITIN_FARE := ITIN_FARE/(1+ROUNDTRIP)]
  netOD <- DT[, .(PASSENGERS = sum(PASSENGERS),FARE_SD = round(sd(ITIN_FARE), 2), ITIN_FARE = round(sum(ITIN_FARE)/(.N), 2), ITIN_YIELD = round(sum(ITIN_YIELD)/(.N), 3), MEAN_STOPS = round(sum(NUM_STOPS)/(.N))), by=.(ORIGIN, DEST)][order(ORIGIN, DEST)]


  # Add city name
  netOD <- netOD %>%
    left_join(airportCode, by = "ORIGIN") %>%
    rename(ORIGIN_CITY = CITY, ORIGIN_CITY_MARKET_ID = CITY_MARKET_ID)

  airportCode <- airportCode %>%
    rename(DEST = ORIGIN, DEST_CITY = CITY, DEST_CITY_MARKET_ID = CITY_MARKET_ID)

  netOD <- netOD %>%
    left_join(airportCode, by = "DEST") %>%
    select(-Latitude.x, -Latitude.y, -Longitude.x, -Longitude.y) %>%
    mutate(FARE_SD = ifelse(is.na(FARE_SD), 0, FARE_SD))
    #mutate(ITIN_FARE = round(ITIN_FARE/PASSENGERS, 2), ITIN_YIELD = round(ITIN_YIELD/PASSENGERS, 2))

  if(leg == FALSE){
    return(netOD)
  }

  if(leg == TRUE){

    print("This code might take longer than usual to execute")

    # Group into different paths (MKT_ID)
  netPath <- x %>%
    select(MKT_ID, ORIGIN, DEST, PASSENGERS, SEQ_NUM, OPERATING_CARRIER) %>%
    arrange(MKT_ID, SEQ_NUM)

  # Selects and merges
  # Data.table method

  DT <- data.table(netPath)

  netPath <- DT[,.(PASSENGERS, SEQ_NUM, OPERATING_CARRIER, Path = paste(ORIGIN[1],paste(DEST, collapse = " "),
                                                                        collapse = " ")), by=MKT_ID]

  # Merge everything
  netPath <- netPath %>%
    group_by(Path, OPERATING_CARRIER) %>%
    summarise(Passengers = sum(PASSENGERS)) %>%
    left_join(carriers, by = "OPERATING_CARRIER") %>%
    select(Path, OPERATING_CARRIER, Description, Passengers) %>%
    arrange(Path)


  # Count words
  netPath$legCount <- stringr::str_count(netPath$Path, "\\S+")

  return(list(netOD = netOD, netLegCount = netPath))

  #assign("netLegCount",netPath, .GlobalEnv)
}


}
