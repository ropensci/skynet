#' Create Nodes
#'
#'
#' @param y Data Frame
#'

createNodes <- function(y){

  nodesTemp <- y %>%
    group_by(dest) %>%
    summarize(passengers = sum(passengers)) %>%
    rename(origin = dest)

  nodes <- y %>%
    rename(passengers = passengers) %>%
    group_by(origin) %>%
    summarize(passengers = sum(passengers))

  nodes <- nodes %>%
    merge(nodesTemp, by = "origin", all = TRUE) %>%
    mutate(passengers.x = replace(passengers.x, is.na(passengers.x), 0),
           passengers.y = replace(passengers.y, is.na(passengers.y), 0),
           freq = (passengers.x + passengers.y)) %>%
    select(origin, freq) %>%
    merge(airportCode, by = "origin", all.x = TRUE)

}


#' Create Transfer nodes
#'
#'
#' @param x Original Data Frame to extract nodes from
#' @param y List to include it into
#'

transferNodes <- function (x, y) {
  netTemp <- x
  netTemp <- dplyr::filter(netTemp, trip_break == "")

  nodesTemp <- netTemp %>%
    group_by(dest) %>%
    summarize(passengers = sum(passengers)) %>%
    rename(origin = dest)

  nodesTr <- netTemp %>%
    group_by(origin) %>%
    summarize(passengers = sum(passengers))

  nodesTr <- nodesTr %>%
    merge(nodesTemp, by = "origin", all = TRUE) %>%
    mutate(passengers.x = replace(passengers.x, is.na(passengers.x), 0),
           passengers.y = replace(passengers.y, is.na(passengers.y), 0),
           freq = (passengers.x + passengers.y)) %>%
    select(origin, freq) %>%
    merge(airportCode, by = "origin", all.x = TRUE)


  nodesTr <- left_join(y[["nodes"]], nodesTr, by = "origin") %>%
    select(origin, freq.y, city.y, city_mkt_id.y, latitude.y, longitude.y) %>%
    rename(freq = freq.y, city = city.y, city_mkt_id = city_mkt_id.y,
           latitude = latitude.y, longitude = longitude.y)


   assign(deparse(substitute(y)), c(y, list(nodesTr = nodesTr)), .GlobalEnv)


}



#' Create Metro Nodes
#'
#'
#' @param y Data Frame
#'

metroNodes <- function(y){

  nodesTemp <- y %>%
    group_by(DEST) %>%
    summarize(PASSENGERS = sum(PASSENGERS)) %>%
    rename(ORIGIN = DEST)

  nodes <- y %>%
    group_by(ORIGIN) %>%
    summarize(PASSENGERS = sum(PASSENGERS))

  airportCodeFull <- airportCodeFull %>%
    select(CITY_MARKET_ID, Latitude, Longitude, AIRPORT_STATE_CODE, DISPLAY_AIRPORT_CITY_NAME_FULL) %>%
    rename(ORIGIN = CITY_MARKET_ID)

  nodes <- nodes %>%
    merge(nodesTemp, by = "ORIGIN", all = TRUE) %>%
    mutate(PASSENGERS.x = replace(PASSENGERS.x, is.na(PASSENGERS.x), 0),
           PASSENGERS.y = replace(PASSENGERS.y, is.na(PASSENGERS.y), 0),
           freq = (PASSENGERS.x + PASSENGERS.y)) %>%
    select(ORIGIN, freq) %>%
    merge(airportCodeFull, by = "ORIGIN", all.x = TRUE)

}
