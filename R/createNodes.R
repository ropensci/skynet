#' Create Nodes
#'
#'
#' @param y Data Frame
#'

createNodes <- function(y){

  nodesTemp <- y %>%
    group_by(DEST) %>%
    summarize(PASSENGERS = sum(PASSENGERS)) %>%
    rename(ORIGIN = DEST)

  nodes <- y %>%
    rename(PASSENGERS = PASSENGERS) %>%
    group_by(ORIGIN) %>%
    summarize(PASSENGERS = sum(PASSENGERS))

  nodes <- nodes %>%
    merge(nodesTemp, by = "ORIGIN", all = TRUE) %>%
    mutate(PASSENGERS.x = replace(PASSENGERS.x, is.na(PASSENGERS.x), 0),
           PASSENGERS.y = replace(PASSENGERS.y, is.na(PASSENGERS.y), 0),
           freq = (PASSENGERS.x + PASSENGERS.y)) %>%
    select(ORIGIN, freq) %>%
    merge(airportCode, by = "ORIGIN", all.x = TRUE)

}


#' Create Transfer nodes
#'
#'
#' @param x Original Data Frame to extract nodes from
#' @param y List to include it into
#'

transferNodes <- function (x, y) {
  netTemp <- x
  netTemp <- dplyr::filter(netTemp, TRIP_BREAK == "")

  nodesTemp <- netTemp %>%
    group_by(DEST) %>%
    summarize(PASSENGERS = sum(PASSENGERS)) %>%
    rename(ORIGIN = DEST)

  nodesTr <- netTemp %>%
    group_by(ORIGIN) %>%
    summarize(PASSENGERS = sum(PASSENGERS))

  nodesTr <- nodesTr %>%
    merge(nodesTemp, by = "ORIGIN", all = TRUE) %>%
    mutate(PASSENGERS.x = replace(PASSENGERS.x, is.na(PASSENGERS.x), 0),
           PASSENGERS.y = replace(PASSENGERS.y, is.na(PASSENGERS.y), 0),
           freq = (PASSENGERS.x + PASSENGERS.y)) %>%
    select(ORIGIN, freq) %>%
    merge(airportCode, by = "ORIGIN", all.x = TRUE)

  # Get fucking nodes
 # paste(deparse(substitute(x)), "nodes", sep = "$"))

  #nodes <- paste(deparse(substitute(y)), "nodes", sep = "$")

  #nodes <- listtest[["nodes"]]

  nodesTr <- left_join(y[["nodes"]], nodesTr, by = "ORIGIN") %>%
    select(ORIGIN, freq.y, CITY.y, CITY_MARKET_ID.y, Latitude.y, Longitude.y) %>%
    rename(freq = freq.y, CITY = CITY.y, CITY_MARKET_ID = CITY_MARKET_ID.y,
           Latitude = Latitude.y, Longitude = Longitude.y)


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
    summarize(PASSENGERS = sum(weight)) %>%
    rename(ORIGIN = DEST)

  nodes <- y %>%
    rename(PASSENGERS = weight) %>%
    group_by(ORIGIN) %>%
    summarize(PASSENGERS = sum(PASSENGERS))

  airportCode <- airportCode %>%
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
