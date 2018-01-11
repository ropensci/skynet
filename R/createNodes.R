#' Create Nodes
#'
#' Creates nodes for SKYNET's functions.
#' Despite being possible to use it individually, it's mainly meant to be used as a complimentary function.
#'
#' @param y Data Frame
#'
#' @export
#'

createNodes <- function(y){

  nodesTemp <- y %>%
    group_by(dest) %>%
    summarize(passengers = sum(passengers)) %>%
    rename(origin = dest)

  nodes <- y %>%
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
#' #@param x Original Data Frame to extract nodes from
#' #@param y List to include it into
#'

#transferNodes <- function (x, y) {
#  netTemp <- x
#  netTemp <- dplyr::filter(netTemp, trip_break == "")

#  nodesTemp <- netTemp %>%
#    group_by(dest) %>%
#    summarize(passengers = sum(passengers)) %>%
#    rename(origin = dest)

#  nodesTr <- netTemp %>%
#    group_by(origin) %>%
#    summarize(passengers = sum(passengers))

#  nodesTr <- nodesTr %>%
#    merge(nodesTemp, by = "origin", all = TRUE) %>%
#    mutate(passengers.x = replace(passengers.x, is.na(passengers.x), 0),
#           passengers.y = replace(passengers.y, is.na(passengers.y), 0),
#           freq = (passengers.x + passengers.y)) %>%
#    select(origin, freq) %>%
#    merge(airportCode, by = "origin", all.x = TRUE)


#  nodesTr <- left_join(y[["nodes"]], nodesTr, by = "origin") %>%
#    select(origin, freq.y, city.y, city_mkt_id.y, latitude.y, longitude.y) %>%
#    rename(freq = freq.y, city = city.y, city_mkt_id = city_mkt_id.y,
#           latitude = latitude.y, longitude = longitude.y)


#   assign(deparse(substitute(y)), c(y, list(nodesTr = nodesTr)), envir = envir)


#}



#' Create Metro Nodes
#'
#'
#' @param y Data Frame
#'

metroNodes <- function(y){

  nodesTemp <- y %>%
    group_by(dest) %>%
    summarize(passengers = sum(passengers)) %>%
    rename(origin = dest)

  nodes <- y %>%
    group_by(origin) %>%
    summarize(passengers = sum(passengers))

#  airportCodeFull <- airportCodeFull %>%
#    select(city_mkt_id, latitude, longitude, airport_state, airport_city_name) %>%
#    rename(origin = city_mkt_id)

  nodes <- nodes %>%
    merge(nodesTemp, by = "origin", all = TRUE) %>%
    mutate(passengers.x = replace(passengers.x, is.na(passengers.x), 0),
           passengers.y = replace(passengers.y, is.na(passengers.y), 0),
           freq = (passengers.x + passengers.y)) %>%
    select(origin, freq) %>%
    merge(MetroFull, by = "origin", all.x = TRUE)

}

globalVariables(c("dest", "passengers", "origin", "passengers.x", "passengers.y",
                  "freq", "airportCode", "airport_state", "airport_city_name",
                  "trip_break", "freq.y", "city.y", "city_mkt_id.y", "MetroFull"))

pos = 1
envir = as.environment(pos)
