#' Create Nodes
#'
#' Creates nodes for SKYNET's functions.
#' Despite being possible to use it individually, it's mainly meant to be used as a complimentary function.
#'
#' @param y Data Frame
#'
#' @return A data frame with one row per airport: \code{origin} (airport IATA code),
#' \code{freq} (number of departing plus arriving passengers) and the airport
#' information from \code{airportCode} (\code{city_mkt_id}, \code{city},
#' \code{latitude} and \code{longitude}).
#'
#' @examples
#' createNodes(OD_Sample)
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

#' Create Metro Nodes
#'
#'
#' @param y Data Frame
#'
#' @return A data frame with one row per metropolitan area: \code{airport}
#' (city market id), \code{pass_dep} (departing passengers), \code{pass_arr}
#' (arriving passengers), \code{pass_tr} (transfer passengers, 0 when the data has
#' no \code{trip_break} column), the metropolitan area information from
#' \code{MetroLookup} (\code{latitude}, \code{longitude} and \code{description}) and
#' \code{freq} (arrivals plus departures minus transfers).
#'
#' @examples
#' OD_Metro <- OD_Sample
#' OD_Metro$origin <- OD_Metro$origin_mkt_id
#' OD_Metro$dest <- OD_Metro$dest_mkt_id
#'
#' nodeStatsMetro(OD_Metro)
#'
#' @export
#'

nodeStatsMetro <- function(y){

  departures <- y %>%
    group_by(origin) %>%
    summarise(pass_dep = sum(passengers)) %>%
    rename(airport = origin)

  arrivals <- y %>%
    group_by(dest) %>%
    summarise(pass_arr = sum(passengers)) %>%
    rename(airport = dest)

  if(!is.null(y[["trip_break"]])){

    transfers <- y %>%
      group_by(dest) %>%
      filter(trip_break == "") %>%
      summarise(pass_tr = sum(passengers)) %>%
      rename(airport = dest)


    nodeStat <- merge(departures, arrivals, by = "airport", all = TRUE)
    nodeStat <- nodeStat %>%
      merge(transfers, by = "airport", all = TRUE) %>%
      mutate(across(everything(), ~ ifelse(is.na(.x), 0, .x)))

  }else{
    nodeStat <- departures %>%
      merge(arrivals, by = "airport", all = TRUE) %>%
      mutate(across(everything(), ~ ifelse(is.na(.x), 0, .x))) %>%
      mutate(pass_tr = 0)
  }

  nodeStat <- nodeStat %>%
    merge(MetroLookup, by.x = "airport", by.y = "origin", all.x = TRUE) %>%
    mutate(freq = (pass_arr + (pass_dep-pass_tr)))

  return(nodeStat)

}

globalVariables(c("dest", "passengers", "origin",
                  "passengers.x", "passengers.y", "freq",
                  "airportCode", "airport_state", "airport_city_name",
                  "trip_break", "freq.y", "city.y",
                  "city_mkt_id.y", "MetroFull", "MetroLookup"))

pos <- 1
envir <- as.environment(pos)
