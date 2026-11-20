#' Get node info
#'
#' Creates node statistics
#' Generates Number of Passenger Arrivals, Departures and Transfers
#'
#' @param x Data Frame to extract information from
#'
#' @return A data frame with one row per airport: \code{airport} (IATA code),
#' \code{pass_dep} (departing passengers), \code{pass_arr} (arriving passengers),
#' \code{pass_tr} (transfer passengers, 0 when the data has no \code{trip_break}
#' column), the airport information from \code{airportCodeFull} (city, coordinates,
#' state, country, ...) and \code{freq} (arrivals plus departures minus transfers).
#'
#' @examples
#' node_stats(OD_Sample)
#'
#'
#' @export
#'



node_stats <- function(x){

departures <- x %>%
  group_by(origin) %>%
  summarise(pass_dep = sum(passengers)) %>%
  rename(airport = origin)

arrivals <- x %>%
  group_by(dest) %>%
  summarise(pass_arr = sum(passengers)) %>%
  rename(airport = dest)

if(!is.null(x[["trip_break"]])){

  transfers <- x %>%
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
  merge(airportCodeFull, by.x = "airport", by.y = "origin", all.x = TRUE) %>%
  mutate(freq = (pass_arr + (pass_dep-pass_tr)))

return(nodeStat)

}

nodeStats <- function(...){
  warning(paste("nodeStats is deprecated, use node_stats(), instead."))
  do.call(node_stats, list(...))
}

globalVariables(c("airportCodeFull", "departures",
                  "arrivals", "transfers", "pass_dep",
                  "pass_arr", "pass_tr"))
