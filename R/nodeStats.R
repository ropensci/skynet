#' Get node info
#'
#' Creates node statistics
#' Generates Number of Passenger Arrivals, Departures and Transfers
#'
#' @param x Data Frame to extract information from
#'
#' @examples
#' \dontrun{
#'
#' nodeStats(OD_2011Q1)
#'
#'}
#' @export
#'


nodeStats <- function(x){

departures <- x %>%
  group_by(origin) %>%
  summarise(pass_dep = sum(passengers)) %>%
  rename(airport = origin)

arrivals <- x %>%
  group_by(dest) %>%
  summarise(pass_arr = sum(passengers)) %>%
  rename(airport = dest)

transfers <- x %>%
  group_by(dest) %>%
  filter(trip_break == "") %>%
  summarise(pass_tr = sum(passengers)) %>%
  rename(airport = dest)

nodeStat <- merge(departures, arrivals, by = "airport", all = TRUE)
nodeStat <- merge(nodeStat, transfers, by = "airport", all = TRUE)
nodeStat <- nodeStat %>%
  merge(airportCodeFull, by.x = "airport", by.y = "origin", all.x = TRUE) %>%
  mutate(freq = pass_dep + pass_arr)

return(nodeStat)

}

globalVariables(c("airportCodeFull", "departures", "arrivals", "transfers", "pass_dep", "pass_arr",
                  "pass_tr"))
