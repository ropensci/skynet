#' Transfer Nodes
#'
#' Extracts Nodes used only by transfer passengers
#'
#' @param x Data set
#'
#' @examples
#' \dontrun{
#' make.nodesTr(OD_2016Q1)
#' }
#' @export


make.nodesTr <- function (x) {

  #Create nodes for transfer info
  netMergedtemp <- x
  netMergedtemp <- dplyr::filter(netMergedtemp, TRIP_BREAK == "")

  nodesTemp <- netMergedtemp %>%
    group_by(dest) %>%
    summarize(passengers = sum(passengers)) %>%
    rename(origin = dest)

  nodesTr <- netMergedtemp %>%
    group_by(origin) %>%
    summarize(passengers = sum(passengers))

  nodesTr <- nodesTr %>%
    merge(nodesTemp, by = "origin", all = TRUE) %>%
    mutate(passengers.x = replace(passengers.x, is.na(passengers.x), 0),
           passengers.y = replace(passengers.y, is.na(passengers.y), 0),
           freq = (passengers.x + passengers.y)) %>%
    select(origin, freq) %>%
    merge(airportCode, by = "origin", all.x = TRUE)

  return(nodesTr)
  #assign("nodesTr", nodesTr, .GlobalEnv)
}

globalVariables("TRIP_BREAK")
