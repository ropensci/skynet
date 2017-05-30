#' Transfer Nodes
#'
#' Extracts Nodes used only by transfer passengers
#'
#' @param x Data set
#'
#' @examples
#' make.nodesTr(OD_2016Q1)
#'
#' @export


make.nodesTr <- function (x) {

  #Create nodes for transfer info
  netMergedtemp <- x
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

  assign("nodesTr", nodesTr, .GlobalEnv)
}
