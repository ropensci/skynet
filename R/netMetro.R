#' Metro Network
#'
#' Generates Metro Network with an iGraph object and a Data Frame.
#'
#' @param x data frame
#' @param undirected If true, creates undirected network
#' @param merge When set to FALSE, it keeps parallel edges instead of collapsing them
#' and summing their weights.
#'
#' @examples
#' \dontrun{
#' make.netMetro(OD_2016Q1)
#'
#' # Creates undirected network with merged edges
#' make.netUnd(OD_2016Q1, undirected = TRUE, merge = TRUE)
#' }
#'
#' @export

make.netMetro <- function(x = NULL, undirected = FALSE, merge = TRUE){

  netMet <- x %>%
    select(origin_mkt_id, dest_mkt_id, passengers, op_carrier, itin_fare, itin_yield, roundtrip) %>%
    group_by(origin_mkt_id, dest_mkt_id) %>%
    mutate(itin_fare = itin_fare/(1+roundtrip)) %>%
    summarise(weight = sum(passengers), fare_sd = round(sd(itin_fare), 2),
              itin_fare = mean(itin_fare), itin_yield = mean(itin_yield)) %>%
    merge(MetroLookup, by = "origin_mkt_id", all.x = TRUE) %>%
    rename(origin_city = description)

  MetroLookupDest <- MetroLookup %>%
    rename(dest_mkt_id = origin_mkt_id)

  netMet <- netMet %>%
    merge(MetroLookupDest, by = "dest_mkt_id", all.x = TRUE) %>%
    rename(dest_city = description, origin = origin_mkt_id,
           dest = dest_mkt_id) %>%
    select(origin, dest, origin_city, dest_city, weight)

  gMet_dir <- graph_from_data_frame(netMet, directed = TRUE)
  netMet <- rename(netMet, passengers = weight)
  nodes <- metroNodes(netMet)

  return(list(netMet = netMet, gMet_dir = gMet_dir, nodes = nodes))


  if(undirected == TRUE){

    gMet_und <- graph_from_data_frame(netMet, directed = FALSE)
    gMet_und <- as.undirected(gMet_und, mode = "collapse",
                              edge.attr.comb=list(weight = "sum", itin_fare = "mean",
                                                  itin_yield = "mean", fare_sd = "mean"))
    return(list(netMet = netMet, gMet_und = gMet_und))

  }else if(merge == FALSE){

    gMet_und <- graph_from_data_frame(netMet, directed = FALSE)
    return(list(netMet = netMet, gMet_und = gMet_und))


  }

}

globalVariables(c("origin_mkt_id", "dest_mkt_id", "dest_city", "MetroLookup",
                  "origin_city"))

