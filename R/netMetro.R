#' Metro Network
#'
#' Generates Metro Network with an iGraph object and a Data Frame.
#' Still to fix some details
#'
#' @param disp Uses the Serrano's disparity filter (\url{http://www.pnas.org/content/106/16/6483.full})
#' to extract the backbone of the network.
#' @param alpha Argument for disparity filter.
#' @param cap Filters original data based on the edge weight.
#' @param pct Argument for cap filter. Value should be imput as percentage.
#' @param merge When set to FALSE, it keeps parallel edges instead of collapsing them
#' and summing their weights.
#'
#' @examples
#' make.netMetro(OD_2016Q1)
#'
#' # Apply Disparity Filter
#' make.netUnd(OD_2016Q1, disp = TRUE, alpha = 0.05)
#'
#' # Apply Percentage Cap
#' make.netUnd(OD_2016Q1, cap = TRUE, pct = 20)
#' @export

make.netMetro <- function(x = NULL, undirected = FALSE, merge = TRUE){

  netMet <- x %>%
    select(ORIGIN_CITY_MARKET_ID, DEST_CITY_MARKET_ID, PASSENGERS) %>%
    group_by(ORIGIN_CITY_MARKET_ID, DEST_CITY_MARKET_ID) %>%
    summarise(weight = sum(PASSENGERS)) %>%
    merge(MetroLookup, by = "ORIGIN_CITY_MARKET_ID", all.x = TRUE) %>%
    rename(ORIGIN_CITY_MARKET_NAME = Description)

  MetroLookupDest <- MetroLookup %>%
    rename(DEST_CITY_MARKET_ID = ORIGIN_CITY_MARKET_ID)

  netMet <- netMet %>%
    merge(MetroLookupDest, by = "DEST_CITY_MARKET_ID", all.x = TRUE) %>%
    rename(DEST_CITY = Description, ORIGIN = ORIGIN_CITY_MARKET_ID, DEST = DEST_CITY_MARKET_ID,
           ORIGIN_CITY = ORIGIN_CITY_MARKET_NAME) %>%
    select(ORIGIN, DEST, ORIGIN_CITY, DEST_CITY, weight)

  gMet_dir <- graph_from_data_frame(netMet, directed = TRUE)

  return(list(netMet = netMet, gMet_dir = gMet_dir))

#  gMet_dir <- graph_from_data_frame(netMet, directed = TRUE)
#  netMet <<- netMet

  if(undirected == TRUE){

    gMet_und <- graph_from_data_frame(netMet, directed = FALSE)
    gMet_und <- as.undirected(gMet_und, mode = "collapse", edge.attr.comb=list(weight = "sum"))
    return(list(netMet = netMet, gMet_und = gMet_und))

  }else if(merge == FALSE){

    gMet_und <- graph_from_data_frame(netMet, directed = FALSE)
    return(list(netMet = netMet, gMet_und = gMet_und))


  }

}

