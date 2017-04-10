#' netMetro
#' @export

netMetro <- function(x = netMerged, undirected = FALSE, merge = TRUE){

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

  gMet_dir <<- graph_from_data_frame(netMet, directed = TRUE)
  netMet <<- netMet

  if(undirected == TRUE){

  gMet_und <<- graph_from_data_frame(netMet, directed = FALSE)
  gMet_und <<- as.undirected(gMet_und, mode = "collapse", edge.attr.comb=list(weight = "sum"))

  }else if(merge == FALSE){

  gMet_und <<- graph_from_data_frame(netMet, directed = FALSE)

  }



}

