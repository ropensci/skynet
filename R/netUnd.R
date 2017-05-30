#' Undirected Network
#'
#' Generates Undirected Network with an iGraph object and a Data Frame.
#'
#' @param x Data frame
#' @param disp Uses the Serrano's disparity filter (\url{http://www.pnas.org/content/106/16/6483.full})
#' to extract the backbone of the network.
#' @param alpha Argument for disparity filter.
#' @param cap Filters original data based on the edge weight.
#' @param pct Argument for cap filter. Value should be imput as percentage.
#' @param merge When set to FALSE, it keeps parallel edges instead of collapsing them
#' and summing their weights.
#'
#' @examples
#' make.netUnd(OD_2016Q1)
#'
#' # Apply Disparity Filter
#' make.netUnd(OD_2016Q1, disp = TRUE, alpha = 0.05)
#'
#' # Apply Percentage Cap
#' make.netUnd(OD_2016Q1, cap = TRUE, pct = 20)
#'
#'
#' @export

make.netUnd <- function(x, disp = FALSE, cap = FALSE, merge = TRUE, alpha = 0.003, pct = 10){

  #-------------------------------------------------
  netUnd_all <- x %>%
    select(ORIGIN, DEST, PASSENGERS) %>%
    group_by(ORIGIN, DEST) %>%
    summarise(weight = sum(PASSENGERS))

  #-------------------------------------------------

  nodes <- createNodes(x)

  gUnd <- graph_from_data_frame(netUnd_all, directed = TRUE, vertices = nodes)
  gUnd <- as.undirected(gUnd, mode = "collapse", edge.attr.comb=list(weight = "sum"))

    if(disp == TRUE){

    # Run disparity filter
    # Creates igraph object
    gUnd_disp <<- semnet::getBackboneNetwork(gUnd, delete.isolates = T, alpha = alpha)
    netUnd_disp <<- get.data.frame(gUnd_disp)

    # Rename fields
    netUnd_disp <- netUnd_disp %>%
      rename(ORIGIN = from, DEST = to, PASSENGERS = weight)

    # Add city name
    netUnd_disp <- netUnd_disp %>%
      left_join(airportCode, by = "ORIGIN") %>%
      rename(ORIGIN_CITY = CITY, ORIGIN_CITY_MARKET_ID = CITY_MARKET_ID)

    airtemp <- airportCode %>%
      rename(DEST = ORIGIN, DEST_CITY = CITY, DEST_CITY_MARKET_ID = CITY_MARKET_ID)

    netUnd_disp <- netUnd_disp %>%
      left_join(airtemp, by = "DEST") %>%
      select(-Latitude.x, -Latitude.y, -Longitude.x, -Longitude.y)

    nodes <- createNodes(netUnd_disp)

    return(list(gUnd_disp = gUnd_disp, netUnd_disp = netUnd_disp, nodes = nodes))

    # ----------------------------------------------------------------------------- #
                           # End of 10% filter command #
    # ----------------------------------------------------------------------------- #


  }else if(cap == TRUE){

    #Run 10% cap
    gUnd_cap <- gUnd
    gUnd_cap <- subgraph.edges(gUnd_cap, which(E(gUnd_cap)$weight > quantile(E(gUnd_cap)$weight, prob = 1-pct/100)), delete.vertices = TRUE)

    # Create datafram based on collapsed edges graph
    netUnd_cap <- igraph::as_data_frame(gUnd_cap)

    netUnd_cap <- netUnd_cap %>%
      rename(ORIGIN = from, DEST = to, PASSENGERS = weight)

    # Add city name
    netUnd_cap <- netUnd_cap %>%
      left_join(airportCode, by = "ORIGIN") %>%
      rename(ORIGIN_CITY = CITY, ORIGIN_CITY_MARKET_ID = CITY_MARKET_ID)

    airtemp <- airportCode %>%
      rename(DEST = ORIGIN, DEST_CITY = CITY, DEST_CITY_MARKET_ID = CITY_MARKET_ID)

    netUnd_cap <- netUnd_cap %>%
      left_join(airtemp, by = "DEST") %>%
      select(-Latitude.x, -Latitude.y, -Longitude.x, -Longitude.y)

    nodes <- createNodes(netUnd_cap)

    return(list(gUnd_cap = gUnd_cap, netUnd_cap = netUnd_cap, nodes = nodes))

    # ----------------------------------------------------------------------------- #
    # End of 10% filter command #
    # ----------------------------------------------------------------------------- #


  }else if(merge == FALSE){

    # Run undirected with merge
    netUnd_all <- x %>%
      select(ORIGIN, DEST, PASSENGERS) %>%
      group_by(ORIGIN, DEST) %>%
      summarise(weight = sum(PASSENGERS))

    nodes <- createNodes(x)

    gUnd <- graph_from_data_frame(netUnd_all, directed = FALSE, vertices = nodes)

    return(list(netUnd = netUnd_all, gUnd = gUnd, nodes = nodes))

  }else{

    # Create dataframe based on collapsed edges graph
    netUnd_all <- igraph::as_data_frame(gUnd)

    netUnd_all <- netUnd_all %>%
      rename(ORIGIN = from, DEST = to, PASSENGERS = weight)

    # Add city name
    netUnd_all <- netUnd_all %>%
      left_join(airportCode, by = "ORIGIN") %>%
      rename(ORIGIN_CITY = CITY, ORIGIN_CITY_MARKET_ID = CITY_MARKET_ID)

    airtemp <- airportCode %>%
      rename(DEST = ORIGIN, DEST_CITY = CITY, DEST_CITY_MARKET_ID = CITY_MARKET_ID)

    netUnd_all <- netUnd_all %>%
      left_join(airtemp, by = "DEST") %>%
      select(-Latitude.x, -Latitude.y, -Longitude.x, -Longitude.y)

    nodes <- createNodes(netUnd_all)

    return(list(gUnd = gUnd, netUnd = netUnd_all, nodes))


  }
}


# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
                            # End of netUnd command #
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
