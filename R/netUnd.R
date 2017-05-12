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

  if(grepl("Int", deparse(substitute(x)), ignore.case = TRUE) == TRUE)
    nodes.y = nodesInt
  else
    nodes.y = nodesOD

  netUnd_all <- x %>%
    data.frame() %>%
    select(ORIGIN, DEST, PASSENGERS) %>%
    group_by(ORIGIN, DEST) %>%
    summarise(weight = sum(PASSENGERS))


  gUnd <<- graph_from_data_frame(netUnd_all, directed = TRUE, vertices = nodes.y)

  gUnd <<- as.undirected(gUnd, mode = "collapse", edge.attr.comb=list(weight = "sum"))

    if(disp == TRUE){

    # Run disparity filter
    # Creates igraph object
    gUnd_disp <<- semnet::getBackboneNetwork(gUnd, delete.isolates = T, alpha = alpha)
    netUnd_disp <<- get.data.frame(gUnd_disp)

    # Rename fields
    netUnd_disp <- netUnd_disp %>%
      rename(ORIGIN = from, DEST = to)

    # Add city name
    netUnd_disp <- netUnd_disp %>%
      left_join(airportCode, by = "ORIGIN") %>%
      rename(ORIGIN_CITY = CITY, ORIGIN_CITY_MARKET_ID = CITY_MARKET_ID)

    airportCode <- airportCode %>%
      rename(DEST = ORIGIN, DEST_CITY = CITY, DEST_CITY_MARKET_ID = CITY_MARKET_ID)

    netUnd_disp <- netUnd_disp %>%
      left_join(airportCode, by = "DEST") %>%
      select(-Latitude.x, -Latitude.y, -Longitude.x, -Longitude.y)

    gUnd_disp <<- gUnd_disp
    netUnd_disp <<- netUnd_disp

    # ----------------------------------------------------------------------------- #
                           # End of 10% filter command #
    # ----------------------------------------------------------------------------- #


  }else if(cap == TRUE){

    #Run 10% cap
    gUnd_cap <- subgraph.edges(gUnd, which(E(gDir_cap)$weight > quantile(E(gDir_cap)$weight, prob = 1-pct/100)), delete.vertices = TRUE)

    # Create datafram based on collapsed edges graph
    netUnd_cap <- igraph::as_data_frame(gUnd_cap)

    netUnd_cap <- netUnd_cap %>%
      rename(ORIGIN = from, DEST = to)

    # Add city name
    netUnd_cap <- netUnd_cap %>%
      left_join(airportCode, by = "ORIGIN") %>%
      rename(ORIGIN_CITY = CITY, ORIGIN_CITY_MARKET_ID = CITY_MARKET_ID)

    airportCode <- airportCode %>%
      rename(DEST = ORIGIN, DEST_CITY = CITY, DEST_CITY_MARKET_ID = CITY_MARKET_ID)

    netUnd_cap <- netUnd_cap %>%
      left_join(airportCode, by = "DEST") %>%
      select(-Latitude.x, -Latitude.y, -Longitude.x, -Longitude.y)

    netUnd_cap <<- netUnd_cap
    gUnd_cap <<- gUnd_cap

    # ----------------------------------------------------------------------------- #
    # End of 10% filter command #
    # ----------------------------------------------------------------------------- #


  }else if(merge == FALSE){

    # Run undirected with merge
    netUnd_all <- x %>%
      select(ORIGIN, DEST, PASSENGERS) %>%
      group_by(ORIGIN, DEST) %>%
      summarise(weight = sum(PASSENGERS))

    gUnd <<- graph_from_data_frame(netUnd_all, directed = FALSE, vertices = nodes.y)
    netUnd_all <<- netUnd_all

  }else{

    # Create dataframe based on collapsed edges graph
    netUnd_all <- igraph::as_data_frame(gUnd)

    netUnd_all <- netUnd_all %>%
      rename(ORIGIN = from, DEST = to)

    # Add city name
    netUnd_all <- netUnd_all %>%
      left_join(airportCode, by = "ORIGIN") %>%
      rename(ORIGIN_CITY = CITY, ORIGIN_CITY_MARKET_ID = CITY_MARKET_ID)

    airportCode <- airportCode %>%
      rename(DEST = ORIGIN, DEST_CITY = CITY, DEST_CITY_MARKET_ID = CITY_MARKET_ID)

    netUnd_all <- netUnd_all %>%
      left_join(airportCode, by = "DEST") %>%
      select(-Latitude.x, -Latitude.y, -Longitude.x, -Longitude.y)

    netUnd_all <<- netUnd_all

  }
}


# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
                            # End of netUnd command #
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #

