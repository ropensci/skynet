#' Directed network
#'
#' Generates Directed Network with an iGraph object and a Data Frame.
#'
#' @param x Data frame
#' @param disp Uses the Serrano's disparity filter (\url{http://www.pnas.org/content/106/16/6483.full})
#' to extract the backbone of the network.
#' @param alpha Argument for disparity filter.
#' @param cap Filters original data based on the edge weight.
#' @param pct Argument for cap filter. Value should be imput as percentage.
#'
#' @examples
#' make.netDir(OD_2016Q1)
#'
#' # Apply Disparity Filter
#' make.netDir(OD_2016Q1, disp = TRUE, alpha = 0.05)
#'
#' # Apply Percentage Cap
#' make.netDir(OD_2016Q1, cap = TRUE, pct = 20)
#'
#' @export
#'

make.netDir <- function(x, disp = FALSE, cap = FALSE, alpha = 0.003, pct = 10){

  #-------------------------------------------------
  netDir_all <- x %>%
    select(ORIGIN, DEST, PASSENGERS, OPERATING_CARRIER) %>%
    group_by(ORIGIN, DEST) %>%
    summarise(weight = sum(PASSENGERS))

  #-------------------------------------------------

  nodes <- createNodes(x)

  gDir <- igraph::graph_from_data_frame(netDir_all, directed = TRUE, vertices = nodes)

  #-------------------------------------------------

  if(disp == TRUE){
    # Run disparity filter

    # Create igraph
    gDir_disp <- semnet::getBackboneNetwork(gDir, delete.isolates = T, alpha = alpha)
    netDir_disp <- get.data.frame(gDir_disp)

    netDir_disp <- netDir_disp %>%
      rename(ORIGIN = from, DEST = to, PASSENGERS = weight)


    # Add city name
    netDir_disp <- netDir_disp %>%
      left_join(airportCode, by = "ORIGIN") %>%
      rename(ORIGIN_CITY = CITY, ORIGIN_CITY_MARKET_ID = CITY_MARKET_ID)

    airTemp <- airportCode %>%
      rename(DEST = ORIGIN, DEST_CITY = CITY, DEST_CITY_MARKET_ID = CITY_MARKET_ID)

    netDir_disp <- netDir_disp %>%
      left_join(airTemp, by = "DEST") %>%
      select(-Latitude.x, -Latitude.y, -Longitude.x, -Longitude.y)

    nodes <- createNodes(netDir_disp)

    return(list(gDir_disp = gDir_disp,netDir_disp = netDir_disp,
                nodes = nodes))

    # ----------------------------------------------------------------------------- #
                          # End of disp filter command #
    # ----------------------------------------------------------------------------- #

  }else if(cap == TRUE){

    # Applies 10% cap
    gDir_cap <- graph_from_data_frame(netDir_all, directed = TRUE, vertices = nodes)
    gDir_cap <- subgraph.edges(gDir_cap, which(E(gDir_cap)$weight > quantile(E(gDir_cap)$weight, prob = 1-pct/100)), delete.vertices = TRUE)

    #Creates Dataframe from graph
    netDir_cap <- igraph::as_data_frame(gDir_cap)

    netDir_cap <- netDir_cap %>%
      rename(ORIGIN = from, DEST = to, PASSENGERS = weight)

    # Add city name
    netDir_cap <- netDir_cap %>%
      left_join(airportCode, by = "ORIGIN") %>%
      rename(ORIGIN_CITY = CITY, ORIGIN_CITY_MARKET_ID = CITY_MARKET_ID)

    airtemp <- airportCode %>%
      rename(DEST = ORIGIN, DEST_CITY = CITY, DEST_CITY_MARKET_ID = CITY_MARKET_ID)

    netDir_cap <- netDir_cap %>%
      left_join(airtemp, by = "DEST") %>%
      select(-Latitude.x, -Latitude.y, -Longitude.x, -Longitude.y)

    nodes <- createNodes(netDir_cap)

    return(list(gDir_cap = gDir_cap, netDir_cap = netDir_cap, nodes = nodes))

    # ----------------------------------------------------------------------------- #
                           # End of 10% filter command #
    # ----------------------------------------------------------------------------- #


  }else{

    # Runs network with full data
    gDir <- graph_from_data_frame(netDir_all, directed = TRUE, vertices = nodes)

    # Add city name
    netDir_all <- netDir_all %>%
      left_join(airportCode, by = "ORIGIN") %>%
      rename(ORIGIN_CITY = CITY, ORIGIN_CITY_MARKET_ID = CITY_MARKET_ID, PASSENGERS = weight)

    airtemp <- airportCode %>%
      rename(DEST = ORIGIN, DEST_CITY = CITY, DEST_CITY_MARKET_ID = CITY_MARKET_ID)

    netDir_all <- netDir_all %>%
      left_join(airtemp, by = "DEST") %>%
      select(-Latitude.x, -Latitude.y, -Longitude.x, -Longitude.y)


    return(list(gDir = gDir, netDir = netDir_all, nodes = nodes))


  }

}

# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
                            # End of netDir command #
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
