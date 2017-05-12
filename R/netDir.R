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

  if(grepl("Int", deparse(substitute(x)), ignore.case = TRUE) == TRUE)
    nodes.y = nodesInt
  else
    nodes.y = nodes


    netDir_all <- x %>%
    data.frame() %>%
    select(ORIGIN, DEST, PASSENGERS, OPERATING_CARRIER) %>%
    group_by(ORIGIN, DEST) %>%
    summarise(weight = sum(PASSENGERS))

  gDir <- igraph::graph_from_data_frame(netDir_all, directed = TRUE, vertices = nodes.y)


  if(disp == TRUE){
    # Run disparity filter

    # Create igraph
    gDir_disp <<- semnet::getBackboneNetwork(gDir, delete.isolates = T, alpha = alpha)
    netDir_disp <<- get.data.frame(gDir_disp)

    netDir_disp <- netDir_disp %>%
      rename(ORIGIN = from, DEST = to)


    # Add city name
    netDir_disp <- netDir_disp %>%
      left_join(airportCode, by = "ORIGIN") %>%
      rename(ORIGIN_CITY = CITY, ORIGIN_CITY_MARKET_ID = CITY_MARKET_ID)

    airportCode <- airportCode %>%
      rename(DEST = ORIGIN, DEST_CITY = CITY, DEST_CITY_MARKET_ID = CITY_MARKET_ID)

    netDir_disp <- netDir_disp %>%
      left_join(airportCode, by = "DEST") %>%
      select(-Latitude.x, -Latitude.y, -Longitude.x, -Longitude.y)



    gDir_disp <<- gDir_disp
    netDir_disp <<- netDir_disp

    # ----------------------------------------------------------------------------- #
                          # End of disp filter command #
    # ----------------------------------------------------------------------------- #

  }else if(cap == TRUE){

    # Applies 10% cap
    gDir_cap <- graph_from_data_frame(netDir_all, directed = TRUE, vertices = nodes.y)
    gDir_cap <- subgraph.edges(gDir_cap, which(E(gDir_cap)$weight > quantile(E(gDir_cap)$weight, prob = 1-pct/100)), delete.vertices = TRUE)

    #Creates Dataframe from graph
    netDir_cap <- igraph::as_data_frame(gDir_cap)

    netDir_cap <- netDir_cap %>%
      rename(ORIGIN = from, DEST = to)

    # Add city name
    netDir_cap <- netDir_cap %>%
      left_join(airportCode, by = "ORIGIN") %>%
      rename(ORIGIN_CITY = CITY, ORIGIN_CITY_MARKET_ID = CITY_MARKET_ID)

    airportCode <- airportCode %>%
      rename(DEST = ORIGIN, DEST_CITY = CITY, DEST_CITY_MARKET_ID = CITY_MARKET_ID)

    netDir_cap <- netDir_cap %>%
      left_join(airportCode, by = "DEST") %>%
      select(-Latitude.x, -Latitude.y, -Longitude.x, -Longitude.y)


    gDir_cap <<- gDir_cap
    netDir_cap <<- netDir_cap

    # ----------------------------------------------------------------------------- #
                           # End of 10% filter command #
    # ----------------------------------------------------------------------------- #


  }else{

    # Runs network with full data
    gDir <- graph_from_data_frame(netDir_all, directed = TRUE, vertices = nodes.y)

    # Add city name
    netDir_all <- netDir_all %>%
      left_join(airportCode, by = "ORIGIN") %>%
      rename(ORIGIN_CITY = CITY, ORIGIN_CITY_MARKET_ID = CITY_MARKET_ID)

    airportCode <- airportCode %>%
      rename(DEST = ORIGIN, DEST_CITY = CITY, DEST_CITY_MARKET_ID = CITY_MARKET_ID)

    netDir_all <- netDir_all %>%
      left_join(airportCode, by = "DEST") %>%
      select(-Latitude.x, -Latitude.y, -Longitude.x, -Longitude.y)

    gDir <<- gDir
    netDir_all <<- netDir_all

  }

}

# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
                            # End of netDir command #
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
