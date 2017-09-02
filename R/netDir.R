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
#' @param carrier Groups data per carrier and OD
#'
#' @examples
#' \dontrun{
#' make.netDir(OD_2016Q1)
#'
#' # Apply Disparity Filter
#' make.netDir(OD_2016Q1, disp = TRUE, alpha = 0.05)
#'
#' # Apply Percentage Cap
#' make.netDir(OD_2016Q1, cap = TRUE, pct = 20)
#' }
#' @export
#'

make.netDir <- function(x, disp = FALSE, cap = FALSE, alpha = 0.003, pct = 10, carrier = FALSE){

  #-------------------------------------------------
  if(carrier == TRUE){

    netDir_all <- x %>%
      select(origin, dest, passengers, op_carrier, itin_fare, itin_yield, roundtrip) %>%
      group_by(origin, dest, op_carrier) %>%
      mutate(itin_fare = itin_fare/(1+roundtrip)) %>%
      summarise(weight = sum(passengers), fare_sd = round(sd(itin_fare), 2), itin_fare = mean(itin_fare), itin_yield = mean(itin_yield)) %>%
      mutate(fare_sd = ifelse(is.na(fare_sd), 0, fare_sd))
  }
  else{
    netDir_all <- x %>%
      select(origin, dest, passengers, op_carrier, itin_fare, itin_yield, roundtrip) %>%
      group_by(origin, dest) %>%
      mutate(itin_fare = itin_fare/(1+roundtrip)) %>%
      summarise(weight = sum(passengers), fare_sd = round(sd(itin_fare), 2), itin_fare = mean(itin_fare), itin_yield = mean(itin_yield)) %>%
      mutate(fare_sd = ifelse(is.na(fare_sd), 0, fare_sd))
  }


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
      rename(origin = from, dest = to, passengers = weight)


    # Add city name
    netDir_disp <- netDir_disp %>%
      left_join(airportCode, by = "origin") %>%
      rename(origin_city = city, origin_city_mkt_id = city_mkt_id)

    airTemp <- airportCode %>%
      rename(dest = origin, dest_city = city, dest_city_mkt_id = city_mkt_id)

    netDir_disp <- netDir_disp %>%
      left_join(airTemp, by = "dest") %>%
      select(-latitude.x, -latitude.y, -longitude.x, -longitude.y)

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
      rename(origin = from, dest = to, passengers = weight)

    # Add city name
    netDir_cap <- netDir_cap %>%
      left_join(airportCode, by = "origin") %>%
      rename(origin_city = city, origin_city_mkt_id = city_mkt_id)

    airtemp <- airportCode %>%
      rename(dest = origin, dest_city = city, dest_city_mkt_id = city_mkt_id)

    netDir_cap <- netDir_cap %>%
      left_join(airtemp, by = "dest") %>%
      select(-latitude.x, -latitude.y, -longitude.x, -longitude.y)

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
      left_join(airportCode, by = "origin") %>%
      rename(origin_city = city, origin_city_mkt_id = city_mkt_id, passengers = weight)

    airtemp <- airportCode %>%
      rename(dest = origin, dest_city = city, dest_city_mkt_id = city_mkt_id)

    netDir_all <- netDir_all %>%
      left_join(airtemp, by = "dest") %>%
      select(-latitude.x, -latitude.y, -longitude.x, -longitude.y)


    return(list(gDir = gDir, netDir = netDir_all, nodes = nodes))


  }

}

# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
                            # End of netDir command #
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
