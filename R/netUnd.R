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
#' @param carrier Groups data per carrier and OD
#'
#' @examples
#' \dontrun{
#' make.netUnd(OD_2016Q1)
#'
#' # Apply Disparity Filter
#' make.netUnd(OD_2016Q1, disp = TRUE, alpha = 0.05)
#'
#' # Apply Percentage Cap
#' make.netUnd(OD_2016Q1, cap = TRUE, pct = 20)
#' }
#'
#' @export

make.netUnd <- function(x, disp = FALSE, cap = FALSE, merge = TRUE, alpha = 0.003, pct = 10, carrier = FALSE){

  if(carrier == TRUE & disp == TRUE){

    stop("SKYNET doesn't support yet parallel edges on its disparity filter.
         Not including the carrier option on the disparity filter mode, or running the carriers option without the disparity filter mode, solves the issue for now.")
  }

  #-------------------------------------------------
  if(carrier == TRUE){

    netUnd_all <- x %>%
      select(origin, dest, passengers, op_carrier, itin_fare, itin_yield, roundtrip) %>%
      group_by(origin, dest, op_carrier) %>%
      mutate(itin_fare = itin_fare/(1+roundtrip)) %>%
      summarise(weight = sum(passengers), fare_sd = round(sd(itin_fare), 2),
                itin_fare = round(mean(itin_fare), 2),
                itin_yield = round(mean(itin_yield), 2)) %>%
      mutate(fare_sd = ifelse(is.na(fare_sd), 0, fare_sd))

  }
  else{

   netUnd_all <- x %>%
    select(origin, dest, passengers, op_carrier, itin_fare, itin_yield, roundtrip) %>%
    group_by(origin, dest) %>%
    mutate(itin_fare = itin_fare/(1+roundtrip)) %>%
    summarise(weight = sum(passengers), fare_sd = round(sd(itin_fare), 2),
               itin_fare = round(mean(itin_fare), 2),
               itin_yield = round(mean(itin_yield), 2)) %>%
    mutate(fare_sd = ifelse(is.na(fare_sd), 0, fare_sd))

  }


  #-------------------------------------------------

  nodes <- createNodes(x)

  gUnd <- graph_from_data_frame(netUnd_all, directed = TRUE, vertices = nodes)
  gUnd <- as.undirected(gUnd, mode = "collapse", edge.attr.comb=list(weight = "sum", itin_fare = "mean", itin_yield = "mean", fare_sd = "mean"))

    if(disp == TRUE){

    # Run disparity filter
    # Creates igraph object
    gUnd_disp <- dispfilter(gUnd, alpha = alpha)
    netUnd_disp <- get.data.frame(gUnd_disp)

    # Rename fields
    netUnd_disp <- netUnd_disp %>%
      rename(origin = from, dest = to, passengers = weight)

    # Add city name
    netUnd_disp <- netUnd_disp %>%
      left_join(airportCode, by = "origin") %>%
      rename(origin_city = city, origin_city_mkt_id = city_mkt_id)

    airtemp <- airportCode %>%
      rename(dest = origin, dest_city = city, dest_city_mkt_id = city_mkt_id)

    netUnd_disp <- netUnd_disp %>%
      left_join(airtemp, by = "dest") %>%
      select(-latitude.x, -latitude.y, -longitude.x, -longitude.y)

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
      rename(origin = from, dest = to, passengers = weight)

    # Add city name
    netUnd_cap <- netUnd_cap %>%
      left_join(airportCode, by = "origin") %>%
      rename(origin_city = city, origin_city_mkt_id = city_mkt_id)

    airtemp <- airportCode %>%
      rename(dest = origin, dest_city = city, dest_city_mkt_id = city_mkt_id)

    netUnd_cap <- netUnd_cap %>%
      left_join(airtemp, by = "dest") %>%
      select(-latitude.x, -latitude.y, -longitude.x, -longitude.y)

    nodes <- createNodes(netUnd_cap)

    return(list(gUnd_cap = gUnd_cap, netUnd_cap = netUnd_cap, nodes = nodes))

    # ----------------------------------------------------------------------------- #
    # End of 10% filter command #
    # ----------------------------------------------------------------------------- #


  }else if(merge == FALSE){

    # Run undirected with merge
    netUnd_all <- x %>%
      select(origin, dest, passengers) %>%
      group_by(origin, dest) %>%
      summarise(weight = sum(passengers))

    nodes <- createNodes(x)

    gUnd <- graph_from_data_frame(netUnd_all, directed = FALSE, vertices = nodes)

    return(list(netUnd = netUnd_all, gUnd = gUnd, nodes = nodes))

  }else{

    # Create dataframe based on collapsed edges graph
    netUnd_all <- igraph::as_data_frame(gUnd)

    netUnd_all <- netUnd_all %>%
      rename(origin = from, dest = to, passengers = weight)

    # Add city name
    netUnd_all <- netUnd_all %>%
      left_join(airportCode, by = "origin") %>%
      rename(origin_city = city, origin_city_mkt_id = city_mkt_id)

    airtemp <- airportCode %>%
      rename(dest = origin, dest_city = city, dest_city_mkt_id = city_mkt_id)

    netUnd_all <- netUnd_all %>%
      left_join(airtemp, by = "dest") %>%
      select(-latitude.x, -latitude.y, -longitude.x, -longitude.y)


    nodes <- createNodes(netUnd_all)

    return(list(gUnd = gUnd, netUnd = netUnd_all, nodes = nodes))


  }
}


# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
                            # End of netUnd command #
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
