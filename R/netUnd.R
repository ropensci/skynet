#' Undirected Network
#'
#' Generates Undirected Network with an iGraph object and a Data Frame.
#'
#' @param x Data frame
#' @param disp Uses the Serrano's disparity filter (\url{https://en.wikipedia.org/wiki/Disparity_filter_algorithm_of_weighted_network})
#' to extract the backbone of the network.
#' @param alpha Argument for disparity filter.
#' @param cap Filters original data based on the edge weight.
#' @param pct Argument for cap filter. Value should be imput as percentage.
#' @param merge When set to FALSE, it keeps parallel edges instead of collapsing them
#' and summing their weights.
#' @param carrier Groups data per carrier and OD
#' @param metro Groups data by metropolitan area
#'
#' @examples
#' \dontrun{
#' make.netUnd(OD_Sample)
#'
#' # Apply Disparity Filter
#' make.netUnd(OD_Sample, disp = TRUE, alpha = 0.05)
#'
#' # Apply Percentage Cap
#' make.netUnd(OD_Sample, cap = TRUE, pct = 20)
#' }
#'
#' @export

make.netUnd <- function(x, disp = FALSE, cap = FALSE,
                        merge = TRUE, alpha = 0.003, pct = 10,
                        carrier = FALSE, metro = FALSE){

  if(carrier == TRUE & disp == TRUE){

    stop("SKYNET doesn't support yet parallel edges on its disparity filter.
         Not including the carrier option on the disparity filter mode,
         or running the carriers option without the disparity filter mode,
         solves the issue for now.")
  }

  if(metro == TRUE){# Metro option
    x <- x %>%
      select(-origin, -dest) %>%
      rename(origin = origin_mkt_id, dest = dest_mkt_id) %>%
      mutate(origin = as.character(origin), dest = as.character(dest))
  }

  #-------------------------------------------------
  if(carrier == TRUE & merge == FALSE){

    netUnd_all <- x %>%
      select(origin, dest, passengers, op_carrier, itin_yield, distance) %>%
      group_by(origin, dest, op_carrier) %>%
      mutate(itin_fare = itin_yield*distance) %>%
      summarise(weight = sum(passengers), fare_sd = round(sd(itin_fare), 2),
                itin_fare = round(mean(itin_fare), 2),
                itin_yield = mean(itin_yield), distance = mean(distance)) %>%
      mutate(fare_sd = ifelse(is.na(fare_sd), 0, fare_sd))
  }

    if(carrier == TRUE & merge == TRUE){
      nodes <- nodeStats(x)
      netUnd_all <- x %>%
        select(origin, dest, passengers, op_carrier, itin_yield, distance) %>%
        graph_from_data_frame(directed = FALSE, vertices = nodes) %>%
        get.data.frame() %>%
        group_by(from, to, op_carrier) %>%
        mutate(itin_fare = itin_yield*distance) %>%
        summarise(weight = sum(passengers), fare_sd = round(sd(itin_fare), 2),
                  itin_fare = round(mean(itin_fare), 2),
                  itin_yield = mean(itin_yield), distance = mean(distance)) %>%
        mutate(fare_sd = ifelse(is.na(fare_sd), 0, fare_sd))
  }
  else{

   netUnd_all <- x %>%
    select(origin, dest, passengers, itin_yield, distance) %>%
    group_by(origin, dest) %>%
    mutate(itin_fare = itin_yield*distance) %>%
    summarise(weight = sum(passengers), fare_sd = round(sd(itin_fare), 2),
               itin_fare = round(mean(itin_fare), 2),
               itin_yield = mean(itin_yield), distance = mean(distance)) %>%
    mutate(fare_sd = ifelse(is.na(fare_sd), 0, fare_sd))

  }


  #-------------------------------------------------

  if(metro == FALSE){
    nodes <- nodeStats(x)
  }else{  # Metro option
    nodes <- nodeStatsMetro(x)
  }

  if(merge == FALSE){
    gUnd <- graph_from_data_frame(netUnd_all,
                                  directed = FALSE, vertices = nodes)
  }
  if(merge == TRUE & carrier == TRUE){
    gUnd <- graph_from_data_frame(netUnd_all,
                                  directed = FALSE, vertices = nodes)
  }else{

  gUnd <- graph_from_data_frame(netUnd_all,
                                directed = TRUE, vertices = nodes)

  gUnd <- as.undirected(gUnd, mode = "collapse",
                        edge.attr.comb=list(weight = "sum",
                                            itin_fare = "mean",
                                            itin_yield = "mean",
                                            fare_sd = "mean",
                                            distance = "mean"))
  }
    if(disp == TRUE){

    # Run disparity filter
    # Creates igraph object
    gUnd_disp <- dispfilter(gUnd, alpha = alpha)
    netUnd_disp <- get.data.frame(gUnd_disp)

    # Rename fields
    netUnd_disp <- netUnd_disp %>%
      rename(origin = from, dest = to, passengers = weight)

    if(metro == FALSE){

    # Add city name
    netUnd_disp <- netUnd_disp %>%
      left_join(airportCode, by = "origin") %>%
      rename(origin_city = city, origin_city_mkt_id = city_mkt_id)

    airtemp <- airportCode %>%
      rename(dest = origin, dest_city = city, dest_city_mkt_id = city_mkt_id)

    netUnd_disp <- netUnd_disp %>%
      left_join(airtemp, by = "dest") %>%
      select(-latitude.x, -latitude.y, -longitude.x, -longitude.y)

     }else{ # Metro Option

      netUnd_disp <- netUnd_disp %>%
        left_join(MetroLookup, by = "origin") %>%
        rename(origin_city = description)

      MetroTemp <- MetroLookup %>%
        rename(dest = origin, dest_city = description)

      netUnd_disp <- netUnd_disp %>%
        left_join(MetroTemp, by = "dest") %>%
        select(-latitude.x, -latitude.y, -longitude.x, -longitude.y)

    }

    nodes <- as.data.frame(get.vertex.attribute(gUnd_disp))

    return(list(gUnd_disp = gUnd_disp,
                netUnd_disp = netUnd_disp, nodes = nodes))

    # --------------------------------------------------------------- #
                           # End of dispfilter command #
    # --------------------------------------------------------------- #


  }else if(cap == TRUE){

    #Run 10% cap
    gUnd_cap <- gUnd
    gUnd_cap <- subgraph.edges(gUnd_cap,
                    which(E(gUnd_cap)$weight > quantile(E(gUnd_cap)$weight,
                    prob = 1-pct/100)), delete.vertices = TRUE)

    # Create datafram based on collapsed edges graph
    netUnd_cap <- igraph::as_data_frame(gUnd_cap)

    netUnd_cap <- netUnd_cap %>%
      rename(origin = from, dest = to, passengers = weight)

    if(metro == FALSE){

    # Add city name
    netUnd_cap <- netUnd_cap %>%
      left_join(airportCode, by = "origin") %>%
      rename(origin_city = city, origin_city_mkt_id = city_mkt_id)

    airtemp <- airportCode %>%
      rename(dest = origin, dest_city = city, dest_city_mkt_id = city_mkt_id)

    netUnd_cap <- netUnd_cap %>%
      left_join(airtemp, by = "dest") %>%
      select(-latitude.x, -latitude.y, -longitude.x, -longitude.y)

    }else{ # Metro Option

      netUnd_cap <- netUnd_cap %>%
        left_join(MetroLookup, by = "origin") %>%
        rename(origin_city = description)

      MetroTemp <- MetroLookup %>%
        rename(dest = origin, dest_city = description)

      netUnd_cap <- netUnd_cap %>%
        left_join(MetroTemp, by = "dest") %>%
        select(-latitude.x, -latitude.y, -longitude.x, -longitude.y)
    }

    nodes <- as.data.frame(get.vertex.attribute(gUnd_cap))

    return(list(gUnd_cap = gUnd_cap, netUnd_cap = netUnd_cap, nodes = nodes))

    # --------------------------------------------------------------- #
    # End of 10% filter command #
    # --------------------------------------------------------------- #

  }else{

    # Create dataframe based on collapsed edges graph
    netUnd_all <- igraph::as_data_frame(gUnd)

    netUnd_all <- netUnd_all %>%
      rename(origin = from, dest = to, passengers = weight)

    if(metro == FALSE){

    # Add city name
    netUnd_all <- netUnd_all %>%
      left_join(airportCode, by = "origin") %>%
      rename(origin_city = city, origin_city_mkt_id = city_mkt_id)

    airtemp <- airportCode %>%
      rename(dest = origin, dest_city = city, dest_city_mkt_id = city_mkt_id)

    netUnd_all <- netUnd_all %>%
      left_join(airtemp, by = "dest") %>%
      select(-latitude.x, -latitude.y, -longitude.x, -longitude.y)

    }else{ #Metro Option

      netUnd_all <- netUnd_all %>%
        left_join(MetroLookup, by = "origin") %>%
        rename(origin_city = description)

      MetroTemp <- MetroLookup %>%
        rename(dest = origin, dest_city = description)

      netUnd_all <- netUnd_all %>%
        left_join(MetroTemp, by = "dest") %>%
        select(-latitude.x, -latitude.y, -longitude.x, -longitude.y)

    }

    return(list(gUnd = gUnd, netUnd = netUnd_all, nodes = nodes))


  }
}


# --------------------------------------------------------------- #
# --------------------------------------------------------------- #
                     # End of netUnd command #
# --------------------------------------------------------------- #
# --------------------------------------------------------------- #
