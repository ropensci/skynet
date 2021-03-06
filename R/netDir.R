#' Directed network
#'
#' Generates Directed Network with an iGraph \strong{gDir} object,
#' a Data Frame \strong{netDir} and a Data Frame
#' with Airport/Nodes statistics \strong{nodes}.
#'
#'
#' @param x Data frame
#' @param disp Uses the Serrano's disparity filter (\url{https://en.wikipedia.org/wiki/Disparity_filter_algorithm_of_weighted_network})
#' to extract the backbone of the network.
#' @param alpha Argument for disparity filter.
#' @param cap Filters original data based on the edge weight.
#' @param pct Argument for cap filter. Value should be imput as percentage.
#' @param carrier Groups data per carrier and OD
#' @param metro Groups data by metropolitan area
#'
#' @examples
#' \dontrun{
#' make_net_dir(OD_Sample)
#'
#' # Apply Disparity Filter
#' make_net_dir(OD_Sample, disp = TRUE, alpha = 0.05)
#'
#' # Apply Percentage Cap
#' make_net_dir(OD_Sample, cap = TRUE, pct = 20)
#' }
#' @export
#'

make_net_dir <- function(x, disp = FALSE, alpha = 0.003,
                        cap = FALSE, pct = 10,
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
  if(carrier == TRUE){

    netDir_all <- x %>%
      select(origin, dest, passengers, op_carrier, itin_yield, distance, year, quarter) %>%
      group_by(origin, dest, op_carrier, year, quarter) %>%
      mutate(itin_fare = itin_yield*distance) %>%
      summarise(weight = sum(passengers), fare_sd = round(sd(itin_fare), 2),
                itin_fare = round(mean(itin_fare), 2),
                itin_yield = mean(itin_yield), distance = mean(distance)) %>%
      mutate(fare_sd = ifelse(is.na(fare_sd), 0, fare_sd)) %>%
      select(-year, -quarter, everything())

    }else{
    netDir_all <- x %>%
      select(origin, dest, passengers, itin_yield, distance, year, quarter) %>%
      group_by(origin, dest, year, quarter) %>%
      mutate(itin_fare = itin_yield*distance) %>%
      summarise(weight = sum(passengers), fare_sd = round(sd(itin_fare), 2),
                itin_fare = round(mean(itin_fare), 2),
                itin_yield = mean(itin_yield), distance = mean(distance)) %>%
      mutate(fare_sd = ifelse(is.na(fare_sd), 0, fare_sd)) %>%
      select(-year, -quarter, everything())
  }


  #-------------------------------------------------

    if(metro == FALSE){
      nodes <- node_stats(x)
    }else{  # Metro option
      nodes <- nodeStatsMetro(x)
}
  gDir <- igraph::graph_from_data_frame(netDir_all,
                                        directed = TRUE, vertices = nodes)

  #-------------------------------------------------

  if(disp == TRUE){
    # Run disparity filter

    # Create igraph
    gDir_disp <- disparity_filter(gDir, alpha = alpha)
    netDir_disp <- get.data.frame(gDir_disp)

    netDir_disp <- netDir_disp %>%
      rename(origin = from, dest = to, passengers = weight)


    if(metro == FALSE){

      # Add city name
      netDir_disp <- netDir_disp %>%
        left_join(airportCode, by = "origin") %>%
        rename(origin_city = city, origin_city_mkt_id = city_mkt_id)

      airTemp <- airportCode %>%
        rename(dest = origin, dest_city = city, dest_city_mkt_id = city_mkt_id)

      netDir_disp <- netDir_disp %>%
        left_join(airTemp, by = "dest") %>%
        select(-latitude.x, -latitude.y, -longitude.x, -longitude.y)

    }else{ # Metro Option

      netDir_disp <- netDir_disp %>%
      left_join(MetroLookup, by = "origin") %>%
      rename(origin_city = description)

    MetroTemp <- MetroLookup %>%
      rename(dest = origin, dest_city = description)

    netDir_disp <- netDir_disp %>%
      left_join(MetroTemp, by = "dest")

    }

     nodes <- as.data.frame(get.vertex.attribute(gDir_disp))
     nodes <- rename(nodes, airport = name)

     netlist <- list(gDir_disp = gDir_disp,netDir_disp = netDir_disp,
                     nodes = nodes)
     class(netlist) <- "skynet"
     return(netlist)

    # ------------------------------------------------------------- #
                     # End of disp filter command #
    # ------------------------------------------------------------- #

  }else if(cap == TRUE){

    # Applies 10% cap
    gDir_cap <- graph_from_data_frame(netDir_all,
                                      directed = TRUE, vertices = nodes)

    gDir_cap <- subgraph.edges(gDir_cap,
                     which(E(gDir_cap)$weight > quantile(E(gDir_cap)$weight,
                     prob = 1-pct/100)), delete.vertices = TRUE)

    #Creates Dataframe from graph
    netDir_cap <- igraph::as_data_frame(gDir_cap)

    netDir_cap <- netDir_cap %>%
      rename(origin = from, dest = to, passengers = weight)

    if(metro == FALSE){

      # Add city name
      netDir_cap <- netDir_cap %>%
        left_join(airportCode, by = "origin") %>%
        rename(origin_city = city, origin_city_mkt_id = city_mkt_id)

      airtemp <- airportCode %>%
        rename(dest = origin, dest_city = city, dest_city_mkt_id = city_mkt_id)

      netDir_cap <- netDir_cap %>%
        left_join(airtemp, by = "dest") %>%
        select(-latitude.x, -latitude.y, -longitude.x, -longitude.y)

      }else{ # Metro Option

    netDir_cap <- netDir_cap %>%
      left_join(MetroLookup, by = "origin") %>%
      rename(origin_city = description)

    MetroTemp <- MetroLookup %>%
      rename(dest = origin, dest_city = description)

    netDir_cap <- netDir_cap %>%
      left_join(MetroTemp, by = "dest")
    }

     nodes <- as.data.frame(get.vertex.attribute(gDir_cap))
     nodes <- rename(nodes, airport = name)

    netlist <- list(gDir_cap = gDir_cap,
                    netDir_cap = netDir_cap, nodes = nodes)
    class(netlist) <- "skynet"
    return(netlist)

    # --------------------------------------------------------------------- #
                           # End of 10% filter command #
    # --------------------------------------------------------------------- #


  }else{

    # Runs network with full data
    gDir <- graph_from_data_frame(netDir_all, directed = TRUE, vertices = nodes)


    if(metro == FALSE){ # Metro Option

      # Add city name
      netDir_all <- netDir_all %>%
        left_join(airportCode, by = "origin") %>%
        rename(origin_city = city,
               origin_city_mkt_id = city_mkt_id, passengers = weight)

      airtemp <- airportCode %>%
        rename(dest = origin, dest_city = city, dest_city_mkt_id = city_mkt_id)

      netDir_all <- netDir_all %>%
        left_join(airtemp, by = "dest") %>%
        select(-latitude.x, -latitude.y, -longitude.x, -longitude.y)

    }else{

      netDir_all <- netDir_all %>%
      left_join(MetroLookup, by = "origin") %>%
      rename(origin_city = description)

    MetroTemp <- MetroLookup %>%
      rename(dest = origin, dest_city = description)

    netDir_all <- netDir_all %>%
      left_join(MetroTemp, by = "dest")

    }

    netlist <- list(gDir = gDir, netDir = netDir_all, nodes = nodes)
    class(netlist) <- "skynet"

    return(netlist)


  }

}

make.netDir <- function(...){
  warning(paste("make.netDir is deprecated, use make_net_dir(), instead."))
  do.call(make_net_dir, list(...))
}


globalVariables(c("op_carrier", "itin_fare", "itin_yield", "roundtrip",
                  "sd", "fare_sd", "city_mkt_id", "latitude.x",
                  "latitude.x", "longitude.x", "longitude.y",
                  "quantile", "distance", "MetroLookup", "origin_mkt_id",
                  "dest_mkt_id", "name", "year", "quarter"))

# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
                            # End of netDir command #
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
