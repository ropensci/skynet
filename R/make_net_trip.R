#' Trip directed network
#'
#' Generates Trip/Route based Directed Network with an iGraph \strong{gDir} object,
#' a Data Frame \strong{netDir} and a Data Frame
#' with Airport/Nodes statistics \strong{nodes}.
#' Returns type of trip:
#' OD = Origin/Destination pair,
#' OT = Origin/Transfer pair,
#' TT = Transfer/Transfer pair,
#' TD = Transfer/Destination pair
#'
#'
#' @param x Data frame
#' @param carrier Groups data per carrier and OD
#'
#' @examples
#' \dontrun{
#' make_net_trip(OD_Sample)
#' }
#' @export
#'

make_net_trip <- function(x, carrier = FALSE){

  if(carriers ==  TRUE){
    df <- x %>%
      select(itin_id,mkt_id, seq_num, origin, dest, passengers, op_carrier, year, quarter,
             itin_yield, distance)

    DT <- as.data.table(df)
    DT <- DT[order(mkt_id, seq_num)]
    DT <- DT[, ':='(row_n = .N, row = 1:.N), by = .(mkt_id)]
    DT <- DT[, route_type := c("OD", "OT", "TD", "TT")[1 * (row == 1 & row_n == 1) +
                                                         2 * (row == 1 & row_n != 1) +
                                                         3 * (row == row_n & row_n != 1) +
                                                         4 * (row != 1 & row != row_n)]]
    df <- DT %>%
      as_tibble() %>%
      group_by(origin, dest, op_carrier, year, quarter, route_type) %>%
      mutate(itin_fare = itin_yield*distance) %>%
      summarise(weight = sum(passengers), fare_sd = round(sd(itin_fare), 2),
                itin_fare = round(mean(itin_fare), 2),
                itin_yield = mean(itin_yield), distance = mean(distance)) %>%
      mutate(fare_sd = ifelse(is.na(fare_sd), 0, fare_sd)) %>%
      select(-year, -quarter, everything())
  }

  if(carriers == FALSE){
    df <- x %>%
      select(itin_id,mkt_id, seq_num, origin, dest, passengers, year, quarter,
             itin_yield, distance)

    DT <- as.data.table(df)
    DT <- DT[order(mkt_id, seq_num)]
    DT <- DT[, ':='(row_n = .N, row = 1:.N), by = .(mkt_id)]
    DT <- DT[, route_type := c("OD", "OT", "TD", "TT")[1 * (row == 1 & row_n == 1) +
                                                         2 * (row == 1 & row_n != 1) +
                                                         3 * (row == row_n & row_n != 1) +
                                                         4 * (row != 1 & row != row_n)]]
    df <- DT %>%
      as_tibble() %>%
      group_by(origin, dest, year, quarter, route_type) %>%
      mutate(itin_fare = itin_yield*distance) %>%
      summarise(weight = sum(passengers), fare_sd = round(sd(itin_fare), 2),
                itin_fare = round(mean(itin_fare), 2),
                itin_yield = mean(itin_yield), distance = mean(distance)) %>%
      mutate(fare_sd = ifelse(is.na(fare_sd), 0, fare_sd)) %>%
      select(-year, -quarter, everything())

  }

  # Create nodes
  nodes <- node_stats(x)

  # Create directed network
  gDir <- graph_from_data_frame(df,
                                directed = FALSE, vertices = nodes)

  # Add city name
  df <- df %>%
    left_join(airportCode, by = "origin") %>%
    rename(origin_city = city,
           origin_city_mkt_id = city_mkt_id, passengers = weight)

  airtemp <- airportCode %>%
    rename(dest = origin, dest_city = city, dest_city_mkt_id = city_mkt_id)

  df <- df %>%
    left_join(airtemp, by = "dest") %>%
    select(-latitude.x, -latitude.y, -longitude.x, -longitude.y)

  netlist <- list(gDir = gTrip, netTrip = df, nodes = nodes)
  class(netlist) <- "skynet"

  return(netlist)
}


globalVariables(c("make_net_trip", "df", "DT", "row_n", "row", "route_type",
                  "itin_id", "gTrip"))
